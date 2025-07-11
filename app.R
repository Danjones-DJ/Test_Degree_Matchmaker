library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(plotly)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Degree Matcher (UCL Edition)"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Find Your Degree", tabName = "matcher", icon = icon("graduation-cap"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .box {
          margin-bottom: 20px;
        }
        .grade-input {
          margin-bottom: 10px;
        }
      "))
    ),
    
    tabItems(
      tabItem(tabName = "matcher",
              fluidRow(
                box(
                  title = "Your Academic Profile", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 4,
                  
                  h4("What subjects are you interested in?"),
                  checkboxGroupInput("subjects", 
                                     label = NULL,
                                     choices = list(
                                       "Mathematics" = "Mathematics",
                                       "Statistics" = "Statistics", 
                                       "Data Science" = "Data Science",
                                       "Economics" = "Economics",
                                       "Computer Science" = "Computer Science",
                                       "Physics" = "Physics",
                                       "Engineering" = "Engineering",
                                       "Business" = "Business",
                                       "Psychology" = "Psychology",
                                       "Biology" = "Biology",
                                       "Chemistry" = "Chemistry",
                                       "Geography" = "Geography",
                                       "History" = "History",
                                       "English" = "English",
                                       "Art" = "Art"
                                     ),
                                     selected = c("Mathematics", "Statistics", "Data Science", "Economics")
                  ),
                  
                  hr(),
                  
                  h4("Your A-Level Grades"),
                  p("Enter your achieved or predicted grades:"),
                  
                  fluidRow(
                    column(6, 
                           selectInput("grade1", "Subject 1:", 
                                       choices = c("A*", "A", "B", "C", "D", "E"), 
                                       selected = "A")
                    ),
                    column(6,
                           selectInput("grade2", "Subject 2:", 
                                       choices = c("A*", "A", "B", "C", "D", "E"), 
                                       selected = "A")
                    )
                  ),
                  
                  fluidRow(
                    column(6,
                           selectInput("grade3", "Subject 3:", 
                                       choices = c("A*", "A", "B", "C", "D", "E"), 
                                       selected = "A")
                    ),
                    column(6,
                           selectInput("grade4", "Subject 4 (Optional):", 
                                       choices = c("None", "A*", "A", "B", "C", "D", "E"), 
                                       selected = "None")
                    )
                  ),
                  
                  br(),
                  actionButton("findDegrees", "🔍 Find Matching Degrees", 
                               class = "btn-primary btn-lg btn-block")
                ),
                
                box(
                  title = "Your Results", 
                  status = "success", 
                  solidHeader = TRUE,
                  width = 8,
                  
                  conditionalPanel(
                    condition = "input.findDegrees == 0",
                    div(
                      style = "text-align: center; padding: 50px;",
                      h3("Welcome to Degree Matcher!"),
                      p("Select your subjects of interest and enter your A-Level grades to find degrees you can get into."),
                      icon("arrow-left", class = "fa-2x")
                    )
                  ),
                  
                  conditionalPanel(
                    condition = "input.findDegrees > 0",
                    div(
                      h4("Your Academic Profile"),
                      verbatimTextOutput("profileSummary"),
                      br(),
                      h4("Matching Degrees"),
                      DT::dataTableOutput("resultsTable"),
                      br(),
                      h4("Salary Information"),
                      plotlyOutput("salaryPlot")
                    )
                  )
                )
              )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Load the UCL dataset
  ucl_data <- reactive({
    url <- "https://raw.githubusercontent.com/Danjones-DJ/Degree-Matchmaker_DJ/refs/heads/main/ucl_pcm_degree_v3"
    read_csv(url) %>%
      select(
        title, degree_type, a_level, a_level_subjects, gcse, ib, 
        url, foundation, sandwich, yearabroad,
        lower_quartile_salary, median_salary, upper_quartile_salary
      )
  })
  
  # Helper functions
  grade_to_score <- function(grade) {
    case_when(
      grade == "A*" ~ 4,
      grade == "A" ~ 3,
      grade == "B" ~ 2,
      grade == "C" ~ 1,
      grade == "D" ~ 0.5,
      TRUE ~ 0
    )
  }
  
  calculate_requirement_score <- function(a_level_req) {
    if (is.na(a_level_req)) return(0)
    grades <- str_extract_all(a_level_req, "A\\*|A|B|C|D|E")[[1]]
    sum(sapply(grades, grade_to_score))
  }
  
  calculate_student_score <- function(student_grades) {
    sum(sapply(student_grades, grade_to_score))
  }
  
  find_matching_degrees <- function(student_grades, subjects_of_interest) {
    student_score <- calculate_student_score(student_grades)
    
    all_degrees <- ucl_data() %>%
      filter(str_detect(title, regex(paste(subjects_of_interest, collapse = "|"), ignore_case = TRUE))
             
             if (nrow(all_degrees) == 0) {
               return(tibble(message = "No degrees found for the specified subjects"))
             }
             
             matching_degrees <- all_degrees %>%
               mutate(
                 requirement_score = map_dbl(a_level, ~calculate_requirement_score(.x)),
                 meets_requirements = student_score >= requirement_score,
                 grade_difference = student_score - requirement_score
               ) %>%
               filter(meets_requirements) %>%
               arrange(desc(grade_difference), title) %>%
               mutate(
                 match_quality = case_when(
                   grade_difference >= 2 ~ "Strong Match",
                   grade_difference >= 1 ~ "Good Match", 
                   grade_difference >= 0 ~ "Okay Match"
                 ),
                 url = paste0('<a href="', url, '" target="_blank">📋 View Details</a>'),
                 a_level_subjects = str_trunc(a_level_subjects, 80)
               ) %>%
               select(
                 title, degree_type, a_level, match_quality, grade_difference,
                 a_level_subjects, gcse, url, 
                 lower_quartile_salary, median_salary, upper_quartile_salary
               )
             
             return(matching_degrees)
  }
  
  # Reactive values
  results <- eventReactive(input$findDegrees, {
    # Collect grades
    grades <- c(input$grade1, input$grade2, input$grade3)
    if (input$grade4 != "None") {
      grades <- c(grades, input$grade4)
    }
    
    # Get subjects
    subjects <- input$subjects
    
    if (length(subjects) == 0) {
      return(list(error = "Please select at least one subject of interest."))
    }
    
    # Find matches
    matches <- find_matching_degrees(grades, subjects)
    
    return(list(
      matches = matches,
      grades = grades,
      subjects = subjects,
      student_score = calculate_student_score(grades)
    ))
  })
  
  # Profile summary output
  output$profileSummary <- renderText({
    result <- results()
    
    if (!is.null(result$error)) {
      return(result$error)
    }
    
    paste0(
      "Your grades: ", paste(result$grades, collapse = ", "), "\n",
      "Total score: ", result$student_score, "\n",
      "Subjects searched: ", paste(result$subjects, collapse = ", "), "\n",
      "Found ", nrow(result$matches), " matching degrees!"
    )
  })
  
  # Results table output
  output$resultsTable <- DT::renderDataTable({
    result <- results()
    
    if (!is.null(result$error) || nrow(result$matches) == 0) {
      return(NULL)
    }
    
    matches <- result$matches %>%
      select(
        `Degree Title` = title,
        `Type` = degree_type,
        `A-Level Requirements` = a_level,
        `Match Quality` = match_quality,
        `Subject Requirements` = a_level_subjects,
        `Details` = url
      )
    
    DT::datatable(
      matches,
      escape = FALSE,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        order = list(list(3, "desc")),
        columnDefs = list(
          list(width = "200px", targets = 0),
          list(width = "100px", targets = 1:3),
          list(width = "150px", targets = 4:5)
        )
      ),
      caption = "UCL Degrees you can get into - sorted by how well you exceed requirements!"
    ) %>%
      DT::formatStyle(
        "Match Quality",
        backgroundColor = DT::styleEqual(
          c("Strong Match", "Good Match", "Okay Match"),
          c("#277a04", "#7bc75b", "#afcca3")
        )
      )
  })
  
  # Salary plot output
  output$salaryPlot <- renderPlotly({
    result <- results()
    
    if (!is.null(result$error) || nrow(result$matches) == 0) {
      return(NULL)
    }
    
    # Prepare data for plotting
    plot_data <- result$matches %>%
      arrange(desc(median_salary)) %>%
      head(20) %>%  # Limit to top 20 for better visualization
      mutate(title = fct_reorder(title, median_salary))
    
    # Create plot
    p <- plot_ly(plot_data) %>%
      add_segments(
        x = ~lower_quartile_salary, xend = ~upper_quartile_salary,
        y = ~title, yend = ~title,
        line = list(color = "gray", width = 4),
        showlegend = FALSE
      ) %>%
      add_markers(
        x = ~median_salary, y = ~title,
        marker = list(color = "#1f77b4", size = 10),
        name = "Median Salary"
      ) %>%
      layout(
        title = "Salary Range for Matching Degrees",
        xaxis = list(title = "Salary (£)"),
        yaxis = list(title = ""),
        margin = list(l = 150)  # Increase left margin for long degree names
      )
    
    p
  })
}

# Run the application
shinyApp(ui = ui, server = server)
