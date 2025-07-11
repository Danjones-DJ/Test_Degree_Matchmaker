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
                  
                  hr(),
                  
                  h4("Filter Options"),
                  checkboxGroupInput("degree_types", 
                                     "Degree Types:",
                                     choices = list(
                                       "BSc" = "BSc",
                                       "BA" = "BA", 
                                       "MEng" = "MEng",
                                       "MSci" = "MSci",
                                       "BEng" = "BEng",
                                       "LLB" = "LLB",
                                       "MBBS" = "MBBS"
                                     ),
                                     selected = c("BSc", "BA", "MEng", "MSci", "BEng")
                  ),
                  
                  checkboxInput("show_salary", "Show Salary Information", value = TRUE),
                  
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
                      
                      # Add salary summary box if salary data is shown
                      conditionalPanel(
                        condition = "input.show_salary == true",
                        div(
                          h4("Salary Overview"),
                          verbatimTextOutput("salarySummary"),
                          br()
                        )
                      ),
                      
                      h4("Matching Degrees"),
                      DT::dataTableOutput("resultsTable")
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
  
  # Helper functions
  options(readr.show_col_types = FALSE)
  
  degree_tree <- function(subject) {
    library(tidyverse)
    
    str <- as.character(subject)
    
    all_sets <- read_csv("https://raw.githubusercontent.com/Danjones-DJ/Degree-Matchmaker_DJ/refs/heads/main/ucl_pcm_degree_v3.csv")
    
    # Select all relevant columns including salary data
    all_sets <- all_sets %>%
      select(any_of(c("title", "degree_type", "a_level", "gcse", "a_level_subjects", "url",
                      "lower_quartile_salary", "median_salary", "upper_quartile_salary"))) %>%
      filter(str_detect(title, regex(str, ignore_case = TRUE))) %>%
      arrange(title)
    
    return(all_sets)
  }
  
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
    grades <- str_extract_all(a_level_req, "A\\*|[A-E]")[[1]]
    sum(sapply(grades, grade_to_score))
  }
  
  calculate_student_score <- function(student_grades) {
    sum(sapply(student_grades, grade_to_score))
  }
  
  find_matching_degrees <- function(student_grades, subjects_of_interest, selected_degree_types) {
    student_score <- calculate_student_score(student_grades)
    
    all_degrees <- map_dfr(subjects_of_interest, ~{
      tryCatch({
        degree_tree(.x) %>%
          mutate(search_subject = .x)
      }, error = function(e) {
        tibble()
      })
    })
    
    if (nrow(all_degrees) == 0) {
      return(tibble(message = "No degrees found for the specified subjects"))
    }
    
    matching_degrees <- all_degrees %>%
      # Filter by selected degree types
      filter(degree_type %in% selected_degree_types) %>%
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
        )
      ) %>%
      select(
        title, degree_type, a_level, match_quality, grade_difference,
        a_level_subjects, gcse, url, search_subject,
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
    
    # Get subjects and degree types
    subjects <- input$subjects
    degree_types <- input$degree_types
    
    if (length(subjects) == 0) {
      return(list(error = "Please select at least one subject of interest."))
    }
    
    if (length(degree_types) == 0) {
      return(list(error = "Please select at least one degree type."))
    }
    
    # Find matches
    matches <- find_matching_degrees(grades, subjects, degree_types)
    
    return(list(
      matches = matches,
      grades = grades,
      subjects = subjects,
      degree_types = degree_types,
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
      "Degree types: ", paste(result$degree_types, collapse = ", "), "\n",
      "Found ", nrow(result$matches), " matching degrees!"
    )
  })
  
  # Salary summary output
  output$salarySummary <- renderText({
    result <- results()
    
    if (!is.null(result$error) || nrow(result$matches) == 0) {
      return("No salary data available")
    }
    
    # Calculate salary statistics
    salary_data <- result$matches %>%
      filter(!is.na(median_salary)) %>%
      summarise(
        min_median = min(median_salary, na.rm = TRUE),
        max_median = max(median_salary, na.rm = TRUE),
        avg_median = mean(median_salary, na.rm = TRUE),
        count_with_salary = n()
      )
    
    if (nrow(salary_data) == 0 || salary_data$count_with_salary == 0) {
      return("No salary data available for matching degrees")
    }
    
    paste0(
      "Salary Range (Median): £", format(salary_data$min_median, big.mark = ","), 
      " - £", format(salary_data$max_median, big.mark = ","), "\n",
      "Average Median Salary: £", format(round(salary_data$avg_median), big.mark = ","), "\n",
      "Degrees with salary data: ", salary_data$count_with_salary, " of ", nrow(result$matches)
    )
  })
  
  # Results table output
  output$resultsTable <- DT::renderDataTable({
    result <- results()
    
    if (!is.null(result$error) || nrow(result$matches) == 0) {
      return(NULL)
    }
    
    # Prepare the matches data
    matches <- result$matches %>%
      mutate(
        url = paste0('<a href="', url, '" target="_blank">📋 View Details</a>'),
        a_level_subjects = str_trunc(a_level_subjects, 80)
      )
    
    # Create different column selections based on salary checkbox
    if (input$show_salary) {
      matches <- matches %>%
        mutate(
          # Format salary columns
          lower_quartile_salary = ifelse(is.na(lower_quartile_salary), "N/A", 
                                         paste0("£", format(lower_quartile_salary, big.mark = ","))),
          median_salary = ifelse(is.na(median_salary), "N/A", 
                                 paste0("£", format(median_salary, big.mark = ","))),
          upper_quartile_salary = ifelse(is.na(upper_quartile_salary), "N/A", 
                                         paste0("£", format(upper_quartile_salary, big.mark = ",")))
        ) %>%
        select(
          `Degree Title` = title,
          `Type` = degree_type,
          `Required` = a_level,
          `Match Quality` = match_quality,
          `Subject Area` = search_subject,
          `Subject Requirements` = a_level_subjects,
          `Lower Quartile Salary` = lower_quartile_salary,
          `Median Salary` = median_salary,
          `Upper Quartile Salary` = upper_quartile_salary,
          `Details` = url
        )
    } else {
      matches <- matches %>%
        select(
          `Degree Title` = title,
          `Type` = degree_type,
          `Required` = a_level,
          `Match Quality` = match_quality,
          `Subject Area` = search_subject,
          `Subject Requirements` = a_level_subjects,
          `Details` = url
        )
    }
    
    # Create datatable with conditional column widths
    column_defs <- if (input$show_salary) {
      list(
        list(width = "150px", targets = 0),    # Degree Title
        list(width = "60px", targets = 1),     # Type
        list(width = "80px", targets = 2),     # Required
        list(width = "80px", targets = 3),     # Match Quality
        list(width = "100px", targets = 4),    # Subject Area
        list(width = "120px", targets = 5),    # Subject Requirements
        list(width = "80px", targets = 6:8),   # Salary columns
        list(width = "60px", targets = 9)      # Details
      )
    } else {
      list(
        list(width = "200px", targets = 0),
        list(width = "100px", targets = 1:3),
        list(width = "150px", targets = 4:5),
        list(width = "80px", targets = 6)
      )
    }
    
    DT::datatable(
      matches,
      escape = FALSE,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        order = list(list(3, "desc")),
        columnDefs = column_defs
      ),
      caption = "Degrees you can get into - sorted by how well you exceed requirements!"
    ) %>%
      DT::formatStyle(
        "Match Quality",
        backgroundColor = DT::styleEqual(
          c("Strong Match", "Good Match", "Okay Match"),
          c("#277a04", "#7bc75b", "#afcca3")
        )
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
