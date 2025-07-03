library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(plotly)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "🎯 Degree Matcher"),
  
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
                  
                  h4("📚 Select Your Subjects of Interest"),
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
                  
                  h4("📊 Your A-Level Grades"),
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
                      h3("👋 Welcome to Degree Matcher!"),
                      p("Select your subjects of interest and enter your A-Level grades to find degrees you can get into."),
                      icon("arrow-left", class = "fa-2x")
                    )
                  ),
                  
                  conditionalPanel(
                    condition = "input.findDegrees > 0",
                    div(
                      h4("📈 Your Academic Profile"),
                      verbatimTextOutput("profileSummary"),
                      br(),
                      h4("🎯 Matching Degrees"),
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
  
  # Helper functions (same as your original code)
  options(readr.show_col_types = FALSE)
  
  degree_tree <- function(subject) {
    library(tidyverse)
    
    str <- as.character(subject)
    
    pure_set <- read_csv("https://raw.githubusercontent.com/Danjones-DJ/degree-matchmaker/refs/heads/main/pure_grades.csv")
    combined_set <- read_csv("https://raw.githubusercontent.com/Danjones-DJ/degree-matchmaker/refs/heads/main/combined_grades.csv")
    minor_set <- read_csv("https://raw.githubusercontent.com/Danjones-DJ/degree-matchmaker/refs/heads/main/minor_grades.csv")
    integrated_set <- read_csv("https://raw.githubusercontent.com/Danjones-DJ/degree-matchmaker/refs/heads/main/integrated_grades.csv")
    
    all_sets <- bind_rows(pure_set, combined_set, minor_set, integrated_set)
    
    all_sets <- all_sets %>%
      select(any_of(c("title", "degree_type", "A_level", "GCSE", "A_level_subjects", "URL"))) %>%
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
  
  find_matching_degrees <- function(student_grades, subjects_of_interest) {
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
      mutate(
        requirement_score = map_dbl(A_level, ~calculate_requirement_score(.x)),
        meets_requirements = student_score >= requirement_score,
        grade_difference = student_score - requirement_score
      ) %>%
      filter(meets_requirements) %>%
      arrange(desc(grade_difference), title) %>%
      mutate(
        match_quality = case_when(
          grade_difference >= 2 ~ "🟢 Strong Match",
          grade_difference >= 1 ~ "🟡 Good Match", 
          grade_difference >= 0 ~ "🔴 Just Meets Requirements"
        )
      ) %>%
      select(
        title, degree_type, A_level, match_quality, grade_difference,
        A_level_subjects, GCSE, URL, search_subject
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
      mutate(
        URL = paste0('<a href="', URL, '" target="_blank">📋 View Details</a>'),
        A_level_subjects = str_trunc(A_level_subjects, 80)
      ) %>%
      select(
        `Degree Title` = title,
        `Type` = degree_type,
        `Required` = A_level,
        `Match Quality` = match_quality,
        `Subject Area` = search_subject,
        `Subject Requirements` = A_level_subjects,
        `Details` = URL
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
      caption = "🎯 Degrees you can get into - sorted by how well you exceed requirements"
    ) %>%
      DT::formatStyle(
        "Match Quality",
        backgroundColor = DT::styleEqual(
          c("🟢 Strong Match", "🟡 Good Match", "🔴 Just Meets Requirements"),
          c("#d4edda", "#fff3cd", "#f8d7da")
        )
      )
  })
}

# Run the application locally
# To run on your computer: shinyApp(ui = ui, server = server)

# To make it accessible on your local network (so others can access it):
# shinyApp(ui = ui, server = server, options = list(host = "0.0.0.0", port = 3838))

# To run from GitHub (anyone can access with this command):
# shiny::runGitHub("your-repo-name", "your-username", subdir = "app-folder")

shinyApp(ui = ui, server = server)