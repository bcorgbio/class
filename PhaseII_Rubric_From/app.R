#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
tasks <- list("Completes all tasks fully; submits all required outputs and materials on time (10 pts)",
           "Completes a majority but not all tasks fully; submission meets the deadline (6 points)",
           "Completes few required taks or submits few required matrials; submission misses the deadline (2 pts)")

effort <- list("Individual or team asks questions or seeks help regularly (10 pts)",
            "Individual or team asks questions or seek help sometimes, but not regularly (6 pts)",
            "Individual or team does not seem help (2 pts)")

format <- list( "Report contains all the appropriate headings and components (including a bibliography) (5 pts)",  "Report contains most of the appropriate headings and components (including a bibliography) (3 pts)",   "Report contains few of the appropriate headings and components (1 pt)")


analysis <-  list("Report contains concisely written code chunks within the results or methods sections that produce the appropriate analyses, figures, and/or tables (10 pts)",
               "One or a few code chunks are unneeded or produce analyses, figures, and/or tables not directly related to the project goals (6 pts)", "No code chunks contained in report or all code chunks are unneeded or produce analyses, figures, and/or tables not directly related to the project goals (2 pts)")

graphics <-  list("Figures, tables, and images are clear, add much to the results in anlysis, and have concise but descriptive captions (5 pts)",   "One or a few figures, tables, and images are unneeded or inaapropriate or they lack captions that are concise or descriptive  (3 pts)",  "All few figures, tables, and images are unneeded or inaapropriated (1 pt)")

writing <- list("The writing is concise, clear, avoids passive constructions, and is in the past tense; grammar is appropriate (5 pts)",   "The writing is unclear or rambling in spots or uses passive constructions and future tense; grammar is shaky in spots (3 pts)",   "The writing is unclear, and/or uses passive constructions or future tense throughout; grammar is shaky for the most part (1 pt)")

sources <- list("Statements of scientific findings and fact are supported by references; references are contained in a BibTex bibliography and inserted with `@` tags  (5 pts)", "Some statements of scientific findings and fact are not supported by references; some references are not contained in a BibTex bibliography or inserted with `@` tags (3 pts)",   "Most statements of scientific findings and fact are not supported by references; references are not contained in a BibTex bibliography nor inserted with `@` tags (1 pt)")


teams <- list("beep-boop-squad",
      "gitin",
     "Coders-R-US",
     "colleg_coders",
     "CroCODEiles",
     "Get it done",
     "Molecool",
     "Rchitects",
     "Game of Codes",
     "Go-Git-ers")

shinyApp(
fluidPage(
  
  title = "Phase II Grade Input",
  
  hr(),
  
  ### Tasks
  fluidRow(
    column(1, "Tasks & Output"),
    column(3,
           selectInput("team", "Select Team", choices = unlist(teams)),
           selectInput("module", "Select Module", choices = list("6","7","8")),
           selectInput("task", "Select rubric category", choices = tasks),
           sliderInput("task_slider", "Tasks & Output Score", 0, 10, 5)
    ),
    column(8, 
           textAreaInput("task_fb", "Feedback", ".....", width = "100%", height="300px")
          
    )
  ),
  hr(),
  
  ### Effort and Engagement
  fluidRow(
    column(1, "Effort & Engagement"),
    column(3,
           selectInput("effort", "Select rubric category", choices = effort),
           sliderInput("effort_slider", "Effort & Engagement Score", 0, 10, 5)
    ),
    column(8, 
           textAreaInput("effort_fb", "Feedback", ".....", width = "100%", height="300px")
           
    )
  ),
  hr(),
  
  ### Format
  fluidRow(
    column(1, "Report Format"),
    column(3,
           selectInput("format", "Select rubric category", choices = format),
           sliderInput("format_slider", "Effort & Engagement Score", 0, 5, 3)
    ),
    column(8, 
           textAreaInput("format_fb", "Feedback", ".....", width = "100%", height="300px")
           
    )
  ),
  hr(),
  
  ### Analyis and code
  fluidRow(
    column(1, "Analysis & Code"),
    column(3,
           selectInput("analysis", "Select rubric category", choices = analysis),
           sliderInput("analysis_slider", "Analysis and Code Score", 0, 10, 5)
    ),
    column(8, 
           textAreaInput("analysis_fb", "Feedback", ".....", width = "100%", height="300px")
           
    )
  ),
  hr(),
  
  #Graphics and Tables
  
  fluidRow(
    column(1, "Graphics & Tables"),
    column(3,
           selectInput("graphics", "Select rubric category", choices = graphics),
           sliderInput("graphics_slider", "Graphics & Tables Score", 0, 5, 3)
    ),
    column(8, 
           textAreaInput("graphics_fb", "Feedback", ".....", width = "100%", height="300px")
           
    )
  ),
  hr(),
  
  #Writing and Style
  fluidRow(
    column(1, "Writing & Style"),
    column(3,
           selectInput("writing", "Select rubric category", choices = writing),
           sliderInput("writing_slider", "Writing & Style Score", 0, 5, 3)
    ),
    column(8, 
           textAreaInput("writing_fb", "Feedback", ".....", width = "100%", height="300px")
           
    )
  ),
  hr(),
  
  #Sources & References
  fluidRow(
    column(1, "Sources & References"),
    column(3,
           selectInput("sources", "Select rubric category", choices = sources),
           sliderInput("sources_slider", "Sources & References Score", 0, 5, 3)
    ),
    column(8, 
           textAreaInput("sources_fb", "Feedback", ".....", width = "100%", height="300px")
           
    )
  ),
  hr(),
  
  #General feedback
  fluidRow(
   column(1, "General Feedback"),
    column(10, 
           textAreaInput("general_fb", "General Feedback", ".....", width = "100%", height="300px")
           
    )
  ),
  hr(),
  
  
  ### generate report
  fluidRow(
  column(3,
         downloadButton("report", "Generate report")
  )
  )
),


  server = function(input, output) {
    output$report <- downloadHandler(
      # For PDF output, change this to "report.pdf"
      filename = function(){paste0(input$team,"_Final_Project_Feedback.html")},
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "Report.Rmd")
        file.copy("Report.Rmd", tempReport, overwrite = TRUE)
        
        # Set up parameters to pass to Rmd document
        params <- list(
          tasks_gr=input$task_slider, 
          effort_gr=input$effort_slider,  
          format_gr=input$format_slider,   
          analysis_gr=input$analysis_slider,  
          graphics_gr=input$graphics_slider, 
          writing_gr=input$writing_slider,
          sources_gr=input$sources_slider,
          tasks_fb=input$task_fb, 
          effort_fb=input$effort_fb,  
          format_fb=input$format_fb,   
          analysis_fb=input$analysis_fb,  
          graphics_fb=input$graphics_fb, 
          writing_fb=input$writing_fb,
          sources_fb=input$sources_fb,
          tasks_rub=input$task, 
          effort_rub=input$effort,  
          format_rub=input$format,   
          analysis_rub=input$analysis,  
          graphics_rub=input$graphics, 
          writing_rub=input$writing,
          sources_rub=input$sources,
           module=input$module,
          team=input$team,
          general_fb=input$general_fb
         )
        
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      }
    )
  }
)
