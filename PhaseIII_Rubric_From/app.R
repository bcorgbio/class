#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
tasks <- list(	"Completes all tasks fully; submits all required outputs and materials on time, including those scheduled before the final deadline (15 pts).",
               "Completes a majority but not all tasks fully; final submission meets the deadline but some preliminary deadlines may have been missed ( 8 pts).",	
               "Completes few required tasks or submits few required materials; most submissions miss their respective deadlines(2 3 pts)."
               )

rigor <- list("The project approaches an important question in organismal biology with a reasonable scope using tools and techniques used in the course (15 pts)",	
              "The project question is well defined, but perhaps trivial, or the project scope is too narrow or ambitious (8 pts).",	"The question is trivial and the scope unreasonable. (3 pts)"
              )
effort <- list(
  "The individual or team pursues help from the instructor, ask questions and/or seeks help regularly (15 pts)",	"Individual or team asks questions or seek help sometimes, but not regularly (8 pts)",	"Individual or team does not seem help (3 pts)"
)

format <- list( "Report contains all the appropriate headings and components (including a bibliography) (10 pts)",	"Report contains most of the appropriate headings and components (including a bibliography) (6 pts)",	"Report contains few of the appropriate headings and components (2 pt)")


analysis <-  list("Report contains concisely written code chunks within the results or methods sections that produce creative analyses, figures, and/or tables that have clear implications for the project’s question (15 pts)",	"One or a few code chunks are unneeded or produce analyses, figures, and/or tables that do not have clear implication for to the project goals (8 pts)",	"No code chunks contained in report or all code chunks are unneeded or produce analyses, figures, and/or tables not directly related to the project goals (3 pts)"
                  )

graphics <-  list(	"Figures, tables, and images are clear, add much to the results in analysis, and have concise but descriptive captions (10 pts)",	"One or a few figures, tables, and images are unneeded or inappropriate or they lack captions that are concise or descriptive (6 pts)",	"All figures, tables, and images are unneeded or inappropriate (2 pt)")

writing <- list("The writing is concise, clear, avoids passive constructions, and is in the past tense; grammar is appropriate (10 pts)",	"The writing is unclear or rambling in spots or uses passive constructions and future tense; grammar is shaky in spots (6 pts)",	"The writing is unclear, and/or uses passive constructions or future tense throughout; grammar is shaky for the most part (2 pt)")

sources <- list(	"The report contains 3 references to primary research pieces that support statements of fact; references are contained in a BibTex bibliography and inserted with @ tags (10 pts)",	"Report contains fewer than 3 references; some statements of scientific findings and fact are not supported by references; some references are not contained in a BibTex bibliography or inserted with @ tags(6 pts)", "Most statements of scientific findings and fact are not supported by references; references are not contained in a BibTex bibliography nor inserted with @ tags (2 pt)")


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
  
  title = "Phase III Grade Input",
  
  hr(),
  
  ### Tasks
  fluidRow(
    column(1, "Tasks & Output"),
    column(3,
           selectInput("team", "Select Team", choices = unlist(teams)),
           selectInput("task", "Select rubric category", choices = tasks),
           sliderInput("task_slider", "Tasks & Output Score", 0, 15, 15)
    ),
    column(8, 
           textAreaInput("task_fb", "Feedback", ".....", width = "100%", height="300px")
          
    )
  ),
  hr(),
  
  ### Rigor
  fluidRow(
    column(1, "Scientific Rigor"),
    column(3,
           selectInput("rigor", "Select rubric category", choices = rigor),
           sliderInput("rigor_slider", "Rigor Score", 0, 15, 15)
    ),
    column(8, 
           textAreaInput("rigor_fb", "Feedback", ".....", width = "100%", height="300px")
           
    )
  ),
  hr(),
  
  ### Effort and Engagement
  fluidRow(
    column(1, "Effort & Engagement"),
    column(3,
           selectInput("effort", "Select rubric category", choices = effort),
           sliderInput("effort_slider", "Effort & Engagement Score", 0, 15, 15)
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
           sliderInput("format_slider", "Effort & Engagement Score", 0, 10, 10)
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
           sliderInput("analysis_slider", "Analysis and Code Score", 0, 15, 15)
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
           sliderInput("graphics_slider", "Graphics & Tables Score", 0, 10, 10)
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
           sliderInput("writing_slider", "Writing & Style Score", 0, 10, 10)
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
           sliderInput("sources_slider", "Sources & References Score", 0, 10, 10)
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
      filename = function(){paste0(input$team,"_Project_Feedback.html")},
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "Report.Rmd")
        file.copy("Report.Rmd", tempReport, overwrite = TRUE)
        
        # Set up parameters to pass to Rmd document
        params <- list(
          rigor_gr=input$rigor_slider, 
          tasks_gr=input$task_slider, 
          effort_gr=input$effort_slider,  
          format_gr=input$format_slider,   
          analysis_gr=input$analysis_slider,  
          graphics_gr=input$graphics_slider, 
          writing_gr=input$writing_slider,
          sources_gr=input$sources_slider,
          rigor_fb=input$rigor_fb,
          tasks_fb=input$task_fb, 
          effort_fb=input$effort_fb,  
          format_fb=input$format_fb,   
          analysis_fb=input$analysis_fb,  
          graphics_fb=input$graphics_fb, 
          writing_fb=input$writing_fb,
          sources_fb=input$sources_fb,
          tasks_rub=input$task,
          rigor_rub=input$rigor,
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
