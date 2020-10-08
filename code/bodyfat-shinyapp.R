library(shiny)
f <- function(x) { x[1]^2 + x[2]^3 }
ui <- fluidPage(
  titlePanel("Body Fat Calculator"),
  ## Code you should write. No unique answer
  hr(),
  #verbatimTextOutput("value"),
  ####create fluid row####
  fluidRow(
    
    #### put input area here ####
    column(4,
           style = "background-color: #E8E8E8",
           
           ##change the title here
           #div(style="display: inline-block; vertical-align:top; text-align:center; width: 100%;",
               #strong("Body Fat Calculator")),
           numericInput("n1", label = h3("Numeric input 1"), value = 1,min = 0),
           numericInput("n2", label = h3("Numeric input 2"), value = 2,min = 0),
           numericInput("n3", label = h3("Numeric input 3"), value = 3,min = 0),
           numericInput("n4", label = h3("Numeric input 4"), value = 4,min = 0),
           
           # br(),
           # ##put input boxes here
           # div(style="display: inline-block;vertical-align:top; width: 200px;",
           #     strong("Sample Mean:"), 
           #     numericInput("n2", label = h3("Numeric input 2"), value = 2)),
           # 
           # div(style="display: inline-block;vertical-align:top; width: 200px;",
           #     strong("Sample SD:"),
           #     textInput("ZMsd1", NULL, width = 60)), 
           # 
           # div(style="display: inline-block;vertical-align:top; width: 200px;",
           #     strong("Sample SE:"),
           #     textInput("ZMse1", NULL, width = 60)),
           # 
           # br(),
           # div(style="display: inline-block;vertical-align:top; width: 200px;",
           #     strong("Population Mean:"),
           #     textInput("ZMmean2", NULL, width = 60)),
           # 
           # 
           # div(style="display: inline-block;vertical-align:top; width: 200px;",
           #     strong("Population SD:"),
           #     textInput("ZMsd2", NULL, width = 60)), 
           # 
           # div(style="display: inline-block;vertical-align:top; width: 200px;",
           #     strong("Population SE:"),
           #     textInput("ZMse2", NULL, width = 60)),
           # 
           # 
           # textInput("ZMalpha", "Alpha:", width = 60, placeholder = ".05"),
           hr(),
           submitButton("Calculate",width = '300px'),
           
    ), ## close column 1
    
    #### put output here ####
    column(8, 
           tabsetPanel(
             tabPanel("Result", textOutput("text")),
             tabPanel("Help",textOutput("help") ),
             tabPanel("Contact Us",textOutput("contact"))
             ) ## close tabset panel
           
    ) ## close column
    
  ), ##close fluid row
  hr()
)

server <- function(input, output) {
  # `value` will in the output
  output$value = renderPrint({
    f(c(input$n1, input$n2))
  })
  output$text <- renderText({ ifelse((!is.na(input$n1))&(!is.na(input$n2)),f(c(input$n1, input$n2)),'Please input all the values') })
  output$contact <- renderText({ 'Feel free to contact us at hkou2@wisc.edu !' })
  output$help <- renderText({ 'See details here: https://github.com/kouhuitong/Body-Fat-Project' })
}
shinyApp(ui = ui, server = server)
