#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
f <- function(x) { -10.215+0.019*x[1] - 0.256*x[2] + 0.601*x[3] - 0.510*x[4]}

ui <- fluidPage(
    titlePanel("Body Fat Calculator (Men Only)"),
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
               selectInput("gender", label = h3("Gender"), 
                           choices = list("Male" = 1), 
                           selected = 1),
               
               numericInput("n1", label = h3("Age (years)"), value = 25,min = 1),
               numericInput("n2", label = h3("Height (inches)"), value = 65,min = 20),
               numericInput("n3", label = h3("Abdomen 2 circumference (cm)"), value = 70,min = 40),
               numericInput("n4", label = h3("Wrist circumference (cm)"), value = 15,min = 10),
               
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
                   tabPanel("Result", textOutput("text"),textOutput("text2")),
                   tabPanel("Help",textOutput("help") ),
                   tabPanel("Contact Us",textOutput("contact"))
                   # tabPanel("Code", withMathJax(), 
                   #          HTML(markdown::markdownToHTML(knit("ztestZ_code.Rmd", quiet = T)))),
               ) ## close tabset panel
               
        ) ## close column
        
    ), ##close fluid row
    hr()
)




server <- function(input, output) {
    # `value` will in the output
    output$value = renderPrint({
        f(c(input$n1, input$n2,input$n3, input$n4))
    })

    
    output$text <- renderText({ ifelse((!is.na(input$n1))&(!is.na(input$n2))&(!is.na(input$n3))&(!is.na(input$n4)),
                                    paste('Your body fat percentage is',f(c(input$n1, input$n2,input$n3, input$n4)),'%'),
                                    'Please input all the values')})
    output$text2 <- renderText({ if (input$gender==1) {
        #male
        if (f(c(input$n1, input$n2,input$n3, input$n4)) > 25){
            paste('Your body fat percentage is above average level in men.')
        }
        else if (f(c(input$n1, input$n2,input$n3, input$n4)) < 18) {
            paste('Your body fat percentage is below average level in men.')
        }
        else { paste('Your body fat percentage is about average level in men.')}
    }
        else {
            #female
            if (f(c(input$n1, input$n2,input$n3, input$n4)) > 31){
                paste('Your body fat percentage is above average level in women.')
            }
            else if (f(c(input$n1, input$n2,input$n3, input$n4)) < 25) {
                paste('Your body fat percentage is below average level in women.')
            }
            else { paste('Your body fat percentage is about average level in women.')}
        }
        })
    output$contact <- renderText({ 'Feel free to contact us at hkou2@wisc.edu !' })
    output$help <- renderText({ 'See details here: https://github.com/kouhuitong/Body-Fat-Project' })
}
shinyApp(ui = ui, server = server)
