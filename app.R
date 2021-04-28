# convergeR
# Real Analysis I - Spring 2021
# Erik Larsson
# SDG

library(shiny)
library(shinythemes)
library(magrittr)

source("functions.R")

# Define UI for application that draws a histogram
ui <- navbarPage("ConvergeR", theme = shinytheme("superhero"),
   
   tabPanel("Home",
            
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        
         # Help text
         helpText("Welcome to ConvergeR! Enrich your conceptual understanding of sequential convergence via interactive data visualizations. Follow the input prompts below to:"),
         # br(),
         helpText("1. Define a closed-form sequence of a single variable"),
         helpText("2. Choose a candidate limit of the sequence"),
         helpText(HTML("3. Choose an &#x0190;-neighborhood to examine the behavior of the sequence around the candidate limit")),
         helpText(HTML("4. Select the range of n &#8712; &Nopf; over which you would like to examine the sequence")),
         
         # Closed form input
         textInput(inputId = "formula", label = "1. Enter a closed form sequence:", value = "1/x"),
        
         # Value of a
         numericInput(inputId = "a", label = "2. Choose a candidate limit:", min = 0, max = 10^3, step = 0.01, value = 0),
        
         # Epsilon
         numericInput(inputId = "epsilon", label = HTML("3. Choose a positive value for &#x0190;:"), min = 10^-10, max = .1, step = 0.01, value = 10^-2),
         
         # range of n in N
         sliderInput(inputId = "n", 
                     HTML("4. Set range of n &#8712; &Nopf;"),
                     min = 1,
                     max = 2*10^3,
                     step = 100,
                     value = c(1,100)
                     ),
         
         # Definition
         actionLink(inputId = "def", label = "What is sequential convergence?")
         
      ),
      
      
      
      # Show a plot of the generated distribution
      mainPanel(
         plotly::plotlyOutput("plot", height = "150%")
      )
   )
   
   
   ),
   
   navbarMenu("About",
    tabPanel("Contact",
             helpText("This application was developed by Erik Larsson. Please feel free to contact me at larssoneg@gmail.com with any questions."),
             br(),
             helpText("Moreover, please feel free to download the source code from github.com/larssoneg and to use the application for your Real Analysis needs. Please cite your use of the application as:"),
             br(),
             helpText("Larsson, Erik. (2021). ConvergeR (version 1.0). RShiny.")
            ),
    tabPanel("Project background",
     helpText("This application was developed as a term project for my Real Analysis I class at Johns Hopkins University. I have found that programming and data visualization are useful ways to solidify the abstract concepts of Real Analysis. Over time, I may expand the application by adding more pages with similar data visualizations related to other Real Analysis concepts.")        
    )
   )
   

)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   # Generate data
   plot_data   <-   reactive( {generate_sequence(form = input$formula, range = input$n[1]:input$n[2])} )
      
   # Generate plot
   output$plot <-   plotly::renderPlotly({ plot_sequence(sequence_data = plot_data(), a = input$a, epsilon = input$epsilon, range = input$n) })
 
   
   # Modal for definition of convergence
   observeEvent(input$def, {
     showModal(
       modalDialog(title = "Sequential Convergence",
        HTML("A sequence <u>(a<sub>n</sub>)</u> converges to <u>a &#8712; &Ropf;</u> if for all <u>&#x0190; > 0</u> there exists an <u>N &#8712; &Nopf;</u> such that if <u>n > N</u> then <u>|a<sub>n</sub> - a| < &#x0190;</u>."),
        HTML("If this is true, then <u>a</u> is called the <u>limit</u> of <u>(a<sub>n</sub>)</u>"),
        br(),
        br(),
        HTML("On the other hand, a sequnce <u>(a<sub>n</sub>)</u> diverges if it does not converge to any limit <u>a</u>."),
        br(),
        HTML("More rigorously, a sequence <u>(a<sub>n</sub>)</u> diverges if, for all <u>a &#8712; &Ropf;</u>, there exists some <u>&#x0190; > 0</u> such that, for all <u>N</u>, there exists some <u>n > N</u> such that <u>|a<sub>n</sub> - a| &GreaterEqual; &#x0190;</u>."),
        HTML("Furthermore, a sequence <u>(a<sub>n</sub>)</u> diverges to infinity if for all <u>M>0</u>, there exists an <u>N</u> such that if <u>n>N</u> then <u>a<sub>n</sub>>M</u>.")
       )
     )
   }
   )
     
}

# Run the application 
shinyApp(ui = ui, server = server)

