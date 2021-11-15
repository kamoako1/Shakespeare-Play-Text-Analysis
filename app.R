#Importing necessary packages
library(shiny)
library(shinythemes)
library(RColorBrewer)
library(ggplot2)
library(wordcloud)
library(tidyverse)
library(tidytext)

#Text pre-processing step
# task4: add in getFreq function for pre-processing

getFreq <- function(book, stopwords = TRUE) {
  # check that only one of three books is selected
  if (!(book %in% books))
    stop("Unknown book")
  
  text <-  tibble(text = readLines(sprintf("./data/%s.txt", book), encoding="UTF-8"))
  
  # could also pass column of text/character instead
  text <- text %>%
    unnest_tokens(word, text) %>%
    count(word, sort = TRUE) 
  
  if(stopwords){
    text <- text %>%
      anti_join(stop_words)
  }
  return(text)
}

# The list of valid books
books <- list("A Mid Summer Night's Dream" = "summer",
              "The Merchant of Venice" = "merchant",
              "Romeo and Juliet" = "romeo")


# task6: add in shinythemes function

ui <- fluidPage(
  theme = shinytheme("journal"),
  titlePanel("Shakespeare's Plays Word Frequencies"), # Application title
  
  # task1: add in the sidebarLayout with sidebarPanel and mainPanel
  
  # put the side bar on the right
  # task2: add in the inputs in the sidebarPanel
  
  sidebarLayout(position = "left",
                
                
                # task3: add in the outputs in the sidebarPanel
                sidebarPanel(
                  selectInput(inputId = "n",
                              "Choose a Book", choices = books),
                  checkboxInput(inputId = "stopwords", label="Stop words", value=TRUE),
                  actionButton(inputId="action", label="Rerun"),
                  hr(),
                  h3("Word Cloud Settings"),
                  sliderInput(inputId="maxwords", label="Max # of Words", min=10, max=200,
                              value=100,step=10),
                  sliderInput(inputId="largewords", label="Largest Words", min=1, max=8,
                              value=4),
                  sliderInput(inputId="smallwords", label="Smallest Words", min=0.1, max=4,
                              value=0.5),
                  hr(),
                  h3("Word Count Settings"),
                  sliderInput(inputId="minwords", label="Min Word Counts", min=10, max=100,
                              value=25),
                  sliderInput(inputId="font", label="Word Font Size", min=8, max=30,
                              value=14),
                ),
                # task1: within the mainPanel, create two tabs (Word Cloud and Frequency)
                
                mainPanel("",
                          tabsetPanel(
                            
                            # task6: and modify your figure heights
                            tabPanel("Word Cloud", plotOutput("cloud",height = "600px")), 
                            tabPanel("Word Counts", plotOutput("freq",height = "600px")), 
                            )
  )),
  

  
)

server <- function(input, output) {
  
  # task5: add in reactivity for getFreq function based on inputs
  freq <- eventReactive(input$action,{ 
    withProgress({
      setProgress(message = "Processing corpus...")
      getFreq(input$n,input$stopwords) # ... = replace with the two inputs from Task 2
    })
  })
  
 output$cloud <- renderPlot({
   v <- freq()
   pal <- brewer.pal(8,"Dark2")
   v %>% 
     with(
       wordcloud(
         word, 
         n, 
         scale = c(input$largewords,input$smallwords),
         random.order = FALSE, 
         max.words = input$maxwords, 
         colors=pal))
 })
output$freq <- renderPlot({
 v <- freq()
 v %>%
   filter(n > input$minwords) %>%
   ggplot(aes(x=reorder(word,n),y=n),
          theme(text=element_text(size=input$font)),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())+
   geom_col() +
   coord_flip()
})

}

shinyApp(ui = ui, server = server)
