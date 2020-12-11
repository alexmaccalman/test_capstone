#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
#source("predict_model_functions.R")

rds_file <- "data/predict250Kfilter4.rds"

listRDS <- readRDS(rds_file)
# assign list elements to a variable
data_5 <- listRDS[1]
data_4 <- listRDS[2]
data_3 <- listRDS[3]
data_2 <- listRDS[4]
top__unigrams <- listRDS[5]
# function to take tibbles out of a list
unnest_tibble <- function(data){
  df <- enframe(data) %>% 
    unnest(value) %>% 
    select(-name)
}
data5 <- unnest_tibble(data_5)
data4 <- unnest_tibble(data_4)
data3 <- unnest_tibble(data_3)
data2 <- unnest_tibble(data_2)
top_unigrams <- unnest_tibble(top__unigrams) 

predict_word <- function(phrase_input){
  ## Prepare Input phrase
  phrase_input <- str_trim(phrase_input, side = "both") #trim white spaces on both sides
  # remove non-ascii characters
  phrase_input <- gsub("[^\x20-\x7E]", "", phrase_input)
  # merge multiple spaces to a single space and remove trailing/leading spaces
  phrase_input <- str_replace(gsub("\\s+", " ", str_trim(phrase_input)), "B", "b")
  phrase_input <- str_to_lower(phrase_input)
  # remove punctuation except apostrophes
  phrase_input <- gsub("[^'[:^punct:]]", "", phrase_input, perl=T)
  # remove apostrophes without creating a space
  phrase_input <- gsub("'", "", phrase_input)
  count <- str_count(phrase_input, "\\w+")
  if(count >= 4){
    in_4gram <- word(phrase_input, -4, -1)
    in_3gram <- word(phrase_input, -3, -1)
    in_2gram <- word(phrase_input, -2, -1)
    in_1gram <- word(phrase_input, -1)
  } else if (count == 3){
    in_3gram <- word(phrase_input, -3, -1)
    in_2gram <- word(phrase_input, -2, -1)
    in_1gram <- word(phrase_input, -1)
  } else if (count == 2){
    in_2gram <- word(phrase_input, -2, -1)
    in_1gram <- word(phrase_input, -1)
  } else if (count == 1){
    in_1gram <- word(phrase_input, -1)
  }
  
  ## Predict the next word  
  # initialize empty tibbles
  five_list <- tibble(last = character(), score = numeric(), table = character())
  four_list <- tibble(last = character(), score = numeric(), table = character())
  tri_list <- tibble(last = character(), score = numeric(), table = character())
  bi_list <- tibble(last = character(), score = numeric(), table = character())
  if(count >= 4){
    five_list <- data5 %>% 
      filter(fourgram == in_4gram) %>% 
      arrange(desc(score)) %>% 
      select(last, score) %>% 
      mutate(table = "five_gram")
  }
  if(count >= 3){
    four_list <- data4 %>% 
      filter(trigram == in_3gram) %>% 
      arrange(desc(score)) %>% 
      select(last, score) %>% 
      mutate(table = "four_gram")
  }
  if(count >= 2){
    tri_list <- data3 %>% 
      filter(bigram == in_2gram) %>% 
      arrange(desc(score))%>% 
      select(last, score) %>% 
      mutate(table = "tri_gram")
  }
  if(count >= 1){
    bi_list <- data2 %>% 
      filter(word == in_1gram) %>% 
      arrange(desc(score))%>% 
      select(last, score) %>% 
      mutate(table = "bi_gram")
  }
  
  top_unigrams %>% 
    mutate(table = "unigram")
  
  
  final <- rbind(five_list, four_list, tri_list, bi_list) %>% 
    group_by(last) %>% 
    summarize(score= max(score)) %>% 
    arrange(desc(score)) %>% 
    head(5)
  # fill in with top unigrams if there are less than 5 predictions
  if(nrow(final) < 5){
    rows <- nrow(final)
    add <- 5 - rows
    final <- rbind(final, head(top_unigrams, add))
  }
  #final
  final %>%
    select(last) %>%
    rename(Prediction = last)
}




# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("Predict the Next Word"),
    sidebarLayout(
      sidebarPanel(
        helpText("This application will predict the next word after you type in some words."),
        
        textInput("phrase", "Input Text to Predict", width = 1000),
      ),
      
      mainPanel(
        h3(helpText("Listed here are the top 5 predicted words.")),
        tableOutput("predicts"))
      )
)
# Define server logic 
server <- function(input, output) {
  
    output$predicts <- renderTable({
        phrase_input <- input$phrase
        table <- as.data.frame(predict_word(phrase_input))
        table
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
