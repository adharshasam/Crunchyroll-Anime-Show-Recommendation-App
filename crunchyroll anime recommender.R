#installing essential packages
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinyjs)
library(dplyr)
library(magrittr)
library(stringr)
library(DT)
library(bslib)

#ui code
ui <- tagList(
  useShinyjs(),
  navbarPage(
    "crunchyroll anime recommender (ﾉ◕ヮ◕)ﾉ*:･ﾟ✧",
    id = "navbar",
    tabPanel(title = "for best results, select all genres that apply!",
             id = "questions",
             h4("✧ helps you find binge-worthy anime shows tailored to your taste!"),
             h4("✧ created by your friendly neighbourhood data nerd who is also an anime geek hehe ♡"),
             br(),
             h4("for best results, select all genres that apply!"),
             checkboxGroupInput(inputId = "genre", 
                                br(),
                                choices =  c("action", "adventure", "comedy", "drama", "family", "fantasy", "food", "harem", "historical", "horror", "idols", "isekai", "jdrama", "magical girls", "martial arts", "mecha", "music", "mystery", "post apocalyptic", "romance", "sci fi", "seinen", "sg drama", "shojo", "shonen", "slice of life", "sports", "supernatural", "thriller")),
             mainPanel(dataTableOutput("final_df"), textOutput("anime_name"), textOutput("genre")),
             tableOutput("tbl")), 
    
    tabPanel(
      title = "k-means clustering for nerds (⁀ᗢ⁀) ",
      value = "hide",
      id = "questions",
      strong(h4("learn about cluster analysis! {beginner-friendly & easy to follow along}")),
      p("✧ clustering is an unsupervised machine learning technique of grouping similar data points into different clusters."),
      p("there are several clustering algorithms in machine learning. however..."),
      p("we will only look at the k-means clustering algorithm and how it can be used to make personalised anime show recommendations!"),
      p("i will explain how k-means clustering can be done to get show recommendations using r."),
      br(),
      strong(h4("k-means clustering:")),
      p("✧ it is one of the simplest and widely-used of all clustering algorithms."),
      p("✧ it is a centroid-based algorithm that tries to minimize the variance of data points within each cluster."),
      em(p("in layman terms, this distance-based algorithm computes centroids for a predefined number of clusters and repeats the process until optimal centroids are found.")),
      em(p("every data point in the dataset is assigned to the cluster whose centroid is closest to it.")),
      br(),
      strong(h4("let's get coding!")),
      p("loading the dataset..."),
      code('anime_data <- read.csv("https://raw.githubusercontent.com/filipefilardi/crunchyroll_filters/master/data/animes.csv")'),
      br(),
      br(),
      p("in real life, datasets are always messy. more often than not, we need to clean and process data before we can start building machine learning models."),
      p("data pre-processing typically looks like this!"),br(),
      em(p("removing unnecessary columns:")),
      code('anime_df <- subset(anime_data, select = -c(anime_img, weight, rate_1, rate_2, rate_3, rate_4, rate_5)'),
      br(),br(),
      em(p("renaming column names:")),
      code('G <- gsub("genre_","",names(anime_df)[-c(1:5)])'),br(),br(),
      em(p("writing a function to group all genres of anime shows into one column:")),
      code('getCols <- function(x) {'),br(),
      code('Reduce(function(x,y){paste0(x,", ",y)},G[which(x==1)]) }'),
      br(),br(),
      code('anime_df$genre <- apply(anime_df[,-c(1:5)],1,function(X) {'),
      br(),
      code('if(is.null(getCols(X))) {'),br(),
      code('""'),br(),
      code('} else {'),br(),
      code('getCols(X) }'),br(),
      code('})'),br(),br(),
      code('final_df <- anime_df[,!grepl("*genre_",names(anime_df))]'),
      br(),br(),
      p("since we are working with text data, we have to first create a corpus to make our data suitable for analysis."),
      p("loading necessary packages..."),
      code('library(tm)'),br(),
      code('library(SnowballC)'),br(),br(),
      em(p("creating a corpus:")),
      code('corpus <- Corpus(VectorSource(final_df$genre))'),
      br(),br(),
      p("usually, we pre-process our corpus (like converting all characters to lower-case, removing punctuation, etc) to make it easier to work with."),
      p("but, upon inspecting a corpus, we see that our corpus is already looking pretty neat!"),
      code("writeLines(as.character(corpus[[20]]))"),
      br(),br(),
      em(p("creating a document-term matrix:")),
      p("we now convert our corpus into a matrix form before we proceed with our cluster analysis."),
      code('dtm <- DocumentTermMatrix(corpus)'),br(),
      code('rownames(dtm) <- final_df$anime'),
      br(),br(),
      strong(h4("let's perform k-means clustering!")),br(),
      em(p("specify the number of clusters (k) to be created. to determine the optimum number of clusters to initialise, we can use the elbow plot.")),
      code('library(tidyverse)'),br(),br(),
      code('set.seed(100)'),br(),
      code('k <- 1:29'),br(),
      code('wss_values <- map_dbl(k, function(k) {kmeans(as.matrix(dtm), k)$tot.withinss})'),br(),
      br(),
      code("plot(k, wss_values, type = 'b', pch = 19, frame = F)"),br(),br(),
      img(src = "elbow.png", width = 700, height = 430),br(),br(),
      p("from the plot, we see that the within-cluster variation dips steadily at k = 20. this might be an optimal number of clusters needed for our data."),
      p("it's important to set seed to ensure reproducibility of results."),
      code('set.seed(100)'),br(),
      code('kmeans20 <- kmeans(dist(as.matrix(dtm)), 20)'),br(),
      code('anime.kmeans20 <- cbind(final_df, kmeans20$cluster)'),br(),
      code("names(anime.kmeans20)[names(anime.kmeans20) == 'kmeans20$cluster'] <- 'cluster'"),br(),br(),
      em("grouping shows into 20 clusters:"),br(),br(),
      code('kmeans.percent20 <- vector()'),br(),
      code('for(i in 1:20) {'),br(),
      code('kmeans.percent20[i] <- round(kmeans20$size[i]/sum(kmeans20$size) * 100, 3)'),br(),
      code('}'),br(),br(),
      code('kmeans.size20 <- data.frame(group = 1:20, size = kmeans20$size)'),br(),br(),
      code('ggplot(kmeans.size20, aes(x = group, y = size, fill = factor(group))) +'),br(),
      code("geom_bar(stat = 'identity') +"),br(),
      code('theme(legend.position = "none") +'),br(),
      code('geom_text(aes(label = paste(size, paste(kmeans.percent20, "%", sep = ""), sep = "\n")), vjust = 0.5, size = 4) +'),br(),
      code('scale_x_discrete(limits = kmeans.size20$group)'),br(),br(),
      img(src = "kmeans20.png", width = 1000, height = 430),br(),br(),
      p("the line of code below can be run to view the shows classified into the largest cluster: cluster 15."),
      code("anime.kmeans20[anime.kmeans20$cluster == 15, c('anime', 'genre')]"),br(),br(),
      p("if you want to find shows similar to your favourite anime, (psst... the name of the show must be present in the dataset for this to work T^T)"),
      p("you can run the code below to find out the cluster to which the show belongs to..."),
      code('anime.kmeans20$cluster[anime.kmeans20$anime == "Naruto Shippuuden"]'),br(),br(),
      p("have fun playing around. peace out (｡･ω･｡) v")
    )
  ))

#server code
server <- function(input, output) {
  
  #data preparation code
  anime_data <- read.csv("https://raw.githubusercontent.com/filipefilardi/crunchyroll_filters/master/data/animes.csv")
  anime_df <- subset(anime_data, select = -c(anime_img, weight, rate_1, rate_2, rate_3, rate_4, rate_5))
  
  G <- gsub("genre_","",names(anime_df)[-c(1:5)])
  
  getCols <- function(x)
  {
    Reduce(function(x,y){paste0(x," ✿ ",y)},G[which(x==1)])
  }
  
  anime_df$genre <- apply(anime_df[,-c(1:5)],1,function(X){
    if( is.null(getCols(X)) ){
      ""
    } else {
      getCols(X)
    }
  })
  
  final_df <- anime_df[,!grepl("*genre_",names(anime_df))]
  
  #code to pattern match input character in genre column and display filtered df 
  get_table <- eventReactive({input$button; input$genre}, {
    final_df %>% filter(str_detect(final_df$genre, input$genre))
  })
  
  output$tbl <- renderTable({
    get_table()
  })
}

#run app
shinyApp(ui = ui, server = server)

