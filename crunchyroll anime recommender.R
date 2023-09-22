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
library(tm)
library(SnowballC)

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
      br(),
      strong(h4("THEORY: k-means clustering")),
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
      p("data pre-processing typically looks like this!"),
      em(p("removing unnecessary columns:")),
      code('anime_df <- subset(anime_data, select = -c(anime_img, weight, rate_1, rate_2, rate_3, rate_4, rate_5))'),
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
      p("loading necessary NLP packages..."),
      code('library(tm)'),br(),
      code('library(SnowballC)'),br(),br(),
      em(p("creating a corpus:")),
      code('corpus <- Corpus(VectorSource(final_df$genre))'),
      br(),br(),
      em(p("inspecting the corpus:")),
      code("writeLines(as.character(corpus[[5]]))"),
      br(),br(),
      em(p("corpus preprocessing:")),
      p("in our case, we only need to remove punctuation in the corpus."),
      code("corpus <- tm_map(corpus, removePunctuation)"),
      br(),br(),
      em(p("creating a document-term matrix:")),
      p("we now convert our corpus into a matrix form before we proceed with our cluster analysis."),
      code('dtm <- DocumentTermMatrix(corpus)'),br(),
      code('rownames(dtm) <- final_df$anime'),
      br(),br(),
      strong(h4("let's perform k-means clustering!")),
      em(p("specifying the number of clusters (k) to be created:")),
      p("to determine the optimum number of clusters to initialize, we can use the elbow plot."),
      p("loading necessary packages..."),
      code('library(cluster)'),br(),
      code('library(factoextra)'),br(),br(),
      code('fviz_nbclust(as.matrix(dtm), kmeans, method = "wss", linecolor = "indianred")'),br(),br(),
      img(src = "elbow_plot.png", width = 700, height = 430),br(),br(),
      p("from the plot, we see that the within-cluster variation dips steadily at the 'elbow' where k = 6. so, we take k = 6 as the optimal number of clusters needed to perform k-means clustering in our case."),br(),
      strong(h4("CODE: k-means clustering")),
      p("to ensure reproducibility of results, we set seed."),
      code('set.seed(100)'),br(),br(),
      p("performing k-means clustering with k = 6 clusters,"),
      code('km <- kmeans(as.matrix(dtm), centers = 6, nstart = 25)'),br(),br(),
      p("appending cluster ids to the final anime show dataframe,"),
      code('final_df <- cbind(final_df, cluster = km$cluster)'),br(),br(),
      p("here's the visualization of the final clusters!"),
      code('fviz_cluster(km, data = as.matrix(dtm), geom = "point", ggtheme = theme_bw())'),br(),br(),
      img(src = "cluster_plot.png", width = 600, height = 430),br()
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
  
  corpus <- Corpus(VectorSource(final_df$genre))
  corpus <- tm_map(corpus, removePunctuation)
  
  dtm <- DocumentTermMatrix(corpus)
  rownames(dtm) <- final_df$anime
  
  set.seed(100) 
  #perform k-means clustering with k = 6 clusters
  km <- kmeans(as.matrix(dtm), centers = 6, nstart = 25)
  final_df <- cbind(final_df, cluster = km$cluster)
  
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
