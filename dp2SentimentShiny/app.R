library(tm)
library(tidytext)
library(topicmodels) 
library(wordcloud)
library(fmsb)
library(shiny)
library(shinythemes)
library(lubridate)
library(ggplot2)

cleaned.books.df <- read.csv("./Data/clean_books.csv")
#sanity-check convert to numeric
cleaned.books.df$pubDate <- as_date(cleaned.books.df$pubDate)
cleaned.books.df$rating <- as.numeric(cleaned.books.df$rating)
cleaned.books.df$ratingCount <- as.numeric(cleaned.books.df$ratingCount)
cleaned.books.df$reviewCount <- as.numeric(cleaned.books.df$reviewCount)
cleaned.books.df$pageCount <- as.numeric(cleaned.books.df$pageCount)

#fix .s in col names
colnames(cleaned.books.df) <- gsub("\\."," ",colnames(cleaned.books.df))

#handle genres: first, identify all genres listed, then paste them together, then get stats
genreStr <- paste(cleaned.books.df$genres,sep=",",collapse=",")
genreVec <- unlist(strsplit(genreStr,","))
genre.frame <- as.data.frame(table(genreVec))
#sort descending to see most common genres
genre.frame <- genre.frame[order(-genre.frame$Freq),] 
top10Genres <- head(genre.frame$genreVec,10)

# Create Shiny object
ui <- fluidPage(theme = shinytheme("lumen"),
                fluidRow(
                  column(12,align="center",
                         titlePanel("Genre Analysis")
                  )
                ),
                fluidRow(
                  # Select type of trend to plot
                  column(12,align="center",
                         selectInput(inputId = "genre", label = NULL,
                                     choices = unique(top10Genres),
                                     selected = "Fiction"))
                ),
                fluidRow(column(4,align="center",plotOutput(outputId = "wordcloud",height="300px")),
                         column(4,align="center",plotOutput(outputId = "pie",height="300px")),
                         column(4,align="center",plotOutput(outputId = "radar",height="300px"))
                )
)

# Define server function
server <- function(input, output) {
  
  # Subset data
  selected_trends <- reactive({
    cleaned.books.df.genre <- cleaned.books.df[which(cleaned.books.df[input$genre]==1),]
    synopsis.corp <- VCorpus(VectorSource(cleaned.books.df.genre$synopsis))
    #clean it
    synopsis.corp.clean <- tm_map(synopsis.corp, removePunctuation)
    synopsis.corp.clean <- tm_map(synopsis.corp.clean, removeNumbers)
    synopsis.corp.clean <- tm_map(synopsis.corp.clean, content_transformer(tolower),lazy=TRUE)
    synopsis.corp.clean
  })
  
  output$wordcloud <- renderPlot({
    #make words stems
    synopsis.corp.clean.stem <- tm_map(selected_trends(), content_transformer(stemDocument),lazy=TRUE)
    #kill stopwords and whitespace
    synopsis.corp.clean.stem <- tm_map(synopsis.corp.clean.stem,content_transformer(removeWords),stopwords("english"))
    synopsis.corp.clean.stem <- tm_map(synopsis.corp.clean.stem,stripWhitespace)
    dtm <- DocumentTermMatrix(synopsis.corp.clean.stem)
    
    #remove sparse terms
    dtms <- removeSparseTerms(dtm, 0.995)
    
    #wordcloud on most-popular synopsis words, by genre identified
    dtms.as.df <- tidy(dtms)
    
    dtms.as.df.agg <- aggregate(dtms.as.df$count, by=list(term=dtms.as.df$term), FUN=sum)
    dtms.as.df.agg = dtms.as.df.agg[order(-dtms.as.df.agg$x),] 
    
    top30Words <- head(dtms.as.df.agg$term,30)
    top30Count <- head(dtms.as.df.agg$x,30)
    
    par(mar=rep(1,4)) 
    wordcloud(words=top30Words,freq=top30Count,colors=brewer.pal(8, "Spectral"))
    title(paste("Top 30 Words"))
  })
  
  # Pull in description of trend
  output$pie <- renderPlot({
    #we create this dtm to get non-stemmed words to feed into sentiment analysis
    dtm_sentiment <- DocumentTermMatrix(selected_trends())
    dtm.sentiment.as.df <- tidy(dtm_sentiment)
    
    #label "term" as "word" for the join...
    colnames(dtm.sentiment.as.df) = c("document","word","count")
    sentiment.df <- merge(x = dtm.sentiment.as.df, y = get_sentiments("nrc"), by = "word", all.y = TRUE)
    sentiment.df <- sentiment.df[which(!is.na(sentiment.df$count)),]
    sentiment.df.agg <- aggregate(sentiment.df$count, by=list(sentiment=sentiment.df$sentiment), FUN=sum)
    
    #remove positive and negative, do pie chart for that
    sentiment.df.posneg <- sentiment.df.agg[which(sentiment.df.agg$sentiment == "negative" | sentiment.df.agg$sentiment == "positive"),]
    
    sentiment.df.others <- sentiment.df.agg[which(sentiment.df.agg$sentiment != "negative" & sentiment.df.agg$sentiment != "positive"),]
    
    #transpose to get into radarchart form
    t.sentiment.df.others<- rbind(sentiment.df.others$sentiment,sentiment.df.others$x)
    t.sentiment.df.others <- as.numeric(t.sentiment.df.others[2,])
    t.sentiment.df.others <- rbind(max(t.sentiment.df.others),min(t.sentiment.df.others),t.sentiment.df.others)
    colnames(t.sentiment.df.others) <- sentiment.df.others$sentiment
    
    par(mar=rep(1,4)) 
  
    pie(sentiment.df.posneg$x/sum(sentiment.df.posneg$x),labels = sentiment.df.posneg$sentiment, col=c(rgb(242/255,125/255,114/255,0.9),rgb(181/255,219/255,169/255,0.9)),
        main=paste("Overall Sentiment"),border=NA)
  })
  
  output$radar <- renderPlot({
    #we create this dtm to get non-stemmed words to feed into sentiment analysis
    dtm_sentiment <- DocumentTermMatrix(selected_trends())
    dtm.sentiment.as.df <- tidy(dtm_sentiment)
    
    #label "term" as "word" for the join...
    colnames(dtm.sentiment.as.df) = c("document","word","count")
    sentiment.df <- merge(x = dtm.sentiment.as.df, y = get_sentiments("nrc"), by = "word", all.y = TRUE)
    sentiment.df <- sentiment.df[which(!is.na(sentiment.df$count)),]
    sentiment.df.agg <- aggregate(sentiment.df$count, by=list(sentiment=sentiment.df$sentiment), FUN=sum)
    
    #remove positive and negative, do pie chart for that
    sentiment.df.posneg <- sentiment.df.agg[which(sentiment.df.agg$sentiment == "negative" | sentiment.df.agg$sentiment == "positive"),]
    
    sentiment.df.others <- sentiment.df.agg[which(sentiment.df.agg$sentiment != "negative" & sentiment.df.agg$sentiment != "positive"),]
    
    #transpose to get into radarchart form
    t.sentiment.df.others<- rbind(sentiment.df.others$sentiment,sentiment.df.others$x)
    t.sentiment.df.others <- as.numeric(t.sentiment.df.others[2,])
    t.sentiment.df.others <- rbind(max(t.sentiment.df.others),min(t.sentiment.df.others),t.sentiment.df.others)
    colnames(t.sentiment.df.others) <- sentiment.df.others$sentiment
    
    par(mar=rep(1,4)) 
    
    colors_border=c( rgb(181/255,219/255,169/255,0.9) )
    colors_in=c( rgb(181/255,219/255,169/255,0.4) )
    
    radarchart(as.data.frame(t.sentiment.df.others), 
               #custom polygon
               pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
               #custom the grid
               cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
               #custom labels
               vlcex=0.8 
    )
    
    title(paste("Sentiment"))
  })
}

# Create Shiny object
shinyApp(ui = ui, server = server)

