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

#aggregate on genres
genre.agg <- aggregate(cleaned.books.df[,c(12:21)], by=list(year=cleaned.books.df$pubDate), FUN=sum)
genre.agg$year <- as.numeric(substring(genre.agg$year,0,4))

# Create Shiny object
ui <- fluidPage(theme = shinytheme("lumen"),
                fluidRow(
                  column(12,align="center",
                         titlePanel("Industry Overview by Genre")
                  )
                ),
                plotOutput(outputId = "bar",height="300px"),
                fluidRow(
                  column(12,align="center",
                    sliderInput(inputId = "year", label = "Year:",
                                min = 1989, max = 2019, value = 1, step = 1,
                                animate = animationOptions(interval = 1500))
                  )
                )
)

# Define server function
server <- function(input, output) {
  output$bar <- renderPlot({
    this.year <- genre.agg[which(genre.agg$year == input$year),]
    this.year <- this.year[,c(3:11)]
    df <- data.frame(genre=colnames(this.year),count=as.numeric(as.vector(this.year[1,])))
    df$percent <- df$count / mean(df$count)
    ggplot() + geom_bar(aes(y = percent, x = genre, fill = genre), data = df,
                        stat="identity")
  })
}

# Create Shiny object
shinyApp(ui = ui, server = server)

