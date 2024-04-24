library(rvest)
library(tidyverse)
library(tidytext)
library(hunspell)
library(sentimentr)


#######define the URL to scrape (remove page number)
url <- "https://www.bbc.co.uk/news/topics/cmj34zmwm1zt?page="

#######Adjust the range of page numbers
numbers <- seq(2, 42, 1)  

#Combine the page numbers to the page URL
urls <- paste0(url, numbers)

counter <- 0
#######Gather all the article URLs from the site. 

article_urls <- map(urls, ~ {
  .x %>%
    read_html() %>%
    html_nodes("a.ssrcss-9haqql-LinkPostLink.ej9ium92") %>% 
    html_attr("href")
})%>% unlist()

urlstart <- "https://www.bbc.co.uk"
article_urls <- paste0(urlstart,article_urls)


# Create a data frame with the article URLs so we can filter it
article_data <- data.frame(URL = article_urls, stringsAsFactors = FALSE)



#We need to return the data to a list so we can map it
filtered_article_data <- as.list(article_data$URL)

# Print the first few rows of the resulting dataframe
head(article_data)

# Initialize a counter
counter <- 0

# Total number of URLs
total <- length(filtered_article_data)


article_data <- map_df(filtered_article_data, ~ {
  url <- .x 
  webpage <- read_html(url)
  counter <<- counter + 1
  # Print the status message
  print(paste("Processing URL", counter, "of", total))
  Sys.sleep(1)
  # Extract article heading
  heading <- webpage %>%
    html_node("h1.ssrcss-1pcdslv-Heading.e10rt3ze0") %>% 
    html_text()
  
  # Extract all text blocks within the article
  text_blocks <- webpage %>%
    html_nodes("div[data-component='text-block'] p.ssrcss-1q0x1qg-Paragraph.e1jhz7w10") %>% 
    html_text()
  
  # Combine all text blocks into a single string
  combined_text <- paste(text_blocks, collapse = " ")
  
  # Extract publication date (year, month, and day)
  pub_date <- webpage %>%
    html_node("time[data-testid='timestamp']") %>% 
    html_attr("datetime") %>% 
    str_extract("\\d{4}-\\d{2}-\\d{2}")  
  
  # Check if pub_date is empty and assign NA if it is
  pub_date <- ifelse(length(pub_date) == 0, NA, pub_date)
  
  # Create a data frame with heading, URL, combined content, and publication date
  data.frame(Heading = ifelse(length(combined_text) > 0, heading, NA),
             URL = url,
             Content = ifelse(length(combined_text) > 0, combined_text, NA),
             PubDate = pub_date,
             stringsAsFactors = FALSE)
})

sentences <- tokenizers::tokenize_sentences(article_data$Content)

sentence_df <- data.frame(
  Headline = rep(article_data$Heading, sapply(sentences, length)),
  Sentence = unlist(sentences),
  stringsAsFactors = FALSE
)

###EXPORT THE SENTENCES SEPERATELY TO USE PYTHON AI TO CREATE SENTIMENT SCORE
exportdf <- data.frame(
  Headline = sentence_df$Headline,sentence = sentence_df$Sentence, stringsAsFactors = FALSE
)

write.csv(exportdf, "climate_sentences.csv") 


#####################################################

#Import dataset where machine learning sentiment analysis has been applied using VaderSentiment in Python

imported_sentiments <- read.csv("climate_sentences_with_scores.csv")

# Calculate average sentiment by article (group by headline)
average_sentiment <- imported_sentiments %>%
  group_by(Headline) %>%
  summarise(avg_sentiment = mean(sentiment_score, na.rm = TRUE))

article_data <- left_join(article_data, average_sentiment, by = c("Heading" = "Headline"))


#####################################################
#PLOT FOR EACH DATA POINT

# Ensure that PubDate is in the correct Date format
article_data$PubDate <- as.Date(article_data$PubDate)

# Plot the line graph
ggplot(article_data, aes(x = PubDate, y = avg_sentiment)) +
  geom_line() +
  labs(x = "Publication Date", y = "Average Sentiment", title = "Sentiment Over Time") +
  theme_minimal()

#####################################################
#PLOT FOR MONTHLY AVERAGE

# Create a new column for the year and month
article_data$YearMonth <- format(article_data$PubDate, "%Y-%m")

# Group by YearMonth and calculate the average sentiment
monthly_avg_sentiment <- article_data %>%
  group_by(YearMonth) %>%
  summarise(avg_sentiment = mean(avg_sentiment, na.rm = TRUE))

# Plot the bar graph
ggplot(monthly_avg_sentiment, aes(x = YearMonth, y = avg_sentiment)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Year-Month", y = "Average Sentiment", title = "Monthly Average Sentiment Over Time") +
  theme_minimal()



write.csv(article_data, "BBC_climate_articles.csv", row.names = FALSE)
