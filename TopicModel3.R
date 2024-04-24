# Load the necessary libraries
library(topicmodels)
library(tidytext)
library(tm)
library(slam)
library(readr)
library(tidyverse)
library(ggrepel)
library(bbplot)
# Read data
article_data <- read_csv("BBC_climate_articles.csv")

# Define a custom stopword list
custom_stopwords <- c("climate", "change", "global", "warming", "carbon", "emissions", "crisis")

# Create a corpus from the 'Content' column of your dataframe
vec_corpus <- Corpus(VectorSource(article_data$Content))

# Preprocess the corpus: convert to lowercase, remove punctuation, numbers, stopwords, and perform stemming
vec_corpus <- tm_map(vec_corpus, content_transformer(tolower))
vec_corpus <- tm_map(vec_corpus, removePunctuation)
vec_corpus <- tm_map(vec_corpus, removeNumbers)
vec_corpus <- tm_map(vec_corpus, removeWords, stopwords("en"))
vec_corpus <- tm_map(vec_corpus, removeWords, custom_stopwords)  # Exclude custom stopwords
vec_corpus <- tm_map(vec_corpus, stemDocument)

# Create a Document-Term Matrix (DTM)
dtm <- DocumentTermMatrix(vec_corpus)

# Remove sparse terms from the DTM
dtm <- removeSparseTerms(dtm, sparse = 0.99)

# Remove rows containing only zeros
dtm <- dtm[row_sums(as.matrix(dtm)) > 0, ]

# Fit LDA model
lda_model <- LDA(dtm, k = 15)

# Get the most likely topic for each document
most_likely_topic <- topics(lda_model)

# Define a custom dictionary of climate-related terms
climate_dictionary <- c("renewable", "energy", "extreme", "weather", "policy", "sustainability")

# Assign topic labels
topic_labels <- c("Climate Protests", "Ice and Water Studies", "Oil and Fossil Fuels", 
                  "Country and Development", "Wildfires and Evacuations", 
                  "Agriculture and Farming", "Forestry and Reforestation", 
                  "Local Stories", "Flooding and Storms", "Temperature Records", 
                  "Public Opinion and Statements", "Heatwaves and Extreme Weather", 
                  "Government Policies and Statements", "Renewable Energy Sources", 
                  "Traditional Power Generation")


terms(lda_model, 10) 
topics(lda_model)

# Filter article_data to keep rows with assigned topics (all columns)
article_data_filtered <- article_data[which(!is.na(most_likely_topic)), ]

# Assign topic labels directly
article_data_filtered$topic <- topic_labels[most_likely_topic]



############################
# Convert PubDate to Date object and extract Year-Month
article_data_filtered$YearMonth <- format(as.Date(article_data_filtered$PubDate), "%Y-%m")

# Count the number of articles per topic per month
article_counts <- article_data_filtered %>%
  group_by(YearMonth, topic) %>%
  summarise(count = n(), .groups = 'drop') 
# Convert YearMonth to Date object
article_counts$YearMonth <- as.Date(paste(article_counts$YearMonth, "01", sep = "-"), "%Y-%m-%d")

# Generate the line graph using ggplot2
ggplot(article_counts, aes(x = YearMonth, y = count, color = topic)) +
  geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
  labs(title = "Frequency of Articles for Each Topic Over Time",
       x = "Year-Month",
       y = "Number of Articles") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Generate a palette of distinct colors for topics
topic_colors <- c("#E41A1C", "#377EB8", "#4DAF4A", "#FF7F00", "#F781BF", "#A65628", 
                  "#984EA3", "#999999", "#DECF3F", "#FFFF33", "#8DD3C7", "#1B9E77", 
                  "#FFD700", "#D55E00", "#CC79A7")

# Generate the line graph using ggplot2 with distinct colors for each topic
ggplot(article_counts, aes(x = YearMonth, y = count, color = topic)) +
  geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
  labs(title = "Frequency of Articles for Each Topic Over Time",
       x = "Year-Month",
       y = "Number of Articles") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = topic_colors)

# Export article_data_filtered as a CSV file
write.csv(article_data_filtered, "article_data_filtered.csv", row.names = FALSE)

# Get the 5 most common topics
top_5_topics <- article_counts %>%
  group_by(topic) %>%
  summarise(total_count = sum(count)) %>%
  arrange(desc(total_count)) %>%
  slice_head(n = 5) %>%
  pull(topic)

# Filter article_counts to include only the top 5 topics
article_counts <- article_counts %>%
  filter(topic %in% top_5_topics)

# Generate the line graph using ggplot2 with distinct colors for each topic
ggplot(article_counts, aes(x = YearMonth, y = count, color = topic)) +
  geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
  labs(title = "Frequency of Articles for Each Topic Over Time",
       x = "Year-Month",
       y = "Number of Articles") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





# Count the number of articles per topic per month
article_counts <- article_data_filtered %>%
  group_by(YearMonth, topic) %>%
  summarise(count = n(), .groups = 'drop') 

# Convert YearMonth to Date object
article_counts$YearMonth <- as.Date(paste(article_counts$YearMonth, "01", sep = "-"), "%Y-%m-%d")

# Generate the bar chart of the total count of articles for each topic
ggplot(article_counts, aes(x = fct_reorder(topic, count), y = count, fill = topic)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Count of Articles for Each Topic",
       x = "Topic",
       y = "Total Count of Articles") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = topic_colors)



