import pip
import pandas as pd
print("Pandas is installed and imported successfully!")
pip.main(['install', 'vaderSentiment'])


import pandas as pd
from vaderSentiment.vaderSentiment import SentimentIntensityAnalyzer

# Load sentences from CSV (adjust the file path accordingly)
csv_file = 'climate_sentences.csv'
df = pd.read_csv(csv_file)

# Initialize VADER Sentiment Analyzer
analyzer = SentimentIntensityAnalyzer()

# Compute sentiment scores for each sentence
sentiment_scores = []
for sentence in df['sentence']:
    sentiment_scores.append(analyzer.polarity_scores(sentence)['compound'])

# Add sentiment scores as a new column
df['sentiment_score'] = sentiment_scores

# Save the updated data back to CSV
df.to_csv('climate_sentences_with_scores.csv', index=False)

print("Sentiment scores added and saved to 'climate_sentences_with_scores.csv'.")
