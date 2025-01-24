library(rvest)
library(tm)
library(textclean)
library(stopwords)
library(SnowballC)
library(topicmodels)
library(ggplot2)
library(writexl)
library(hunspell)
library(textstem)

url1 <- "https://www.thedailystar.net/business/news/industries-strained-tax-hike-energy-crisis-3800296"
url2 <- "https://www.thedailystar.net/business/economy/news/how-industries-are-coping-3338946?"


webpage1 <- read_html(url1)
article_text1 <- webpage1 %>% html_nodes("article") %>% html_text(trim = TRUE)

webpage2 <- read_html(url2)
article_text2 <- webpage2 %>% html_nodes("article") %>% html_text(trim = TRUE)

texts <- list(article_text1, article_text2)

clean_texts <- lapply(texts, function(text) {
  text <- tolower(text)
  text <- removePunctuation(text)
  text <- removeNumbers(text)
  text <- stripWhitespace(text)
  text <- textclean::replace_contraction(text)
  text
})

tokens_list <- lapply(clean_texts, function(text) {
  tokens <- unlist(strsplit(text, "\\s+"))
  tokens <- tokens[!tokens %in% stopwords("en")]
  tokens_stemmed <- wordStem(tokens)
  tokens_lemmatized <- lemmatize_strings(tokens_stemmed)
  tokens_corrected <- tokens_lemmatized[hunspell_check(tokens_lemmatized)]
  paste(tokens_corrected, collapse = " ")
})

corpus <- Corpus(VectorSource(tokens_list))

dtm <- DocumentTermMatrix(corpus)

dtm_tfidf <- weightTfIdf(dtm)


k <- 4 
lda_model <- LDA(dtm, k = k, control = list(seed = 1234))


terms_per_topic <- terms(lda_model, 5)
print("Most probable words for each topic:")
print(terms_per_topic)


topic_proportions <- posterior(lda_model)$topics
print("Topic proportions for each document:")
print(topic_proportions)


cleaned_data_df1 <- data.frame(Cleaned_Text = tokens_list[[1]])
cleaned_data_df2 <- data.frame(Cleaned_Text = tokens_list[[2]])

write_xlsx(cleaned_data_df1, "Cleaned_Article_Text_Link1.xlsx")
write_xlsx(cleaned_data_df2, "Cleaned_Article_Text_Link2.xlsx")

print("Cleaned data has been saved to 'Cleaned_Article_Text_Link1.xlsx' and 'Cleaned_Article_Text_Link2.xlsx'")


topic_term_matrix <- as.data.frame(as.table(terms(lda_model, 5)))
colnames(topic_term_matrix) <- c("term", "topic", "beta")
topic_term_matrix$topic <- as.factor(topic_term_matrix$topic)

ggplot(topic_term_matrix, aes(x = reorder(term, beta), y = beta, fill = topic)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  facet_wrap(~topic, scales = "free", ncol = 2) +
  coord_flip() +
  labs(title = "Top Words in Each Topic", x = "Terms", y = "Beta Value") +
  theme_bw() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

