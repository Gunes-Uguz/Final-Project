library(readr)
library(rvest)
library(httr)
library(dplyr)
library(gender)
library(babynames)
library(syuzhet)
library(ggplot2)
library(gridExtra)

# Load ASIN Codes from the CSV file created
asin_data <- read_csv("asin_codes.csv")

# Scrape Amazon Product details
scrape_amazon_data <- function(asin) {
  url <- paste0("https://www.amazon.com/dp/", asin)

  headers <- c(
    "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
    "Accept-Language" = "en-US,en;q=0.9"
  )

  page <- httr::GET(url, httr::add_headers(.headers = headers))
  if (page$status_code != 200) {
    warning(paste("Failed to retrieve:", url))
    return(NULL)
  }

  content <- read_html(page)

  reviews <- content %>%
    html_nodes(".review-text-content span") %>%
    html_text(trim = TRUE)

  usernames <- content %>%
    html_nodes(".a-profile-name") %>%
    html_text(trim = TRUE)

  star_ratings <- content %>%
    html_nodes(".review-rating") %>%
    html_text(trim = TRUE) %>%
    gsub(" out of 5 stars", "", .) %>%
    as.numeric()

  price <- content %>%
    html_nodes(".a-price .a-offscreen") %>%
    html_text(trim = TRUE) %>%
    .[1]

  num_reviews <- min(length(usernames), length(reviews), length(star_ratings))
  if (num_reviews == 0) return(NULL)

  product_data <- data.frame(
    ASIN = rep(asin, num_reviews),
    Username = usernames[1:num_reviews],
    Review = reviews[1:num_reviews],
    StarRating = star_ratings[1:num_reviews],
    ActualPrice = rep(price, num_reviews),
    stringsAsFactors = FALSE
  )

  return(product_data)
}


all_data <- lapply(asin_data$x, function(asin) {
  Sys.sleep(2)  
  tryCatch(scrape_amazon_data(asin), error = function(e) NULL)
})

all_data <- Filter(Negate(is.null), all_data)
final_df <- bind_rows(all_data)


final_df <- left_join(final_df, asin_data, by = "ASIN")

#  Gender & Generation Estimation 

final_df$FirstName <- sapply(strsplit(final_df$Username, " "), `[`, 1)
gender_results <- gender(unique(final_df$FirstName), method = "ssa")

final_df <- left_join(final_df, gender_results[, c("name", "gender")],
                      by = c("FirstName" = "name"))

estimate_name_year <- function(name_input) {
  name_data <- babynames %>%
    filter(tolower(name) == tolower(name_input)) %>%
    group_by(year) %>%
    summarise(total = sum(n)) %>%
    ungroup()
  
  if (nrow(name_data) == 0) return(NA)
  round(sum(name_data$year * name_data$total) / sum(name_data$total))
}

get_generation <- function(year) {
  case_when(
    is.na(year) ~ NA_character_,
    year >= 1928 & year <= 1945 ~ "Silent",
    year >= 1946 & year <= 1964 ~ "Boomer",
    year >= 1965 & year <= 1980 ~ "Gen X",
    year >= 1981 & year <= 1996 ~ "Millennial",
    year >= 1997 & year <= 2012 ~ "Gen Z",
    year >= 2013 ~ "Gen Alpha",
    TRUE ~ NA_character_
  )
}

final_df <- final_df %>%
  mutate(
    EstimatedYear = sapply(FirstName, estimate_name_year),
    Generation = get_generation(EstimatedYear)
  )

#  Sentiment Analysis from Product Review Texts 

final_df <- final_df %>%
  mutate(
    PriceNumeric = as.numeric(gsub("[$,]", "", ActualPrice)),
    Review = as.character(Review)  
  ) %>%
  filter(!is.na(Review))

final_df$Sentiment <- get_sentiment(final_df$Review, method = "syuzhet")

# Visualization  

final_df <- na.omit(final_df)

plot1 <- ggplot(final_df, aes(x = PriceNumeric, y = Sentiment, color = gender)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values = c("female" = "turquoise", "male" = "hotpink")) +
  labs(
    title = "Sentiment vs Price by Gender",
    x = "Price ($)",
    y = "Review Sentiment",
    color = "Gender"
  ) +
  theme_minimal()

plot2 <- ggplot(final_df, aes(x = PriceNumeric, y = StarRating, color = gender)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values = c("female" = "turquoise", "male" = "hotpink")) +
  labs(
    title = "Star Rating vs Price by Gender",
    x = "Price ($)",
    y = "Star Rating",
    color = "Gender"
  ) +
  theme_minimal()

plot3 <- ggplot(final_df, aes(x = PriceNumeric, y = Sentiment, color = Generation)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ gender) +
  scale_color_brewer(palette = "Set2") +
  labs(
    title = "Review Sentiment vs Price by Generation and Gender",
    x = "Price ($)",
    y = "Review Sentiment",
    color = "Generation"
  ) +
  theme_minimal()

plot4 <- ggplot(final_df, aes(x = PriceNumeric, y = StarRating, color = Generation)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ gender) +
  scale_color_brewer(palette = "Set2") +
  labs(
    title = "Star Rating vs Price by Generation and Gender",
    x = "Price ($)",
    y = "Star Rating",
    color = "Generation"
  ) +
  theme_minimal()

# Save each individual plot as a PNG
ggsave("sentiment_vs_price_by_gender.png", plot = plot1, width = 8, height = 6)
ggsave("star_rating_vs_price_by_gender.png", plot = plot2, width = 8, height = 6)
ggsave("sentiment_vs_price_by_generation_and_gender.png", plot = plot3, width = 8, height = 6)
ggsave("star_rating_vs_price_by_generation_and_gender.png", plot = plot4, width = 8, height = 6)


combined_plot <- grid.arrange(plot1, plot2, plot3, plot4, nrow = 2)

ggsave("combined_plots.png", plot = combined_plot, width = 12, height = 10)


#  Regressions

final_df$gender <- as.factor(final_df$gender)
ols_sentiment <- lm(Sentiment ~ PriceNumeric + gender, data = final_df)
summary(ols_sentiment)
capture.output(summary(ols_sentiment), file = "ols_sentiment_summary.txt")

ols_review_sentiment <- ggplot(final_df, aes(x = PriceNumeric, y = Sentiment, color = gender)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) + 
  scale_color_manual(values = c("female" = "turquoise", "male" = "hotpink")) +
  labs(
    title = "Sentiment vs. Price by Gender",
    x = "Price ($)",
    y = "Review Sentiment",
    color = "Gender"
  ) +
  theme_minimal()

ols_star <- lm(StarRating ~ PriceNumeric + gender, data = final_df)
summary(ols_star)

capture.output(summary(ols_star), file = "ols_star_summary.txt")

ols_star_rating <- ggplot(final_df, aes(x = PriceNumeric, y = StarRating, color = gender)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +  
  scale_color_manual(values = c("female" = "turquoise", "male" = "hotpink")) +
  labs(
    title = "Star Rating vs. Price by Gender",
    x = "Price ($)",
    y = "Star Rating",
    color = "Gender"
  ) +
  theme_minimal()


ggsave("OLS_star_rating.png", plot = ols_star_rating, width = 8, height = 6)
ggsave("OLS_review_sentiment.png", plot = ols_review_sentiment, width = 8, height = 6)