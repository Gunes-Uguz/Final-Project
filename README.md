# Final Project
## Girl Math Lens on Consumer Behavior

This project scrapes Amazon product details and reviews to explore how review sentiment and star ratings vary by gender and generation. 
It uses a mix of web scraping, natural language processing, and demographic inference to draw insights from customer feedback.

---

## Overview

Here's what the scripts do:

1. **Determine Products of Interest** Scrapes ASINs (Amazon product IDs) from search results for "men luxury" and "women luxury" products.
2. **Collects review data** for each ASIN – including usernames, review texts, star ratings, and prices.
3. **Infers gender** from usernames using name-based prediction tools using gender package.
4. **Estimates generation** based on the popularity of first names across U.S. birth years.
5. **Analyzes sentiment** in review text using the `syuzhet` sentiment lexicon.
6. **Visualizes patterns** in sentiment and star ratings against product prices across gender and generation.
7. **Runs basic regression models** to see how price and gender influence sentiment and star ratings.

---

## Files Created

- `asin_codes.csv` – ASIN codes scraped from Amazon
- `final_df` – The combined dataset after cleaning and analysis
- Plot images:
  - `sentiment_vs_price_by_gender.png`
  - `star_rating_vs_price_by_gender.png`
  - `sentiment_vs_price_by_generation_and_gender.png`
  - `star_rating_vs_price_by_generation_and_gender.png`
  - `combined_plots.png` – all four visualizations arranged together
  - `OLS_review_sentiment.png` and `OLS_star_rating.png` – plots with regression lines
- Text summaries:
  - `ols_sentiment_summary.txt`
  - `ols_star_summary.txt`

---

## Required Libraries

The script uses the following R packages:

```r
install.packages(c(
  "readr", "rvest", "httr", "dplyr", "gender", "babynames",
  "syuzhet", "ggplot2", "gridExtra", "magrittr"
))
