library(rvest)
library(magrittr)

# Use the links of your search results
urls <- c(
  "https://www.amazon.com/s?k=women+luxury&ref=nb_sb_noss", 
  "https://www.amazon.com/s?k=men+luxury&crid=TY7P7MSM0C4R&sprefix=men+luxury%2Caps%2C174&ref=nb_sb_noss_1"
  )
 
all_asins <- c()
for (url in urls) {
  page <- read_html(url)
  
  product_nodes <- page %>%
    html_elements(xpath = "//div[@data-asin and @data-component-type='s-search-result']")
  
  asins <- product_nodes %>%
    html_attr("data-asin") %>%
    na.omit()
  
  all_asins <- c(all_asins, asins)
}

top_20_from_each <- head(all_asins, 40)

write.csv(top_20_from_each, file = "asin_codes.csv", row.names = FALSE)

cat("The top 20 ASIN codes from each URL (total 40 ASINs) have been saved to 'asin_codes.csv'\n")
