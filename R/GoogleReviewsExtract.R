source("R/extract_func.R")

# read in the html
reviews_html = xml2::read_html("data/KenGarffHondaOfOrem-GoogleReviews.html")

# XPATH for reviews
REVIEW = '//*[contains(concat( " ", @class, " " ), concat( " ", "gws-localreviews__google-review", " " ))]'

reviews_nodeset = reviews_html %>% 
  rvest::html_nodes(xpath = REVIEW)

extractedData = map(reviews_nodeset, extractGoogleReviewData) %>% bind_rows()

write_csv(extractedData, "data/extractedData.csv")