# in case you don't have these installed
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("rvest")) install.packages("rvest")
if (!require("xml2")) install.packages("xml2")
if (!require("tidytext")) install.packages("tidytext")
if (!require("stringr")) install.packages("stringr")
library(tidyverse)
library(rvest)
library(xml2)
library(tidytext)
library(stringr)

# helper function
ifNoneThenZero = function(x) {
  x = suppressWarnings(as.numeric(x))
  ifelse(is.na(x), 0, x)
}


# Extractor Functions -----------------------------------------------------

getName = function(node) {
  reviewerName = node %>% 
    xml_child(1) %>% 
    xml_child(1) %>% 
    xml_attrs() %>% 
    .[["alt"]]
  
  list(reviewerName = reviewerName)
}

getReviewText = function(node) {
  reviewText = node %>% 
    html_node('.review-full-text')
  
  if(length(reviewText) == 0) {
    reviewText = node %>% 
      html_node('.Jtu6Td')
    list(reviewText = xml_text(reviewText))
  } else {
    list(reviewText = xml_text(reviewText)) 
  }
}

getReveiwerData = function(node) {
  reviewerData = node %>% 
    html_node('.A503be') %>% 
    xml_text()
  
  list(isLocalGuide = grepl("Local Guide", reviewerData),
       reviews = ifNoneThenZero(str_extract(reviewerData, "([0-9]+)(?= review)")),
       photos = ifNoneThenZero(str_extract(reviewerData, "([0-9]+)(?= photo)")))
}

getRating = function(node) {
  rating = suppressWarnings({
    node %>% 
      html_node(".Fam1ne.EBe2gf") %>% 
      html_attr("aria-label") %>% 
      str_extract("(?!Rated )([0-9.]+)") %>% 
      as.numeric(.)})
  
  list(rating = rating)
}

getTimeReviewed = function(node) {
  timeReviewed = node %>% 
    html_node(".dehysf") %>% 
    xml_text()
  
  list(timeReviewed = timeReviewed)
}

getLikes = function(node) {
  likes = suppressWarnings({
    node %>% 
      html_node(".QWOdjf.i8w0muTVe_Wg-ekHcgmb48aU") %>% 
      xml_text() %>% 
      ifNoneThenZero()
  })
  
  list(likes = likes)
}

getResponseData = function(node) {
  responseNodes = node %>% 
    html_node('.LfKETd') %>% 
    xml_contents()
  
  if(length(responseNodes) == 2) {
    list(responseTime = responseNodes[[1]] %>% xml_child(3) %>% xml_text(),
         responseText = responseNodes[[2]] %>% xml_text()) 
  } else {
    list(responseTime = NA,
         responseText = NA)
  }
}


# Master Function ---------------------------------------------------------

extractGoogleReviewData = function(node) {
  list(getName(node),
       getReviewText(node),
       getReveiwerData(node),
       getRating(node),
       getTimeReviewed(node),
       getLikes(node),
       getResponseData(node)) %>% 
    reduce(append) %>% 
    bind_cols()
}
