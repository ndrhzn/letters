library(rvest)

#scrape all urls to letters pages------------------------------------------

get_letter_links <- function(){
  
  page <- html_session('http://www.slovnyk.ua/index.php')

  urls <- page %>% 
    html_nodes('.letterlink') %>% 
    html_attr('href')
  
  urls = paste0('http://www.slovnyk.ua/', urls)
  
  foo <- vector()

  for(url in urls) {

    page <- html_session(url)

    pagelink <- page %>%
      html_nodes('.pagelink_a') %>%
      html_attr('href') %>%
      unique()


    if(!is.null(pagelink)) {

      foo <- append(foo, pagelink)

    }

  }
  
  foo <- paste0('http://www.slovnyk.ua/', foo)
  
  urls <- append(urls, foo)
  
  return(urls)
  
}

letters <- get_letter_links()

#scrape all word groups for all letters------------------------------------

get_words_links <- function(letters_urls) {
  
  urls <- data.frame()
  
  for(link in letters_urls) {
    
    page <- html_session(link)
    
    group <- page %>% 
      html_nodes('.wordhref') %>% 
      html_text()
    
    url <- page %>% 
      html_nodes('.wordhref') %>% 
      html_attr('href')
    
    df <- data.frame(group = group, 
                     url = paste0('http://www.slovnyk.ua/', url),
                     stringsAsFactors = F)
    
    urls <- rbind.data.frame(urls, df)
    
    Sys.sleep(1)
    
  }
  
  return(urls)
  
}

words_links <- get_words_links(letters)

#scrape all words-----------------------------------------------------------

get_words <- function(urls) {
  
  for(url in urls) {
    
    message(paste('getting words from', url))
    
    page <- html_session(url)
    
    words <- page %>% 
      html_nodes('.wordhref') %>% 
      html_text()
    
    write.table(words, 'data/slovnyk_ua/words.csv', col.names = F, row.names = F, append = T)
    
    Sys.sleep(1)
  
  }

}

write.table(subset(words_links$group, grepl('swrd', words_links$url)),
            file = 'data/slovnyk_ua/words.csv', 
            col.names = F, row.names = F, append = T)

get_words(subset(words_links$url, !grepl('swrd', words_links$url)))


