library(stringr)

words <- read.csv('data/slovnyk_ua/words.csv', stringsAsFactors = F, header = F)$V1

get_letters_positions <- function(words) {
  
  words <- sort(words)
  words <- tolower(words)
  words <- subset(words, nchar(words) != 1)
  words <- subset(words, !grepl(words, pattern = '\\d'))
  words <- trimws(words)
  words <- unique(words)
  
  #define alphabet
  alphabet <- c('а', 'б', 'в', 'г', 'ґ', 'д', 'е', 'є', 'ж', 'з', 'и', 'і', 'ї', 'й', 'к', 'л', 'м', 'н', 'о', 'п', 'р', 'с', 'т', 'у', 'ф', 'х', 'ц', 'ч', 'ш', 'щ', 'ь', 'ю', 'я')
  
  #create empty df
  #positions = data.frame(stringsAsFactors = F)
  
  for (word in words) {
    
    #check if letters are in the word and subset alphabet
    letters = str_detect(pattern = alphabet, string = word)
    letters = alphabet[letters]
    
    #get positions
    for(letter in letters) {
      
      position = sapply(strsplit(word, ''), function(x) which(x == letter))
      
      if (length(dim(position)) > 1) {
        
        position = position[,1]
        
      }
      
      df = data.frame(word = word, letter = letter, position = position,
                      stringsAsFactors = F)
      
      #positions = rbind.data.frame(positions, df)
      
      #save positions to file
      write.table(df, 'data/slovnyk_ua/positions.csv', 
                  row.names = F, col.names = F,
                  append = T)
      
    }
    
  }
  
  #return(positions)
  
}

get_letters_positions(words)
