clean_text <- function(data){
  
  data <- data %>%
    mutate(text = tolower(text),
           text = str_replace_all(text, '[\r?\n]',''),
           text = str_replace_all(text, '[^\\w\\s]',''),
           text = str_replace_all(text, '\\d+', ''),
           text = trimws(text),
           postID = row_number())
  
  return(data)
}

create_sparse_matrix <- function(data){
  data <- data %>%
    unnest_tokens(word, text) %>%
    anti_join(get_stopwords()) %>%
    filter(!str_detect(word, "[0-9]+")) %>%
    add_count(word) %>%
    filter(n > 100) %>%
    select(-n) %>%
    count(postID, word) %>%
    cast_sparse(postID, word, n)
}
