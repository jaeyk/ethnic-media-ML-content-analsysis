visualize_wf <- function(data, custom_title){
  
asian <- data %>%
  filter(group == "Asian Americans") %>%
  mutate(diff = Collective_gain - Collective_loss) %>%
  ggplot(aes(x = fct_reorder(bigram, diff), y = diff)) +
  geom_col() +
  coord_flip() +
  labs(title = {{custom_title}},
       subtitle = "Asian Americans",
       x = "Bigrams",
       y = "# of Collective Gain - # of Collective Loss") 

black <- data %>%
  filter(group != "Asian Americans") %>%
  mutate(diff = Collective_gain - Collective_loss) %>%
  ggplot(aes(x = fct_reorder(bigram, diff), y = diff)) +
  geom_col() +
  coord_flip() +
  labs(subtitle = "African Americans",
     x = "Bigrams",
     y = "# of Collective Gain - # of Collective Loss")

asian / black
}

# The following tidy_text function heavily draws on https://www.tidytextmining.com/ngrams.html

tidy_text <- function(data, var1, var2){
  
  # Clean sources 
  data$source <- gsub('[[:digit:]]', '', data$source) 
  data$source <- gsub('[[:punct:]]+', '', data$source) %>% trimws()
  
  # Clean text
  data <- clean_text(data)
  
  # Filter
  data <- data %>%
    filter({{var1}} == 1 | {{var2}} ==1)
  
  # Mutate 
  data <- data %>%
    mutate(linked_fate = {{var1}} - {{var2}}) %>%
    mutate(linked_fate = case_when(linked_fate == "1" ~ "Collective_gain",
                                   linked_fate == "-1" ~ "Collective_loss",
                                   TRUE ~ as.character(linked_fate)))
  
  # Remove stop words; This part of code comes from Mhairi McNeill:https://stackoverflow.com/a/37526926
  
  stopwords <- paste(tm::stopwords('en'), collapse = "\\b|\\b")
  
  stopwords_regex <- paste0("\\b", stopwords, "\\b")
  
  data$text <- gsub(stopwords_regex, "", data$text)
  
  data 
  
}

tokenize_text <- function(data){
  
  data %>%
    # tokenize
    unnest_tokens(bigram, text, token = "ngrams", n = 2)
  
}

create_word_frequency <- function(data){
  
  data %>%
    group_by(linked_fate, group) %>%
    count(bigram, sort = TRUE) %>%
    # Select only interested columns
    select(linked_fate, group, bigram, n)
  
}


filter_n <- function(data){
  
  asian_lp <- data %>% filter(group == "Asian Americans", linked_fate == "Collective_gain") 

  asian_lh <- data %>% filter(group == "Asian Americans", linked_fate != "Collective_gain") 

  asian_lp <- asian_lp %>% arrange(desc(n)) %>% head(15) 
  
  asian_lh <- asian_lh %>% arrange(desc(n)) %>% head(15) 
  
  black_lp <- data %>% filter(group != "Asian Americans", linked_fate == "Collective_gain") 

  black_lh <- data %>% filter(group != "Asian Americans", linked_fate != "Collective_gain") 
  
  black_lp <- black_lp %>% arrange(desc(n)) %>% head(15) 
  
  black_lh <- black_lh %>% arrange(desc(n)) %>% head(15)
  
  asian <- bind_rows(asian_lp, asian_lh) 
  black <- bind_rows(black_lp, black_lh) 
  
  bind_rows(asian, black) %>%
    spread("linked_fate", "n") %>%
    replace(is.na(.), 0)
  
}

filter_words <- function(data){
  
  data %>%
    mutate(etc = as.numeric(str_detect(bigram, "year|years|san|francisco|oakland|washington|th|street|month|months|week|weeks|western|california|united|los|international|district|hong|west|first|second|new york|per cent|alameda|county|bay area|mr|dont know|will|sun reporter|east bay|men women|contra costa|make sure|can get|berkeley|early|community|park|junior|student|dont want|university|task force|association|man|american|americans|pacific|asian|chinese|black|school"))) %>%
    filter(etc != 1)
  
}

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

visualize_diagnostics <- function(sparse_matrix, many_models){
  
  heldout <- make.heldout(sparse_matrix)
  
  k_result <- many_models %>%
    mutate(exclusivity = map(topic_model, exclusivity),
           semantic_coherence = map(topic_model, semanticCoherence, sparse_matrix),
           eval_heldout = map(topic_model, eval.heldout, heldout$missing),
           residual = map(topic_model, checkResiduals, sparse_matrix),
           bound =  map_dbl(topic_model, function(x) max(x$convergence$bound)),
           lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
           lbound = bound + lfact,
           iterations = map_dbl(topic_model, function(x) length(x$convergence$bound)))
  
  k_result %>%
    transmute(K,
              `Lower bound` = lbound,
              Residuals = map_dbl(residual, "dispersion"),
              `Semantic coherence` = map_dbl(semantic_coherence, mean),
              `Held-out likelihood` = map_dbl(eval_heldout, "expected.heldout")) %>%
    gather(Metric, Value, -K) %>%
    ggplot(aes(K, Value, color = Metric)) +
    geom_line(size = 1.5, alpha = 0.7, show.legend = FALSE) +
    facet_wrap(~Metric, scales = "free_y") +
    labs(x = "K (number of topics)",
         y = NULL,
         title = "Model diagnostics by number of topics")}
