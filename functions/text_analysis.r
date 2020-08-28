visualize_wf <- function(data, alp, siz, wid){
  
  data %>%
  ggplot(aes(x = Collective_gain, y = Collective_loss)) +
    geom_jitter(alpha = alp, 
                size = siz, 
                width = wid, # % occupy the bins  
                height = 0.25) +
    geom_text(aes(label = word), check_overlap =  TRUE, vjust = 1.5) +
    scale_x_log10(labels = scales::percent_format()) +
    scale_y_log10(labels = scales::percent_format()) +
    geom_abline(color = "red") +
    facet_wrap(~group) +
    labs(x = "Collective gain",
         y = "Collective loss")
  
}

# The following tidy_text function heavily draws on https://www.tidytextmining.com/ngrams.html

tidy_text <- function(data, var1, var2){
  
  # Manipulate strings
  data$source <- gsub('[[:digit:]]', '', data$source) 
  data$source <- gsub('[[:punct:]]+', '', data$source) %>% trimws()
  
  # Filter
  data <- data %>%
    filter({{var1}} == 1 | {{var2}} ==1)
  
  # Mutate 
  data <- data %>%
    mutate(linked_fate = {{var1}} - {{var2}}) %>%
    mutate(linked_fate = case_when(linked_fate == "1" ~ "Collective_gain",
                                   linked_fate == "-1" ~ "Collective_loss",
                                   TRUE ~ as.character(linked_fate)))}

tokenize_text <- function(data){

  data %>%
    # tokenize
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% # Apply bigrams 
    # separate 
    separate(bigram, c("word1", "word2", sep = " ")) %>%
    # remove stop words
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word) %>%
    unite(bigram, word1, word2, sep = " ") %>%
    filter(!word %in% str_remove_all(stop_words$word, "'"),
           str_detect(word, "[a-z]"))
  
}

create_word_frequency <- function(data){
  
  data %>%
    group_by(linked_fate, group) %>%
    count(word, sort = TRUE) %>%
    # Subjoin
    left_join(tidy_articles %>%
                group_by(linked_fate, group) %>%
                summarise(total = n())) %>%
    # Create freq variable
    mutate(freq = n/total) %>%
    # Select only interested columns
    select(linked_fate, group, word, freq) %>%
    pivot_wider(names_from = c("linked_fate"),
                values_from = "freq") %>%
    arrange("Collective_gain", "Collective_loss") 
  
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
