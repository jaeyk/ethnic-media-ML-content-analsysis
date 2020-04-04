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
         title = "Model diagnostics by number of topics")
  
}

# This is Julia Silge's code 

visualize_stm <- function(topic_model, news_sparse){
  
td_gamma <- tidy(topic_model, matrix = "gamma",
                 document_names = rownames(news_sparse))

top_terms <- tidy(topic_model) %>%
  arrange(beta) %>%
  group_by(topic) %>%
  top_n(7, beta) %>%
  arrange(-beta) %>%
  select(topic, term) %>%
  summarise(terms = list(term)) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) %>% 
  unnest()

gamma_terms <- td_gamma %>%
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>%
  arrange(desc(gamma)) %>%
  left_join(top_terms, by = "topic") %>%
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, gamma))

gamma_terms %>%
  top_n(5, gamma) %>%
  ggplot(aes(topic, gamma, label = terms, fill = topic)) +
  geom_col(show.legend = FALSE) +
  geom_text(hjust = 0, nudge_y = 0.0005, size = 3,
            family = "IBMPlexSans") +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 0.09),
                     labels = percent_format()) +
  theme_tufte(base_family = "IBMPlexSans", ticks = FALSE) +
  theme(plot.title = element_text(size = 16,
                                  family="IBMPlexSans-Bold"),
        plot.subtitle = element_text(size = 13)) +
  labs(x = NULL, y = expression(gamma),
       title = "Top 5 topics by prevalence")

}