# content-analysis-for-evaluating-ML-performances


**How Content Analysis Helps Evaluating Supervised Machine Learning Performances**


- The main goal of machine learning is making predictions. But how do we know algorithms make good predictions? For supervised machine learning, the quality of predictions is evaluated by comparing predicted values with observed values. The assumption here is these observations reveal ground truth. 
- However, this statement is often a very strong assumption. Raw data is an oxymoron ([Gitelman 2013](https://mitpress.mit.edu/books/raw-data-oxymoron)). Every data is a social construction. Thus, without examining how the training data was created, "garbage in and garbage out" is inevitable ([Geiger et al. 2019](https://stuartgeiger.com/papers/gigo-fat2020.pdf)). 
- The content analysis helps understand how training data was created. That information provides reasonable base estimates to evaluate the performances of supervised machine learning algorithms. In this article, I document how I have carried out this project from end to end. If possible, I provide all the code and data (including the training but not the text data for copyright issues) used for this project. 

## Motivation 

This project is part of my dissertation research. I have mostly worked with survey and experimental data to study political opinion among racial minority groups in the United States. However, as I started working on my dissertation prospectus, I realized that survey data is not a panacea. The major surveys on racial minority groups started in the 1990s and 2000s. It does not provide a good data source for studying what political issues were prevalent among racial minority communities during the civil rights movement. These data missed the defining period in American racial politics. As an alternative strategy to collect data, I developed a project that uses machine learning to classify newspaper articles circulated among racial minority groups from the 1960s through the 1980s. These articles were intentionally collected from the West Coast newspapers as my research focus was multiracial coalition-building.  


## Workflow 

It took me around one semester (Fall 2018) to train my four undergraduate research assistants, collect around 80,000 newspaper articles, and systematically label them according to the two major issue areas (linked progress = collective gain issue, linked hurt = collective loss issue). For the sake of time, I skipped further discussions on the case selection strategy and data collection process. These details will be provided in the paper, which I plan to circulate before summer 2020. The earlier draft was presented at the 2019 Western Political Science Association annual meeting and was selected to receive [the Don T. Nakanishi Award for Distinguished Scholarship in Asian Pacific American politics](https://www.wpsanet.org/award/).

The original data came from the [Ethnic NewsWatch](https://www.proquest.com/products-services/ethnicnewswatch_hist.html) database, which has compiled more than 2.5 million articles published in U.S. ethnic newspapers and magazines. Proquest created this database and it does not allow web scraping. One can still download their articles and save them as HTML files. I have developed a [simple HTML parser](https://github.com/jaeyk/proquest_parser) to turn the database search results into CSV files.

In this article, I focus on the rest of the research process: **content analysis, text classification, model evaluation, and data visualization.** 


### Content analysis [[Code](https://github.com/jaeyk/content-analysis-for-evaluating-ML-performances/blob/master/code/01_content_analysis.Rmd)]

- Four RAs were divided into two groups. 
- Each group labeled 1,008 articles according to two binary categories (linked progress and linked hurt). These articles were randomly selected from the original corpus stratifying on publication years. 
- These RAs were never informed about the research objective. Yet, they had plenty of training opportunities. They learned about the distribution of topics in these newspaper articles by participating in the topic modeling analysis. In the process, they also read 2,400 articles and discussed them in a group. (I ended up not using topic modeling but the process helped the RAs to learn more about the corpus.) Therefore, when they started labeling these articles, they had quite good knowledge about the articles they read. 
- Content analysis is an attempt to confirm whether the data under investigation follows the **conceptual** framework a researcher proposed. A good measure should be an **accurate, and reliable** of a construct. Thus, we check whether the labels (measures) created by these RAs were accurate and reliable. 

#### Accuracy 

We looked at to what extent to two RAs (RA `A` and `B`) **agreed** with each other (`ifelse(A == B, 1,0)`). I created two new variables according to this condition and calculated their mean values (percent agreement). Figure 1 shows that these rates are between 80 (linked progress) and linked hurt 90 (linked hurt) percent. Knowing the agreement rate is useful as it provides a reasonable baseline estimate for machine learning performances. If my undergraduates, trained for several weeks, had difficult times to label these articles, then I don't guess that an algorithm will do a much better job. What an algorithm is mimicking and scaling up human behaviors. It also follows the bias and noise embedded in the training data. 

```{R}
df <- bind_rows(
  mutate(training_asian, group = "Asian Americans"),
  mutate(training_black, group = "African Americans")
) %>%
  mutate(linked_progress = ifelse(Promoting_collective_gains_A == Promoting_collective_gains_B, 1, 0),
         linked_hurt = ifelse(Preventing_collective_losses_A == Preventing_collective_losses_B, 1, 0)
```

![](https://github.com/jaeyk/content-analysis-for-evaluating-ML-performances/blob/master/outputs/content_analysis_agreement.png)
Figure 1. How two RAs agreed with each other.

#### Reliability 

The agreement rates look good. However, it is too early to celebrate. Every measure has true value plus bias and noise. Now, we should turn to noise. The two RAs could agree to each other by chance. To address this problem, I calculated Cohen's kappa coefficient (k). The statistic shows to what extent the outcome is systematic or stochastic. If k is 1, then there was perfect agreement. If k is 0, then the agreement is by chance. Figure 2 shows that k statistic is especially low for Asian American data. As I trained my RAs for weeks, I do not think that the problem lies in inadequate coder training. I assume that it has more to do with the relative difficulty of labeling Asian American newspaper articles. This test shows that Asian American data is much noisier. 

![](https://github.com/jaeyk/content-analysis-for-evaluating-ML-performances/blob/master/outputs/content_analysis_kappa.png)
Figure 2. Inter-coder reliability score. 

#### Correlation coefficients 

The correlation coefficients between the two binary labels are neither about the accuracy or reliability. But it's worth checking out as it gives some ideas on how to think about the relationship between the two binary labels. The classification problem assumes that **these labels (or categories) are strictly different (or mutually exclusive)**. Yet, again, this is also an empirical question. The result shows that the strength of correlations is much weaker in the Asian American case compared to the African American case. This result shows that there more intersecting areas exist between the two categories in the African American case relative to the Asian American case.

![](https://github.com/jaeyk/content-analysis-for-evaluating-ML-performances/blob/master/outputs/corr_analysis.png)
Figure 3. Correlation between the two binary categories. 

### Text classification [[Code](https://github.com/jaeyk/content-analysis-for-evaluating-ML-performances/blob/master/code/02_text_classification.ipynb)]

I followed the standard procedure. I cleaned the text data, did feature engineering (turned the text data into a document-term matrix and reduced the number of features), trained various algorithms, and checked their performances using accuracy and balanced accuracy scores. 

Here the main challenge is doing this with two different datasets and two different response variables. Instead of creating one-size-fits-all model, I built and trained a model for each data and for each response variable. We have seen that the data genetring process for each data and each response variable is distinct from the content analysis above. Thus, it is reasonable to take this separation approach.  

Practically, taking this approach could make code lengthy and complicated. To avoid this problem, I created lots of custom functions that made applying the identical procedure to different data and different response variables easy. 

For example, the below is my custom function for testing multiple models. Making and utilizing these functions makes doing machine learning much easier, faster, and more interpretable.  

```{Python}
def test_models(models, data):
    lasso = test_model(models[0], data[0], data[1], data[2], data[3]) 
    bayes = test_model(models[1], data[0], data[1], data[2], data[3])
    xgboost = test_model(models[2], data[0], data[1], data[2], data[3])
    return(lasso, bayes, xgboost)
```

The other important thing I did is resampling. I upsampled the minority class from the training data before feature engineering. It is because that class (positive value) is what I cared in this study and accuracy score could be misleading if the algorithm misses this class severely. After importing the training data, I checked the balance between the two classes and included resampling in the model training function. In the next section, I showed the performance of machine algorithms with or without resampling. 

### Model evaluation [[Code](https://github.com/jaeyk/content-analysis-for-evaluating-ML-performances/blob/master/code/03_model_evaluations.Rmd)]

Accuracy score simply calculates the degree to which the model predicts both positive and negative values correctly. Balanced accuracy score adds up the accuracy score for each class and divide it by the number of classes. 

Figure 4 shows that resampling (to be precise, upsampling) makes a critical difference especially for the balanced accuracy scores of the three machine learning algorithms.

![](https://github.com/jaeyk/content-analysis-for-evaluating-ML-performances/blob/master/outputs/ml_performances.png)
Figure 4. ML performances with or without resampling

Figure 5 shows that how machine learning algorithms performed when we used the inter-coder agreement rate as a benchmark (dashed line). As expected, machine learning algorithms performed either similarly or slightly less than human coders.

![](https://github.com/jaeyk/content-analysis-for-evaluating-ML-performances/blob/master/outputs/ml_content.png)
Figure 5. ML performances against the human benchmark 

### Data visualization [[Code](https://github.com/jaeyk/content-analysis-for-evaluating-ML-performances/blob/master/code/04_time_series_visualization.Rmd)]

![https://github.com/jaeyk/content-analysis-for-evaluating-ML-performances/blob/master/outputs/time_series_plot.png]
Figure 6. Time series trends 