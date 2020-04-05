# content-analysis-for-evaluating-ML-performances


**How Content Analysis Helps Evaluating Supervised Machine Learning Performances**


- The main goal of machine learning is making predictions. But how do we know algorithms make good predictions? For supervised machine learning, the quality of predictions is evaluated by comparing predicted values with observed values. The assumption here is these observations reveal ground truth.
- However, this statement is often a very strong assumption. Raw data is an oxymoron ([Gitelman 2013](https://mitpress.mit.edu/books/raw-data-oxymoron)). Every data is a social construction. Thus, without examining how the training data was created, "garbage in and garbage out" is inevitable ([Geiger et al. 2019](https://stuartgeiger.com/papers/gigo-fat2020.pdf)).
- Content analysis helps understand how training data was created, providing reasonable base estimates to evaluate the performances of supervised machine learning algorithms. In this article, I document how I have carried out this project from beginning to end. When possible, I provide all the code and data used for this project (including the training but not the text data due to copyright issues).

## Motivation

This project is part of my dissertation research. I have mostly worked with survey and experimental data to study political opinion among racial minority groups in the United States. However, as I started working on my dissertation prospectus, I realized that survey data is not a panacea. The major surveys on racial minority groups started in the 1990s and 2000s. It does not provide a good data source for studying what political issues were prevalent among racial minority communities during the civil rights movement. These data missed the defining period in American racial politics. As an alternative strategy to collect data, I developed a project that uses machine learning to classify newspaper articles circulated among racial minority groups from the 1960s through the 1980s. These articles were intentionally collected from the West Coast newspapers as my research focus was multiracial coalition-building.


## Workflow

I needed one semester (Fall 2018) to train my four undergraduate research assistants (RAs) to collect approximately 80,000 newspaper articles and to systematically label them according to the two major issue areas: linked progress (collective gain issue) and linked hurt (collective loss issue). For the sake of time, I have skipped further discussions on the case selection strategy and data collection process. These details will be provided in the paper, which I plan to circulate before summer 2020. The earlier draft was presented at the 2019​ Western Political Science Association annual meeting and was selected to receive [the Don T. Nakanishi Award for Distinguished Scholarship in Asian Pacific American politics](https://www.wpsanet.org/award/).

The original data came from the [Ethnic NewsWatch](https://www.proquest.com/products-services/ethnicnewswatch_hist.html) database, which has compiled more than 2.5 million articles published in U.S. ethnic newspapers and magazines. ProQuest created this database and does not allow web scraping. However, one can still download their articles and save them as HTML files. I have developed a [simple HTML parser](https://github.com/jaeyk/proquest_parser) to turn the database search results into CSV files.

In this article, I focus on the rest of the research process: **content analysis, text classification, model evaluation, and data visualization.**


### Content analysis [[Code](https://github.com/jaeyk/content-analysis-for-evaluating-ML-performances/blob/master/code/01_content_analysis.Rmd)]

- The four RAs were divided into two groups.
- Each group labeled 1,008 articles according to two binary categories: linked progress and linked hurt. These articles were randomly selected from the original corpus stratifying on publication years.
- Although these RAs were never informed about the research objective, they had plenty of training opportunities. They learned about the distribution of topics in these newspaper articles by participating in topic modeling analysis. During this process, they also read 2,400 articles and discussed them in a group. (While I ended up not using topic modeling, the process helped the RAs to learn more about the corpus.) Therefore, when they started labeling these articles, the RAs had extensive knowledge about the articles they had read.
- Content analysis is an attempt to confirm whether the data under investigation follows the researcher’s proposed **conceptual** framework. A good measure should be an **accurate and reliable** construct. Thus, we checked whether the labels (measures) created by these RAs were accurate and reliable.

#### Accuracy

We investigated the extent to which two RAs (RA `A` and RA `B`) **agreed** with each other (percent agreement). Figure 1 shows that these rates are between 80 (linked progress) and 90 (linked hurt) percent. Knowing the agreement rate is useful, as it provides a reasonable baseline estimate for machine learning performances. If my undergraduates, who had trained for several weeks, had difficulty labelling these articles, then I would not predict that an algorithm would do a much better job. An algorithm is mimicking and scaling up human behaviors; thus, it also follows the bias and noise embedded in the training data.

```{R}
df <- bind_rows(
  mutate(training_asian, group = "Asian Americans"),
  mutate(training_black, group = "African Americans")
)
```

![](https://github.com/jaeyk/content-analysis-for-evaluating-ML-performances/blob/master/outputs/content_analysis_agreement.png)
Figure 1. How two RAs agreed with each other.

#### Reliability

The agreement rates looked good. However, it was too early to celebrate. Every measure has true value plus bias and noise. The two RAs could have agreed with each other by chance. Therefore, I calculated Cohen's kappa coefficient (k), which shows to what extent the outcome is systematic or stochastic. If k is 1, then there is perfect agreement. If k is 0, then the agreement is by chance. Figure 2 shows that the k statistic is particularly low for Asian American data. As I trained my RAs for weeks, I do not think that the problem lies in inadequate coder training. I assume that it has more to do with the relative difficulty of labeling Asian American newspaper articles. This test shows that Asian American data is much noisier.

![](https://github.com/jaeyk/content-analysis-for-evaluating-ML-performances/blob/master/outputs/content_analysis_kappa.png)
Figure 2. Inter-coder reliability score.

### Text classification [[Code](https://github.com/jaeyk/content-analysis-for-evaluating-ML-performances/blob/master/code/02_text_classification.ipynb)]

I followed the standard procedure: cleaned the text data, performed feature engineering (turned the text data into a document-term matrix and reduced the number of features), trained various algorithms, and checked their performances using accuracy and balanced accuracy scores.

Here, the main challenge was doing this with two different datasets and two different response variables. Instead of creating a one-size-fits-all model, I built and trained a model for each data and each response variable. The content analysis showed that the data-generating process for each data and each response variable was distinct; thus, it is reasonable to take this separation approach.

From a practical perspective, taking this approach could make the code lengthy and complicated. To avoid this problem, I created numerous custom functions that made applying the identical procedure to different data and different response variables easier.

For example, below is my custom function for testing multiple models. Making and utilizing these functions made the machine learning much easier, faster, and more interpretable.

```{Python}
def test_models(models, data):
    lasso = test_model(models[0], data[0], data[1], data[2], data[3])
    bayes = test_model(models[1], data[0], data[1], data[2], data[3])
    xgboost = test_model(models[2], data[0], data[1], data[2], data[3])
    return(lasso, bayes, xgboost)
```

The other important thing I did was resampling. I upsampled the minority class from the training data before feature engineering. Since that class (positive value) is what I cared about in this study, the accuracy score could be misleading if the algorithm misses this class severely. After importing the training data, I checked the balance between the two classes and included resampling in the model training function. In the next section, I show the performance of the machine algorithms with or without resampling.

### Model evaluation [[Code](https://github.com/jaeyk/content-analysis-for-evaluating-ML-performances/blob/master/code/03_model_evaluations.Rmd)]

The accuracy score simply calculates the degree to which the model correctly predicts both positive and negative values. The balanced accuracy score adds up the accuracy score for each class and divides it by the number of classes.

Figure 3 shows that resampling (to be precise, upsampling) makes a critical difference, particularly for the balanced accuracy scores of the three machine learning algorithms.

![](https://github.com/jaeyk/content-analysis-for-evaluating-ML-performances/blob/master/outputs/ml_performances.png)
Figure 3. ML performances with or without resampling

Figure 4 shows how the machine learning algorithms performed when we used the inter-coder agreement rate as a benchmark (dashed line). As expected, the machine learning algorithms performed either similarly (with resampling) or slightly less than human coders (without resampling).

![](https://github.com/jaeyk/content-analysis-for-evaluating-ML-performances/blob/master/outputs/ml_content.png)
Figure 4. ML performances against the human benchmark

### Data visualization [[Code](https://github.com/jaeyk/content-analysis-for-evaluating-ML-performances/blob/master/code/04_time_series_visualization.Rmd)]

I plotted the results (training data + predicted data) as two time-series plots. The Asian American data has extensive noise because the sample size was smaller (thus, wider confidence intervals). However, this data was also less reliable from the beginning. When aggregating data (using a higher-level unit of analysis), the data becomes less noisy. In contrast, when disaggregating data (using a lower-level unit of analysis), the data becomes noisier. In the bottom pattern (monthly observations), it is extremely difficult to read how the two labels relate to each other in the Asian American data. We can do a more fine-grained analysis using the African American data but not the Asian American data.

![](https://github.com/jaeyk/content-analysis-for-evaluating-ML-performances/blob/master/outputs/time_series_plot.png)
Figure 5. Time series trends


## Conclusion

Garbage in and garbage out is true, and machine learning is no exception. Knowing exactly how garbage the training data is enables estimating how well machine learning will perform. Furthermore, this knowledge also helps investigators have a deeper understanding of the quality of the machine-predicted data. In [my other project](https://github.com/jaeyk/ITS-Text-Classification), I discussed how we can use machine learning to create data for causal inference. When using machine-predicted data for statistical and causal inferential problems, we should take this data quality problem seriously. Systematic efforts to document the data collection process are key to achieving this goal.
