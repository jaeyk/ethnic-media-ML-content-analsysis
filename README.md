# content-analysis-for-evaluating-ML-performances


**How Content Analysis Helps Evaluating Supervised Machine Learning Performances**


- The main goal of machine learning is making predictions. But how do we know algorithms make good predictions? For supervised machine learning, the quality of predictions is evaluated by comparing predicted values with observed values. The assumption here is these observations reveal ground truth.
- However, this statement is often a very strong assumption. Raw data is an oxymoron ([Gitelman 2013](https://mitpress.mit.edu/books/raw-data-oxymoron)). Every data is a social construction. Thus, without examining how the training data was created, "garbage in and garbage out" is inevitable ([Geiger et al. 2019](https://stuartgeiger.com/papers/gigo-fat2020.pdf)).
- Content analysis helps understand how training data was created, providing reasonable base estimates to evaluate the performances of supervised machine learning algorithms. In this article, I document how I have carried out this project from beginning to end. When possible, I provide all the code and data used for this project (including the training but not the text data due to copyright issues).
- The key takeaway: **Using less reliable training data leads to not only less accurate predictions but also more extreme interpretations**.

## Motivation

This project is part of my dissertation research. I have mostly worked with survey and experimental data to study political opinion among racial minority groups in the United States. However, as I started working on my dissertation prospectus, I realized that survey data is not a panacea. The major surveys on racial minority groups started in the 1990s and 2000s. It does not provide a good data source for studying what political issues were prevalent among racial minority communities during the civil rights movement. These data missed the defining period in American racial politics. As an alternative strategy to collect data, I developed a project that uses machine learning to classify newspaper articles circulated among racial minority groups from the 1960s through the 1980s. These articles were intentionally collected from the West Coast newspapers as my research focus was multiracial coalition-building.


## Workflow

![](https://github.com/jaeyk/content-analysis-for-evaluating-ML-performances/blob/master/outputs/workflow.png)
Figure 1. Workflow

Figure 1 describes the workflow from selecting cases to testing the hypotheses. The case selection strategy reduces alternative explanations. Meta issues among ethnoracial minority groups in the US could be divided into two categories: providing collective gains (**linked progress issue**) and preventing collective losses (**linked hurt issue**). Content analysis assesses data quality by measuring what and how human coders label the training data. Text classification demonstrates that Asian American newspapers issued linked progress articles by 110% more than African American newspapers did. By contrast, African American newspapers produced linked hurt articles by 133% more than Asian American newspapers did. The gap between the two groups widened up to 10 times when the training data were measured by the minimum
rather than the maximum threshold.

## Data collection

I needed one semester (Fall 2018) to train my four undergraduate research assistants (RAs) to collect approximately 80,000 newspaper articles and to systematically label them according to the two major issue areas: linked progress (collective gain issue) and linked hurt (collective loss issue). For the sake of time, I have skipped further discussions on the case selection strategy and data collection process. These details will be provided in [the preprint](https://osf.io/preprints/socarxiv/pg3aq/). The earlier draft was presented at the 2019​ Western Political Science Association annual meeting and was selected to receive [the Don T. Nakanishi Award for Distinguished Scholarship in Asian Pacific American politics](https://www.wpsanet.org/award/).

The original data came from the [Ethnic NewsWatch](https://www.proquest.com/products-services/ethnicnewswatch_hist.html) database, which has compiled more than 2.5 million articles published in U.S. ethnic newspapers and magazines. ProQuest created this database and does not allow web scraping. However, one can still download their articles and save them as HTML files. I have developed a [simple HTML parser](https://github.com/jaeyk/proquest_parser) to turn the database search results into CSV files.


### Training data [[Data](https://github.com/jaeyk/content-analysis-for-evaluating-ML-performances/tree/master/raw_data)]
I hired four undergraduate research assistants and labeled the training data based on the following procedures. The human coders labeled two meta issues and a list of topics because these topics are useful for testing construct validity. Throughout these procedures, none of the human coders were informed about the research hypotheses.

1. Detecting topics and distributing articles: I employed topic modeling to inductively discover topics from each newspaper. I randomly divided these topics (N = 48) into two parts and assigned 100 articles from each topic in the first part to one team of two human coders and 100 articles from each topic in the second part to another team. I asked the human coders to label each topic based on these articles without consulting the other team member.

2. Topic labeling: The two teams spent two weeks labeling topics and then another week agreeing on the common labels through intergroup discussions. In the process, the human coders created a list of topics related to the articles.

3. Meta issue labeling: After the topic labeling, I randomly selected 1,008 articles from the Asian American corpus and 1,008 articles from the African American corpus stratifying on the year variable. Year was selected as a stratifying variable because key issues may change over time. I then paired the human coders into two groups again and assigned one group to the Asian American corpus sample and the other group to the African American corpus sample. The human coders were not aware of the hypotheses the research attempts to test. Each team coded whether the articles in the sample were about promoting collective gains (yes = 1, no = 0) or preventing collective losses (yes = 1, no = 0).

### Content analysis [[Code](https://github.com/jaeyk/content-analysis-for-evaluating-ML-performances/blob/master/code/01_content_analysis.Rmd)]

I assessed the quality of the human-labeled training data by using three criteria. The first two criteria are for measuring inter-coder reliability, and the last one is for measuring construct validity.

#### Percentage agreement

Percentage agreement measures the percentage of the agreed coding decisions made by pairs of coders. The calculation of percentage agreement is fairly simple. Suppose two human coders measure a binary variable. Subtracting the values recorded by coder 1 from the values recorded by coder 2 returns some 0s. The number of 0s divided by the number of units provides the percentage agreement. Percentage agreement is an intuitive method to assess the accuracy of human coding. Nevertheless, it is also a crude one, as it does not account for a certain degree of agreement that would simply arise by chance. Figure 2 shows that the inter-coder agreement reached 88% for the linked hurt articles in both the African American and Asian American corpora. The metric is slightly lower for the linked progress articles: 7% down for the African American newspaper and 8% down for the Asian American one. However, this difference is marginal.

![](https://github.com/jaeyk/content-analysis-for-evaluating-ML-performances/blob/master/outputs/content_analysis_agreement.png)
Figure 2. Percentage agreement for linked progress and linked hurt articles

#### Cohen's kappa

The agreement rates looked good. However, it was too early to celebrate. Every measure has true value plus bias and noise. The two RAs could have agreed with each other by chance. Therefore, I calculated Cohen's kappa coefficient (k), which shows to what extent the outcome is systematic or stochastic. If k is 1, then there is perfect agreement. If k is 0, then the agreement is by chance. Figure 3 shows that the k statistic is particularly low for Asian American data. As I trained my RAs for weeks, I do not think that the problem lies in inadequate coder training. I assume that it has more to do with the relative difficulty of labeling Asian American newspaper articles. This test shows that Asian American data is much noisier. Non-political issues, such as food and sports, could confuse human coders and increase disagreement between them. The right panel in the figure demonstrates how removing these articles increases inter-coder reliability. The results indicate that the training data are less reliable for non-political articles.

![](https://github.com/jaeyk/content-analysis-for-evaluating-ML-performances/blob/master/outputs/content_analysis_kappa_comp.png)
Figure 3. Cohen's kappa with or without non-political topics

#### Construct validity test

Construct validity is about whether the measures measure what they are supposed to measure based on the underlying theory ([Cronbach and Meehl 1955](https://pdfs.semanticscholar.org/7c02/17dd67d1e8c6957da3e1babd1ea9b07f7f74.pdf)). At the conceptual level, linked progress and hurt labels and topic labels are closely associated because the meta issues should represent these topics. Articles labeled as linked progress are expected to cover topics on supportive government policies (convergent validation) more likely than oppressive government policies (discriminant validation). Articles labeled as linked hurt should behave in an opposite way ([Campbell and Fiske 1959](https://www2.psych.ubc.ca/~schaller/528Readings/CampbellFiske1959.pdf)). I test these assumptions by calculating the difference between the number of linked progress articles and that of linked hurt articles associated with particular topics.

![](https://github.com/jaeyk/content-analysis-for-evaluating-ML-performances/blob/master/outputs/content_analysis_topics_gran.png)
Figure 4. Construct validity test

In Figure 4, the X-axis is the difference between the number of linked progress articles and that of linked hurt articles labeled by the human coders belong to the identical topic. The Y-axis indicates these topics. The red bar plot indicates that the maximum threshold is used to define linked progress and hurt labels.The blue bar plot indicates the use of the minimum threshold for the measurement. The analysis confirms that meta and specific issues hang together, as expected by the theory. This relationship holds regardless of whether one uses the maximum or minimum threshold to define the meta issues in the training data. In the figure, the topics strongly associated with linked progress articles are at the top, and the topics highly related to linked hurt articles are at the bottom.

![](https://github.com/jaeyk/content-analysis-for-evaluating-ML-performances/blob/master/outputs/content_data_sources_sub.png)
Figure 5. Threshold change and the proportion of linked progress and hurt articles

One noticeable difference between the two groups is the extent to which the threshold change affects the proportion of linked progress and hurt articles. At the minimum, articles could be defined as linked progress or linked hurt if one of the two human coders said so (minimum threshold). At the maximum, articles could be defined as such if all the human coders agreed (maximum threshold). The minimum threshold is a naive
approach, and the maximum threshold provides more reliable data. As Figure 5 illustrates, using the maximum threshold increases the proportion of linked progress articles by 300% and that of linked hurt articles by 140% in the Asian American corpus. The same change moves up the proportion of linked progress articles by 200% and that of linked hurt articles by 46% in the African American corpus. Overall, the Asian American corpus displays a greater degree of variability. This pattern is expected because the labels in the Asian American corpus are less reliable than their African American counterparts on the basis of their kappa scores. The content of the labels changes little regardless of whether the training data are measured by the minimum or the maximum threshold. Nevertheless, the asymmetry in the class size variation implies that the impact of the threshold change on the predicted labels would be disproportionately high for the Asian American corpus compared with the African American one.

### Training algorithms [[Code](https://github.com/jaeyk/content-analysis-for-evaluating-ML-performances/blob/master/code/02_text_classification.ipynb)]

I followed the standard procedure: cleaned the text data, performed feature engineering (turned the text data into a document-term matrix and reduced the number of features), trained the least absolute shrinkage and selection
operator (Lasso), naive Bayes, and extreme gradient boosting (XGBoost) algorithms, and checked their performances using accuracy and balanced accuracy scores.

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

The other important thing I did was randomly oversampling the minority class in the training data with replacement (upsampling). Since that class (positive value) is what I cared about in this study, the accuracy rate could be misleading if the algorithm misses this class severely. After importing the training data, I checked the balance between the two classes and included upsampling in the model training function. In the next section, I show the performance of the machine algorithms with or without upsampling.

### Model evaluation [[Code](https://github.com/jaeyk/content-analysis-for-evaluating-ML-performances/blob/master/code/03_model_evaluations.Rmd)]

The accuracy score simply calculates the degree to which the model correctly predicts both positive and negative values. The balanced accuracy score adds up the accuracy score for each class and divides it by the number of classes.

![](https://github.com/jaeyk/content-analysis-for-evaluating-ML-performances/blob/master/outputs/ml_content_comp.png)
Figure 6. Classifier performance evaluations

In Figure 6, the X-axis indicates the accuracy or the balanced accuracy rate. The Y-axis indicates different classifiers. The dotted lines represent the percentage agreement between the human coders. The figure demonstrates that more reliable and balanced data produced better prediction outcomes. The worst performance is found in the bottom left panel, where the training data are measured by the minimum threshold and no resampling is used. The average accuracy rate is 77%, but the average balanced accuracy rate is 63%. The best performance is found in the top right panel, where the training data are measured by the maximum threshold and upsampling is used. The average accuracy rate goes up by 19%, and the average balanced accuracy rate moves up by 32%.

### Data visualization [[Code](https://github.com/jaeyk/content-analysis-for-evaluating-ML-performances/blob/master/code/04_time_series_visualization.Rmd)]

Data quality affects not only prediction accuracy but also substantive results.

![](https://github.com/jaeyk/content-analysis-for-evaluating-ML-performances/blob/master/outputs/time_series_plot.png)
Figure 7. Time series trends (maximum threshold)

Figure 7 displays how the proportion of linked progress and hurt articles varies between the two groups over time. In this case, the training data are measured by the maximum threshold. The Y-axis shows the percentage of articles in the corpus classified as the given meta issue type, and the X-axis indicates either publication years or months. The ribbons indicate 95% confidence intervals. The blue line indicates the proportion of exclusive linked progress articles, the red line indicates the proportion of exclusive linked hurt articles, and the purple line indicates the proportion of articles classified as both linked progress and hurt. Overall, the blue line is above the red line in the Asian American case, and the pattern is reversed in the African American case. Put differently, Asian American newspapers issued linked progress articles far more frequently than linked hurt articles. This pattern is reversed in the African American case.

![](https://github.com/jaeyk/content-analysis-for-evaluating-ML-performances/blob/master/outputs/time_series_plot.png)
Figure 8. How text classification is sensitive to measurement decisions

The contrast between the groups can be more closely examined using bar plots. I also dropped the mixed category to make the comparison simple. In Figure 8, the X-axis is the group, and the Y-axis is the proportion of linked progress and hurt articles. The publication years in the Asian American and African American corpora do not precisely match. The Asian American corpus was collected from 1976 to 1989 and the African American corpus from 1968 to 1979. To show whether this difference matters for comparison, I matched the two data on their publication years in the right panel and did not do the same in the left panel. In both panels, Asian American newspapers clearly preferred linked progress, whereas African American newspapers preferred linked hurt. The errors bars represent 95% confidence intervals. As the height differences between the bar plots are much greater than these intervals, it is easy to see that these differences are statistically significant. To be precise, I calculated the differences in the proportions of linked progress and hurt articles between the Asian American and African American corpora. Matching the two corpora decreases the gap between the two groups. In the following measures, the lower range comes from the matched data and the upper range comes from the unmatched data. The data show that Asian American newspapers issued linked progress articles by 110%-240% more than African American newspapers did. By contrast, African American newspapers produced linked hurt articles by 133%-180% more than their Asian American counterparts did. Measuring the training data by the minimum threshold widens the gap between the two groups. When the threshold shifted to the minimum, Asian American newspapers reported on linked progress up to three times more than African American newspapers did. By contrast, African American newspapers covered linked hurt up to 10 times more than their Asian American counterparts.


## Conclusion

Garbage in and garbage out is true, and machine learning is no exception. Knowing exactly how garbage the training data is enables estimating how well machine learning will perform. Furthermore, this knowledge also helps investigators have a deeper understanding of the credibility of the machine-predicted data. In [my other project](https://github.com/jaeyk/ITS-Text-Classification), I discussed how we can use machine learning to create data for causal inference. When using machine-predicted data for statistical and causal inferential problems, we should take this data quality problem seriously. Systematic efforts to document the data collection process are key to achieving this goal.
