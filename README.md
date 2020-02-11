# content-analysis-for-evaluating-ML-performances


**How Content Analysis Helps Evaluating Supervised Machine Learning Performances**


- The main goal of machine learning is making predictions. But how do we know algorithms make good predictions? For supervised machine learning, the quality of predictions is evaludated by comparing predicted values with observed values. The assumption here is these observations reveal ground truth. 
- However, this statement is often a very strong assumption. Raw data is an oxymoron ([Gitelman 2013](https://mitpress.mit.edu/books/raw-data-oxymoron)). Every data is social construction. Thus, without examining how the training data was created, "garbage in garbage" is out is inevitable ([Geiger et al. 2019](https://stuartgeiger.com/papers/gigo-fat2020.pdf)). 
- Content analysis helps understand how training data was created. That information provides reasonable base estimates to evaluate the performances of supervised machine learning algorithms. In this article, I document how I have carried out this project from end to end. If possible, I provide all the code and data (including the training but not the text data for copyright issues) used for this project. 

## Motivation 

This project is part of my dissertation research. I have mostly worked with survey and experimental data to study political opinion among racial minority groups in the United States. However, as I started working on my dissertation prospectus, I realized that survey data is not a panacea. The major surveys on racial minority groups started in the 1990s and 2000s. It does not provide a good data source for studying what political issues were prevalent among racial minority communities during the civil rights movement. These data missed the defining period in American racial politics. As an alternative strategy to collect data, I developed a project that uses machine learning to classify newspaper articles circulated among racial minority groups from the 1960s through the 1980s. These articles were intentionally collected from the West Coast newspapers as my research focus was multiracial coalition building.  


## Workflow 

It took me around one semester (Fall 2018) to train my four undergraduate research assistants, collect around 80,000 newspaper articles, and systematically label them according to the two major issue areas (linked progress = collective gain issue, linked hurt = collective loss issue). For the sake of time, I skipped further discussions on the case selection stategy and data collection process. These details will be provided in the paper, which I plan to circulate before the summer 2020. The earlier draft was presented at the 2019 Western Political Sciecne Association annual meeting and was selected to receive [the Don T. Nakanishi Award for Distinguished Scholarship in Asian Pacific American politics](https://www.wpsanet.org/award/).

The original data came from the [Ethnic NewsWatch](https://www.proquest.com/products-services/ethnicnewswatch_hist.html) database, which has complied more than 2.5 million articles published in U.S. ethnic newspapers and magainzes. Proquest created this database and it does not allow web scraping. One can still download their articles and save them as HTML files. I have developed a [simple HTML parser](https://github.com/jaeyk/proquest_parser) to turn the databse search results into CSV files.

In this article, I focus on the rest of the research process: **content analysis, text classification, model evaluation, and data visualization.** 


### Content analysis [Code](https://github.com/jaeyk/content-analysis-for-evaluating-ML-performances/blob/master/code/01_content_analysis.Rmd)

- Four RAs were divided into two groups. 
- Each group labeled 1,008 articles according to two binary categories (linked progress and linked hurt). These articles were randomly selected from the original corpus stratifying on publication years. 
- These RAs were never informed about the research objective. Yet, they had a plenty of training opportunities. They learned about the distribution of topics in these newspaper articles by participating in the topic modeling analysis. In the process, they also read 2,400 articles and discussed them in a group. (I ended up not using topic modeling but the process helped the RAs to learn more about the corpus.) Therefore, when they started labeling these articles, they had quite good knowledge about the articles they read. 
- Content analysis is an attempt to confirm whether the data under investigation follows the **conceptual** framework a researcher proposed. A good concept is **valid, accurate, and reliable**. Thus, we check whether the lables (measures) created by these RAs were valid, accurate, and reliable. 

#### Accuracy 

![](https://github.com/jaeyk/content-analysis-for-evaluating-ML-performances/blob/master/outputs/content_analysis_agreement.png)

#### Reliability 

#### Validity 

### Text classification [Code](https://github.com/jaeyk/content-analysis-for-evaluating-ML-performances/blob/master/code/02_text_classification.ipynb)


### Model evaluation [Code](https://github.com/jaeyk/content-analysis-for-evaluating-ML-performances/blob/master/code/03_model_evaluations.Rmd)


### Data visualization [Code](https://github.com/jaeyk/content-analysis-for-evaluating-ML-performances/blob/master/code/04_time_series_visualization.Rmd)