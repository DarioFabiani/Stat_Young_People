# Stat_Young_People
# Statistical Learning - Young People Survey

### Abstract
The purpose of this project is to discover if we can predict spending habits, and in case other preferences, based on possible types of personalities.
We use the Young People Survey dataset (available at [Kaggle](https://www.kaggle.com/miroslavsabo/young-people-survey/)).  
This dataset includes 1010 observations for 150 different varibles. It has been built over a questionary answered by Slovakian people aged between 15 and 30. These people were asked to respond questions regarding their Music and Movies preferences, their Hobbies and Phobias, their Spending and Health Habits, some of their Demographic characteristics and their thoughts about life.   
All variables in the survey are Likert Scales, which mean that the values should be considered as ordered factors, given that we cannot consider the various levels as distant as two integer could be. Therefore we decided to transform the variables into the correct type (ordered factors).  

57 over 150 questions regard personality traits and views about life, thus we decided to find some patterns between these features, in order to utilize them to predicte spending habits and other preferences expressed in the survey.   
We're in the scenario of *unsupervised learning*, as we have a set of 57 features measured over 1010 observations, and we want to discover unknown subgroups within these features. We first had a look to these 57 questions and tried to understand which characteristics of personality were observed by each variable, simply using the Myers-Briggs indicator, often used in psichology. Obviously this method was very rough as it is subject to our personal thoughts and could misleading the results.

We then decided to go for a cluster analysis, a set of techniques that help discover hidden structures on the basis of a dataset.  
To perform the Cluster analysis, we chose a package called "ClusOfVar", which allow to construct a synthetic variable based on the combination of the original ones; the technique applied by this package computes this variable via a PCA performed in each cluster and then retains the first principal component as the synthetic variable.  

In order to identify the clusters we used the "stability" method in the same package, which is based on boostraps. Its output is not really helpful, thus we opted for a visual interpretation and selected 5 clusters.
Afterwards, we assigned the scores for the firsts principal components of PCAMIX applied to the K clusters to the n observations.    
Eventually, we performed a *Ordered Logistic Regression* to answer our research question.  

