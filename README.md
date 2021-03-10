# University-of-Liverpool--Ion-Switching

Data : Please refer tohttps://www.kaggle.com/c/liverpool-ion-switching

In this project, the main objective is to predict the number of open_channels present, based
on electrophysiological signal data. These predictions are at submicroscopic level.
By studying ion channels, which may be possible with the aid of machine learning, it could
have a far-reaching impact on fighting infections, enabling pain signals, and stimulating
muscle contraction. 

Algorithm tried : Neural Network, KNN, ANOVA, Simple linear model, Linear model with
degree polynomial 2, Naive Bayes were implemented

Final algorithm : XGBoost turned out to be a good model for predicting the number of open
channels present, based on electrophysiological signal data, which gave
RMSE of 0.054751(lowest out of all models) and was able to achieve a
score of 0.912 on Kaggle.
