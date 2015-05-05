# TFI
TFI Restaurant Revenue Prediction

TFIRaw.R Is the main script, everything is self contained. It runs a hyperparameter search and builds a GLM Ridge model with all the features as numeric and factors.
downloadData.sh is a shell script that gets the data from Kaggle, it requires a cookies.txt files with your login information in order to download it directly from command line. The links on the script are where the data is stored.

This model will give an error score of 1820596.57990 enough to put you within the 19% and it's only 92785.09 away from the winning score.
