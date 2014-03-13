Naive_Bayes_Classifier_R
========================

Raw implementation of Naive Bayes Classifier with R on mushroom data set from UCI repository: 
http://archive.ics.uci.edu/ml/datasets/Mushroom

a. How to run the program

This program is written in R, using only R base package and no other ML R package is used. 
Three options are required: the training dataset, the test dataset, and the output filename.
Usage: Rscript nbc_mushroom.R mushroom.training.txt mushroom.test.txt mushroom.output.txt

b. How to interpret the output (sample output in "Q2_sample_output.txt")

The output will first display the prior probabilities.
Then Likelihood values for each of the 21 features are printed. 
Next, test records with target class and predicted class are printed. 
At last, the program prints the prediction accuracy of the Naive Bayes Classifier. 
