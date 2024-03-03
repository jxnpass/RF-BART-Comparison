## Introduction

The following repository lists one of my research projects completed during my studies BYU studying statistical science. The aim is to compare two tree-based ensemble machine learning methods, namely random forests (RF) and Bayeisan Additive Regression Trees (BART), and how they both fit to the data, prevent from overfitting, compare in computation time, and generate predictions for different tasks. 

This repository goes through a number of research questions aiming to predict on different responses, or targets. Please view my blog (blog pending) to view the results and summary of these comparisons. I learned that RF and BART are fairly comparative when it comes to predictive accuracy, but on certain tasks differently, BART has a lot more user flexibility that, if understood correctly, can become a versatile computation tool with certain utilities RF lacks. 

For each project and R file, I do the following steps: 
1. Load in and split data into training and test sets
2. Tune Random Forest and BART to find the best parameters to fit the model to the data. 
    - This typically involves another split within the training data. 
    - Because no RF and BART fit will be exactly the same (even with the same set of parameters), I used cross validations for each set of parameters and averaged the metrics for each model. 
3. Depending on the metric I wanted to maximize, I selected the best-tuned model from my cross validation list and predicted onto the testing data.
4. I compared results for RF and BART, taking into account both predictive accuracy and computation time. 

## Example 0: Simulated Data

I simulated data to follow a $y = x^2$ path, where $x \in [0,3]$, and introduced random noise (i.e. added error) such that $\epsilon \sim N(0,.5)$. The goal was to view how nonparametric methods like RF and BART perform on a parametric data problem, simple and small enough to see how each method is influenced by overfitting, underfitting, tuning, and the default parameters. 

![ex0](/ex0_simulateddata/graphics/1-ex0graph.png)

The script [simulation.R](/ex0_simulateddata/simulation.R) goes through the process of generating the data, tuning the models, and fitting Random Forest and BART for comparisons on predictive accuracy and computation time. 

For those interested in using the script on their own computer, there is nothing too complex as far as just running source. However, if you want to alter the BART tuning grid such that it goes from 30 minutes to only about 3, you can edit the parameters such that ```sigquant``` and ```k``` are not tested. It will produce a number of graphs regarding predictive accuracy and computation time for your own study. 

## Example 1: Knee Sleeve 

This data was gathered from a BYU Mechanical Engineer department student-developed device that tracks movement from a simple knee exercise. The main goal was to develop a machine learning algorithm that predicts when the angle of a knee has extended to its maximum point and  how many degrees is that angle, using 16 sensors from the knee sleeve itself. This project inspired me to look deeper at the differences between RF and BART as well as how each differ in fitting methods. 

![ex1](/ex1_kneesleeve/graphics/1-ex1graph.png)

The project utilized three different scripts, [kneesleeve.R](/ex1_kneesleeve/kneesleeve.R),[ModelFunctionsandMetrics.R](/ex1_kneesleeve/ModelFunctionsandMetrics.R), and 
[DataImport.R](/ex1_kneesleeve/DataImport.R). The work is organized in the [kneesleeve.R](/ex1_kneesleeve/kneesleeve.R) file. Those interested in experimenting can select names of datasets from the [DataImport.R](/ex1_kneesleeve/DataImport.R) file. Data is found in the folders S1-S4. 

## Example 2: Diabetes

This data was downloaded from [Kaggle](https://www.kaggle.com/datasets/iammustafatz/diabetes-prediction-dataset), mainly for comparing how RF and BART do with a classification (categorical) prediction task. In this case, the goal is to utilize certain medical features in the data that predicts the likelihood of an individual having diabetes. 

Random Forests are typically labelled as powerful methods for classification, but I was curious to see how BART, which is considered pre-dominantly a model for regression, performed on the diabetes set. [diabetes.R](ex2-diabetes/diabetes.R) carries all the work for this project. BART tuning takes a very long time, and I only ever did once, so I recommend not doing it at all (which is why I commented it out). Since this dataset was heavily skewed in favor of non-diabetics, it is necessary that future investments on the diabetes project consider oversampling positives or undersampling negatives (which wasn't exactly my goal since I wanted to see how overfit happened with each model). 

## Credits 

I acknowledge and thank my research advisor Dr. William Chirstensen PhD at BYU, Dr. Rob McCulloch from ASU and founding-father of BART for providing me advice on presentation, theory, applications, and other usages for development of this project. I also thank BYU Mechanical Engineering department for their knee sleeve data, and [Kaggle](https://www.kaggle.com/datasets/iammustafatz/diabetes-prediction-dataset) for the diabetes data. 
