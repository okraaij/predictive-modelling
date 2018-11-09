# Customer behaviour analysis

An implementation of a customer behaviour analysis for Dixons Carphone to help predict whether a prospective customer will visit their website, or go to one of their competitors, based on demographic attributes. 

## Overview

- This repository contains the implementation of a customer behaviour analysis for Dixons Carphone as part of the MSc Business Analytics at the University of Edinburgh.
- This project focused desiging binary and multi-classification models that can help predict whether a customer will visit the website of Carphone Warehouse or one of its competitors, based on 5 demographic attributes of a prospective customer. 
- The [R implementation](/customer-behaviour-analysis.R) contains the following scripts:
  1. Applying bootstrapping to the obtained data from Audience View
  2. Naive Bayes
  3. Support Vector Machine with radial kernel 
  4. Hyperparameter tuning for the SVM models. 
- The data that was used in this project was:
  - Real customer traffic data sourced from Audience View 
  - Bootstrapped data based on properties of the obtained the Audience View data (due to small sample size)
  - **Please note:** this data is not included in the repository due to a confidentiality agreement between The University of Edinburgh and Dixons Carphone.

## Project description

The aim of this paper was to design binary and multi-classification models that can help predict whether a customer will visit the website of Carphone Warehouse or one of its competitors, based on 5 demographic attributes of a prospective customer. The results and insights from the segmentation analysis performed formed the basis for this paper. Weka and R were used to implement a Naïve Bayes classifier, Support Vector Machine (SVM) and a C4.5 Decision Tree, alongside a Random Forest model to benchmark the results obtained from each model. Multiple input parameters were adjusted to optimise models and three evaluation metrics were primarily used to assess each model's performance; Misclassification Rate, Accuracy Rate and Area Under the Curve (AUC). Due to the poor performance of the multi-classification models built - largely due to the dominance of Amazon attracting traffic for the Samsung Galaxy S8 and iPhone 7 - binary models were also built and tested. This involved amending the simulated datasets for each handset, such that all retailers, other than Carphone Warehouse, were set to ‘other’. The binary models provided an improvement to the multi-classification models with a lower Misclassification Rate, high Accuracy Rate and higher AUC. It was found that Amazon’s dominance was evident across the both handsets and that no classifier constructed was able to classify which retailer a customers would visit, solely based on their demographic attributes.
