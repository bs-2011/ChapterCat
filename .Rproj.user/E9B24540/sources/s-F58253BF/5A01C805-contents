---
title: " Categorize chapters based on skills required"
author: "Balveer Singh"
date: "3 January 2018"
output: word_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## 1.1 Introduction  
  Categorize chapters based on skills required, Chapters requiring a particular skill can be clubbed into one category. Some mathematics chapters requires less of knowledge but more accuracy i.e. they invlove more calculations, such chapters can be grouped into one category denoting the skill accuracy.  ***  

## 1.2 Package Name
___ChapterCat___

## 1.3 DataSet used
___blust_sample_v2.csv___

## 1.4 Model used
___Bayesian Clustering___ 

## 1.5 Attribute list

*	Step.Duration..sec.:  the total step duration spend by student for that problem.
*	Attempts: total number of attempts by the student on the problem.
*	Hints: total number of hints requested by the student for the problem.
* count_subskill : Total number of subskill in any problem
*	Opportunity.SubSkills. : a count that increases by one each time the student encounters a step with he listed knowledge component.
*	Correct.First.Attempt: the tutor's evaluation of the student's first attempt on the step—1 if correct, 0 if an error.    
  
  ***

## Description
 * First, I applied a PCA algorithm in the dataset to convert all the columns into two colums so that we can visualize the clusters.
 * By applying PCA, I converted 10 columns to 2 columns.
 * After that we can apply bayesian clustering into PCA1, PCA2 columns.
 * We can choose optimum number of cluster by using dendograms.
 

