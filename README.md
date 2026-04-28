# Stroop Task & Inattentional Blindness Analysis

This project explores the cognitive phenomenon of Inattentional Blindness (IB) based on a 2005 senior research project conducted by Rachel Mullet and Lauren Garafola. 

## Background & Hypothesis
Inattentional Blindness refers to situations in which a person fails to see an obvious stimulus right in front of their eyes. In Mullet and Garafola's original study, subjects watched an online video of college students passing a basketball and were tasked with counting the passes. During the video, a person in a black gorilla suit walked through the scene in a very obvious way. At the end of the task, subjects were asked if they saw the gorilla—most did not.

The researchers hypothesized that susceptibility to IB could be described and predicted using the subjects' performance on the Stroop Color Word test. 

## The Dataset
The analysis uses a self-contained dataset featuring a binary outcome variable and three distinct cognitive scores derived from the Stroop test:
* **seen:** The response indicating whether the subject noticed the gorilla (`0 = no`, `1 = yes`).
* **W (Word alone):** A score derived from reading a list of color words (e.g., red, green, black).
* **C (Color alone):** A score derived from naming the color in which a series of Xs are printed.
* **CW (Color-Word):** The Stroop task score, derived from the subject's attempt to name the ink color of a printed color word when the text and the ink color do not agree.

## Analysis Workflow
The R script provided in this repository assesses the researchers' hypothesis through:
1. **Exploratory Data Analysis:** Visualizing score distributions, individual patterns, and correlations using raincloud plots, boxplots, and heatmaps.
2. **Statistical Testing:** Welch Two-Sample t-tests to evaluate mean differences in Stroop scores between those who saw the gorilla and those who missed it.
3. **Logistic Regression:** Fitting generalized linear models (GLM) to predict the `seen` outcome based on `W`, `C`, and `CW` scores. The models are compared and evaluated using AIC metrics and ROC/AUC curves.

## Technologies Used
* **Language:** R
* **Libraries:** `ggplot2`, `patchwork`, `tidyr`, `ggdist`, `GGally`, `pheatmap`, `pROC`

## How to Run
The dataset is embedded directly within the script. Simply clone the repository, install any missing R packages, and run the main analysis script in your preferred R environment.
