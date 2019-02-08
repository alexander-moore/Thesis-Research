# Thesis-Research
### Fall 2018

This repository is my ongoing undergraduate research project for Reed College, advised by [Dr. Kelly McConville](https://github.com/mcconvil)

The impacts of complex survey design in the training of machine learning algorithms is an unignorable effect. This project examines multiple methods to account for complex survey design in the training of neural networks. The following methods are considered:
- **Weighted Resampling**: Data preprocess by resampling the data according to the inclusion probability of the observations.
- **Weighted Loss Function**: Weight the loss function in the training of the network by the inclusion probability of the observations.
- **Pi Feature**: Make available the inclusion probability of the observation as a feature in the training and testing of the network. 

![](images/model_img.png)
![](images/both.png)
![](images/badfit.png)
![](images/stayGood.png)


## Outline

**Chapter 1: Survey Statistics and Imputation** Introduces machine learning, neural networks, survey statistics, and the pairwise significances of these fields to the research topic.

**Chapter 2: Machine Learning and Neural Networks** Introduces machine learning building blocks such as fitting, bias-variance, and supervised learning. Neural networks discussed at length including mathematic fundamentals and properties relevant to study.

**Chapter 3: Proof of Concept in Toy Data**: February 2019.

**Chapter 4: Explorations in Real Data**: March 2019.

## File Structure

The repository is organized as follows:
- **Developement**: These files are the corpus of research and experimentation with the most recent models and methods.

- **Stomping Grounds**: These files are for experimentation and developement of an R package.

- **Images**: A collection of images relevant to the hypotheses, design, and outputs of the research.

- **Thesis Writing Rmd**: Contains the R MarkDown file with the body of writing forming the Thesis Book. **Outline** describes the chapters of this file.

