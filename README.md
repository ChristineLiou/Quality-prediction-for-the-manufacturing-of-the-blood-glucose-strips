# Quality-prediction-for-the-manufacturing-of-the-blood-glucose-strips
Machine learning models to predict the quality of manufacturing of blood glucose test strips are presented in this study. A variety of models are built by inspection data from an anonymous manufacturer in Taiwan. The aim of this study is to predict the quality of the strips in advance and to reduce the cost of defective products before shipment.

The proposed approach is divided into two parts. In-Process Quality Control (IPQC) data are examined in the first part. Since the data of defective products are relatively rare, we should make the data more balanced before building a model. Synthetic Minority Over-Sampling Technique (SMOTE) and Random Over-sampling Example (ROSE) are used to balance the data, and subsequently classification models are developed by the decision tree and random forest. Evaluation results from Receiver Operating Characteristic Curve (ROC) show that the decision tree and random forest after SMOTE have better performance than the counterpart of ROSE model.

After exploring the IPQC data, the second part of this study combines different inspection results of blood glucose test strips under same batch of raw materials. Standard deviation filtering and Principal Component Analysis (PCA) are used to select or extract appropriate features before building the models. Different clusterings from K-means, hierarchical, and K-medoids methods are employed to facilitate the understanding on quality grading for blood glucose test strips. Different clusterings are externally validated against original category labels. Results show that performance of more than three groups degrades significantly regardless of the cluster methods.

# Outline
> IPQC data analysis (Part 1)

+ 2.1 Knowing the data
  - 2.1.1 Original data
  - 2.2.2 Data after preprocessing
+ 2.2 Analysis 
  - 2.2.1 SMOTE + Decision Tree
  - 2.2.2 SMOTE + Random Forest
  - 2.2.3 ROSE + Decision Tree
  - 2.2.4 ROSE + Random Forest
+ 2.3 Compare ROC curve
    
> IPQC + FQC data analysis (Part 2)


# File description

# Summary

