# Water-treatment---Bayesian-belief-net-modelling-in-R

The factors influence a chance of water quality failure in water treatment works and service reservoirs were examined using BBN learning which is an unsupservised learning method to construct a BBN model, conditional probability, and performed inferences usin BBN learn in R.

1. Summary
	- Correlation coefficient between each factor and water quality was calculated to select only high relevance attributes for the model building step. Total 19 attributes were selected from 56 attributes.
	- 3 BBN structure learning were performed using a score-based hill climbing algorithm to help facilitate computational time. 3 types of scores: BIC, BDE, and BDS, were selected and compared to find the best score for structure learning. Then, 10-fold cross-validation was carried out to examine the loss from learning of each model.
	- Used the final model from earlier step to predict Ecoli and Coliform counts. 
	- Performed distribution parameters learning using Bayesian method.
	- Performed inference from the parameters.

2. Tool: R with RcolorBrewer, VIM, mice, Rgraphviz, bnlearn, ggplot2, RBGL, gRain,gRbase libraries.

3. Algorithm: Bayesian belief net.

4. Evaluation: 
	- BIC, BDE and BDS with cross validation for model selection.
	- Confusion matrix to verify final model performance.


5. Dataset: Water quality in the treatment and service resevoir dataset with a total of 29,763 observations, with 56 variables.

