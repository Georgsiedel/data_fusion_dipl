This is the repository featuring the R-Code from my Diploma Thesis. The entire project, like the Thesis itself, is written and documented in German language.

Full text is available here: https://nbn-resolving.org/urn:nbn:de:bsz:14-qucosa2-739723

The project was about data fusion of traffic scenarios. The data of choice were all 2019 traffic accidents in German state Saxony (~65.000 instances) as documented in the electronic accident type map (EUSKa). Sadly, the dataset itself cannot be available here due to confidentiality agreements.
Data fusion herein is simulated by predicting the variable "Accident Type" in the "recipient" dataset, where this variable is missing, based on the common vaariables both datasets "donor" and "recipient" share. This way, one step of a typical data fusion procedure is shown. The actual value of Accident Type in the recipient dataset is known and used as validation means.
The goal of the data fusion was to not only achieve good prediction accuracy on the data fusion task, but also to recreate the original variable distribution and conditional distributions, a key requirement for succesful data fusion.

The code is split in the following way:
- 01 to 03 prepare the dataset through cleaning up, deleting features and sparse data and more. 02_EDA features an explorative data analysis and allows for more datapreparation in 03.
- 04 does a data split into a donor and a recipient (the datafusion is simulated from the same data origin). It is checked whether all features are still similarily distributed.
- 05 to 08 applies a Random Forest algorithm to predict the accident type feature (classification task), which is deleted from the recipient dataset to simulate a missing feature for data fusion. Hyperparamter tuning, variable importance measurement and recursive feature selection are carried our.
- 09 to 12 apply different algorithms on the same problem: XGBoost, NNET (a simple Perceptron), a SVM and "DHD", which (in difference to the ML methods before) is a classic statistical distribution matching method.
- 13 assesses and compares the results of all predictors to get information on how to best create ensembles of the predictors
- 14 and 15 experiment with majority voting as well as stacking ensembles.


Please refer to the thesis for details.
