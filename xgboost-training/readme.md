# How it works

This app uses the canonical credit-risk data prepared in `R/credit-data`. It keeps the same train/validation split and predictors, then retrains only XGBoost so you can change a few parameters and watch the optimization evolve.

The first row follows **log-loss**, **AUC**, and the train–validation loss gap as trees are added. The second row shows the fixed learning rate plus two convergence signals: the size of the training gradient and how much the latest 10 trees changed validation probabilities.

Use **Inspect step** to move through the fitted model without retraining. The bottom row then updates with the training-gradient distribution, a 2D map where borrowers are close when they often share leaves, and the distribution of probability changes produced by the latest 10-tree block.

The app intentionally focuses on **how boosting learns**, not on choosing the best credit model. The other credit-risk labs cover evaluation, variable effects, importance, and SHAP explanations.
