This app explains a credit-risk prediction from two complementary perspectives.

The large panel is a **local SHAP explanation**: each bar shows how one variable moves the selected model's probability of default away from its average prediction.

The upper-right chart keeps the development portfolio fixed and marks the current client's PD. The lower-right **SHAP dependence plot** shows how the active variable behaves across the development sample. When you move one profile slider, that variable automatically becomes active and the red point follows the current profile.

**Random case** loads one real observation from the development sample. You can then modify that profile as a counterfactual scenario.

The four models deliberately have different structures: a linear logistic regression, a nonlinear spline logistic regression, a decision tree, and an ensemble of randomized bagged trees.
