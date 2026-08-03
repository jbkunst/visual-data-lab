This app explains a credit-risk prediction from two complementary perspectives.

The large panel is a **local SHAP explanation**: each bar shows how one variable moves the selected model's probability of default away from its average prediction.

The upper-right chart keeps the development portfolio fixed and marks the current client's PD. The lower-right **SHAP dependence plot** shows how the active variable behaves across the development sample. When you move one profile slider, that variable automatically becomes active and the red point follows the current profile.

**Random case** loads one real observation from the development sample. You can then modify that profile as a counterfactual scenario.

The four models deliberately increase in flexibility: logistic regression, a decision tree, Random Forest, and XGBoost.

### How the local SHAP calculation works

The background is a fixed random sample of 1,000 development clients. For each background client \(z_i\), the calculation draws one random ordering of the variables and starts at the prediction \(p(z_i)\). Following that ordering, it replaces one variable at a time with the value from the profile \(x\) being explained, until the path reaches \(p(x)\).

At every step, the change in predicted probability is assigned to the variable that entered. The calculation keeps one row per background client and variable, containing the background ID, step, variable, accumulated probability, and probability difference. With nine variables, the complete trace has 1,000 × 9 = 9,000 rows.

For background client *i*, the contribution of variable *j* in its random ordering is:

`φᵢⱼ = p(after variable j enters) - p(before variable j enters)`

Because every path begins at `p(zᵢ)` and ends at `p(x)`, it satisfies:

`Σⱼ φᵢⱼ = p(x) - p(zᵢ)`

The final SHAP value for variable *j* is the average contribution over the *N* background clients:

`φⱼ = (1/N) Σᵢ φᵢⱼ`

Therefore:

`p(x) = (1/N) Σᵢ p(zᵢ) + Σⱼ φⱼ`

The background establishes the reference prediction, while the random variable orderings determine how the distance from that reference to \(p(x)\) is distributed among variables.
