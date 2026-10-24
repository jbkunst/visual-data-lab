The app fits nine classifiers to the same three-class, two-dimensional data and predicts every point on one shared grid.

The colored background is the **predicted class**, not a continuous probability surface. Each observed point uses the color of its true class, so a point on a differently colored region is a visible classification error.

## Models

- **Multinomial logistic regression:** a linear probabilistic baseline.
- **KNN:** a local classifier whose regions follow nearby observations.
- **Radial SVM:** a smooth nonlinear classifier.
- **rpart:** one tree with axis-aligned, rectangular regions.
- **PPtree:** one compact tree whose splits may use linear combinations of `x1` and `x2`.
- **PPtreeExt:** a more flexible projection pursuit tree that allows additional oblique regions.
- **Random forest:** an ensemble of axis-aligned trees.
- **PPforest:** an ensemble of projection pursuit trees.
- **XGBoost:** a boosted ensemble of axis-aligned trees.

Every card reports accuracy on the 70% training partition and the 30% test partition. These values describe the current sample; they are not a repeated performance benchmark.

**Compare results** opens a 2×2 summary. The first panel crosses accuracy with macro-F1 and connects each model's train and test points, making the generalization gap visible. The other panels compare fitting versus shared-grid prediction time, test recall for each class, and pairwise agreement on the prediction grid. Timing describes this browser session and should not be treated as a formal benchmark.

## Datasets

**Diagonal bands** favors models that can separate classes using linear combinations. **Two islands** places one class in disconnected regions, exposing the rigidity of compact trees. **Iris petals** provides a familiar real three-class example.

## Relationship to earlier work

This app develops the prediction-grid idea used in Joshua Kunst's [`klassets`](https://github.com/jbkunst/klassets) from binary probability surfaces into a consistent comparison of multiclass decision regions.

It is also a didactic companion to da Silva, Cook, and Lee's work on PPtreeExt. Their application compares `rpart`, PPtree, and PPtreeExt while developing and diagnosing two algorithmic extensions. This app keeps the 2D visual comparison but expands it to linear, local, kernel, tree, forest, and boosting classifiers.

## References

- Natalia da Silva, Dianne Cook, and Eun-Kyung Lee (2026), [*An Enhanced Projection Pursuit Tree Classifier with Visual Methods for Assessing Algorithmic Improvements*](https://doi.org/10.1080/10618600.2026.2719812). [arXiv version](https://arxiv.org/abs/2602.21130).
- Natalia da Silva, Dianne Cook, and Eun-Kyung Lee (2021), [*A Projection Pursuit Forest Algorithm for Supervised Classification*](https://doi.org/10.1080/10618600.2020.1814300).
- Y. D. Lee, Dianne Cook, J. W. Park, and E.-K. Lee (2013), [*PPtree: Projection Pursuit Classification Tree*](https://doi.org/10.1214/13-EJS810).
- Implementations: [`PPtreeViz`](https://cran.r-project.org/package=PPtreeViz), [`PPtreeExt`](https://cran.r-project.org/package=PPtreeExt), and [`PPforest`](https://cran.r-project.org/package=PPforest).
