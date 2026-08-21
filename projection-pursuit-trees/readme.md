`rpart` makes one-variable-at-a-time cuts, so its regions are rectangular. A projection pursuit (PP) tree searches for a linear combination, such as `0.7 x1 - 0.4 x2`, and cuts that projection. Its boundaries can therefore be oblique.

The original PPtree has a rigid multiclass structure: with `G` classes it uses at most `G - 1` splits and assigns each class to one terminal node. PPtreeExt relaxes that structure, so a class may occupy several leaves. The **Two islands** example makes this difference visible because class A appears in two disconnected regions.

Use **Successive cuts** to reveal the fitted PPtree or PPtreeExt boundaries in construction order. `rpart` exposes maximum depth directly. The original PPtree does not: its depth follows its class-separation structure. For PPtreeExt, lower entropy tolerance allows additional splits and higher tolerance stops earlier.

The colored field is a dense grid of model predictions. Filled points are training observations and outlined points are test observations. Train/test accuracy and elapsed fit time are descriptive for the current sample, not a repeated benchmark.

For this compact first version, each successive split is drawn as its complete line. In the fitted tree, only the part of that line inside its active parent node performs a split.

The robot example is `data41-1` in the benchmark table from the paper: the two-sensor version of the UCI Wall-Following Robot Navigation dataset. It is the only one of the 94 benchmark datasets with exactly two predictors, so it can be displayed directly without projection or variable selection.

## Relationship to the authors' app

This app is an independent, didactic companion to the authors' [`explorapp`](https://natyds.shinyapps.io/explorapp/), not a reproduction or replacement. Their application compares `rpart`, PPtree, and PPtreeExt using three families of simulated 2D problems:

- **Basic-Sim:** three bivariate normal clusters with adjustable means, correlations, and class sizes.
- **Sim-Outlier:** the same simulation plus an outlier cluster assigned to one class.
- **MixSim:** Gaussian mixtures with an adjustable number of classes, sample size, and average and maximum overlap.

The original app was built to develop and diagnose both extensions: class subsetting and multiple splits. This version focuses on the multiple-splits extension and keeps the same central idea—evaluating each fitted model on a dense prediction grid—but reorganizes it as a more guided and visually polished learning experience. It adds curated examples, the paper's only real 2D benchmark, a common 2×2 comparison, train/test and structural metrics, and a step-by-step reveal of the fitted PP cuts.

## References and credits

- Natalia da Silva, Dianne Cook, and Eun-Kyung Lee (2026), [*An Enhanced Projection Pursuit Tree Classifier with Visual Methods for Assessing Algorithmic Improvements*](https://doi.org/10.1080/10618600.2026.2719812). [arXiv version](https://arxiv.org/abs/2602.21130).
- Y. D. Lee, Dianne Cook, J. W. Park, and E.-K. Lee (2013), [*PPtree: Projection Pursuit Classification Tree*](https://doi.org/10.1214/13-EJS810).
- Original interactive application: [`explorapp`](https://natyds.shinyapps.io/explorapp/) and its [`PPtreeExt::explorapp()` source code](https://github.com/natydasilva/PPtreeExt/blob/master/R/explorapp.R).
- Implementations: [`PPtreeViz`](https://cran.r-project.org/package=PPtreeViz) and [`PPtreeExt`](https://github.com/natydasilva/PPtreeExt). The prediction-grid idea is related to [`parttree`](https://grantmcdermott.com/parttree/), but the PP boundaries here come directly from each fitted tree.
- [UCI Wall-Following Robot Navigation dataset](https://archive.ics.uci.edu/dataset/194/wall%2Bfollowing%2Brobot%2Bnavigation%2Bdata), DOI [10.24432/C57C8W](https://doi.org/10.24432/C57C8W), licensed CC BY 4.0.

Install PPtreeExt from R-universe if needed, then run the app:

```r
install.packages("PPtreeExt", repos = c("https://natydasilva.r-universe.dev", "https://cloud.r-project.org"))
shiny::runApp("projection-pursuit-trees")
```
