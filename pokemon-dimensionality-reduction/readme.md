## How it works

The app builds a mixed-data feature matrix from the maintained PokeAPI tables. It combines morphology, battle stats, capture and breeding traits, binary species flags, Pokémon types, egg groups, growth rate, body color, body shape, and habitat.

### Preprocessing

The feature space is prepared deliberately for PCA, t-SNE, and UMAP:

- **Continuous variables** such as height, weight, battle stats, capture rate, happiness, hatch counter, and female ratio are median-imputed and standardized.
- **Binary variables** such as legendary, mythical, baby, genderless, and form/gender flags stay as **0/1**. Rare flags are not standardized, because doing so could give a rare `1` an artificially huge z-score.
- **Nominal variables** such as type, egg group, growth rate, color, shape, and habitat are one-hot encoded as **0/1** indicators.
- Feature blocks are divided by `sqrt(number of columns in the block)`. This prevents a categorical block from dominating Euclidean distance simply because it expands into many dummy columns.

The block weights are explicit in `prepare_data.R` and all default to `1`, so the preprocessing is easy to inspect or tune later.

First choose what **similarity** should mean. The balanced recipe gives every
semantic block the same total weight. The other recipes emphasize battle and
morphology, Pokémon types, or breeding and ecological traits. Custom weights
make this definition explicit: a zero removes a block and larger values give it
more influence over pairwise distances.

Then choose a projection method and change its main parameters:

- **PCA** gives a fast linear baseline.
- **t-SNE** exposes perplexity and iteration count.
- **UMAP** exposes neighborhood size and minimum distance.

The generation range controls which Pokémon enter both the feature matrix and
the projection. Use it to compare a single generation, a period such as Gen 3–5,
or the complete Pokédex. Smaller samples also change the effective neighborhood
represented by t-SNE perplexity and UMAP neighbors.

Changing the method, similarity recipe, or generation range recomputes the map.
Click **Re-run projection** after changing algorithm parameters, custom weights,
or the random seed. The chart uses the regular PokeAPI sprite as the point
marker. Hover a Pokémon to see its official artwork, types, generation, size,
battle stats, capture rate, happiness, growth rate, habitat, hatch counter, and
legendary/mythical status.

Use **Spotlight primary type** after a projection to emphasize one type without
recalculating or moving the map. The colored halos reproduce the type-oriented
reading of the original Pokémon flexdashboard while keeping every Pokémon
visible in the common projection.

Click any Pokémon to open its full profile. The percentile panels compare its
numeric attributes with the selected generations, while nearest neighbors are
computed in the current weighted feature space before dimensionality reduction.

Drag over the map to zoom into an area; use **Shift + drag** to move around and
the chart's reset button to return. **Visual settings** changes sprite size
without recalculating the projection.

The coordinates are exploratory rather than a distance metric to interpret
literally. Compare recipes and methods instead of treating one arrangement as
the correct map. PCA preserves global linear variation; t-SNE and UMAP focus
more strongly on local neighborhoods and can move or rotate clusters between
runs.

### Data

There is no invented rarity score. The source data provides **capture rate** directly, plus explicit **legendary** and **mythical** flags that can be used to explore rarity-like structure without imposing an arbitrary classification.

Data: [PokeAPI/pokeapi](https://github.com/PokeAPI/pokeapi)  
Sprites and artwork: [PokeAPI/sprites](https://github.com/PokeAPI/sprites)

This app revisits the t-SNE map from the original
[Pokémon flexdashboard](https://jkunst.com/flexdashboard-highcharter-examples/pokemon/)
and makes the definition of similarity part of the experiment.
