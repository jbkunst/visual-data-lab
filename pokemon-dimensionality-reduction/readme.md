## How it works

The app builds a mixed-data feature matrix from the maintained PokeAPI tables. It combines morphology, battle stats, capture and breeding traits, binary species flags, Pokémon types, egg groups, growth rate, body color, body shape, and habitat.

### Preprocessing

The feature space is prepared deliberately for PCA, t-SNE, and UMAP:

- **Continuous variables** such as height, weight, battle stats, capture rate, happiness, hatch counter, and female ratio are median-imputed and standardized.
- **Binary variables** such as legendary, mythical, baby, genderless, and form/gender flags stay as **0/1**. Rare flags are not standardized, because doing so could give a rare `1` an artificially huge z-score.
- **Nominal variables** such as type, egg group, growth rate, color, shape, and habitat are one-hot encoded as **0/1** indicators.
- Feature blocks are divided by `sqrt(number of columns in the block)`. This prevents a categorical block from dominating Euclidean distance simply because it expands into many dummy columns.

The block weights are explicit in `prepare_data.R` and all default to `1`, so the preprocessing is easy to inspect or tune later.

Choose a projection method and change its main parameters:

- **PCA** gives a fast linear baseline.
- **t-SNE** exposes perplexity and iteration count.
- **UMAP** exposes neighborhood size and minimum distance.

Click **Run projection** to recompute the two-dimensional coordinates. The chart uses the regular PokeAPI sprite as the point marker. Hover a Pokémon to see its official artwork, types, generation, size, battle stats, capture rate, happiness, growth rate, habitat, hatch counter, and legendary/mythical status.

The coordinates are exploratory rather than a distance metric to interpret literally. They are useful for spotting groups, evolution families, unusual type combinations, differences in capture difficulty and species traits, and differences between projection methods.

### Data

There is no invented rarity score. The source data provides **capture rate** directly, plus explicit **legendary** and **mythical** flags that can be used to explore rarity-like structure without imposing an arbitrary classification.

Data: [PokeAPI/pokeapi](https://github.com/PokeAPI/pokeapi)  
Sprites and artwork: [PokeAPI/sprites](https://github.com/PokeAPI/sprites)
