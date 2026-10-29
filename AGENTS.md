# Agent Notes

This repository values simple, elegant Shiny code over framework-like abstractions.

When editing apps:

- Prefer the existing style of the app over introducing helpers or new patterns.
- Do not add an abstraction unless it removes clear repetition or complexity.
- Do not build an external framework around the apps; keep app-specific structure inside each app folder.
- Use shared scripts only for repository tasks such as building the catalog, exporting Shinylive apps, or publishing.
- Do not run `R/build_site.R` locally. GitHub Actions owns the site build and generated `apps.yml` and `docs/` outputs.
- Preserve the Shinylive pool strategy: every public Shinylive app declares `ShinylivePool`, and apps are grouped by their dominant dependency stack to reduce cold-start downloads while reusing the browser cache within a pool. Prefer a few meaningful pools over one pool per app.
- Treat pool names as technical dependency profiles, not editorial categories: `standard` is the common Highcharter-centered stack, `modeling` is the ggplot2/modeling stack, `extended` is for less common specialized stacks such as Plotly, and `light` is for small focused dependency sets.
- Keep UI text and educational flow simple.
- Use `vdltheme::theme_vdl()` and, when needed, `vdltheme::highcharter_theme_vdl()` instead of defining app-local themes. Pokémon and Matrix are the visual-theme exceptions.
- Make small, reviewable changes.
- Avoid clever workarounds for fragile browser behavior; stop and report tradeoffs.
- Use `readme.md` for "How it works", optional `resources.md` for references, and `credits.md` for the visible signature.
- Keep every app's "How it works" to one or two short paragraphs, at most 80 words, explaining what to change and observe. No headings, lists, derivations, implementation details, or deployment instructions.
- Keep "Resources" / "Inspiration and resources" equally small: at most four references and 60 words in `resources.md`. Include only useful links or citations, not explanations or technical notes. Never move an oversized "How it works" into "Resources"; shorten or remove the excess. Both limits apply to all existing and new apps.
- Put explanatory markdown inside an accordion before credits.
