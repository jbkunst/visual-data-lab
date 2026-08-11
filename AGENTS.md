# Agent Notes

This repository values simple, elegant Shiny code over framework-like abstractions.

When editing apps:

- Prefer the existing style of the app over introducing helpers or new patterns.
- Do not add an abstraction unless it removes clear repetition or complexity.
- Do not build an external framework around the apps; keep app-specific structure inside each app folder.
- Use shared scripts only for repository tasks such as building the catalog, exporting Shinylive apps, or publishing.
- Do not run `R/build_site.R` locally. GitHub Actions owns the site build and generated `apps.yml` and `docs/` outputs.
- Keep UI text and educational flow simple.
- Make small, reviewable changes.
- Avoid clever workarounds for fragile browser behavior; stop and report tradeoffs.
- Use `readme.md` for "How it works", optional `resources.md` for references, and `credits.md` for the visible signature.
- Put explanatory markdown inside an accordion before credits.
