# Visual Data Lab

Interactive experiments in data, statistics, machine learning, and visualization.

Public site:

<https://jbkunst.github.io/visual-data-lab/>

## Repository Structure

- `app-template/`: minimal skeleton for new apps.
- `<app-folder>/`: one Shiny app per top-level folder.
- `<app-folder>/DESCRIPTION`: gallery metadata and runtime configuration.
- `<app-folder>/readme.md`: short "How it works" content used by the app.
- `<app-folder>/credits.md`: visible author/signature block.
- `<app-folder>/screenshot.png`: gallery preview.
- `R/build_site.R`: builds the gallery and Shinylive exports.
- `R/run_app.R`: runs an app from a fresh copy of the repository.
- `index.qmd`: Quarto source for the gallery.

Generated files such as `apps.yml` and `docs/` are not source files and are not versioned.

## App Metadata

Each public app needs a `DESCRIPTION` file:

```text
Title: App Title
Description: A short sentence that explains what learners can explore.
Categories: statistics, simulation
```

The folder name is the app slug.

Shinylive is the default runtime. It does not need to be declared explicitly.

For an app hosted on Posit Connect Cloud:

```text
Runtime: server
AppURL: https://example.share.connect.posit.cloud/
```

Server apps are deployed independently from the gallery build. Their `AppURL` is used directly by the site.

Draft apps can remain in the repository without being published:

```text
Status: draft
```

## Adding a New App

1. Copy `app-template/` to a new top-level folder.
2. Build the app in `app.R`.
3. Fill in `DESCRIPTION`.
4. Add `readme.md` and `credits.md`.
5. Run the app locally and check the interaction.
6. Generate and commit `screenshot.png`.
7. Run `source("R/build_site.R")` from the repository root.
8. Check the generated gallery locally.

Adding a new app should be self-contained in its app folder. **Do not modify `.github/workflows/pages.yml` just to add an app.** App-specific data preparation, models, assets, and other runtime files belong in the app folder and should be committed when they are part of the app.

Every app `credits.md` should use the standard signature:

```md
App made by [Joshua Kunst](https://jkunst.com) with ❤️ and ☕ using Shiny for R ✨. Code [here](https://github.com/jbkunst/visual-data-lab).
```

Generate a gallery screenshot locally with:

```r
webshot2::appshot(
  "kmeans",
  file = "kmeans/screenshot.png",
  delay = 3,
  vwidth = 1440,
  vheight = 900
)
```

Replace `kmeans` with the app folder name. Screenshots are source assets and should be committed with the app. GitHub Actions reuses the committed screenshot during the site build.

If a Shinylive app cannot be exported, the build fails. Move it to the server runtime only when there is a deliberate reason to host it on Posit Connect Cloud.

## Input Help and Tooltips

Use input tooltips conservatively. Add them only when a control represents a non-obvious statistical or mathematical concept, uses a confusing scale, changes the result conceptually, or needs context that does not fit naturally in its label.

- Treat two to four tooltips per app as an upper limit, not a target.
- Do not add help to obvious controls such as the number of observations unless there is an important non-obvious consequence.
- Keep each tooltip focused on one idea and preferably below 25 words.
- Do not repeat the general explanation already available in `readme.md`.
- Preserve existing label wrappers such as `tags$small()` and the current sidebar spacing.
- Use namespace-qualified calls instead of adding `library(bsicons)`.
- Give icon-only triggers a short accessible `title`.
- Add a small spacing utility such as `class = "ms-1"` so the icon remains visually separate from the label.
- Support hover, keyboard focus, and click/touch with `options = list(trigger = "hover focus click")`.
- Test the icon at normal and narrow widths and verify the interaction directly in the running app.

Use this pattern, adapting the outer wrapper to the existing label:

```r
label = tags$small(tagList(
  "Parameter name",
  bslib::tooltip(
    bsicons::bs_icon(
      "info-circle",
      class = "ms-1",
      title = "About parameter name"
    ),
    "Short explanation.",
    placement = "right",
    options = list(trigger = "hover focus click")
  )
))
```

Before implementing tooltips across an app, list the selected inputs and why each one needs help. It is valid for an app to need no tooltips.

## Build

From an interactive R session at the repository root:

```r
source("R/build_site.R")
```

The build:

1. reads app metadata;
2. skips drafts;
3. exports every Shinylive app to `docs/live/`;
4. stops if a declared Shinylive app cannot be exported;
5. uses `AppURL` for server apps;
6. prepares gallery screenshots and `apps.yml`;
7. renders the Quarto site to `docs/`.

When run interactively, the generated site is served locally on port `8000`.

## Publishing

GitHub Actions runs the same build for pull requests and pushes to `master`.

Pull requests build the complete site as a compatibility check but do not publish it. Pushes to `master` build `docs/` and deploy that directory as a GitHub Pages artifact.

GitHub Pages should use **GitHub Actions** as its publishing source. No generated `docs/` branch or commit is required.

Apps with `Runtime: server` are published separately by Posit Connect Cloud from their own app folders.

## Markdown Notes

Many apps include Markdown with `htmltools::includeMarkdown()`. Keep reusable explanatory text in `readme.md` and visible signature text in `credits.md`.

For MathJax inside included Markdown, use double backslashes:

```md
\\(k\\)
\\((r_i, g_i, b_i, x_i, y_i)\\)
```

## Local App Helper

Run an app from a fresh copy of the repository with:

```r
source("https://raw.githubusercontent.com/jbkunst/visual-data-lab/master/R/run_app.R")
run_app("kmeans")
```
