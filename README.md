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
- `R/build_site.R`: builds the gallery and Shinylive exports in GitHub Actions.
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
7. Push the changes and let GitHub Actions build and validate the gallery.

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

### Test one Shinylive app locally

Do not run `R/build_site.R` locally. To test a single app, remove only the
ignored Shinylive output, export the app, and serve it in the background:

```r
app_folder <- "underfitting-overfitting"
slug <- app_folder
port <- 8000

unlink("docs/live", recursive = TRUE, force = TRUE)

shinylive::export(
  appdir = app_folder,
  destdir = "docs/live",
  subdir = slug,
  package_cache = FALSE
)

server <- httpuv::runStaticServer(
  dir = "docs/live",
  host = "127.0.0.1",
  port = port,
  background = TRUE,
  browse = FALSE
)

later::later(
  function() {
    browseURL(sprintf("http://127.0.0.1:%s/%s/", port, slug))
  },
  delay = 1
)
```

Use `dir`, not `path`, with `httpuv::runStaticServer()`. Passing `path` through
`...` can produce `Not compatible with requested type: [type=character;
target=logical]` in current `httpuv` versions. Stop the background server with:

```r
server$stop()
```

If the page is blank after a successful export, close previous preview tabs
and retry on a new port. A stale Service Worker may remain associated with the
old local origin. Browser console errors are more informative than repeating
the export blindly.

## Shared Theme and Visual Style

Apps use the shared `vdltheme` package and its bundled IBM Plex Sans font:

```r
library(vdltheme)

apptheme <- theme_vdl()
options(highcharter.theme = highcharter_theme_vdl())
```

Only call `highcharter_theme_vdl()` in apps that use Highcharts. Pokémon and
Matrix keep their own visual themes; all other apps and `app-template` use
`theme_vdl()`.

The shared package is the source of truth for the common visual language:

- IBM Plex Sans is bundled in `vdltheme`; apps must not depend on Google Fonts
  or another network font at runtime.
- Bootstrap colors flow into the default Highcharts palette in this order:
  primary, danger, warning, success, info, and secondary.
- Highcharts legends use normal-weight text, line and scatter markers are
  circles, and chart tooltips use the same light treatment as input help.
- Use the shared theme defaults before adding app-specific colors or CSS.
  Credit-risk apps may keep their semantic palette: red increases bad-risk
  probability, blue decreases it, and dark blue identifies the active case.
- Prefer `highchartProxy()` when an interaction only changes series data,
  categories, plot lines, or the active observation. Re-render the full widget
  only when its structure changes.
- Keep labels and educational copy short. Put the app explanation in a closed
  accordion before `credits.md`, using `readme.md` for "How it works" and
  optional `resources.md` for references.

For Posit Connect Cloud, install the tagged package locally before regenerating
the app manifest. Do not install packages from inside `app.R`:

```r
remotes::install_github(
  "jbkunst/visual-data-lab",
  subdir = "vdltheme",
  ref = "vdltheme-v0.0.3",
  upgrade = "never"
)

rsconnect::writeManifest(appDir = "app-folder")
```

## WebAssembly Releases for Shinylive

Shinylive cannot install the regular Linux or Windows build of a repository
package. `vdltheme` therefore has a tagged WebAssembly build attached to each
package release. The workflow is defined in
`.github/workflows/release-vdltheme-wasm.yml` and uses `r-wasm/actions`.

The workflow's known-good build configuration is intentional:

- run on `ubuntu-24.04`;
- check out the `vdltheme-v<version>` release tag, not the moving `main`
  branch;
- check out `r-wasm/actions` with `ref: v3` into `.actions`;
- invoke the local composite action with `uses: ./.actions/build-rwasm`;
- build `packages: "./vdltheme"` with
  `webr-image: ghcr.io/r-wasm/webr:main`;
- grant `contents: write` and upload the generated files back to the same
  release tag.

Earlier `v1` and reusable-workflow variants were not compatible with this
working build. Do not downgrade or simplify these pins without validating a
complete release and a Shinylive export. The tag prefix also matters: the job
runs only for tags beginning with `vdltheme-v`.

When changing `vdltheme`:

1. Bump `Version` in `vdltheme/DESCRIPTION` and commit the complete package
   change.
2. Push the commit, then publish a tag named `vdltheme-v<version>`:

   ```sh
   gh release create vdltheme-v0.0.4 \
     --target main \
     --title vdltheme-v0.0.4 \
     --generate-notes
   ```

3. Confirm that **Release vdltheme WebAssembly** succeeds. It attaches
   `library.data.gz` and `library.js.metadata` to the GitHub release.
4. Install that exact tag locally before testing Shinylive or regenerating a
   Posit Connect manifest:

   ```r
   remotes::install_github(
     "jbkunst/visual-data-lab",
     subdir = "vdltheme",
     ref = "vdltheme-v0.0.4",
     upgrade = "never",
     force = TRUE
   )
   ```

   Installing directly from the local `vdltheme/` directory is not equivalent:
   it omits the GitHub `Remote*` metadata that Shinylive uses to locate the
   package's Wasm release. The Pages workflow installs the tagged GitHub package
   for the same reason.

5. Update the matching `vdltheme` ref in `.github/workflows/pages.yml`. Rewrite
   manifests for server apps whose deployment uses the new package version.
6. Push the source changes and let the Pages workflow export all Shinylive
   apps. Do not commit its generated `docs/` output.

The release workflow can also rebuild an existing tag manually:

```sh
gh workflow run release-vdltheme-wasm.yml -f tag=vdltheme-v0.0.4
```

Keep the package release, the locally installed tag, the Pages workflow ref,
and server-app manifests on the same version. A release is not ready for
Shinylive until both Wasm assets exist. Package dependencies must themselves be
available to webR, and runtime assets such as fonts must be bundled in the
package rather than fetched from the internet.

If export reports `vdltheme not available in Wasm binary repository`, check in
this order:

1. the package version matches the release tag;
2. the release contains both Wasm assets;
3. the local package was installed with `remotes::install_github()` from that
   exact tag;
4. `.github/workflows/pages.yml` references the same tag;
5. the browser is not trying to fetch a runtime asset such as
   `fonts.googleapis.com`—IBM Plex Sans must come from `vdltheme/inst/fonts`.

## Input Help and Tooltips

Use input tooltips conservatively. Add them only when a control represents a non-obvious statistical or mathematical concept, uses a confusing scale, changes the result conceptually, or needs context that does not fit naturally in its label.

- Treat two to four tooltips per app as an upper limit, not a target.
- Do not add help to obvious controls such as the number of observations unless there is an important non-obvious consequence.
- Keep each tooltip focused on one idea and preferably below 25 words.
- Do not repeat the general explanation already available in `readme.md`.
- Preserve existing label wrappers such as `tags$small()` and the current sidebar spacing.
- Use namespace-qualified calls instead of adding `library(bsicons)`.
- Do not add `title` to the icon: it creates a native browser tooltip that competes with `bslib::tooltip()`.
- Add a small spacing utility such as `class = "ms-1"` so the icon remains visually separate from the label.
- Support hover, keyboard focus, and click/touch with `options = list(trigger = "hover focus click")`.
- Keep tooltip theming minimal; a softer dark background preserves Bootstrap's default white text.
- Test the icon at normal and narrow widths and verify the interaction directly in the running app.

Customize the shared tooltip appearance only when an app needs it:

```r
apptheme <- theme_vdl(tooltip_bg = "#495057")
```

Use the shared label helper:

```r
label = input_label_vdl(
  "Parameter name",
  "Short explanation."
)
```

Before implementing tooltips across an app, list the selected inputs and why each one needs help. It is valid for an app to need no tooltips.

## Fillable App Layout

For apps built around a fillable sidebar and cards, use these four parameters as
the default layout convention:

```r
card <- purrr::partial(
  bslib::card,
  full_screen = TRUE,
  wrapper = purrr::partial(bslib::card_body, padding = 0)
)

ui <- page_fillable(
  padding = 0,
  layout_sidebar(
    fillable = TRUE,
    padding = "0.75rem",
    sidebar = sidebar(...),
    layout_columns(
      gap = "0.75rem",
      ...
    )
  )
)
```

Each parameter controls a different level of spacing:

- `page_fillable(padding = 0)` removes spacing around the full application page.
- `layout_sidebar(padding = "0.75rem")` adds spacing around the main card area.
- `layout_columns(gap = "0.75rem")` controls the space between cards.
- `card_body(padding = 0)` lets plots and HTML widgets use the full card body.

Pass plots and widgets directly to the configured `card()` helper so its wrapper
is applied. An explicit `card_body()` without `padding = 0` restores Bootstrap's
default card padding and bypasses this convention.

Use `col_widths` and `row_heights` explicitly when the composition matters; a
2-by-2 comparison should normally use equal column widths and
`row_heights = c(1, 1)`. Prefer these `bslib` layout arguments over custom CSS
for page padding, card gaps, or card-body spacing. Keep app-specific exceptions
inside the app instead of expanding the shared theme into a layout framework.

## Build

Do not run `R/build_site.R` locally. The script is part of the GitHub Actions
workflow and rebuilds generated files such as `apps.yml` and `docs/`. Push the
source changes and let GitHub Actions execute the build in its controlled
environment.

The build:

1. reads app metadata;
2. skips drafts;
3. exports every Shinylive app to `docs/live/`;
4. stops if a declared Shinylive app cannot be exported;
5. uses `AppURL` for server apps;
6. prepares gallery screenshots and `apps.yml`;
7. renders the Quarto site to `docs/`.

## Publishing

GitHub Actions runs the same build for pull requests and pushes to `main`.

Pull requests build the complete site as a compatibility check but do not publish it. Pushes to `main` build `docs/` and deploy that directory as a GitHub Pages artifact.

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
source("https://raw.githubusercontent.com/jbkunst/visual-data-lab/main/R/run_app.R")
run_app("kmeans")
```
