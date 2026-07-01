# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

**CarletonStats** is an R package providing bootstrapping, permutation tests, grouped bar plots, and statistical demos for introductory statistics courses (Stat 120 at Carleton College). It is published on CRAN.

## Common Commands

```r
# Load the package during development
devtools::load_all()

# Run all tests
devtools::test()

# Run a single test file
testthat::test_file("tests/testthat/test-Carl.R")

# Rebuild documentation from roxygen2 comments
devtools::document()

# Full R CMD check (what CRAN runs)
devtools::check()

# Check without rebuilding vignettes (faster)
devtools::check(vignettes = FALSE)

# Install locally
devtools::install()
```

## Architecture

### S3 dispatch pattern

Every major function (`boot`, `bootCor`, `bootPaired`, `bootSlope`, `permTest`, `permTestAnova`, `permTestCor`, `permTestPaired`, `permTestSlope`, `groupedBar`, `stemPlot`) follows the same three-file pattern:

- `R/<fn>.R` — defines the generic via `UseMethod()`; holds all roxygen2 docs and `@export`
- `R/<fn>.default.R` — implements the vector/default interface (takes raw vectors as arguments)
- `R/<fn>.formula.R` — implements the formula interface (`y ~ group, data = ...`), parses the formula, then delegates to the `.default` method

### Return value conventions

- Bootstrap functions return a numeric vector of resampled statistics with S3 class `carlboot`. All metadata (observed statistic, group names, CI level, plot flags, etc.) is stored as **attributes** on that vector.
- Permutation functions return a numeric vector with S3 class `carlperm`, also using attributes for metadata (observed statistic, p-value, alternative, group stats, plot flags, etc.).
- Both classes have `print`, `plot`, `summary`, and (for `carlboot`) `confint` methods in `R/print.R`, `R/plot.R`, `R/summary.R`, and `R/confint.R`.
- Printing triggers plotting: `print.carlboot` and `print.carlperm` call `plot()` when `plot.hist = TRUE` (the default), and use `patchwork::wrap_plots` when both `plot.hist` and `plot.qq` are requested.

### P-value calculation

All permutation functions share the internal helper `.calc_pvalue()` in `R/pvalue.R`. It uses the +1 adjustment `(count + 1) / (B + 1)` to avoid p-values of exactly 0.

### Plotting

Plots are ggplot2-based (using `ggplot2`, `scales`, `patchwork`). The `plot.carlboot` and `plot.carlperm` methods in `R/plot.R` render histograms of the resampling distribution with observed and mean statistics marked as points.

### Documentation

Docs are generated with **roxygen2** (version 7.2.3). The generic's `.R` file carries all `@param` tags and `@examples`; the `.default` and `.formula` files use `@describeIn <generic>` to attach their docs. Run `devtools::document()` after editing roxygen comments — never hand-edit `man/` or `NAMESPACE`.

### Data

Three built-in datasets live in `data/` as `.rda` files: `Icecream`, `Milkshakes`, `states03`. Documentation for these is in `R/Data.R`.


### Coding

* Always run `air format .` after generating code
* Use the base pipe operator (`|>`) not the magrittr pipe (`%>%`)
* Don't use `_$x` or `_$[["x"]]` since dbplyr must work on R 4.1.
* Use `\() ...` for single-line anonymous functions. For all other cases, use `function() {...}` 

### Testing

- Tests for `R/{name}.R` go in `tests/testthat/test-{name}.R`. 
- All new code should have an accompanying test.
- If there are existing tests, place new tests next to similar existing tests.
- Strive to keep your tests minimal with few comments.

### Documentation

- Every user-facing function should be exported and have roxygen2 documentation.
- Wrap roxygen comments at 80 characters.
- Internal functions should not have roxygen documentation.
- Whenever you add a new (non-internal) documentation topic, also add the topic to `_pkgdown.yml`. 
- Use `pkgdown::check_pkgdown()` to check that all topics are included in the reference index.

### `NEWS.md`

- Every user-facing change should be given a bullet in `NEWS.md`. Do not add bullets for small documentation changes or internal refactorings.
- Each bullet should briefly describes the change to the end user, and mention the related issue in parentheses.
- A bullet can consist of multiple sentences but should not contain any new lines (i.e. don't wrap the bullet).
- If the change is related to a function, put the name of the function early in the bullet.
- Order bullets alphabetically by function name. Put all bullets that don't mention function names at the beginning.

### Writing

- Use sentence case for headings.
- Use US English.

### Proofreading

If the user asks you to proofread a file, act as an expert proofreader and editor with a deep understanding of clear, engaging, and well-structured writing. 

Work paragraph by paragraph, always starting by making a TODO list that includes individual items for each top-level heading. 

Fix spelling, grammar, and other minor problems without asking the user. Label any unclear, confusing, or ambiguous sentences with a FIXME comment.

Only report what you have changed.