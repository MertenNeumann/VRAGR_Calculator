# VRAG-R calculator

A Shiny calculator for the **Violence Risk Appraisal Guide-Revised**, using
the German coding manual by Rettenberger, Gregório Hertz & Eher (2017).
English is the default interface language; Deutsch can be selected independently
in each session. Language changes never change scoring, inputs or CSV exports.

The app provides **Single case**, **Dataset**, **Methods** and **About the app**.
There is no Explore tab. It is a computation aid for appropriately qualified
users, not a substitute for the VRAG-R or PCL-R manuals or an individual assessment.

## Run locally

Use R **4.5.3** (see `.R-version`) and the versions in `renv.lock`:

```sh
Rscript --vanilla scripts/setup.R
Rscript scripts/run.R
```

Run from the repository root. The default server port is 3838; set `VRAGR_PORT`
to select another local port. The managed cloud environment already supplies R
through `/workspace/.vragr-cloud/run.sh`, for example
`/workspace/.vragr-cloud/run.sh Rscript scripts/run.R`.
No deployment credentials are needed for local use. Setup keeps the inherited
proxy and CA trust; it does not disable certificate verification.

On Debian/Ubuntu, installing R packages from source may require
`build-essential`, `gfortran`, `cmake`, `pkg-config`, `zlib1g-dev`,
`libuv1-dev` and `libx11-dev`. The prepared cloud environment uses verified
prebuilt packages; its restore succeeds without installing those development
headers. A fresh source build on another machine has not been verified here.

The lockfile includes development dependencies such as `testthat` and its
dependencies. Keep `snapshot.dev` enabled in `renv/settings.json` when updating
it with `renv::snapshot()`, so a fresh CI restore can run the test suite.

## Workflows

**Single case:** start with empty inputs or explicitly load the synthetic example.
Mark genuinely unavailable information as such; it is not automatically No or zero.
Items 5 and 8 have an explicit zero-count action. Expand the twelve item sections,
check the coding notes, and click **Calculate result**. Results include total,
risk category, percentile, five-/twelve-year reference rates, missing-item count
and item scores. Download the input/result CSV or a plot. Editing scoring inputs
hides the previous result and blocks old download URLs until recalculation.

**Dataset:** upload XLSX, comma/semicolon CSV, tab-separated TXT or SPSS SAV
(10 MiB, at most 100,000 rows). Select the Excel worksheet and decimal mark where
needed. Map all 75 inputs to distinct columns, check the explicit category
suggestions, and optionally select a unique pseudonymous case ID. Missing cells
remain missing; unknown categories are not silently inferred. Invalid rows stay
in order with status/error explanations and empty result fields. A file with no
scoreable rows cannot produce a result export. All relevant import/mapping edits
invalidate the saved result. Downloadable [synthetic examples](examples/example-data.csv)
show the input format; [input descriptions](R/schema.json) follow the manual.

Both exports use UTF-8, comma field separators, decimal points and stable English
headers, independent of UI language. `Prob_5_years` and `Prob_12_years` are 0–1
numbers; `Percentile` is 0–100. Inputs, item scores, row/optional ID, status,
version, source filename and UTC calculation time are included. Repeated CSV
downloads of one calculation retain their filename and bytes. Plot labels follow
the current display language. UI time is Europe/Berlin with CET/CEST.

## R API

```r
source("VRAGR.R")
data <- read.csv("examples/example-data.csv", check.names = FALSE)
result <- vragr(data)                 # validates; stops on invalid input values
result <- vragr(data, strict = FALSE) # retains invalid rows with empty results
```

The core uses base R only, requires the named 75 input columns, and supports
empty data frames. Genuine missing values use `NA`. Binary fields use `No`/`Yes`,
Item 2 `None`/`Moderate`/`Severe`, and Item 11
`None`/`Only14`/`Other`/`Female14`. Counts are non-negative integers; Item 7
is positive age in completed years; PCL-R fields are 0, 1 or 2. Extra columns
are ignored by scoring. The compatibility functions `compute_vrag(...)` and
`compute_vrag_data(...)` keep legacy percentage probabilities; the new API
uses probabilities between 0 and 1.

## Sources and limitations

Read [Model and source review](docs/MODEL_AND_SOURCES.md) for all item bands,
weights, missing-data rules, follow-up definitions, verified discrepancies and
the explicitly documented midpoint-rounding convention. Rates come from the
published reference groups; this app does not recalibrate them for another
population or adjust them for subsequent violence-free time. The German manual
was checked in full for the implemented rules. The full Harris (2015) and
Hare (2003) books were not independently reviewed.

## Checks

```sh
Rscript tests/run.R
python3 -m pip install -r tests/requirements-browser.txt
python3 -m playwright install chromium
# Start the app in another process before the browser test.
python3 tests/browser_smoke.py
```

Set `VRAGR_TEST_URL` for a different local port and `CHROMIUM_PATH` if using an
existing browser. Screenshots and downloads go to ignored `tests/artifacts/`.
The R tests include 81 independent manual-derived reference cases, every item
band and weight, missing and invalid data, all import formats, state/freshness,
timestamps and a 100,000-row batch. Browser tests exercise EN/DE, independent
sessions, actual uploads/downloads, custom/raw mappings, stale exports, long
labels, mobile navigation and widths 390–1920px. All test data are synthetic.

## Data, deployment and reuse

See [Data and deployment](docs/DATA_AND_DEPLOYMENT.md) before using real case data.
The app processes data on the server. This revision does not deploy the app,
push Git changes, or publish a cloud snapshot. Existing rsconnect metadata is
preserved, not treated as deployment authorization.

The original repository did not specify a project licence; this revision does
not assign one. Adapted OGRS components retain their MIT notice in
[NOTICE](NOTICE) and [LICENSES/OGRS-MIT.txt](LICENSES/OGRS-MIT.txt).
Manual and instrument rights are separate.

Bug reports and suggestions are welcome through
[GitHub Issues](https://github.com/MertenNeumann/VRAGR_Calculator/issues).
Please use synthetic examples and do not post original case data or personal
information. The [OGRS 3 calculator](https://mertenneumann.shinyapps.io/OGRS_Calculator/)
is available separately for general reoffending.
