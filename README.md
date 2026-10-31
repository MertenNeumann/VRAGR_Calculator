# VRAG-R · Calculator

A Shiny application for the **Violence Risk Appraisal Guide—Revised (VRAG-R)**.
It calculates the total score, risk category, percentile rank and five- and
twelve-year reference rates of violent recidivism. Coding follows the German
manual by Rettenberger, Gregório Hertz & Eher (2017), checked against the original
publication by Harris, Rice, Quinsey & Cormier (2015).

**English is the default language.** The **Language / Sprache** dropdown switches
the interface to German and back without clearing inputs, mappings or results.
Both languages use the same scoring rules and the **German 2017 percentile table**.
Changing the display language does not select a different model.

**Where the sources conflict, the German manual takes precedence**, including
item descriptions, coding rules and interpretation, not only percentiles.

The app is a computation aid. The linked manuals remain the full references for
administration, item coding and interpretation.

## Installation and startup

Use **R 4.5.3** and the package versions recorded in `renv.lock`. Run these commands
from the repository root:

```sh
Rscript --vanilla scripts/setup.R
Rscript scripts/run.R
```

The default local server port is 3838; set `VRAGR_PORT` to use another port.
The setup script preserves the configured proxy and CA trust. No deployment
credentials or external database are needed for local calculations.

For Linux source installations, development packages may be required, including
`build-essential`, `gfortran`, `cmake`, `pkg-config`, `zlib1g-dev`, `libuv1-dev`
and `libx11-dev` on Debian/Ubuntu. The prepared cloud environment uses prebuilt
packages and provides R through `/workspace/.vragr-cloud/run.sh`, for example
`/workspace/.vragr-cloud/run.sh Rscript scripts/run.R`.

The lockfile includes the test dependencies. Keep `snapshot.dev` enabled in
`renv/settings.json` when running `renv::snapshot()`, so a clean CI restore also
installs `testthat` and its dependencies.

## Interface and calculations

The four tabs are **Single case**, **Dataset**, **Methods** and **About the app**;
in German, **Einzelfall**, **Datensatz**, **Methodische Hinweise** and
**Über die App**. Each tab explains its controls and available actions.

**Single case.** Every item initially shows **No information**. Enter the known
answers, leaving genuinely unavailable information marked as missing. For the
Cormier–Lang lists, selecting **Information available** starts all offence counts
at **0**. Review the full list and enter the relevant counts. Switching an item
back to No information excludes its stored values; returning to Information
available preserves values already entered. **Reset inputs** returns all items
to missing and restores the offence counts to zero.

Click **Calculate result** after entering enough information to score at least
eight of the twelve items. The result shows the total, category, percentile,
five-/twelve-year reference rates, missing-item count and individual item scores.
Changing scoring inputs hides the result and blocks downloads until recalculation.
An explicitly loaded example is available for trying the workflow.

**Dataset.** Upload XLSX, comma- or semicolon-separated CSV, tab-separated TXT, or
SPSS SAV. The limits are **10 MiB** and **100,000 data rows**. Select the worksheet
and decimal mark where needed, map all 75 input fields to distinct columns, and
check the suggested category mappings. An optional pseudonymous ID must be present
and unique in every row. CSV identifiers are read as text to preserve leading zeroes.
The downloadable [example dataset](examples/example-data.csv) illustrates the layout.

Blank cells remain missing; unfamiliar category values need an explicit mapping.
Invalid rows stay in their original order with a status, error explanation and
empty risk-result fields. A file with no scoreable rows cannot be exported as a
calculated result. Changes to the file, import settings, mappings or ID selection
invalidate the saved result. Raw preview values and column names are never translated.

## Measures, plots and exports

The **score** ranges from −34 to +46; the **risk category** ranges from 1 to 9.
A **percentile** describes the proportion of the reference sample with an equal
or lower score. It is distinct from the published probability of violent recidivism.
The five- and twelve-year rates are cumulative from the start of the relevant
opportunity period, not annual rates or consecutive follow-up periods.

The default plot shows the **empirical percentile curve**. An optional
**normal-distribution view** restores the earlier shaded-bell-curve presentation
using the same percentile rank. It is an illustration, not a fitted distribution
of VRAG-R totals. At percentile 100, the marker is drawn at the finite right edge.
Plot choice and language do not change calculated values, timestamps or CSV exports.
The current plot can be downloaded as PNG, JPEG, PDF, SVG or TIFF.

**Both result exports use English headers, UTF-8, comma separators and a decimal
point**, regardless of interface language. `Prob_5_years` and `Prob_12_years` are
unrounded 0–1 numbers; `Percentile` is a 0–100 value. Exports include coded inputs,
item scores, row/optional case ID, status, app/model version, source filename and
UTC calculation time. Repeated CSV downloads of one calculation retain their
filename and bytes. The interface displays calculation time in Europe/Berlin.

## Sources and review

- Rettenberger, M., Gregório Hertz, P. & Eher, R. (2017).
  *Die deutsche Version des Violence Risk Appraisal Guide-Revised (VRAG-R).*
  [German manual, BM-Online 8](https://krimpub.krimz.de/frontdoor/deliver/index/docId/40/file/bm-online8.pdf).
- Harris, G. T., Rice, M. E., Quinsey, V. L. & Cormier, C. A. (2015).
  *Violent offenders: Appraising and managing risk* (3rd ed.).
  [doi:10.1037/14572-000](https://doi.org/10.1037/14572-000).

Both sources agree on the implemented weights, category boundaries and final
reference rates. The English book's observed rates in Table 5.7 differ from its
final norms in Appendix C; the app uses the latter. Its percentile table is
coarser and partly differs from the German table, which remains the app's reference.

See [Model and sources](docs/MODEL_AND_SOURCES.md) and the
[original-source review](docs/ORIGINAL_SOURCE_REVIEW.md) for exact pages, numerical
decisions and inconsistencies in the book's worked examples. No ad hoc correction
is applied to make an inconsistent example override the general scoring rules.
The separate Hare manual is still required for full PCL-R item assessment.

## R API

```r
source("VRAGR.R")
data <- read.csv("examples/example-data.csv", check.names = FALSE)
result <- vragr(data)
result_with_invalid_rows <- vragr(data, strict = FALSE)
```

The scoring core uses base R. It requires the 75 named input columns described
in [the input schema](R/schema.json); extra columns are ignored. Missing values
are `NA`. Binary fields use `No`/`Yes`, Item 2 uses `None`/`Moderate`/`Severe`,
and Item 11 uses `None`/`Only14`/`Other`/`Female14`. Counts must be non-negative
integers, age is in positive completed years, and PCL-R inputs are 0, 1 or 2.
The legacy `compute_vrag(...)` and `compute_vrag_data(...)` functions keep their
percentage-valued probabilities; `vragr()` returns probabilities between 0 and 1.

## Tests

```sh
Rscript tests/run.R
python3 -m pip install -r tests/requirements-browser.txt
python3 -m playwright install chromium
# Start the app in another process before the browser checks.
python3 tests/browser_smoke.py
```

Set `VRAGR_TEST_URL` for another local port or `CHROMIUM_PATH` for an existing
browser. Screenshots and downloads go to ignored `tests/artifacts/`.
The suite covers all 81 possible total scores, every item band and offence
weight, missing/invalid values, original published examples, imports, default
answers, state preservation, both plots, export freshness and a 100,000-row batch.
Browser checks cover both languages, separate sessions, real uploads/downloads,
long raw labels and mobile/desktop layouts. Test inputs are synthetic or derived
from published worked examples; no user case records are included.

## Data processing, contact and reuse

Calculations and uploads are processed **on the server**. Read
[Data and deployment](docs/DATA_AND_DEPLOYMENT.md) for temporary-file handling,
hosting limits and deployment responsibilities. Existing rsconnect metadata is
not an automatic deployment instruction. OpenAI Codex assisted with code, interface,
documentation and tests; no AI model is used for case calculations.

Report bugs or suggest additions through
[GitHub Issues](https://github.com/MertenNeumann/VRAGR_Calculator/issues), using
synthetic examples rather than personal information or original case data.
The [OGRS 3 calculator](https://mertenneumann.shinyapps.io/OGRS_Calculator/) is
available separately for general reoffending.

The original repository did not specify a project licence. Adapted OGRS components
retain their MIT notice in [NOTICE](NOTICE) and [LICENSES/OGRS-MIT.txt](LICENSES/OGRS-MIT.txt).
Manual and instrument rights remain separate; the supplied book is not redistributed.
