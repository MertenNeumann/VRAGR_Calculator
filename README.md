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
item descriptions, coding rules and interpretation.

The app is a computation aid. The linked manuals remain the full references for
administration, item coding and interpretation.

## Installation and startup

Use **R 4.6.2** and the package versions recorded in `renv.lock`.
`.R-version` records the supported R version. From the repository root, run:

```sh
Rscript --vanilla scripts/setup.R
Rscript scripts/run.R
```

The setup script restores the project library, including test dependencies.
`.Rprofile` activates that library when R starts in the repository root.
Installation requires access to CRAN. Linux source installations also require
compilers and development libraries; see [Contributing](CONTRIBUTING.md).
No external database or API credentials are required for local calculations.

The default local address is `127.0.0.1:3838`. Set `VRAGR_PORT` to use another
port and `VRAGR_HOST` to change the bind address. When running both calculators
at once, use different ports. See [Data processing and deployment](docs/DATA_AND_DEPLOYMENT.md)
for hosting requirements.

## Interface and calculations

The four tabs are **Single case**, **Dataset**, **Methods** and **About the app**;
in German, **Einzelfall**, **Datensatz**, **Methodische Hinweise** and
**Über die App**. Each tab explains its controls and available actions.

The language setting is local to each Shiny session. It translates headings,
controls, validation messages, results and plots. It does not translate uploaded
values, IDs, column names or filenames, or alter their mappings. A new session
starts in English.

Results and downloads require the current inputs and settings to match a saved
calculation. Edits hide results while they differ; reverting those edits can
restore access to the existing calculation. Replacing an upload requires
recalculation. Language changes do not affect this check.

**Single case.** Every item initially shows **No information**. Enter the known
answers, leaving genuinely unavailable information marked as missing. For the
Cormier–Lang lists, selecting **Information available** starts all offence counts
at **0**. Review the full list and enter the relevant counts. Switching an item
back to No information excludes its stored values; returning to Information
available preserves values already entered. **Reset inputs** returns all items
to missing, restores the offence counts to zero and clears the saved result.

Each item header shows whether its inputs are complete, partially missing or
need checking, even when the panel is collapsed. Multi-field items show the
number of missing inputs. The badge distinguishes partial information that can
still produce an item score from an item that remains missing, following the
instrument's coding rules. Badges update as inputs, examples, reset actions and
the display language change.

Click **Calculate result** after entering enough information to score at least
eight of the twelve items. The result shows the total, category, percentile,
five-/twelve-year reference rates, missing-item count and individual item scores.
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
calculated result. Raw preview values and column names are never translated.

## Measures, plots and exports

The **score** ranges from −34 to +46; the **risk category** ranges from 1 to 9.
A **percentile** describes the proportion of the reference sample with an equal
or lower score. It is distinct from the published probability of violent recidivism.
The five- and twelve-year rates are cumulative from the start of the relevant
opportunity period.

The default plot shows the **empirical percentile curve**. An optional
**normal-distribution view** illustrates the same percentile rank using a shaded
standard-normal curve. It is an illustration, not a fitted distribution
of VRAG-R totals. At percentile 100, the marker is drawn at the finite right edge.
Plot choice and language do not change calculated values, timestamps or CSV exports.
The current plot can be downloaded as PNG, JPEG, PDF, SVG or TIFF.

**Exports always use a stable English schema, UTF-8, comma-separated fields and
a decimal point**, regardless of the interface language. `Prob_5_years` and `Prob_12_years` are
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
source("VRAGR_Calculator.R")
data <- read.csv("examples/example-data.csv", check.names = FALSE,
                 colClasses = "character", na.strings = "")
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

## Simulated data

```r
source("VRAGR_Calculator.R")
set.seed(123)
data <- sim_vrag_data(100, prob_missing = 0)
scores <- vragr(data)
case <- sim_vrag_case(envir = NULL)
```

`sim_vrag_data(N, prob_missing = 0.1)` returns
`N` rows in the scoring input schema. `sim_vrag_case(prob_missing = 0, envir = .GlobalEnv)`
returns one row invisibly. It retains the original helper's global item assignments
by default; pass `envir = NULL` to avoid assignment or supply another environment.

The dataset helper accepts nonnegative whole-number row counts, including zero.
Both helpers accept missingness probabilities between 0 and 1, applied independently
to each input field. Use `prob_missing = 0` for fully scoreable cases; otherwise
use the non-strict scoring mode to retain rows with insufficient information.
The functions use R's current random-number generator; call `set.seed()` externally
for reproducibility. Their illustrative distributions are not calibrated to a
real population and must not be used as independent scoring references.

## Development and testing

Run the R regression suite from the repository root:

```sh
Rscript tests/run.R
```

The suite covers independent scoring references, validation, imports, exports,
session state and translations. Browser tests exercise calculations, uploads,
downloads and responsive layouts in both languages. See
[Validation](docs/VALIDATION.md) for coverage and limitations, and
[Contributing](CONTRIBUTING.md) for test setup and development guidance.

## Data processing, contact and reuse

Calculations and uploads are processed **on the server**. Read
[Data processing and deployment](docs/DATA_AND_DEPLOYMENT.md) for temporary-file
handling, hosting requirements and deployment-specific privacy information.

Report bugs, feature requests and suggestions through
[GitHub Issues](https://github.com/MertenNeumann/VRAGR_Calculator/issues), using
synthetic examples rather than personal information or original case records.
The separate [OGRS 3 calculator](https://mertenneumann.shinyapps.io/OGRS_Calculator/)
and its [source code](https://github.com/MertenNeumann/OGRS_Calculator) are also available.

Application code is under the [MIT license](LICENSE), Merten Neumann.
The license does not grant rights to the cited manuals, instruments or other
published source material. Adapted OGRS components retain their MIT notice in
[NOTICE](NOTICE) and [LICENSES/OGRS-MIT.txt](LICENSES/OGRS-MIT.txt).

OpenAI Codex assisted with code, interface, documentation and tests. Case
calculations use the documented R code; no AI model is used to score cases.
