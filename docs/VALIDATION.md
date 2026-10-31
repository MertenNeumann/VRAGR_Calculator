# Validation record

App version **2.1.0**, validated locally on 31 October 2026 with R 4.6.2
and the package versions in `renv.lock`. Test inputs are synthetic or derived
from published worked examples; no user case records are included.

| Check | Observed result |
| --- | --- |
| `Rscript tests/run.R` | **21 test groups, 562 passing expectations**, no failures, warnings or skipped checks |
| Current dependency versions | All **75 locked R packages** match the stable CRAN index checked on 31 October 2026; R 4.6.2 and Playwright 1.64.1 match the official release indexes |
| Clean restore | `testthat` confirmed absent in a new isolated library, then all locked dependencies restored and the full R suite passed |
| German-manual scoring fixtures | All **81 attainable totals**, expected item scores, category boundaries, percentiles and five-/twelve-year rates match the independent fixtures |
| Original-source fixtures | All nine final category/rate rows in Harris Table C.1 match; Appendix G and both explicit interpretations of Appendix H produce the expected scores and rates |
| Source precedence | German-manual definitions and percentiles retained where the English source differs; disagreements and worked-example inconsistencies documented separately |
| Missing data | Published +11 / +35 example gives 14; Item 3 rounding, facet 4 thresholds, positive/negative totals and the exact −8.5 midpoint convention pass |
| Default answers | Every single-case item starts missing; all 45 Cormier–Lang counts start at zero, enter scoring only when information is available, preserve edits across status/language switches and reset to zero |
| Invalid/empty data | Unknown categories, negative/fractional counts, invalid PCL-R scores and empty data frames handled as specified |
| Large batch | 100,000 fully observed rows score correctly |
| Imports | XLSX worksheet selection, both CSV separators, TXT and SAV; malformed tables, empty/duplicate headers, text IDs and literal `NA` values checked |
| Server state | Explicit calculation, complete input/mapping snapshots, language-independent results, freshness and optional ID checks pass |
| Plots | Empirical and optional standard-normal views render without warnings, including upper-tail percentile 100; plot choice preserves scoring state, calculation time and CSV data |
| `python3 tests/browser_smoke.py` | **Pass**, using Playwright and the installed system Chromium |
| Browser functionality | Missing/zero defaults, real calculation, uploads, mapping, invalid-row retention, five plot download formats, stable CSV filenames/bytes/timestamps and server-side stale-download rejection pass |
| Language and layout | EN/DE switching, independent English-start sessions, raw names/values, custom mappings, long unbroken headers and 390/768/1024/1280/1440/1920px viewports pass |
| Visual review | German normal/empirical single-case views, About and fully opened mobile navigation reviewed; no horizontal page overflow or clipped selected labels in the checked controls |
| Source and development text | Browser checks confirm German source precedence and the shortened bilingual Codex disclosure; README remains English-only |

Browser screenshots and downloads are generated under ignored `tests/artifacts/`.
Logs in this prepared instance are retained under
`/workspace/.vragr-cloud/logs/`, including `r462-tests-final.log`,
`r462-browser-final.log`, `r462-app-final.log` and `r462-installer-repeat.log`. They are not committed
application content.

## Dependency and runtime checks

The R 4.6.2 refresh updates ggplot2 from 3.5.2 to 4.0.3 and callr from 3.7.6
to 3.8.0. S7 0.2.2 is a new dependency; MASS, Matrix, lattice, mgcv and nlme
are no longer required and were removed by `renv::snapshot()`. Every retained
package was compared with the current CRAN index. The lockfile was generated
from packages actually installed under R 4.6.2, not by editing version fields.

The full suite passed after restoring into a newly created, isolated renv
library where `testthat` was confirmed absent beforehand. Test dependencies
remain in the lockfile and `snapshot.dev` remains enabled. The self-contained
cloud installer pins 96 native runtime/compiler artifacts with SHA-256 hashes;
rerunning it completed successfully. Its R dependencies are restored from the
project lockfile. Playwright 1.64.1 and its current compatible Python
dependencies were installed for the browser checks. The documented startup
command serves the app with the project library.

The command sandbox can prevent renv's local socket lock. R checks and browser
connections used the supported command context that permits those operations,
without disabling renv isolation or TLS verification.

## Limits of this record

These software checks are not a clinical validation or certification. The
German manual is the governing reference. The supplied Harris et al. (2015)
publication was checked in the sections listed in the
[original-source review](ORIGINAL_SOURCE_REVIEW.md); the separate full Hare
PCL-R manual was not available for independent review. Exact midpoint rounding,
the empirical percentile extension and finite normal-plot endpoints are
documented software decisions, not additional published normative entries.

The results above describe local checks, not a claimed GitHub Actions run.
No deployment to shinyapps.io or cloud snapshot publication is part of this
revision. A separately published deployment requires its own smoke test and
deployment-specific operating/privacy information.
