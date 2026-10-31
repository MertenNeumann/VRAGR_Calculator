# Validation record

Validated locally on 31 October 2026 with R 4.5.3 and the versions in
`renv.lock`. All data are synthetic. The implementation is an uncommitted
working-tree revision of baseline `d522a4b7e2fb33995e2df26a58535a4c359bc57f`.

| Check | Observed result |
| --- | --- |
| `Rscript tests/run.R` | **17 test groups, 470 passing expectations**, no failures or skipped checks |
| Independent scoring fixtures | All **81 attainable totals**, expected item scores, category boundaries, percentiles and five-/twelve-year rates match the manual-derived fixtures |
| Missing data | Published +11 / +35 example gives 14; Item 3 rounding, facet 4 thresholds, positive/negative totals and the exact −8.5 midpoint convention pass |
| Invalid/empty data | Unknown categories, negative/fractional counts, invalid PCL-R scores and empty data frames handled as specified |
| Large batch | 100,000 fully observed rows score correctly; a separate local timed call took approximately 4.4 seconds (not a general performance guarantee) |
| Imports | XLSX worksheet selection, both CSV separators, TXT and SAV; malformed tables, empty/duplicate headers, text IDs and literal `NA` values checked |
| Server state | Explicit calculation, complete input/mapping snapshots, language-independent results, freshness and optional ID checks pass |
| `python3 tests/browser_smoke.py` | **Pass**, using Playwright and the installed system Chromium |
| Browser functionality | Real calculation, uploads, mapping, invalid-row retention, five plot download formats, stable CSV filenames/bytes/timestamps and server-side stale-download rejection pass |
| Language and layout | EN/DE switching, independent English-start sessions, raw names/values, custom mappings, long unbroken headers, selected/empty prompts and 390/768/1024/1280/1440/1920px viewports pass |
| Visual review | English/German single-case and dataset screenshots and the fully opened German mobile menu reviewed; no horizontal page overflow or clipped selected labels in the checked controls |
| Runtime | Documented project start command and local HTTP response verified with the renv project library |
| Dependency state | Setup/restore exits successfully; `renv::status()` reports a consistent project; repeated pinned cloud installation succeeds |

Browser screenshots and downloads are generated under ignored `tests/artifacts/`.
R/browser/installation logs in this prepared instance are retained under
`/workspace/.vragr-cloud/logs/`, including `project-tests-final.log`,
`browser-tests-final.log`, `final-app.log`, `renv-final.log` and
`install-project-final.log`. They are not committed application content.

The command sandbox initially prevented renv's local socket lock and produced
slow retry warnings. Running R through the supported network-capable command
context resolves that environment issue without disabling renv isolation or
TLS verification. The prebuilt environment works; the optional development
headers needed for a fresh source build are described in README.md.

## Limits of this record

These software checks are not a clinical validation or certification.
The public German manual was checked; the complete Harris (2015) and Hare
(2003) books were not independently available. Exact midpoint rounding and
the empirical percentile extension are documented decisions, not additional
published normative entries. See [Model and sources](MODEL_AND_SOURCES.md).

CI has been configured but was not executed on GitHub. No Git push, deployment
to shinyapps.io, cloud snapshot publication or fresh-task restoration was
performed. A separately published deployment requires its own smoke test and
deployment-specific operating/privacy information.
