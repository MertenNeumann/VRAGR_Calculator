# Changelog

## Unreleased

- Update the EN/DE data-processing text and provider links for Connect Cloud,
  clarify logging and non-identifiable inputs, and use neutral row-ID wording.
- Show live item-completion badges, missing-input counts and scoreability in
  collapsed single-case item headers, with English/German and invalid-input states.
- Flag cleared numeric fields as incomplete when an item is marked as known,
  consistently with the existing numeric-input validation.
- Shorten the requested interpretation text in both interface languages.
- Simplify the related OGRS 3 calculator description in both languages.
- Rename the standalone scoring script to `VRAGR_Calculator.R` and update app,
  test and documentation references. Add the MIT copyright/license notice.
- Strengthen simulation argument validation and add optional missingness and
  environment selection to `sim_vrag_case()`, retaining legacy defaults.
- Fix named column mappings so field names determine assignment even when a
  mapping vector is reordered; reject incomplete or duplicate field names.
- Keep page titles as plain text during UI localisation.
- Bound SPSS import reads to detect the documented row limit.
- Restore dependencies into the actual project library, align the R version
  pin and CI system libraries, and make test runners reject warnings, skipped
  checks and empty runs.
- Separate user and contributor documentation, align shared technical guidance,
  and replace cloud-instance validation notes with reproducible test documentation.
- Updated to R 4.6.2 and current stable CRAN dependencies, including ggplot2
  4.0.3 and callr 3.8.0. Updated Playwright to 1.64.1 and aligned
  setup instructions and CI with the tested versions.
- Regenerated the lockfile from the installed packages: added S7 0.2.2 and
  removed five dependencies no longer needed by ggplot2. Verified a clean
  restore and the scoring/browser checks with the updated runtime.
- Licensed the project under MIT and aligned the README, package metadata and
  notices while preserving attribution for adapted OGRS components.

## 2.1.0 — 2026-10-31

- Compared the implementation with Harris et al. (2015),
  including item rules, final norms, missing-data instructions and worked
  examples. Documented observed-versus-final rates, percentile differences and
  inconsistencies in the book examples. The German manual remains authoritative
  wherever the sources conflict; numerical scoring rules remain unchanged.
- Defaulted all single-case items to No information. Cormier–Lang counts begin
  at zero when information is available, preserve edits across status/language
  changes, and return to zero on reset.
- Added an optional shaded standard-normal percentile view alongside the default
  empirical curve, including finite upper-tail handling and both views in exports.
- Reviewed German UI wording and specialist labels while preserving the German
  manual's category definitions. Reworked the English-only README.
- Replaced the boxed R badge with a restrained blue wordmark accent and removed
  the final AI-disclaimer sentence in both languages.
- Added original-source, default-state, plot and bilingual browser regressions.

## 2.0.0 — 2026-10-31

- Reviewed all implemented rules against the German VRAG-R manual; documented
  scope, source-table typo, numerical conventions
  and calibration limitations.
- Fixed Item 3 prorating/rounding, divergent duplicated scoring, missing app
  upper-tail percentiles, ambiguous probability keys, invalid-value handling,
  the masked missing-count bug, empty datasets and unqualified file readers.
- Added a shared base-R scoring core and 81 independent manual-derived cases.
  Named fields are order independent; legacy percentage APIs remain available.
- Added English/German UI, white/grey cards and blue accents following the
  OGRS 3 design. Separated Single case, Dataset, Methods and About; no Explore.
- Added explicit calculation, empty initial inputs, synthetic examples, stale
  result protection and server-side download freshness checks.
- Added checked XLSX/CSV/TXT/SAV imports, explicit column/category mappings,
  preserved raw previews/IDs, invalid-row retention and consistent timestamped
  UTF-8 CSV exports.
- Replaced the Gaussian percentile picture with the empirical cumulative
  reference; dataset plots show discrete counts. Preserved five plot formats.
- Added renv dependencies, run/setup scripts, CI, model/operating documentation,
  unit/server tests and bilingual browser/layout regression tests.
- Preserved the original project's unspecified licence; included MIT notices
  for reused OGRS infrastructure.
