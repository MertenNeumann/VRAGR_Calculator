# Changelog

## 2.1.0 — 2026-10-31

- Checked the implementation against the supplied Harris et al. (2015) original,
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
  scope, unavailable primary books, source-table typo, numerical conventions
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
  for reused OGRS infrastructure. No remote deployment was performed.
