# Software validation

The test suites check scoring consistency and application behavior. They are
not a clinical validation study, certification or evidence of calibration in a
new population. Coding and interpretation require the published instrument
manuals; see [Model and sources](MODEL_AND_SOURCES.md).

## Independent references

`tests/fixtures/manual-reference.csv` contains 81 cases, covering every
attainable total from −34 through +46, with independent item scores and the
German manual's categories, percentiles and reference rates. Additional fixtures
transcribe Harris et al. (2015) final norms and worked examples, including both
interpretations of the inconsistent Appendix H checklist. See the
[original-source review](ORIGINAL_SOURCE_REVIEW.md) for that discrepancy.

Fixture expectations are independent of the scoring implementation. See the
[fixture notes](../tests/fixtures/README.md) for provenance. Tests and examples
contain no user case records.

## Coverage

The R suite checks every item band and Cormier–Lang weight, missing-data
prorating, rounding conventions, invalid and empty data, a 100,000-row batch,
imports, IDs, exports, calculation freshness, translations and both plot views.
Browser checks also exercise the five plot download formats and multiple
viewport widths. Numerical conventions and percentile extensions are documented
in the model guide rather than treated as additional published norms.

The R runner reports the tests and assertions executed and fails on failures,
warnings, skipped checks or an empty run. Browser tests exercise real Shiny
sessions, calculations, mappings, uploads, downloads, language switching and
responsive layouts. Inspect the current test output and CI results for the
revision being evaluated; historical results do not establish that a later
revision passes.

See [Contributing](../CONTRIBUTING.md) for reproducible commands, browser
requirements and artifact locations. Test dependencies are recorded in
`renv.lock` and `tests/requirements-browser.txt`.

## Limits

Passing tests establish the behavior exercised by those checks. They do not
verify every possible input, assess the quality of professional item coding,
validate local risk calibration or establish that a hosting environment is
suitable for sensitive data. A hosted release needs its own functional checks
and deployment-specific operating/privacy information.
