# Independent synthetic fixtures

`manual-reference.csv` contains 81 synthetic, fully observed rows: one for each
total −34…46. Each expected item score was assigned from the coding bands on
printed pp. 21–42 of Rettenberger, Gregório Hertz & Eher (2017). Independent
integer addition supplies the expected total; Table 1 (p. 11) supplies expected
category and rates. Percentiles were transcribed from Table 2 (p. 12), with the
documented −17 sign correction and cumulative extension to 45/46.

The scoring implementation did not generate expected values. Fixture inputs
are chosen representatives of item bands, not records or simulated samples
drawn from a real offender population. The separate tests exercise other
within-band inputs, every weight and missing values.

`cases.xlsx` has a Notes worksheet and a Cases worksheet containing a text ID
`001` and the synthetic prorating example used in `examples/example-data.csv`.
It tests worksheet selection and identifier preservation. No file contains
original case data. See `docs/MODEL_AND_SOURCES.md` for source details.
