# Model and source review

Review date: 31 October 2026. Current app version: **2.1.0**.
The initial German-manual review used baseline
`d522a4b7e2fb33995e2df26a58535a4c359bc57f`.

## Sources and scope

The implemented coding specification is **Rettenberger, M., Gregório Hertz,
P. & Eher, R. (2017), Die deutsche Version des Violence Risk Appraisal
Guide-Revised (VRAG-R)**, BM-Online 8, Kriminologische Zentralstelle.
[Official full text](https://krimpub.krimz.de/frontdoor/deliver/index/docId/40/file/bm-online8.pdf).
Page numbers below are printed page numbers; the PDF viewer page is two higher.
The reviewed PDF has SHA-256
`acf4b2ea74945ffc5c0f46e872a68bad8c6876242861822570cadc50b4494fa7`.
It is not redistributed in this repository.

The manual identifies **Harris, Rice, Quinsey & Cormier (2015), Violent
offenders: Appraising and managing risk, 3rd ed.**, [doi:10.1037/14572-000](https://doi.org/10.1037/14572-000),
as the model source. Item 12 refers to **Hare (2003), Manual for the
Psychopathy Checklist-Revised, 2nd ed.** The supplied original Harris et al.
book has now been checked against the implementation: Chapter 5, the relevant
outcome discussion, Appendices A–F and the scoring examples in G/H. The
separate Hare manual remains unavailable; its full PCL-R diagnostic criteria
have not been independently checked. The public German manual remains the
implementation's percentile reference. See the [original-source review](ORIGINAL_SOURCE_REVIEW.md)
for agreements, table differences and inconsistencies in the book examples.
**Where the sources conflict, the German manual is authoritative for the app**,
including item definitions, coding and interpretation, not only percentiles.
The historical `www.vrag-r.org` endpoint could not be retrieved; no current
rules were inferred from that unavailable website.

## Item inventory checked against the manual

There are 12 weighted items and 75 input fields. Column names and subitem
order retain the original API. `R/schema.json` contains English descriptions,
German terminology and page references. Original counts represent charges
**or** convictions where specified, not an OGRS-style convictions-only rule.

| Item | Inputs / raw measure | Weighted scores | Manual |
| --- | --- | --- | --- |
| 1 | Lived with both biological parents to 16, Yes / No; death exception | −2 / +2 | pp. 21–22 |
| 2 | None / moderate / severe school maladjustment | −3 / +1 / +4 | pp. 23–24 |
| 3 | Six Yes / No problem areas | raw <3: −2; 3: 0; 4: +1; >4: +4 | pp. 25–26 |
| 4 | Ever married or qualifying cohabitation / never | −1 / +1 | p. 27 |
| 5 | Weighted nonviolent charges/convictions before index | 0: −3; 1–2: −1; 3–8: +1; 9–17: +3; ≥18: +5 | pp. 28–32, Table 3 |
| 6 | Failure on conditional release, No / Yes | −2 / +4 | p. 33 |
| 7 | Completed years at index offence | <26: +2; 26–30: +1; 31–33: −1; 34–38: −2; 39–45: −4; ≥46: −7 | p. 34 |
| 8 | Weighted violent charges/convictions before index | 0: −2; 1–4: +2; 5–18: +3; ≥19: +4 | pp. 35–38, Table 4 |
| 9 | Prior qualifying correctional admissions | 0: −2; 1: +2; 2: +3; 3–4: +4; ≥5: +6 | p. 39 |
| 10 | Twelve conduct indicators before 15 | raw 0: −2; 1: 0; 2–4: +4; ≥5: +5 | p. 40 |
| 11 | None / exclusively children <14 / other / ≥1 female victim ≥14 | −2 / −1 / +2 / +3 | p. 41 |
| 12 | Five professionally coded PCL-R facet 4 items, 0–2 | raw 0: −6; [1,2.5): −3; [2.5,3.5): +2; [3.5,7.5): +3; ≥7.5: +6 | p. 42 |

Cormier–Lang weights, in the documented subitem order:

- Item 5: `7,3,2,5,1,5,5,2,1,1,1,5,1,2,1,1,1,1,1,1,1,1,1,1,1,1,1`.
- Item 8: `28,7,6,6,7,5,3,2,3,15,12,10,6,6,2,8,5,4`.

These weights and categorical thresholds also match Harris et al. (2015),
Appendix C and Tables A.1/B.1. The UI uses the German manual's property-value
boundary **≥ $5,000** in its Canadian-dollar classification. Harris Appendix D
(p. 305) describes the historical Canadian threshold applicable at the offence
date; its over/under wording is not a new automatic amount rule. The app accepts
category counts, not monetary amounts. It does not invent a euro conversion or an automatic mapping
to present German statutes. Attempts, overlapping offences, index offences,
admissions, self-report and source-quality decisions still require the manual.
Item 11 includes convincing evidence and the index offence even without
charges; Items 5 and 8 do not use that broader rule. Item 4's historical
heterosexual-relationship restriction is stated rather than silently removed.
The additional review found broader wording for the five-point category in
English Table B.1: it also lists assault causing bodily harm. The app retains
the German Table 4 wording for Item_8_6 and translates that wording into English;
it does not silently add the broader English category. Likewise, the German
manual's "Mittäterschaft (Delikte in Absprache mit anderen Tätern)" label is
retained despite the English term "conspiracy". These differences are recorded
in the original-source review rather than resolved by an English override.

## Missing information and numerical decisions

**Item 3 (p. 25):** with at least one known problem area, compute
`6 × known Yes / known subitems`, round to the nearest whole number, then
apply item coding. All unknown makes Item 3 missing. For example 3 Yes,
2 No and 1 unknown gives 3.6 → 4 → item score +1.

**Item 12 (p. 42):** all five known gives their sum. With four known items,
use `5 × sum / 4`; with fewer than four, the VRAG-R item is missing. The
unrounded facet score is compared with the VRAG-R boundaries. For integer
0–2 inputs this yields the same VRAG-R item scores as the original explicit
one-missing lookup (including exactly 2.5 and 7.5). It does not reproduce or
claim to validate the full Hare manual's assessment instructions.

**Whole-score prorating (p. 20):** if at most four of the 12 items are
missing, sum the known weighted scores. For a nonnegative sum use the
positive item maxima; for a negative sum use the negative minima. Divide
the observed sum by the sum of those extrema for the observed items and
multiply by the extrema of the missing items. Add this adjustment and round
the final total. More than four missing items leaves all derived risk measures
empty. Zero yields zero. No missing data are replaced with No or zero.

The extrema are `−2,−3,−2,−1,−3,−2,−7,−2,−2,−2,−2,−6` and
`2,4,4,1,5,4,2,4,6,5,3,6`; possible total scores are −34 through +46.
The published p. 20 example is reproduced: observed +11, observed positive
maximum +35, missing Items 9 and 10 maximum +11, adjusted 14.457… → **14**.

The German manual does not explicitly settle exact .5 ties. This implementation
documents a nearest-integer, **halves away from zero** convention, rather than
R's implicit ties-to-even. It can affect a prorated result exactly at a half:
−8.5 now becomes −9 rather than −8. This is an explicit software convention,
not a claimed additional empirical rule.

For partially unknown subitems in **5, 8 and 10**, the app conservatively marks
the whole item missing. No subitem prorating formula is supplied there by the
manual. Invalid data (negative or fractional counts, invalid categories,
out-of-range PCL-R scores) are distinguished from genuine missing information.
Age must be positive and in completed years; no unsupported age-10/120 bounds
or automatic eligibility decisions have been introduced. All single-case items
initially remain missing. For Items 5 and 8, selecting Information available
uses the displayed counts, initially zero for every category. The user must
review these counts; selecting No information excludes them from scoring.
Blank cells in imported datasets remain missing and are not replaced by zero.

## Reference measures

Table 1, p. 11, is implemented without recalibration:

| Category | Total | Five-year rate | Twelve-year rate |
| --- | --- | --- | --- |
| 1 | ≤−24 | .09 | .15 |
| 2 | −23…−17 | .12 | .24 |
| 3 | −16…−11 | .16 | .33 |
| 4 | −10…−4 | .20 | .42 |
| 5 | −3…3 | .26 | .51 |
| 6 | 4…11 | .34 | .60 |
| 7 | 12…17 | .45 | .69 |
| 8 | 18…26 | .58 | .78 |
| 9 | ≥27 | .76 | .87 |

Table 2, p. 12, supplies empirical cumulative percentiles for −34…44.
Its row printed “17 / 24” is an evident missing minus sign: it lies between
−18 and −16, whereas +17 has its own 78.1 entry. The implementation uses
**−17 → 24**. Values 45 and 46 remain at **100**, a documented monotone
extension because the cumulative table already reaches 100 at 44. These two
values are not represented as separately published normative entries.
The default single-case plot shows this empirical cumulative distribution.
An optional standard-normal curve illustrates the same percentile through its
normal quantile and shaded area; it does not fit a normal distribution to
VRAG-R totals. Percentile 100 is shown at the finite right plot boundary,
avoiding the former infinite `qnorm(1)` at upper-tail values. Changing views
does not alter calculations, reference values or CSV exports.

Violent recidivism is defined on pp. 7–8; it includes new charges and qualifying
institutional violence, not simply any reconviction. The reference sample of
1,261 male offenders and the juvenile, female, jurisdiction and at-risk-period
limitations are discussed on pp. 7–15. The app does not adjust the baseline
rates for violence-free years in the community (pp. 14–16), custody length,
treatment or dynamic changes. Neither rates nor percentiles establish an
individual's certain outcome or the accuracy of local calibration.

## Bugs corrected before the interface refactor

1. Item 3 prorating was not rounded before categorical comparison, producing
   a false missing item for values such as 3.6 and changing the total.
2. App and standalone code were duplicated and divergent. The app omitted
   percentiles for 45/46; it also returned duplicate, misspelled five-year
   probability keys for different follow-up periods. A single core now serves
   both workflows, with distinct five-/twelve-year outputs.
3. The private missing-score helper used `sum(idx_missing > 4)` instead of
   counting missing values. An outer guard previously masked this error.
4. Invalid binary categories were silently scored as No. Negative and fractional
   counts and ages could reach scoring. Validation now rejects invalid rows.
5. The `1:nrow(data)` loop mishandled empty datasets. Empty outputs now retain
   their defined schema; named fields also avoid positional column-order errors.
6. CSV/TXT readers were called without importing readr. Readers are now qualified.
7. Edits could leave old results available for download. Both workflows and all
   download handlers now check a complete scoring/input snapshot.

`vragr(data)` is the new independent API (probabilities 0–1).
`compute_vrag(...)` and `compute_vrag_data(...)` retain the 75-field input
order and legacy percentage probabilities. The single-case API preserves the
original 75-argument signature, including named and positional scalar inputs.
Misspelled duplicate result keys are intentionally not retained. The progress
argument is retained for compatibility; the vectorised core does not create a
Shiny progress bar. Legacy simulation helpers remain available using base R;
`sim_vrag_case()` retains its historical global item bindings. Neither helper
is used by the app or reference fixtures, and their toy distributions do not
represent an offender population.

## Validation evidence

`tests/fixtures/manual-reference.csv` contains **81 independent cases**, one
for every attainable integer total. Inputs and expected item scores were
constructed from the manual's item coding options; expected total scores
are their independent sums. Percentiles were transcribed from the PDF table
and rates/categories from Table 1, never generated by the scoring code.
Additional tests cover every age/category boundary, all 45 Cormier–Lang
weights, each item coding option, the published prorating example, positive
and negative missing-score adjustments, invalid values, empty datasets and
a 100,000-row batch. Workflow/server/browser tests cover imports, raw-value
preservation, missing/zero defaults, English/German state, both plot views,
stale exports and responsive layouts. Additional independent fixtures from
Harris et al. cover final norms and the worked-example discrepancy; see the
[original-source review](ORIGINAL_SOURCE_REVIEW.md).

These are software checks, not an independent clinical validation study.
