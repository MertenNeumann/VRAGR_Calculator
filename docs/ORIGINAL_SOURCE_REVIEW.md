# Review against Harris et al. (2015)

This comparison supplements the [German-manual specification](MODEL_AND_SOURCES.md).

**Harris, G. T., Rice, M. E., Quinsey, V. L. & Cormier, C. A. (2015).**
*Violent offenders: Appraising and managing risk* (3rd ed.). American
Psychological Association. [doi:10.1037/14572-000](https://doi.org/10.1037/14572-000).

The reviewed edition has PDF SHA-256
`b068949e7845a1de349b5a48aa2f374ddc25e31ed1ae639902b295960f52bc84`.
The book is **not redistributed** in this repository.
Page references below are printed pages; PDF pages are 17 higher.

**Source precedence:** the German manual remains the governing reference
where wording, tables or interpretations conflict. The English publication is
an additional audit source, not an instruction to replace German specifications.

## Scope and result

The review covers the outcome definition in Chapter 5 (pp. 122–126), VRAG-R
development and calibration (pp. 142–145), Cormier–Lang Tables A.1/B.1
(pp. 287/291), all twelve items and final norms in Appendix C (pp. 295–299),
relevant supplementary instructions in Appendix D (pp. 301–306), application
questions in Appendix E (pp. 311–315), missing-data rules in Appendix F
(pp. 321–324), and the scored examples in Appendices G/H (pp. 338–341 and
354–356).

**No change to numerical item weights, category cutoffs or final five-/twelve-year
rates was required.** These agree with the German implementation. The English
review did identify differences in offence labels and other source differences
that must not be silently treated as software errors.

| Component | Original source | Finding / implementation |
| --- | --- | --- |
| All twelve item bands | Appendix C, pp. 295–298 | Match the scoring core, including age at the first index offence of a series |
| Item 3 | pp. 295–296 | Prorate to six known-equivalent indicators; round before converting to item score; at least one indicator must be known |
| All 45 offence weights | Tables A.1/B.1, pp. 287/291 | Match the implemented vectors |
| Item 9 | p. 297 | Prior qualifying admissions; no transfers or stand-alone overnight stays |
| Item 11 | p. 297 | Entire history including the index offence, convincing evidence, not limited to charges or convictions |
| Item 12 | pp. 297–298 | Five PCL-R facet 4 items; at most one missing; the separate Hare manual remains the assessment reference |
| Missing whole items | Appendix F, pp. 321–324 | At most four; signed extrema-based total adjustment; no acceptable substitute variables for VRAG-R |
| Prorating example | p. 324 | Observed +11 with Items 9/10 missing gives 14 after adjustment and rounding |
| Exact midpoint ties | Appendices C/F | Nearest-whole-number instruction does not explicitly settle exact .5 ties; the documented halves-away-from-zero convention is retained |
| Final rates / categories | Table C.1, p. 298 | Match the German manual exactly; distinguish these final norms from Table 5.7 observations |
| Percentiles | Table C.2, p. 299 | Coarser and partly different from the German table; keep the declared German 2017 reference |

## Observations are not the final norms

Table 5.7 (p. 144) describes observed category rates. The text on pp. 144–145
explains that the five-year final estimates use a logistic fit and the longer
follow-up uses a linear fit. The final norms appear in **Table C.1**, which
is also the table reproduced in the German manual. Examples:

| Category | Table 5.7 observed, 5 / 12 years | Table C.1 final norms used by the app |
| --- | --- | --- |
| 1 | .09 / .16 | .09 / .15 |
| 3 | .15 / .31 | .16 / .33 |
| 4 | .22 / .43 | .20 / .42 |
| 7 | .41 / .66 | .45 / .69 |
| 9 | .76 / .90 | .76 / .87 |

Replacing app rates with the chapter's observed rates would therefore be an
incorrect source comparison. `harris-final-norms.csv` independently transcribes
all nine final category/rate rows from Appendix C.

## Percentile reference retained

The English book's Table C.2 lists selected scores with integer percentiles.
The German manual's Table 2 lists a fuller table with decimal values. They
are not interchangeable, nor are all differences explained by simple rounding:

| Score | Harris Table C.2 | German Table 2 used by the app |
| --- | --- | --- |
| −23 | 14 | 13.2 |
| −11 | 35 | 35.6 |
| 0 | 52 | 51.4 |
| 7 | 63 | 62.1 |
| 30 | 94 | 93.6 |

The app remains a calculator using the **German 2017 percentile reference** in
both interface languages. No new percentile values are interpolated from the
sparse English table. Score/category/rate calculations are unaffected by this
presentation reference. The existing documented cumulative extension to 100
at scores 45/46 is retained.

The optional normal curve transforms that same selected empirical percentile
using the standard-normal quantile. It is a visual illustration, not a claim
that actual VRAG-R totals are normally distributed. Percentile 100 is drawn
at the finite right plot boundary, rather than passing `qnorm(1)` infinity to
the graphics device. The empirical view remains the default.

## Offence labels and jurisdiction

The five-point category in English Table B.1 includes **assault causing bodily
harm** as well as criminal negligence and impaired driving causing bodily harm.
German Table 4 lists negligent injury and injury from impaired driving, without
that first English component. The app retains the **German Table 4 category**
for `Item_8_6`, in both interface languages. Its numeric weight remains 5. The
English wording is documented here rather than silently extending the German
category. Coding a particular offence remains a manual-based professional task.

English Table A.1 uses **conspiracy**, while German Table 3 uses
**Mittäterschaft (Delikte in Absprache mit anderen Tätern)**. The app preserves
the German label and its coding meaning.
The review does not equate every German legal concept of joint perpetration with
the Canadian concept of conspiracy or create a statutory crosswalk.

The same precedence applies to other short category descriptions: for example,
Item_5_22 retains resistance to enforcement officers rather than importing the
English table's additional obstruction-of-justice wording. English UI labels
translate the governing German descriptions. Readability edits retain the
under-16 qualifier in Item_8_15 and do not turn Item_10_11 into a requirement
that truancy must also have occurred.

Appendix D (pp. 304–306) requires charges or convictions before the index
offence, no duplicate counting of a charge and conviction for the same act,
and use of the more serious supported classification. Attempts generally
count as the completed offence except for separately weighted attempted
murder. Unknown details may justify the documented lower-bound classification;
they do not justify inventing an unknown offence count.

The book describes historical Canadian monetary thresholds (p. 305); the German
manual uses the 5,000-dollar boundary shown in the app. These are coding
guides, not a euro conversion or an automatic amount-to-category algorithm.
The input is the professionally coded count for each category, with the German
manual taking precedence when the source wording differs.

## Worked examples and internal inconsistencies

**Appendix G, pp. 339–341:** the twelve printed item scores sum to **30**,
category **9**, with a final twelve-year rate of **.87**. The app reproduces
the item-score arithmetic and rate. The numeric scoring-position fixture is
not a new recoding of the published history under German offence definitions.
German percentile 93.6 is retained; English Table C.2
gives 94 at this score. The example's narrative "only 5% score higher" is not
used to overwrite either normative table.

**Appendix H, pp. 354–356:** the intended item scores sum to **−17**, category
**2**, with twelve-year rate **.24**. The app reproduces this when two substance
indicators are Yes, as implied by the stated Item 3 score of −2 and the final
total. The printed checklist also has a third substance indicator ticked.
Taking those three ticks literally produces Item 3 score **0**, total **−15**,
category **3**, and twelve-year rate **.33**. The software follows Appendix C's
general rule, not the inconsistent annotation. Both interpretations are explicit
regression fixtures so the discrepancy cannot be hidden by a special case.

`tests/fixtures/harris-reference.csv` contains only numeric/categorical coding
inputs and expected outputs derived from these published examples, not the
case histories or names. These fixtures complement, rather than replace,
the 81 German-manual reference cases covering every attainable total.

## Outcome and application limits

The outcome is specified violent conduct operationalised primarily through
criminal charges, plus equivalent qualifying institutional conduct (pp. 122–123,
200–201). The original definition includes contact sexual offences and armed
robbery, and excludes simple robbery and non-contact sexual offences. It is
not a general reconviction measure and not confined to sexual recidivism.
The German manual's outcome summary uses the broader label "Raub"; the app
does not impose an English exclusion as a silent amendment. For conflicting
application or interpretation details, the German manual is the governing source.

Appendix E confirms the distinctions already described in the app: Canadian
development data and limits of generalisation; adolescents mostly becoming
adults before the opportunity period; no established equivalent female
calibration; age at index rather than current age; and the difference between
time in custody and continuous violence-free time in the community. The
calculator continues to display unadjusted final reference rates. It does not
silently apply the separate postrelease adjustment discussed on pp. 313–315
or substitute other variables for missing VRAG-R items.

This is a software/source consistency review, not a new clinical validation
study or an independent review of the full Hare PCL-R assessment manual.
