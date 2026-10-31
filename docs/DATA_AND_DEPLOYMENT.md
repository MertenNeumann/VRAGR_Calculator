# Data processing and deployment

## What the application does

- Calculations and uploads run in R on the Shiny server, not only in the browser.
- Session-local reactive values hold input, mappings, results and timestamps.
  No application code writes a case database or case-detail logs.
- Imports use Shiny's temporary upload paths. On replacement and recognised
  session end, cleanup removes only files whose resolved paths are within this
  R process's temporary directory. It never recursively deletes directories.
- Connections may take time to be recognised as ended. Crashes can prevent
  cleanup. Hosting/access logs, backups and platform retention are independent
  of application cleanup. Immediate or complete deletion cannot be promised.
- CSV/plot downloads create temporary server outputs. Users and their browser
  control any downloaded copies; application cleanup does not remove them.

Request only the required fields and an optional pseudonymous ID. Names, birth
dates and original case reference numbers are unnecessary. CSV IDs are read
as text, retaining leading zeroes. Raw uploaded names/values/previews are never
translated. Exports contain the mapped coding inputs, not an entire copy of
every unused uploaded column. Invalid rows retain row identity and errors.

## Hosting and privacy information

The existing rsconnect metadata refers to the author's shinyapps.io deployment.
The operator's identity, appropriate privacy contact, lawful basis, authorised
users, access controls and retention schedule must be established by the person
responsible for that deployment; they are not inferred from Git metadata.
An app author/contact link is not automatically the deployment's privacy contact.

The [Posit security and compliance documentation](https://docs.posit.co/shinyapps.io/guide/security_and_compliance/index.html),
checked on 31 October 2026, describes AWS **us-east-1 (USA)** hosting. This is
not a guarantee that a particular deployment is suitable for sensitive case
data. Self-hosting has its own operating and security conditions. Publish the
actual deployment-specific privacy information before processing real cases.

## Release procedure

1. Restore R dependencies using `Rscript --vanilla scripts/setup.R` and check
   `renv::status()`. Never bypass TLS or artifact verification for setup.
2. Run `Rscript tests/run.R`, then start `Rscript scripts/run.R` and execute
   `python3 tests/browser_smoke.py`. Review the generated screenshots, including
   German long labels and mobile navigation. Use synthetic data only.
3. Review the model/source decisions and release notes; keep app version,
   DESCRIPTION and lockfile consistent. Do not deploy installed libraries,
   test artifacts, caches, credentials or original case datasets.
4. If separately authorised, deploy to the verified intended rsconnect target.
   Read account bindings normally; never extract platform credentials or infer
   authorization from a `.dcf` file. No deployment is automatic in CI.
5. Test the deployed version separately in a new session. A local test, Git
   push and cloud-environment publication are not proof of remote deployment.

No new credentials are needed for the local workflows. Publication, pushing
and deployment were not performed by this revision.
