# Bounded-carriage necessity evidence template

Use this structure for a measured artifact under
`docs/exec-plans/evidence/necessity/`. Replace placeholders with actual
measurements; the template itself provides no acceptance evidence. The current
carriage tiers and bounds are defined in [the transaction specification](../../spec/midgard-tx.md).

## Binding

- Family and authenticated item:
- Source revision and dirty-source disclosure:
- Compiler identity, flags, and generated blueprint digest:
- Applied validator hashes and parameter application:
- Target-network parameter snapshot and digest:
- Fixture path, content identity, and reproduction command:

Changing a bound identity invalidates the measurement for current acceptance.

## Measurements

Measure the simplest allowed representation first. For each applicable direct,
publication, or incremental route, record:

| Representation           | Actual L1 transaction bytes / limit | Memory / limit | CPU / limit | Fee | Required reserve | Verdict |
| ------------------------ | ----------------------------------- | -------------- | ----------- | --- | ---------------- | ------- |
| Fill from a measured run |                                     |                |             |     |                  |         |

Name the working directory, exact command, exit status, result artifact, and
whether the result uses real applied scripts, emulator evaluation, or diagnostic
framing. Include publication and consumption costs. A diagnostic estimate does
not establish deployability.

## Why the simpler representation is insufficient

Identify the exact measured limiting constraint and the first failing boundary.
Explain why the selected decomposition is necessary without reducing admitted
transaction capability. Do not infer failure from a test that never reaches the
relevant validator or from an artificial emulator limit.

## Preserved semantics

Identify the commitment shared by all routes and the current tests proving the
same logical result. Include valid controls and rejection of omission,
duplication, reordering, substitution, wrong domain, and trailing data where
applicable. Record remaining gaps explicitly; a fallback's existence alone does
not close acceptance.
