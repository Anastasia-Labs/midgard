# Verification ledger

All runs are dated 2026-10-16. Commands below run from the repository root.
This is a plan/prototype/documentation change; no production runtime code was
modified. Existing unrelated dirty files were preserved.

## Reproduce the design experiments

`probes.mjs` accepts the repository, an already-installed dependency checkout,
and a new evidence directory. It reads immutable Git sources and uses that
checkout's TypeScript and Effect dependencies. It creates disposable fixtures
under the evidence directory, removes only those fixtures in `finally`, and
leaves its results JSON. No service, database, network or deployment is needed.

```sh
node docs/exec-plans/agent-friendly-midgard-2026-10-16/probes.mjs \
  /home/gumbo/midgard-hub/midgard \
  /home/gumbo/midgard-hub/midgard-integration \
  /tmp/midgard-agent-friendly-20261016/consolidated-proof
```

The frozen integration SHA is embedded in the script. Six experiment groups
cover map equality, type completeness, recording-builder completion, the exact
own-dist selector, installed-consumer export behavior and container runtime
closure. Results and limitations are in [REPORT.md](REPORT.md) and
[design-probes.json](evidence/design-probes.json). These experiments do not
replace real builders, manifests, installed Midgard packages or Docker startup.

## Executed evidence

The initial individual experiments and the first consolidated run used Node
24.13.1, TypeScript 5.7.3 and Effect 3.21.2. The final rerun uses the declared
Node 22.22.2. Final command results are recorded in
[checks.json](evidence/checks.json).

- Consolidated command above: six experiment groups, exit 0. It proves the
  reported observations and synthetic controls, not production acceptance.
- `node scripts/contrib.mjs --help`: one real control-surface invocation,
  exit 0.
- `node scripts/contrib.mjs prepare --package midgard-node --plan --output /tmp/midgard-agent-friendly-20261016/feature-proof/plan.json`:
  one plan, exit 0; package/schema/mode and prerequisites inspected.
- `node scripts/contrib.mjs prepare --package midgard-no-such-package-20261016 --plan`:
  one negative case, expected exit 1 with unknown-package diagnostic.
- The planning walkthrough compared SHA-256 content of all 2,953 existing
  package dist files before/after: unchanged. No fixtures or services were
  created, and evidence remains after the command exits. This observes output
  immutability; it is not a system-call trace of every possible side effect.
- `node scripts/doctor.mjs --json`: environment diagnosis only; see recorded
  rows. The initial sandboxed attempt returned 1 with EPERM/unknown probes and
  a missing validation build stamp. A local read-only rerun distinguishes the
  environment from sandbox restrictions. Neither is counted as a suite pass.
- `node scripts/ci/check-agent-skills.mjs`: skill structure and relative links.
- Repository documentation-link and enforcement-tag checks use the existing
  exported checker functions with tracked paths plus this task's untracked
  additions. This checks new files without staging or altering the user's
  index. New feature-map rules also receive an explicit tag check.
- Scoped whitespace, Markdown/JSON/JavaScript formatting and JavaScript syntax
  checks cover only this task's new report/probe files and feature-map edit.

The original [feature-proof.json](evidence/feature-proof.json) preserves the
first walkthrough; the final pinned-Node result is recorded alongside it.
The ledger records each final command's count and exit code.

## Not checked

No full package builds/suites, Aiken compilation, ledger regeneration, transaction
preparation lanes, actual installed Midgard consumer run, Docker image build,
devnet/Preprod wallet journey, crash/rollback/fault-proof acceptance, hosted CI
or merge readiness were run. They are unnecessary to validate this plan and
documentation, and remain explicit gates for implementing its proposals.

The three-feature map validates one supported planning feature end to end.
Focused tests and compiled boundaries are mapped to existing owners, with
their prerequisites and current coverage limits; their executions are not
claimed. Live acceptance remains with the existing acceptance/devnet skills.

Two independent design candidates and an independent judge supplied structural
comparison, all through the same provider. Automatic approval review rejected
the external CLI panel because it could not verify the destination for private
repository material. No external seat launched; cross-provider review is a gap.
No additional approval was needed to complete the local investigation.
