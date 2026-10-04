# Devnets, acceptance and read-only diagnostics

```sh
node scripts/contrib.mjs devnet plan --run-id my-check
node scripts/contrib.mjs devnet generate --run-id my-check
node scripts/contrib.mjs acceptance --input /absolute/stack.json --plan
node scripts/contrib.mjs acceptance --input /absolute/stack.json
node scripts/contrib.mjs diagnose --url http://127.0.0.1:3000/readyz --output readiness.json
node scripts/contrib.mjs diagnose --snapshot observation.json
```

Read the existing devnet and acceptance skills before operating services.
Devnet allocations bind checkout and run ID to compose identity and three
ports. Generation checks/leases those ports and invokes the existing isolated
generator with a fresh private run directory under the contributor registry.
The saved allocation records that path; ambient run-directory variables do not
select an existing deployment. Its receipt binds the generated asset bytes.
It launches no chain and resets no state. Hash collisions or ports
occupied by another application are explicit refusals; choose a different run
ID. A generated allocation is not a persistent reservation after generation.

Acceptance wraps the existing `e2e-stack` command with workspace/run/port
ownership, guarded builds, cancellation and exact execution provenance. It
preserves the existing stack's deployment, storage, controller-lock and saved
evidence checks. Its execution receipt alone does not certify payout/drill or
program acceptance; follow the existing acceptance skill's completion gates.

Diagnostics accepts a local read-only HTTP endpoint or a saved JSON snapshot.
It never opens a journal, migrates a schema or acquires a protocol lease. [script: scripts/contrib/diagnostics.mjs]
Credentials, transaction material and URL credentials/query parameters are
redacted by default. Progress classification requires a `progress` observation
with `observedAtMs`, `eligibleWork`, `lastSuccessfulTransitionMs` and
`expectedProgressWithinMs`; optional `processAlive`, `safetyHold` and
`dependencyFailure` explain death, holds and outages. Existing endpoint schemas
are retained verbatim with redaction and remain `unknown` until they supply
those explicit progress facts. Quiet work queues are idle, not stalled.

All commands expose failure instead of silently resetting, redeploying,
inventing compatibility state or upgrading a smoke run into acceptance.
