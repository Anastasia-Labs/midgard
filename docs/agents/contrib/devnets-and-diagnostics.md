# Devnets, acceptance and read-only diagnostics

```sh
node scripts/contrib.mjs devnet plan --run-id my-check
node scripts/contrib.mjs devnet generate --run-id my-check
node scripts/contrib.mjs devnet campaign run --input /absolute/campaign.json --plan
node scripts/contrib.mjs devnet campaign run --input /absolute/campaign.json
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

A campaign runs an ordered list of devnet stages at one pinned commit and
replaces hand-written rerun scripts. The file (`midgard-devnet-campaign/v1`;
example: `demo/midgard-node-tools/config/devnet-campaign.example.json`) names
`id`, the full `commit`, `deploymentProfile`, an absolute `directory` outside
the checkout, and `stages`, each with an `id`, a required `timeoutSeconds` and
one kind: `devnet-stack` (`args` for `dist/devnet-stack.js`), `phase4`
(`action` `up` or `down` on a `runDir`, through `phase4-process/scripts/`) or
`watcher-journeys` (`runDir`, `files`, optional `env`; run through
`contrib test --config vitest.watcher-journeys.config.ts` with the journey
environment and the frozen blueprint). Unknown fields are refused.

- It refuses a checkout whose HEAD is not `commit` or whose tracked files
  changed, and never moves HEAD: create a worktree at the commit
  (`contrib worktree create --branch NAME --base COMMIT`). The one tolerated
  change is `deployment:build`'s generated profile file, and only when it is
  that profile's.
- It builds once (install, `deployment:build <profile>`, the runtime packages'
  guarded builds, the native builds; fresh steps are skipped), then freezes
  `plutus.json` and its build record into the directory and records the hash.
- Stages run in order, each under its deadline (SIGTERM, SIGKILL two minutes
  later) with its own `logs/<stage>.log`, into one `campaign-receipt.json`
  that records argv, cwd, environment keys (never values), times, exit status,
  log digest and, for journeys, the `contrib test` receipt. It stops at the
  first stage that does not pass; services a failed run started stay up.
- Rerunning the same file resumes: stages passed with the same definition are
  skipped, and everything from the first stage not passed runs again, each
  attempt in a new log. A different commit or profile against an existing
  receipt is refused: a code change needs a new campaign directory.
- Exit 0 all passed, 1 failed, 3 incomplete (cancelled, or a journey that
  skipped tests). `--plan` prints the refusal, build steps and stages and runs
  nothing. Journey stages need the test Postgres (as `contrib test` does) and
  the run's hand-staged `work/` inputs.

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
