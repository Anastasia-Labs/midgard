# Verifying a fault-proof family

The commands, in the order to run them. Read this when you reach step 5 (DA
fixture) or step 10 (verify) of the skill. Run everything from the repository
root unless a line says otherwise, and anything longer than a few seconds
under `nice -n 19` with a `timeout`.

CI step names below are those in `.github/workflows/midgard-node-ci.yml` and
`.github/workflows/aiken-ci.yml` as of 2026-09-25.

## 1. Presence

```bash
node .agents/skills/adding-fault-proof-families/scripts/family-checklist.mjs <category>
```

Exit 0 before anything else. Exit 2 means the script could not read a source
(a moved file or renamed table), not that the family is complete.

## 2. Aiken

```bash
node onchain/aiken/scripts/pinned-compiler.mjs
```

It must exit 0 before any `aiken` command. Focused module checks and the
zero-tests trap are in
[aiken-contract-build](../../aiken-contract-build/SKILL.md); use its guarded
selector (`onchain/aiken/scripts/guard-focused-selector.mjs`) rather than a
bare `aiken check -m`, which exits 0 when it collects nothing.

Format your new `.ak` files the way "Run normalized Aiken auto-formatter
check" does (`.github/workflows/aiken-ci.yml:183–187`): `aiken fmt` on each
file, then strip trailing whitespace; a second pass must change nothing. The
CI step formats only tracked files, so an untracked file is not
checked until it is committed.

## 3. Blueprint

The fault-proofs emulator tests load `onchain/aiken/plutus.json`
(`demo/midgard-fault-proofs/tests/support/emulator/blueprints.ts:29`, or
`MIDGARD_REAL_BLUEPRINT_PATH`). Rebuild it after any Aiken change, as CI does
in "Build testnet Aiken blueprint":

```bash
pnpm --dir demo deployment:build preprod-testing
```

A stale `plutus.json` makes emulator tests fail against the old validators.
Never commit it.

## 4. Focused package tests

Run the files your change touches, not whole suites. The fault-proofs suite
took 20.5 minutes at eight forks on 2026-09-21 (its `vitest.config.ts`).

```bash
pnpm --dir demo/midgard-sdk run build
pnpm --dir demo/midgard-sdk exec vitest run tests/fraud-proof-catalogue-registration.test.ts tests/reference-scripts.test.ts
pnpm --dir demo/midgard-core exec vitest run tests/deployment-manifest-identity.test.ts
pnpm --dir demo/midgard-fault-proofs exec vitest run tests/workflow.test.ts tests/family-application-registry.test.ts tests/typed-reason-disposition.test.ts
pnpm --dir demo/midgard-fault-proofs exec vitest run tests/<kebab>-lifecycle.test.ts
pnpm --dir demo/midgard-watcher exec vitest run tests/runtime/deployment-identity.test.ts
```

Then each package's typecheck, which is where most tables in
[touch-points.md](touch-points.md) are enforced:

```bash
pnpm --dir demo/midgard-sdk run typecheck
pnpm --dir demo/midgard-fault-proofs run typecheck
pnpm --dir demo/midgard-node run typecheck
pnpm --dir demo/midgard-watcher run typecheck
pnpm --dir demo/da-committee-node run typecheck
pnpm --dir demo/midgard-node-tools run typecheck
```

CI builds the SDK before the packages that import it; do the same.

## 5. Node tests and the DA fixture

`demo/midgard-node` tests need the test Postgres on 127.0.0.1:5433 even for a
single file; without it Vitest reports "No test files found"
(`scripts/start-test-postgres.sh`, header). That script is a no-op when
something already listens on the port.

Regenerate the DA deployment fixture after the contract set changes, then
rerun without the variable to confirm it matches:

```bash
MIDGARD_WRITE_DA_DEPLOYMENT_FIXTURE=1 pnpm --dir demo/midgard-node exec vitest run tests/da-deployment-fixture-generation.test.ts
pnpm --dir demo/midgard-node exec vitest run tests/da-deployment-fixture-generation.test.ts tests/deployment-manifest.test.ts
```

The fixture lands in
`demo/da-committee-node/tests/fixtures/da-contract-deployment-info.json`.

## 6. Devnet journey counts (no CI job runs this)

```bash
pnpm --dir demo/midgard-node-tools exec vitest run --config vitest.watcher-journeys.config.ts devnet/watcher-journeys/catalogue.test.ts
```

Name the file. The same config includes every `devnet/watcher-journeys`
test, including the `*-live.test.ts` files, with a one-hour timeout.
As of 2026-09-25 the catalogue file ran 10 tests in about 17 seconds.

## 7. What CI runs

| CI step                                                                          | Covers                                         |
| -------------------------------------------------------------------------------- | ---------------------------------------------- |
| Aiken CI / Compile and run the Aiken test suite with the pinned fork             | every Aiken test                               |
| Aiken CI / Run normalized Aiken auto-formatter check                             | `aiken fmt`                                    |
| Midgard Node CI / Build testnet Aiken blueprint                                  | `plutus.json` for later steps                  |
| Midgard Node CI / Build and test Midgard core DA transport                       | core identity tables                           |
| Midgard Node CI / Build, typecheck, and test Midgard SDK                         | catalogue, token names, contract chain         |
| Midgard Node CI / Build, typecheck, and test fault-proof tooling                 | registry, classification, adapters, lifecycles |
| Midgard Node CI / Verify DA committee transport and admission                    | DA fixture helper                              |
| Midgard Node CI / Build, typecheck, lint, format-check, and test Midgard watcher | watcher deployment identity                    |
| Midgard Node CI / Typecheck Midgard node                                         | node category tables                           |
| Midgard Node CI / Test Midgard node                                              | role mirror, DA fixture drift                  |
| Midgard Node CI / Typecheck, lint, build, and test Midgard node tools            | journey owner entry (not the count test)       |

Report which of these you ran locally, with their pass counts, and which you
left to CI.
