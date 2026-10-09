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
(`demo/midgard-fault-proofs/tests/support/emulator/blueprints.ts:29`).
`contrib test` rebuilds it after an Aiken change (`deployment:build`, as CI
does in "Build testnet Aiken blueprint"), or copies a matching one from
another checkout; the suites' global setup refuses a stale one. Never commit
it.

## 4. Focused package tests

Run the files your change reaches (`--related <changed path>...` finds them),
not whole suites. The fault-proofs suite
took 20.5 minutes at eight forks on 2026-09-21 (its `vitest.config.ts`).

```bash
node scripts/contrib.mjs test --package midgard-sdk --file tests/fraud-proof-catalogue-registration.test.ts --file tests/reference-scripts.test.ts
node scripts/contrib.mjs test --package midgard-core --file tests/deployment-manifest-identity.test.ts
node scripts/contrib.mjs test --package midgard-fault-proofs --file tests/workflow.test.ts --file tests/family-application-registry.test.ts --file tests/typed-reason-disposition.test.ts
node scripts/contrib.mjs test --package midgard-fault-proofs --file tests/<kebab>-lifecycle.test.ts
node scripts/contrib.mjs test --package midgard-watcher --file tests/runtime/deployment-identity.test.ts
```

Each run builds the stale dists its package needs (the SDK among them) first.

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

CI builds the SDK before the packages that import it; do the same
(`node scripts/contrib.mjs build --package midgard-sdk`).

## 5. Node tests and the DA fixture

`demo/midgard-node` tests need the test Postgres on 127.0.0.1:5433 even for a
single file; without it Vitest reports "No test files found"
(`scripts/start-test-postgres.sh`, header). That script is a no-op when
something already listens on the port.

Regenerate the DA deployment fixture after the contract set changes, then
rerun without the variable to confirm it matches:

```bash
node scripts/contrib.mjs artifacts sync --channel da-deployment-fixture
node scripts/contrib.mjs test --package midgard-node --file tests/da-deployment-fixture-generation.test.ts --file tests/deployment-manifest.test.ts
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

| CI step                                                               | Covers                                         |
| --------------------------------------------------------------------- | ---------------------------------------------- |
| Aiken CI / Compile and run the Aiken test suite with the pinned fork  | every Aiken test                               |
| Aiken CI / Run normalized Aiken auto-formatter check                  | `aiken fmt`                                    |
| Midgard Node CI / Build testnet Aiken blueprint                       | `plutus.json` for later steps                  |
| Midgard Node CI / Build and test Midgard core DA transport            | core identity tables                           |
| Midgard Node CI / Build, typecheck, and test Midgard SDK              | catalogue, token names, contract chain         |
| Midgard Node CI / Typecheck fault-proof tooling                       | registry category coverage                     |
| Midgard Node CI / Test fault-proof tooling                            | registry, classification, adapters, lifecycles |
| Midgard Node CI / Verify DA committee transport and admission         | DA fixture helper                              |
| Midgard Node CI / Test Midgard watcher                                | watcher deployment identity                    |
| Midgard Node CI / Typecheck Midgard node                              | node category tables                           |
| Midgard Node CI / Test Midgard node                                   | role mirror, DA fixture drift                  |
| Midgard Node CI / Typecheck, lint, build, and test Midgard node tools | journey owner entry (not the count test)       |

Report which of these you ran locally, with their pass counts, and which you
left to CI.
