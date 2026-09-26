# Build paths, environments and the blueprint

Read this when you need to know which command produced the blueprint a suite
is reading, what `--env` changes, or why the node refuses a blueprint you just
built. Verified against the tree on 2026-09-26.

## Two ways to build, and what each produces

| Command                                               | Environment                  | Compiler check                                                                      | Writes                                                                                                                                                                              |
| ----------------------------------------------------- | ---------------------------- | ----------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `pnpm --dir demo deployment:build <profile>`          | `--env <profile>`, `-` → `_` | `aiken` on `PATH` must print `AIKEN_FORK_VERSION` from `aiken-ci.yml`, or it throws | regenerates `onchain/aiken/env/*.ak` and `demo/midgard-core/src/generated-deployment-profiles.ts`, then `onchain/aiken/plutus.json` and `onchain/aiken/plutus.json.deployment.json` |
| `aiken build --env testnet [--trace-level … --out …]` | whatever you pass            | none                                                                                | `plutus.json` (or the `--out` path) only                                                                                                                                            |

Sources: `demo/package.json:7`, `demo/scripts/deployment-profiles.mjs:236-292`,
`config/deployments/README.md` ("Build").

`deployment:build` details that bite:

- It spawns the bare `aiken` from `PATH`. It does **not** honour
  `MIDGARD_AIKEN_BIN` (`deployment-profiles.mjs:265`), unlike
  `pinned-compiler.mjs`, `run-focused-check.mjs` and the pre-commit hook.
- It takes no `--out` and no trace flags. It always writes
  `onchain/aiken/plutus.json` in the checkout you run it from.
- It deletes the previous `plutus.json.deployment.json` before compiling, so
  a failed build leaves no stale build record (`deployment-profiles.mjs:271-273`).
- It rewrites `SELECTED_DEPLOYMENT_PROFILE` in the tracked
  `generated-deployment-profiles.ts` to the profile you name. The checked-in
  selection is `preprod-testing`, and `Aiken CI / Check deployment profiles`
  runs `deployment:check preprod-testing`, so building another profile leaves
  a tracked file dirty that must not be committed.

CI uses `deployment:build preprod-testing` in
`Midgard Node CI / Build testnet Aiken blueprint`
(`.github/workflows/midgard-node-ci.yml:187-188`). `Aiken CI` never builds a
blueprint; it runs `aiken check` with no `--env`
(`.github/workflows/aiken-ci.yml:190-192`).

## The build record the node checks

The node's real-blueprint loader reads `<blueprint>.deployment.json` next to
the blueprint, checks its profile and digest against
`SELECTED_DEPLOYMENT_PROFILE`, and refuses the blueprint unless the recorded
`blueprintHash` equals the SHA-256 of the file it loaded
(`demo/midgard-node/src/services/midgard-contracts.ts:190-209`, used by
`loadRealBlueprintSha256` and `loadRealBlueprint`). Consequences:

- A blueprint from a plain `aiken build` has no build record, so node code
  paths that load the real blueprint fail on it.
- Rebuilding with plain `aiken build` over a `deployment:build` output leaves
  the old record with a now-wrong hash; the node refuses it.
- A traced build is never loadable by the node, because the record comes only
  from `deployment:build`, which cannot trace.

Suites that read the blueprint path directly (for example the
`demo/midgard-validation` emulator tests via `MIDGARD_REAL_BLUEPRINT_PATH`,
`demo/midgard-validation/tests/validation-machine.test.ts:91`) do not check
the record, so a plain or traced `--env testnet` build is fine for them.

## The environment switch

`onchain/aiken/env/` holds one module per deployment profile plus two aliases,
all generated from `config/deployments/*.yaml` and
`config/deployments/env.ak.template` by `deployment-profiles.mjs:204-213`:

| Aiken env              | File               | Same bytes as            |
| ---------------------- | ------------------ | ------------------------ |
| default (no `--env`)   | `env/default.ak`   | `env/mainnet.ak`         |
| `testnet`              | `env/testnet.ak`   | `env/preprod-testing.ak` |
| `preprod_testing` etc. | `env/<profile>.ak` | —                        |

Checked with `diff` on 2026-09-26. The template part (script-hash pins, the
user-events witness prefix, fixed constants) is identical in every env; what
differs is the profile block: timing (block maturity, response window, shift
and registration durations, DA attestation and response windows), economics
(bond, slashing penalties, prover reward) and limits. For example
`block_maturity_duration_v1` is `300_000` under `testnet` and `604_800_000`
under the default env; `required_bond` is `900_000_000` versus
`100_000_000_000`.

So omitting `--env` compiles **mainnet** parameters. A plain `aiken check`,
and both `run-focused-check.mjs` and `guard-focused-selector.mjs` without
`MIDGARD_AIKEN_ENV` run under the default env. Set `MIDGARD_AIKEN_ENV=testnet`
when a test depends on testnet constants
(`onchain/aiken/scripts/run-focused-check.mjs:102-116`,
`onchain/aiken/scripts/guard-focused-selector.mjs`).

`Aiken CI / Check deployment profiles` fails if the env files drift from the
YAML profiles. `Aiken CI / Verify generated user events witness prefix`
(main-targeting pushes and PRs only) fails if the generated prefix in
`env/default.ak` or `env/testnet.ak` is stale.

## `--out`

`aiken build --out <path>` writes the blueprint to a path relative to
`onchain/aiken`, and leaves `plutus.json` untouched. Use it for a blueprint
you want kept apart from the checkout's (a traced diagnostic build, a
comparison build). The parent directory must already exist: with a missing
parent the fork (`aiken v1.1.23+5adf783`, probed 2026-09-26) exits 1, and
with output piped it prints no reason. `deployment:build` has no `--out`.

## `plutus.json` is never committed

It stopped being tracked in `238fc45fb`. It is not in `.gitignore` (only
`plutus.json.deployment.json` is, `.gitignore:35`), so it shows as untracked
after every build. The pre-commit hook refuses a staged
`onchain/aiken/plutus.json` (`.githooks/pre-commit:50-58`). Blind spots: the
hook runs only where `.githooks/install` has been run, `MIDGARD_SKIP_HOOKS=1`
bypasses it, and no CI step checks for a tracked blueprint.
