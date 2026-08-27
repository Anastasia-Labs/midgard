# Testing Status

> Audited 2026-07-10 against branch `tx-validation` (HEAD `269bf6b3`) plus its
> contemporaneous working tree; reconstructed on clean base `55afdc54`. Source
> paths and CI claims below were reconciled to that clean base. What is tested,
> at which fidelity (pure unit → emulator → integration → real network), and what
> CI actually runs.
>
> Commands, CI wiring, and top-level gaps were revalidated 2026-07-22 against
> `tx-validation` HEAD `0aeaa700`; the full proof audit date remains 2026-07-10.
> The transition-trace matrix, CLI, and emulator representation were reconciled
> again on 2026-08-26 against the current tree.

## 1. Fidelity ladder for the fault-proof system

| Level                                       | Exists?                                   | Where                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          |
| ------------------------------------------- | ----------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| Aiken unit/property tests                   | 🟠 partial                                | The canonical V1 transition-trace suite now covers every `OmittedDueL1Event`, `OutOfWindowSourceEvent`, and `CountFault` sub-variant, including exact honest-side negatives; native-tx codec, counted roots, invalid-range normalization, zero-input step-02, computation-thread, catalogue, and state-queue controls remain as previously inventoried. The six `q49_l295_*` state-queue selectors do **not** test removal; `fault-proof.ak` still has no direct Aiken test ([`onchain-reference.md`](onchain-reference.md) §6).                                               |
| TypeScript unit tests                       | ✅ broad                                  | prepare-\* logic, MPF proofs, lease protocol, contract inspection, transition-trace preparation/detection/reconstruction/submission, retained-DA binding, blueprint decoding, and validation-dispute submission                                                                                                                                                                                                                                                                                                                                                                |
| Lucid Emulator end-to-end                   | ✅ Q13 plus existing family evidence      | the `submit-init-emulator*.test.ts` family includes Q13 `input-no-idx` plus transition-trace finals 0, 3, and 6 through permanent token and faulty-block removal. The final-6 audit covers omitted/out-of-window withdrawal evidence with ledger-sorted live reference-input indices and an honest-late rejection; the count-fault final-0 scenario uses an isolated test-only state-queue admission stub because production admission rejects the malformed header before adjudication. Q13's evidence is local only; Q14–Q20 and system-wide preprod acceptance remain open. |
| Cross-process / network integration         | 🟠 adjacent only                          | DA layer: `da-committee-node` in-process protocol tests and `multi-node-integration.test.ts`. The pre-consolidation exact-50k runner was invalidated because the complete newest V1 payload exceeds the retained DA bound. Nothing drives a fault proof across processes                                                                                                                                                                                                                                                                                                       |
| Preprod / real testnet                      | 🟠 reported, not independently reproduced | PR #461's author supplied a preprod zero-input transaction sequence through removal after the counted-root/native-MPF changes. This hardening review did not rerun it, and the repository still lacks a reproducible automated preprod acceptance artifact. The older operator-local 2026-05-08 canonical-root report predates counted roots and the MPF rewrite. System-wide readiness therefore remains unconfirmed; `public_testnet_readiness.md` still lists fault proofs "Partial, not public-testnet ready".                                                             |
| Autonomous end-to-end (detect→prove→remove) | ❌ complete drill                         | watcher ingestion, indexing, finality, rollback, and durable-state foundations exist, but no unattended detect→prove→remove acceptance run is recorded                                                                                                                                                                                                                                                                                                                                                                                                                         |

## 2. Test inventory — `demo/midgard-fault-proofs/tests/`

| File                                                                                                                                   | Fidelity                                 | Proves                                                                                                                                                         |
| -------------------------------------------------------------------------------------------------------------------------------------- | ---------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `submit-init-emulator*.test.ts` (including `-input-no-idx` and `-transition-trace-subvariants`)                                        | Emulator + real `plutus.json`            | Q13 input-no-idx plus transition-trace omitted/out-of-window/count lifecycles, permanent tokens, removal, and honest-late rejection                            |
| `spend-input-witness.test.ts`                                                                                                          | Emulator                                 | reference-witness publication under real UPLC cost accounting                                                                                                  |
| `transition-trace-challenger.test.ts`                                                                                                  | unit (synthetic fixtures, mocked libp2p) | reconstruction, all detection families, witness building, retained-DA fetch protocol                                                                           |
| `prepare-double-spend.test.ts` / `prepare-invalid-range.test.ts` / `prepare-non-existent-input.test.ts` / `prepare-zero-input.test.ts` | unit (mocked fetch/file)                 | violation detection + MPF proof generation per family; zero-input also pins and requires authoritative counted-root agreement                                  |
| `remove-fraudulent-block.test.ts`                                                                                                      | unit (mocked `fetch`)                    | lease-protocol correctness only — not removal tx logic                                                                                                         |
| `submit-init.test.ts`                                                                                                                  | unit (fake lucid)                        | signer precedence, per-category deployment-readiness gating                                                                                                    |
| `inspect-contracts.test.ts`                                                                                                            | unit + real blueprint                    | blueprint↔deployment consistency, catalogue root/membership                                                                                                   |
| `bin.test.ts`                                                                                                                          | unit                                     | CLI parsing/boundaries, including authenticated transition-trace preparation, repeatable reference inputs, zero-input category, and mandatory preparation root |
| `aiken-blueprint-data.test.ts`                                                                                                         | unit                                     | blueprint-data decoding and validation                                                                                                                         |
| `cardano-capability-retained-da-v1.test.ts`                                                                                            | unit + retained-DA fixtures              | Cardano capability boundary corpus and retained-DA binding                                                                                                     |
| `validation-dispute-submit.test.ts`                                                                                                    | unit                                     | canonical validation-dispute file decoding and transaction-validity submission                                                                                 |

## 3. Exact local verification commands

```bash
# On-chain (Aiken v1.1.22, pinned in onchain/aiken/aiken.toml:3)
cd onchain/aiken && aiken fmt --check && aiken check
aiken build --env testnet                      # blueprint used by TS tests/deploys

# Plutarch (legacy MPF; not CI-wired)
cd onchain/plutarch && cabal test helpers-tests

# Fault-proof package (unit + emulator e2e; CI-wired)
pnpm --dir demo/midgard-fault-proofs test      # vitest run

# Local validation / SDK
pnpm --dir demo/midgard-validation test
pnpm --dir demo/midgard-sdk test

# DA layer
pnpm --dir demo/midgard-core test
pnpm --dir demo/da-committee-node test

# Node
pnpm --dir demo/midgard-node test              # NODE_ENV=emulator
pnpm --dir demo/midgard-node run test:da-phase5-e2e

# Haskell offchain (mockchain; not CI-wired)
cd offchain && cabal test mockchain-tests
```

The current tree pins Aiken v1.1.22. CI hash-guards the two protected pre-Goal
tracked libraries, applies the pinned formatter to every other tracked Aiken
source, normalizes the formatter's trailing-space artifact, and rejects any
resulting source diff before running the contract checks and testnet build. This
checkpoint includes the canonical formatter pass across the touched Aiken tree.
Historical results from reconstructed base `55afdc54` are not final-tree evidence.

Live-stack acceptance: `.agents/skills/midgard-e2e-acceptance/SKILL.md` (local Kupmios
only; real DA attestation required — `attest-state-queue-once` forbidden as an acceptance
path, `:78-81,822-829`). Contract building: `.agents/skills/aiken-contract-build/SKILL.md`
(`aiken build --env testnet`; default env is not e2e-compatible, `:31-33`).

## 4. CI wiring

| Suite                                                                            | CI                                                                             |
| -------------------------------------------------------------------------------- | ------------------------------------------------------------------------------ |
| normalized Aiken formatting check + `aiken check`                                | ✅ `.github/workflows/aiken-ci.yml`                                            |
| `aiken build --env testnet`                                                      | ✅ `.github/workflows/midgard-node-ci.yml`                                     |
| midgard-core / da-committee-node / lucid-midgard / midgard-node / DA phase-5 e2e | ✅ `.github/workflows/midgard-node-ci.yml` (+ nightly benchmark workflow)      |
| `demo/midgard-fault-proofs` build/typecheck/tests                                | ✅ `.github/workflows/midgard-node-ci.yml`; package paths trigger the workflow |
| midgard-sdk / midgard-validation build/typecheck/tests                           | ✅ `.github/workflows/midgard-node-ci.yml`                                     |
| Plutarch / Haskell offchain                                                      | ❌ manual only                                                                 |
| tx-prep / preprod operator-lifecycle                                             | ❌ manual, env-gated                                                           |

## 5. Known coverage gaps (test debt, ordered)

1. **CI**: wire the legacy Plutarch helper suite into CI or retire the remaining
   dependency on it with explicit replacement evidence. The TypeScript fault-proof,
   validation, and SDK suites are already required by `midgard-node-ci.yml`.
2. **Aiken-level tests** for computation-thread Init/Success/cancel, fault-proof mint
   coupling, catalogue immutability, and both `RemoveFaultyBlockHeader` branches —
   including a regression for the cross-operator descendant case
   (`state-queue.ak:661`) and a duplicate-`Init` double-mint probe.
3. **Phase A/B reject codes never exercised**: `UnsupportedFieldNonEmpty`,
   `PlutusEvaluationUnavailable`, `CertificatesForbidden`, `NonZeroWithdrawal`
   (21/25 covered by `phase-a.test.ts`/`phase-b.test.ts`).
4. **Preprod re-run** of one full family (double-spend) with publishable evidence to
   confirm or supersede the operator-local canonical-root mismatch report.
5. **Removal integration** against a real node (`/stateQueueMutationLease` currently
   mocked-fetch only) and a post-removal event re-inclusion scenario.
6. **DA**: compression has startup and payload-envelope/sizing coverage, but no
   focused codec property suite; committee-store retention behavior remains untestable
   because committee-store pruning remains deliberately inert; Q54 retention enforcement
   is complete and its residual is routed to Q58/W-O7.
