# Testing Status

> Audited 2026-07-10 against branch `tx-validation` (HEAD `269bf6b3`) plus its
> contemporaneous working tree; reconstructed on clean base `55afdc54`. What is tested, at which fidelity (pure unit → emulator → integration →
> real network), and what CI actually runs.

## 1. Fidelity ladder for the fault-proof system

| Level                                       | Exists?           | Where                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       |
| ------------------------------------------- | ----------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Aiken unit/property tests                   | 🟠 partial        | transition-trace proof families (23), native-tx codec, counted roots, invalid-range normalization — **nothing** for computation-thread, fault-proof token, catalogue, or state-queue removal at the Aiken level ([`onchain-reference.md`](onchain-reference.md) §6)                                                                                                                                                                                                                                                                         |
| TypeScript unit tests                       | ✅ broad          | prepare-\* logic, MPF proofs, lease protocol, contract inspection, transition-trace detection/reconstruction (all 11 files in `demo/midgard-fault-proofs/tests/`)                                                                                                                                                                                                                                                                                                                                                                           |
| Lucid Emulator end-to-end                   | ✅ for 4 families | `submit-init-emulator.test.ts` (4136 lines): full chains for double-spend, invalid-range, non-existent-input, transition-trace from `submitInit` through fault-proof-token mint to `submitRemoveFaultyBlock`, incl. tail + non-tail removal topologies and lease edge cases; `spend-input-witness.test.ts` (180-input witness)                                                                                                                                                                                                              |
| Cross-process / network integration         | 🟠 adjacent only  | DA layer: `da-committee-node` in-process protocol tests, `multi-node-integration.test.ts`, node-side `da-multi-process-50k-integration` (CI + nightly). Nothing drives a fault proof across processes                                                                                                                                                                                                                                                                                                                                       |
| Preprod / real testnet                      | ❌                | Documented blocker: `demo/midgard-node/docs/PREPROD_DOUBLE_SPEND_FAULT_PROOF_GAP_REPORT.md` (2026-05-08, gitignored) — systemic canonical-root/commitment mismatch across roots. **Freshness caveat**: predates counted roots (PR #458) and the MPF-based rewrite of proof builders; current `prepare-*`/`phas.ts` build genuine `@aiken-lang/merkle-patricia-forestry` proofs. No in-repo evidence of a preprod re-attempt either way. `public_testnet_readiness.md:29,43-49` still lists fault proofs "Partial, not public-testnet ready" |
| Autonomous end-to-end (detect→prove→remove) | ❌                | no watcher exists                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |

## 2. Test inventory — `demo/midgard-fault-proofs/tests/`

| File                                                                                                    | Fidelity                                 | Proves                                                                               |
| ------------------------------------------------------------------------------------------------------- | ---------------------------------------- | ------------------------------------------------------------------------------------ |
| `submit-init-emulator.test.ts`                                                                          | Emulator + real `plutus.json`            | the strongest e2e evidence in the repo (see above)                                   |
| `spend-input-witness.test.ts`                                                                           | Emulator                                 | reference-witness publication under real UPLC cost accounting                        |
| `transition-trace-challenger.test.ts`                                                                   | unit (synthetic fixtures, mocked libp2p) | reconstruction, all detection families, witness building, retained-DA fetch protocol |
| `prepare-double-spend.test.ts` / `prepare-invalid-range.test.ts` / `prepare-non-existent-input.test.ts` | unit (mocked fetch)                      | violation detection + MPF proof generation per family                                |
| `remove-fraudulent-block.test.ts`                                                                       | unit (mocked `fetch`)                    | lease-protocol correctness only — not removal tx logic                               |
| `submit-init.test.ts`                                                                                   | unit (fake lucid)                        | signer precedence, per-category deployment-readiness gating                          |
| `inspect-contracts.test.ts`                                                                             | unit + real blueprint                    | blueprint↔deployment consistency, catalogue root/membership                         |
| `bin.test.ts`                                                                                           | unit                                     | CLI parsing (incl. `--fault-category transitionTrace`)                               |
| `da-decoder-first-guard.test.ts`                                                                        | unit (source grep)                       | architectural seam: only `reconstruct.ts` may decode DA payloads directly            |

## 3. Exact local verification commands

```bash
# On-chain (Aiken v1.1.21, pinned in onchain/aiken/aiken.toml:3)
cd onchain/aiken && aiken fmt --check && aiken check
aiken build --env testnet                      # blueprint used by TS tests/deploys

# Plutarch (legacy MPF; not CI-wired)
cd onchain/plutarch && cabal test helpers-tests

# Fault-proof package (unit + emulator e2e; not CI-wired)
pnpm --dir demo/midgard-fault-proofs test      # vitest run

# Local validation / SDK
pnpm --dir demo/midgard-validation test
pnpm --dir demo/midgard-sdk test               # currently --passWithNoTests

# DA layer
pnpm --dir demo/midgard-core test
pnpm --dir demo/da-committee-node test

# Node
pnpm --dir demo/midgard-node test              # NODE_ENV=emulator
pnpm --dir demo/midgard-node run test:da-phase5-e2e

# Haskell offchain (mockchain; not CI-wired)
cd offchain && cabal test mockchain-tests
```

Live-stack acceptance: `.agents/skills/midgard-e2e-acceptance/SKILL.md` (local Kupmios
only; real DA attestation required — `attest-state-queue-once` forbidden as an acceptance
path, `:78-81,822-829`). Contract building: `.agents/skills/aiken-contract-build/SKILL.md`
(`aiken build --env testnet`; default env is not e2e-compatible, `:31-33`).

## 4. CI wiring (the headline problem)

| Suite                                                                            | CI                                                                                                                                                                     |
| -------------------------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `aiken fmt --check` + `aiken check`                                              | ✅ `.github/workflows/aiken-ci.yml:31,33`                                                                                                                              |
| `aiken build --env testnet`                                                      | ✅ `midgard-node-ci.yml:84`                                                                                                                                            |
| midgard-core / da-committee-node / lucid-midgard / midgard-node / DA phase-5 e2e | ✅ `midgard-node-ci.yml:88-100` (+ nightly benchmark workflow)                                                                                                         |
| **`demo/midgard-fault-proofs` tests**                                            | ❌ **never run in any workflow** (the package appears in no workflow's path-trigger list or job) — the emulator e2e proof of the whole dispute pipeline is manual-only |
| midgard-sdk / midgard-validation tests                                           | ❌ sdk built (`:96`) but not tested; validation suite not wired                                                                                                        |
| Plutarch / Haskell offchain                                                      | ❌ manual only                                                                                                                                                         |
| tx-prep / preprod operator-lifecycle                                             | ❌ manual, env-gated                                                                                                                                                   |

## 5. Known coverage gaps (test debt, ordered)

1. **CI**: wire `midgard-fault-proofs`, `midgard-validation`, and Plutarch suites into CI
   (they exist and pass locally; they guard the security-critical path).
2. **Aiken-level tests** for computation-thread Init/Success/cancel, fault-proof mint
   coupling, catalogue immutability, and both `RemoveFaultyBlockHeader` branches —
   including a regression for the cross-operator descendant case
   (`state-queue.ak:661`) and a duplicate-`Init` double-mint probe.
3. **Transition-trace sub-variant gaps**: `SourcePhaseMismatch` (0 tests), `CountFault`
   (1/5), `OmittedDueL1Event`/`OutOfWindowSourceEvent` (deposit-only).
4. **Phase A/B reject codes never exercised**: `UnsupportedFieldNonEmpty`,
   `PlutusEvaluationUnavailable`, `CertificatesForbidden`, `NonZeroWithdrawal`
   (20/22 covered by `phase-a.test.ts`/`phase-b.test.ts`).
5. **Preprod re-run** of one full family (double-spend) to confirm or retire the
   canonical-root gap report.
6. **Removal integration** against a real node (`/stateQueueMutationLease` currently
   mocked-fetch only) and a post-removal event re-inclusion scenario.
7. **DA**: no `da-compression.ts` unit test; committee-store retention behavior untestable
   (no retention code exists).
