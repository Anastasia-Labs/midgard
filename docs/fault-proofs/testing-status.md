# Testing Status

> Reconciled 2026-08-29 against the current working tree. This document records
> what is tested, at which fidelity (unit → emulator → integration → real
> network), and what CI actually runs.
>
> The canonical catalogue has 29 positional categories
> (`00000000`–`0000001c`) with complete runtime deployment-entry mappings and
> mandatory authenticated reference scripts. `transitionTrace` remains
> `00000004` with one route plus eight terminal validators. Core/node manifest
> identity and watcher proof-thread authority require the complete topology.
> This identity movement is fresh-genesis/redeploy only, with no migration or
> compatibility path. These tests do not establish autonomous watcher
> detection/proving or a detect→prove→remove production drill.
>
> The appended two-step network-id family has passing focused SDK/evidence
> tests and a real-blueprint emulator lifecycle through permanent mint,
> faulty-header removal, honest-output refusal, and cancellation. The source
> catalogue, runtime deployment table, inspection, and node/core manifest
> schemas assign it `0000001c`. The inspection logic derives the new catalogue
> root, but its static expected-root assertion is stale.

> Q20: standalone min-fee has 11 focused Aiken
> controls, the full 190-case `validation_machine_v1` regression, four
> prepare/envelope tests, and a two-case real-blueprint emulator suite. The
> emulator covers both fee polarities, both cancellation states, same-NFT
> resume, malformed evidence, exact compiled-validator boundary refusal,
> permanent proof mint, and fraudulent-commitment removal. Production
> catalogue registration is now `minFee` (`00000013`); family-specific CLI and
> autonomous watcher mounting remain open. The
> 32-case canonical-evidence suite also admits min-fee only from security-grade
> DA/L1 evidence and rejects diagnostic/unauthenticated roots.

## 1. Fidelity ladder for the fault-proof system

| Level                                       | Exists?                                        | Where                                                                                                                                                                                                                                                                                                                                                |
| ------------------------------------------- | ---------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Aiken unit/property tests                   | 🟠 broad but incomplete                        | Transition-trace subvariants, native codec/bounds, registered family predicates, computation-thread/catalogue machinery, HeaderV1 commit/Q60, DA Q62/Q63, reward routing, fault-token/removal controls, and Q61 timeout correction. The duplicate-claim counterexample passes, proving idempotency remains open; live correction remains unverified. |
| TypeScript unit tests                       | ✅ broad                                       | The fault-proof package covers evidence preparation, MPF proofs, contract inspection, family schemas/provers, transition-trace and validation disputes, retained DA, blueprint decoding, removal leases, and workflow scaffolding. Test-file count is queryable inventory, not a coverage claim.                                                     |
| Lucid Emulator end-to-end                   | ✅ broad local lifecycle evidence              | the `submit-init-emulator*.test.ts` family includes the transition-trace final-0/3/6 subvariants and the completed standalone families through permanent token → faulty-block removal, alongside the registered family and validation-dispute evidence. System-wide preprod acceptance remains open.                                                 |
| Cross-process / network integration         | 🟠 adjacent only                               | DA layer: `da-committee-node` in-process protocol tests and `multi-node-integration.test.ts`. The pre-consolidation exact-50k runner was invalidated because the complete newest V1 payload exceeds the retained DA bound. Nothing drives a fault proof across processes                                                                             |
| Preprod / real testnet                      | 🟠 no current reproducible acceptance artifact | The repository does not contain a current automated preprod proof-through-removal artifact bound to the current blueprint, catalogue, deployment manifest, and reference-script identities. System-wide readiness therefore remains unconfirmed; `public_testnet_readiness.md` still lists fault proofs "Partial, not public-testnet ready".         |
| Autonomous end-to-end (detect→prove→remove) | ❌ complete drill                              | watcher ingestion, indexing, finality, rollback, and durable-state foundations exist, but no unattended detect→prove→remove acceptance run is recorded                                                                                                                                                                                               |

## 2. Focused fault-proof and state-correction test inventory

| File                                                                                                                                   | Fidelity                                 | Proves                                                                                                                                      |
| -------------------------------------------------------------------------------------------------------------------------------------- | ---------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------- |
| `submit-init-emulator*.test.ts` (including `-input-no-idx`)                                                                            | Emulator + real `plutus.json`            | Q13 input-no-idx lifecycle plus the existing e2e evidence described above                                                                   |
| `submit-init-emulator-min-fee-v1.test.ts` / `prepare-min-fee.test.ts`                                                                  | Emulator + exact-CBOR unit               | Q20 both fee polarities, reference-only two-step lifecycle, cancel/resume/negatives, mint/removal, and exact nine-field fee envelope        |
| `spend-input-witness.test.ts`                                                                                                          | Emulator                                 | reference-witness publication under real UPLC cost accounting                                                                               |
| `transition-trace-challenger.test.ts`                                                                                                  | unit (synthetic fixtures, mocked libp2p) | reconstruction, all detection families, witness building, retained-DA fetch protocol                                                        |
| `prepare-double-spend.test.ts` / `prepare-invalid-range.test.ts` / `prepare-non-existent-input.test.ts` / `prepare-zero-input.test.ts` | unit (mocked fetch/file)                 | violation detection + MPF proof generation per family; zero-input also pins and requires authoritative counted-root agreement               |
| `remove-fraudulent-block.test.ts`                                                                                                      | unit (mocked `fetch`)                    | lease-protocol correctness only — not removal tx logic                                                                                      |
| `submit-init.test.ts`                                                                                                                  | unit (fake lucid)                        | signer precedence, per-category deployment-readiness gating                                                                                 |
| `inspect-contracts.test.ts`                                                                                                            | unit + real blueprint                    | blueprint↔deployment consistency, catalogue root/membership                                                                                |
| `workflow-v1.test.ts`                                                                                                                  | unit + retained-DA fixtures              | deterministic classification, replay boundary, journals, preflight, resume/reconcile, and terminal verification; focused suite passes 28/28 |
| `workflow-tier3-actions-v1.test.ts`                                                                                                    | unit                                     | double-spend and network-id publication/healing/certification actions precede tier-3 proof steps; 2/2 pass                                  |
| `remove-unattested-block.test.ts`                                                                                                      | unit                                     | Q61 journal validation, rollback reopening, target rotation, topology replanning, confirmation fencing, and lease release; 10/10 pass       |
| `midgard-sdk/tests/state-queue-correction-transition-v1.test.ts`                                                                       | unit                                     | exact timeout mint-arm/topology transition derivation plus tamper and wrong-arm refusal; 3/3 pass                                           |
| `midgard-node/tests/state-queue-correction-reinclusion.test.ts`                                                                        | unit                                     | exact-header transaction/event reopening, idempotent already-reopened handling, and conflict refusal; 5/5 pass                              |
| `midgard-watcher/tests/attestation-timeout-observation-v1.test.ts`                                                                     | unit                                     | digest-bound waiting/near-timeout/timed-out/attested observation without mutation authority; 3/3 pass                                       |
| `midgard-node/tests/e2e-state-correction-acceptance.test.ts`                                                                           | unit                                     | strict aggregate acceptance schema and fail-closed economics/recovery/family completeness checks; 7/7 pass                                  |
| `midgard-node/tests/e2e-state-correction-local-authority.test.ts`                                                                      | unit                                     | fail-closed local Kupmios authority binding, including loopback/provider/manifest requirements; 5/5 pass                                    |
| `midgard-node/tests/e2e-state-correction-reconciliation.test.ts`                                                                       | unit + captured-source fixtures          | independent L1/workflow/final-state reconciliation and forged/disagreeing-source refusal; 6/6 pass                                          |
| `bin.test.ts`                                                                                                                          | unit                                     | CLI parsing, including zero-input category and mandatory preparation root                                                                   |
| `aiken-blueprint-data.test.ts`                                                                                                         | unit                                     | blueprint-data decoding and validation                                                                                                      |
| `cardano-capability-retained-da-v1.test.ts`                                                                                            | unit + retained-DA fixtures              | Cardano capability boundary corpus and retained-DA binding                                                                                  |
| `validation-dispute-submit.test.ts`                                                                                                    | unit                                     | canonical validation-dispute file decoding and transaction-validity submission                                                              |

Current-tree exceptions: `inspect-contracts.test.ts` is 11/12. It derives the
29-category root
`c686373893084eff5efe51a52821055f994caa4c26a363df37ec97df23380b62`
and passes the deployment/negative cases, but still asserts the older static
root. This is an active identity-pin failure.

The focused Q61 Aiken selection collects and passes twelve timeout/removal/race
controls under `state_queue_removal.test` with Aiken v1.1.23+5adf783.

## 3. Exact local verification commands

```bash
# On-chain (canonical patched fork: Aiken v1.1.23+5adf783)
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

The current tree declares Aiken v1.1.23 and CI pins the Anastasia Labs fork
at `5adf7837cbddb5d329fd51d9c0cd73f561eaf95c` (`aiken
v1.1.23+5adf783`). CI hash-guards the protected
tracked libraries, applies the pinned formatter to every other tracked Aiken
source, normalizes the formatter's trailing-space artifact, and rejects any
resulting source diff before running the contract checks and testnet build. This
checkpoint includes the canonical formatter pass across the touched Aiken tree.

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
2. **Aiken/integration closure**: current working-tree tests cover terminal
   mint coupling and both structural removal branches, including rotated
   operators. The duplicate-`Init` counterexample confirms the missing
   claim-lock/idempotency. Node-side transaction/event re-inclusion is covered
   locally; real-node concurrent correction is not.
3. **Phase A/B reject-code reachability**: reconcile codes that are declared
   but intentionally unrepresentable or unreachable; keep executable evidence
   for every structural N/A.
4. **Phase A/B reject codes never exercised**: `UnsupportedFieldNonEmpty`,
   `PlutusEvaluationUnavailable`, `CertificatesForbidden`, `NonZeroWithdrawal`
   (21/25 covered by `phase-a.test.ts`/`phase-b.test.ts`).
5. **Preprod run** of one full family with publishable evidence bound to the
   current blueprint, catalogue root, deployment manifest, and reference scripts.
6. **Removal integration** against a real node (`/stateQueueMutationLease`
   remains mocked-fetch only), including concurrent correction and live-chain
   confirmation of the locally tested post-removal re-inclusion path.
7. **DA**: compression has startup and payload-envelope/sizing coverage, but no
   focused codec property suite; committee-store retention behavior remains untestable
   because committee-store pruning remains deliberately inert; Q54 retention enforcement
   is complete and its residual is routed to Q58/W-O7.
