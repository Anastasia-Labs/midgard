# Midgard Fault-Proof System

Current-state documentation reviewed against the working tree on 2026-09-01.

## Current identity

- The canonical catalogue contains **32 positional categories**, IDs
  `00000000` through `0000001f`.
- `networkId`, `missingNativeScriptUtxo`, `nativeScriptInvalid`, and `minAda`
  occupy IDs `0000001c` through `0000001f`.
- The applied catalogue root is
  `85ecf82f70e409621d5324c54ae8e2deedbb7c37698e28ba7d76481c17bb6e90`.
- The checked-in testnet blueprint contains **563 validators** and has SHA-256
  `b885c3abb0eeaace296011a108fbe4a06d0e5303bfb9d73bbec48fc30f32f9de`.
- `transitionTrace` remains category `00000004`; its deployed graph is one
  route validator plus eight terminal validators.
- `mpf-chunked-proof` is shared verifier machinery, not a catalogue category.

The catalogue is immutable after genesis. Any change to positional category
order, applied first-step hashes, or catalogue root requires a fresh
development deployment. Canonical V1 has no migration path for an identity
that was never publicly deployed.

## What is implemented

All 32 planned catalogue families have Aiken validators in the generated
blueprint and catalogue/deployment wiring in the SDK, node, core, and watcher
identity surfaces. The final three formerly open standalone families are now
implemented:

- `missingNativeScriptUtxo`: seven-step proof over authenticated predecessor
  UTxO membership and native-script material;
- `nativeScriptInvalid`: five-step proof with bounded signer scanning and a
  resumable native-script evaluator;
- `minAda`: five-step transaction/UTxO proof using the shared canonical V1
  minimum-Ada formula.

The off-chain fault-proof package contains preparation, evidence, submit,
resume, cancellation, production-artifact, and production-workflow modules for
these families. It exposes 25 manifest-bound production runner factories. The
watcher application currently installs 25 of the 32 catalogue categories.

Dedicated init → steps → permanent token → faulty-block removal tests now exist
for `missingNativeScriptUtxo`, `nativeScriptInvalid`, and both `minAda`
polarities. The shared state-queue publication blocker is resolved by five
authenticated withdraw-zero rewarding scripts: all six applied scripts and
their signed publication transactions fit Van Rossem's 16,384-byte limit. The
direct native-script-invalid lifecycle passes. Missing-native-script-UTxO now
reaches proof submission but fails validation, while both min-ADA polarities
stop at the family-specific 28,658-byte `fraudProofMinAdaStep02` script.

## Readiness judgement

The fault-proof system is **implemented but not release-complete**. The
remaining launch blockers are:

1. make the shared genesis/setup path and all dedicated Lucid Evolution
   lifecycles pass under Van Rossem's transaction-size and ExUnit limits;
2. a watcher-installed production runner for every enabled catalogue family;
3. complete per-family soundness, cancellation/resume, removal, and maximum-
   shape emulator coverage under those same limits;
4. public retained-DA/proof-bundle retrieval sufficient for an independent
   challenger throughout the challenge window;
5. live balance-conservation and concurrent-claim acceptance for the compiled
   non-zero economics;
6. a reproducible watcher-driven detect → prove → remove run against a real
   local node and on preprod.

Do not describe Midgard as fault-proof ready for a public testnet until these
gates pass.

## Document map

| Document                                                   | Authority                                                                   |
| ---------------------------------------------------------- | --------------------------------------------------------------------------- |
| [`catalogue-status.md`](catalogue-status.md)               | Exact 32-category inventory and per-layer status                            |
| [`coverage-matrix.md`](coverage-matrix.md)                 | Ledger-rule coverage and remaining proof/release gaps                       |
| [`architecture.md`](architecture.md)                       | Catalogue, computation-thread, token, removal, DA, and watcher architecture |
| [`onchain-reference.md`](onchain-reference.md)             | Aiken module map and compiled identity                                      |
| [`offchain-reference.md`](offchain-reference.md)           | SDK, fault-proof package, workflow runtime, watcher, and node map           |
| [`testing-status.md`](testing-status.md)                   | Test fidelity, current gaps, and exact commands                             |
| [`execution-plan.md`](execution-plan.md)                   | Remaining work only                                                         |
| [`challenger-runbook.md`](challenger-runbook.md)           | Supported autonomous challenger procedure and fail-closed boundaries        |
| [`manual-recovery-runbook.md`](manual-recovery-runbook.md) | Recovery, rollback, and escalation procedure                                |

Family-specific `*-plan-v1.md` files have been reduced to implementation
references where their designs landed. Decision records under `decisions/`
remain normative records rather than status ledgers.

## Maintenance rule

Update the catalogue status, coverage matrix, testing status, execution plan,
and public-testnet readiness checklist in the same change whenever a category,
runner installation, emulator lifecycle, or release gate changes. Derive
counts, IDs, blueprint identity, and catalogue root from code and generated
artifacts; never carry them forward from older documentation.
