# Fault-Proof Testing Status

Current test inventory reviewed against the working tree on 2026-09-01.

## Inventory

| Surface                                                      | Current inventory | Judgement                                                   |
| ------------------------------------------------------------ | ----------------: | ----------------------------------------------------------- |
| Aiken fault-proof test declarations under validator families |               768 | Broad; family and shared-boundary depth varies              |
| TypeScript test files in `demo/midgard-fault-proofs/tests`   |               169 | Broad unit, workflow, and emulator coverage                 |
| `submit-init-emulator*.test.ts` files                        |                82 | Broad Lucid Evolution coverage; two suites on fixture drift |
| Catalogue categories                                         |                32 | All compiled and registered                                 |
| Production workflow runner factories                         |                25 | Library runtime incomplete for seven categories             |
| Watcher-installed workflow categories                        |                25 | Autonomous application incomplete for seven categories      |

The generated testnet blueprint on the working tree (built 2026-09-01 with
`v1.1.23+5adf783`) contains 567 validators and has SHA-256
`597c38912123f7f2c167bb73b61c3b37be44cd274be506538ee9bd4437711c96`, and a
rebuild from the working tree with the pinned fork reproduces it
byte-for-byte. The inspection suite pins catalogue root
`85ecf82f70e409621d5324c54ae8e2deedbb7c37698e28ba7d76481c17bb6e90`, but that
suite currently fails on deployment-fixture drift (see below), so the pin is
not re-verified against this blueprint.

## Fidelity

| Level                              | Status                    | What it establishes                                                                                                                                           |
| ---------------------------------- | ------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Aiken unit/property                | Broad                     | Family predicates, exact successor binding, cancellation, maximum frontiers, shared machinery, removal, and protocol controls                                 |
| TypeScript unit                    | Broad                     | Codecs, evidence, retained-DA replay, production artifacts, journals, runner admission, funding, reconciliation, and classifiers                              |
| Lucid Evolution                    | Final families green      | Shared state-queue setup fits; missing-native-script-UTxO, native-script-invalid (29/33-signer staged frontiers), and both min-ADA polarities pass            |
| Real-node/cross-process            | Partial adjacent coverage | DA and correction components have focused integration, but no complete fault proof is driven across independent production processes                          |
| Preprod                            | Missing current artifact  | No reproducible proof-through-removal artifact is bound to the current 32-category identity                                                                   |
| Autonomous detect → prove → remove | Incomplete                | Watcher application installs 25/32 categories; no complete release acceptance artifact exists                                                                 |
| Van Rossem resource admission      | Enforced                  | Shared harness pins 16,384 bytes, 16.5M memory, and 10B CPU; state-queue publication, the min-ADA split, and every exercised final-family lifecycle obey them |

## Exact emulator status

Dedicated standalone lifecycle tests cover `missingNativeScriptUtxo`,
`nativeScriptInvalid`, and both `minAda` polarities, and all of them pass
under the shared Van Rossem limits. Fabricated deposit and withdrawal drive
removal, and value-not-preserved and mint-authorization drive
cancellation/resume.

The shared harness is pinned to Van Rossem's 16,384-byte transaction limit,
16,500,000 memory units, and 10,000,000,000 CPU steps. State-queue setup
uses five authenticated withdraw-zero rewarding scripts for commit,
unattested-timeout removal, unavailable-timeout removal, fraud removal, and
merge. The 5,222-byte applied minting policy publishes in 5,498 bytes; the
5,652–8,347-byte rewarding scripts publish in 6,161–8,842 bytes. A focused
admission test publishes all six under the shared limit.

The former 28,658-byte monolithic `fraudProofMinAdaStep02` script no longer
exists. Step 02 is a 3,319-byte authenticated dispatcher
(`onchain/aiken/validators/fraud-proofs/min-ada/step-02.ak`) whose
transaction and UTxO branches are separate withdraw-zero rewarding validators
in `onchain/aiken/validators/fraud-proofs/min-ada/step-02-yields.ak` (15,522
and 6,571 bytes in the generated blueprint), each with its own
reference-script role NFT in `demo/midgard-sdk/src/reference-scripts.ts`.
`submit-init-emulator-min-ada-standalone.test.ts` asserts that the signed
publication transaction for each of the three scripts fits within 16,384
bytes, then drives both polarities through cancel/resume, header removal, and
permanent-evidence retention.

The native-script-invalid direct lifecycle passes, and the 29-signer and
33-signer maximum frontiers pass through the deterministic staged route with
Van Rossem headroom; a forced 29-signer direct submission is rejected before
builder work. Missing-native-script-UTxO passes both its direct and its staged
step-05→06→07 predecessor-material paths, including cancel/resume and removal.

Emulator green is not deployability. The `validationTraceDispute`,
`transitionTrace`, and `withdrawalMistag` lifecycles publish 50 reference
scripts whose raw bodies exceed 16,384 bytes through the harness's
`oversized: true` publication path, which skips the L1 byte-margin assertion
(the validation-dispute suite additionally raises `maxTxSize` to 262,144 for
those publications). The mint-authorization, network-id, and
value-not-preserved helpers use the same flag for every step, so their
publication fit is unasserted even though their bodies are under the limit.
Production publication refuses oversized bodies, so the three affected
families cannot be deployed as compiled; see the size table in
[`catalogue-status.md`](catalogue-status.md).

Two suites in the package are currently red because their fixtures predate
the reference-script role-NFT change, not because of a validator or
transaction-fit problem: `inspect-contracts.test.ts` (9 of 12 tests fail on
the `referenceScriptAuthPolicy` deployment-info shape check) and
`submit-init-emulator-min-ada-v1.test.ts` (the validation-dispute journey to
the `E_MIN_ADA` conviction fails at stage setup with "Reference-script auth
policy must be a native script").

## Verification commands

```bash
# Aiken source and generated blueprint
cd onchain/aiken
aiken fmt --check
aiken check
aiken build --env testnet

# Fault-proof package
pnpm --dir demo/midgard-fault-proofs typecheck
pnpm --dir demo/midgard-fault-proofs test

# Shared codecs and validation
pnpm --dir demo/midgard-core test
pnpm --dir demo/midgard-sdk test
pnpm --dir demo/midgard-validation test

# Watcher, node, and DA
pnpm --dir demo/midgard-watcher test
pnpm --dir demo/midgard-node test
pnpm --dir demo/da-committee-node test
```

The Aiken project must use the repository-pinned fork and the `testnet`
environment. The legacy Plutarch/Haskell helper suites remain manual unless
their dependencies are retired with replacement evidence.

## CI

Primary Aiken formatting/check/build and the core, SDK, validation,
fault-proof, Lucid, node, watcher, and DA package checks are CI-wired. CI does
not currently establish:

- a clean public deployment;
- a full rollback/restart/concurrent-correction matrix;
- all 32 watcher installations;
- a maximum-shape lifecycle sweep for every category under the shared Van
  Rossem limits (the three final families are green; the sweep is not a gate);
- repair of the inspection-suite and min-ADA dispute-journey fixture drift so
  those suites are green again;
- preprod proof-through-removal acceptance.

## Last focused verification

On 2026-09-01, after rebuilding the `midgard-core`, `midgard-sdk`, and
`midgard-validation` dists (stale dists produce phantom failures in consumer
suites):

- the Van Rossem limit-pin regression passed (1/1);
- both min-ADA standalone polarities passed, including the three-script
  publication-size assertions, cancel/resume, header removal, and
  permanent-evidence retention (2/2);
- the native-script-invalid standalone lifecycle passed, including the
  29-signer and 33-signer staged frontiers and the forced-direct rejection
  (4/4);
- the missing-native-script-UTxO standalone lifecycle passed on both the
  direct and the staged predecessor-material paths (2/2);
- `inspect-contracts.test.ts` failed 9/12 on deployment-fixture drift
  (`referenceScriptAuthPolicy` shape), so the catalogue-root pin was not
  re-verified;
- `submit-init-emulator-min-ada-v1.test.ts` failed 1/1 at stage setup on the
  same reference-script auth-policy fixture drift.

These focused checks are not a substitute for the complete commands above.
