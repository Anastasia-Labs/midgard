# `receivePurposeLanguage` V1 size plan

Cites [the shared primer](00-primer.md). Category `00000034` proves
`ReceivePurposePlutusV3Forbidden { execution_index }` in both directions: an
accepted transaction whose receive purpose selected PlutusV3 (wrongful
acceptance), and a forced transaction rejected under that reason whose receive
purpose actually selected a native or MidgardV1 source (wrongful rejection).
The machine-readable fit evidence is
[`receive-purpose-language-v1-fit-ledger.json`](receive-purpose-language-v1-fit-ledger.json)
(shared `midgard-van-rossem-fit-ledger-v1` schema), written by
`demo/midgard-fault-proofs/tests/receive-purpose-language-lifecycle.test.ts`
under `MIDGARD_WRITE_FIT_LEDGER=1` and pinned by
`receive-purpose-language-fit-ledger.test.ts`.

## 1. Identity

| Physical validator                                         | Applied parameters (in order)                                                                    | Raw body (bytes) | Signed publication (bytes) | Reserve margin |
| ---------------------------------------------------------- | ------------------------------------------------------------------------------------------------ | ---------------: | -------------------------: | -------------: |
| `fraud_proofs/receive_purpose_language/step_01.main.spend` | `step_02_validator_script_hash`, `computation_thread_token_policy_id`, `hub_oracle`              |           14,592 |                     14,975 |            897 |
| `fraud_proofs/receive_purpose_language/step_02.main.spend` | `step_03_validator_script_hash`, `computation_thread_token_policy_id`                            |           15,388 |                     15,736 |            136 |
| `fraud_proofs/receive_purpose_language/step_03.main.spend` | `computation_thread_token_policy_id`, `fraud_proof_token_policy_id`, `fraud_proof_token_address` |            1,860 |                      2,253 |         13,619 |

Deployment entries: `fraudProofReceivePurposeLanguage` (first-step hash role,
the catalogue leaf for `00000034`), `fraudProofReceivePurposeLanguageStep02`,
`fraudProofReceivePurposeLanguageStep03`. The chain is linear; no rewarding or
minting validator is family-specific, and the family carries no role NFT of its
own. Blueprint: testnet build
`1a30174645c166bd33f4060f22debfbac175a8fda053e307ab7a893520b413e9` with the
pinned fork `aiken v1.1.23+5adf783` (807 validators).

Canonical state progression: `BoundExecutionV1 → AuthenticatedReceiveLanguageV1
→ terminal`, with the wire encodings pinned by the cross-language golden
vectors `receive_*_golden_vector` (Aiken) and "pins the cross-language golden
vectors" (`receive-purpose-language.test.ts`).

| Step | State transition                                         | Imported semantic engine                                                                                     | Maximum dynamic evidence                                                                            |
| ---- | -------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------ | --------------------------------------------------------------------------------------------------- |
| 01   | initial thread → bound execution coordinate              | proof-thread substrate; shared accepted native-tx carriage or forced-leaf membership + source verification   | one transaction inclusion proof, or one forced-leaf membership proof plus the leaf's compact source |
| 02   | bound coordinate → authenticated purpose/language        | validation-trace descriptor membership, machine-state/work-root binding, three validation-merkle memberships | one descriptor membership, one trace proof, the native-scripts control, three sibling paths         |
| 03   | authenticated purpose/language → permanent mint and burn | `terminal_contradiction_v1`                                                                                  | none                                                                                                |

Step 02 binds purpose kind `3` (receive), the selected source descriptor, its
language tag, and the execution leaf at the accused index; the decisive rule is
exactly `purpose_kind == 3 && language_tag == 3`, and the forced direction
additionally binds the descriptor's `Rejected` verdict to the hash of the
reason's rejection code. The purpose and execution frontiers are indexed by
execution position (`execution_count == purpose_count`); `purpose_index` is the
kind-local coordinate hashed into the purpose leaf.

## 2. Why it is this size

Step 02 carries the whole authentication seam of the family: MPF membership of
the descriptor under the counted validation-traces root, descriptor
well-formedness and trace-proof folding, machine-state hashing,
`encode_native_scripts_control_v1` (26 fields, eight compact frontiers) for the
work-root check, the three leaf hashers of `script_proof_v1`, and three
validation-merkle membership folds. Step 01 is the shared accepted carriage
(`pass_native_tx_to_next_step_carried`) plus the forced-leaf binder and
`verify_native_tx_proof_source_v1`; step 03 is the substrate terminal rule only.
No transaction-wide decoder and no unrelated rejection predicate is reachable
from any of the three applied scripts.

## 3. Options considered

- **Prune**: not needed for fit; the step-02 reserve margin (136 bytes) is
  positive but thin, so the first pruning candidate on regeneration pressure
  is a receive-specific control re-encoder that omits the frontiers step 02
  never reads (resolved items, redeemers, outputs, output descriptors, mints)
  and folds them from committed peak hashes instead.
- **Yield split**: rejected; the three checks of step 02 are one inseparable
  authentication and a withdraw-zero yield would add a role NFT, a second
  parse of the largest witness, and a deployment role for no byte gain.
- **Chaining**: rejected; the evidence is not resumable (no scan) and every
  transaction fits with an order of magnitude of ExUnit headroom.
- **Redesign**: not applicable.

## 4. Chosen design

The three-step linear thread as deployed. Security argument: step 01 binds the
exact subject (accepted: the header-committed transaction id through the
shared carriage; forced: the counted forced-transactions root, the leaf's
`ForcedTxInvalid { ReceivePurposePlutusV3Forbidden { execution_index } }`
verdict, the leaf's committed source bytes and the thread's header hash, with
the direction fixed to wrongful rejection) and the exact execution coordinate
(the reason's index must equal the bound index). Step 02 refuses every
substitution at its seams: a foreign validation-traces root, a substituted or
verdict-flipped descriptor, a foreign event key, a substituted machine state,
trace proof or control, a substituted purpose item, a substituted language
(the source and execution leaves change), a substituted execution path, an
execution index at or past `execution_count`, and, in the forced direction, a
descriptor whose rejection-code hash is not the reason's. Step 03 mints only
when the substrate contradiction holds for the bound direction. Omitting any
step is impossible: each successor is an applied script hash and the thread
token travels through all three.

## 5. Size and budget projection

Measured (signed, shared Van Rossem parameters, local UPLC evaluation) at the
maximum real shape of Section 7:

| Transaction         | Accepted PlutusV3 (bytes / mem / CPU) | Forced native (bytes / mem / CPU) |
| ------------------- | ------------------------------------- | --------------------------------- |
| init                | 1,365 / 705,617 / 241,672,461         | 1,365 / 711,753 / 243,520,955     |
| step 01             | 2,448 / 1,459,339 / 492,352,464       | 1,761 / 1,091,688 / 447,903,172   |
| step 02             | 3,621 / 1,997,235 / 800,005,489       | 3,492 / 1,963,235 / 795,002,633   |
| step 03 (mint+burn) | 916 / 267,321 / 97,355,498            | 916 / 293,732 / 106,308,421       |
| removal             | 2,060 / 3,014,651 / 1,027,123,206     | 2,060 / 3,016,941 / 1,031,461,762 |
| cancel (01/02/03)   | 611 / ≤ 124,808 / ≤ 42,452,566        | —                                 |

Minimum margins across the ledger: publication reserve 136 bytes (step 02),
lifecycle bytes 12,763 (accepted step 02), memory 13,483,059 and CPU
8,968,538,238 (both at removal). Every lifecycle row also clears the 20%
proof-fit reserve. Each transaction references exactly one family step plus the
shared witness scripts, so the referenced-bytes fee band never leaves the
first 25 KiB tier for the family's own script.

Consensus-envelope ExUnit rows, from the focused Aiken selectors:

| Selector                                                            | Shape                                                                                   |     Memory |           CPU |
| ------------------------------------------------------------------- | --------------------------------------------------------------------------------------- | ---------: | ------------: |
| `receive_authenticates_exact_purpose_source_language_and_execution` | one leaf per frontier                                                                   |  1,533,000 |   744,610,000 |
| `receive_authenticates_consensus_bounded_frontiers`                 | every control frontier at 4,095 leaves (12 peaks, 11 siblings), trace proof 32 siblings |  6,260,000 | 2,930,000,000 |
| `receive_authenticates_maximum_depth_memberships`                   | every frontier at `maximum_leaf_count` (32 peaks, 31 siblings), trace proof 32 siblings | 13,570,000 | 6,480,000,000 |

The 4,095-leaf row is the supported ceiling: no Midgard transaction field may
exceed 32,768 preimage bytes, so no purpose, source, execution, input, output,
redeemer or mint frontier can reach 4,096 leaves. The `maximum_leaf_count` row
is the validator's absolute cost ceiling and is unreachable by an admissible
transaction. Byte projection for step 02 at the 4,095-leaf envelope, from the
measured 3,621-byte transaction: three extra siblings per path (+306), twelve
peaks on all eight control frontiers instead of the six single peaks measured
(+3,420), a 32-sibling trace proof (+680) and a deep descriptor membership
(+520) give about 8,550 signed bytes, leaving roughly 7,800 bytes of margin.

## 6. Off-chain work

Present: SDK chain
`demo/midgard-sdk/src/fraud-proof/contracts/families/receive-purpose-language.ts`;
catalogue registration and node manifest resolution; family module
`demo/midgard-fault-proofs/src/receive-purpose-language/` (finding
classification, evidence preparation from retained DA and authenticated L1
state, accepted/forced step-01 builders, step-02/03 builders, cancel, journal
workflow with restart reconciliation, manifest-bound production workflow,
actuator, replay); central runner factory and watcher installation. Added in
this pass: `stateQueuePolicyId` on `ReceivePurposeLanguageContracts` (the
accepted step-01 path authenticates the state-queue block with it; the manifest
workflow now resolves it from the deployment binding), and the actuator's
removal resolving through the canonical catalogue as a registered family. No
role NFT, funding row or submit route is family-specific.

## 7. Emulator scenario tests

`tests/receive-purpose-language-lifecycle.test.ts` runs, from generic
computation-thread `Init` through the applied reference scripts:

1. wrongful acceptance (PlutusV3 receive, 256 purposes, 16 validation traces)
   through the production actuator to the permanent mint and the state-queue
   removal;
2. wrongful forced rejection (native receive, same shape) to mint and removal;
3. honest accepted native receive refused on chain at step 03;
4. honest forced PlutusV3 rejection refused on chain at step 03;
5. coordinate mutation on both sides (bound index ≠ authenticated index;
   forced reason coordinate ≠ redeemer index);
6. every authentication seam substituted once and refused by the validator
   (forced leaf reason/header/root/direction; validation-traces root,
   descriptor, event key, machine state, trace proof, control, purpose item,
   language, execution path);
7. cancel from step 01, 02 and 03;
8. no checkpoint (non-resumable family);
9. permanent proof-token mint followed by removal in both directions;
10. the maximum shape above: the accused receive purpose plus 255 native
    spend purposes (distinct spent out-refs under one trivial script, because
    the machine's script-discovery bitmap caps distinct witnesses at 64), with
    every widened field checked against `MIDGARD_CONSENSUS_LIMITS`;
11. the adjacent index (`execution_index == execution_count`) refused on chain.

`tests/receive-purpose-language-publication-fit.test.ts` publishes the three
applied scripts under the 15,872-byte reserve;
`tests/receive-purpose-language-fit-ledger.test.ts` pins the ledger.

## 8. Aiken tests

`lib/midgard/fraud-proofs/receive-purpose-language/rule.test.ak` (32 checks):
both successful directions for languages 0, 3 and 128; honest refusals;
reason-coordinate and reason-constructor substitution; bind guards (root
width, zero count, negative index); forced-direction authentication with the
bound rejection code; accepted/forced verdict polarity refusals; rejection-code
substitution; source, purpose, language and unknown-tag substitution;
validation-root and descriptor substitution; execution index at the count in
both directions; the accused leaf among eight; the consensus-bounded and
merkle-envelope measurements; three encoding golden vectors.

## 9. Verification commands

```sh
cd onchain/aiken && $MIDGARD_AIKEN_BIN check -m "midgard/fraud_proofs/receive_purpose_language/rule.{..}"   # 32 checks, 0 errors
cd onchain/aiken && $MIDGARD_AIKEN_BIN build --env testnet                                                   # sha256 1a301746…
cd demo/midgard-fault-proofs && ./node_modules/.bin/vitest run \
  tests/receive-purpose-language.test.ts tests/receive-purpose-language-retained-da.test.ts \
  tests/receive-purpose-language-lifecycle.test.ts tests/receive-purpose-language-publication-fit.test.ts \
  tests/receive-purpose-language-fit-ledger.test.ts
```

## 10. Ordering and dependencies

The family imports the frozen Wave 0 substrate, the shared native-tx carriage
of step 01, `validation_trace_v1`/`validation_merkle_v1`/`script_proof_v1`
and the ID31 retained-DA reconstruction seam
(`buildExecutionSourceMachineAuthenticationFromRetainedDa`). No applied
parameter of another family changes; the catalogue leaf is already registered.

## 11. Risks

- Step 02's 136-byte publication reserve is the thinnest in the family; a
  library-level growth of the validation-trace or merkle modules on
  regeneration would need the Section 3 prune.
- The machine caps distinct script witnesses at 64 through its discovery
  bitmap; the lifecycle widens purposes with spend purposes instead, and the
  Aiken consensus-bounded row covers wider frontiers than any such fixture.
