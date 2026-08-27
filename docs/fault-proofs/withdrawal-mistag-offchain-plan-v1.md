# Withdrawal mistag: standalone single-party proof plan (v1)

Plan date: 2026-08-26. Scope: GOAL_SPEC Q41 / catalogue decision D-S8.
This plan is the implementation contract and as-built record for the on-chain
and off-chain work. The family is registered as `withdrawalMistag` at
`00000014`; generic Init, catalogue/inspection, node/core deployment identity,
watcher proof-thread topology, and all five mandatory authenticated reference
scripts are wired. Family-specific CLI, autonomous watcher detector/prover
mounting, preprod, and live evidence remain open. The identity change requires
fresh genesis/redeployment; there is no migration or compatibility path.

## 1. Decision: standalone, not transition-trace

`withdrawal-mistag` is a standalone, single-party computation-thread family.
It does not extend the transition-trace route.

The deciding distinction is between **transition fidelity** and **source-tag
truth**:

- `transition-trace/proof.ak`'s `validate_valid_withdrawal_transition` first
  requires the committed source leaf to say `WithdrawalIsValid`, then proves
  that deleting the named UTxO would disagree with the committed post-root.
- Its `validate_invalid_withdrawal_no_op_transition` first requires the same
  committed source leaf to carry any non-valid tag, then proves that a changed
  post-root disagrees with the required no-op.
- Neither arm authenticates the L2 output bytes, compares owner/value, verifies
  the withdrawal signature, counts tokens, or checks whether the exact L1
  payout output meets the target-network output rules. A perfectly
  self-consistent delete for a false `WithdrawalIsValid` tag, or a perfectly
  self-consistent no-op for a false invalid tag, is therefore not a trace
  fault. Adding another route over the same trace cannot make the committed
  tag non-circular.

The missing fact is deterministic over public authenticated evidence: the
committed withdrawal leaf, its event-to-step/transition-step opening, and the
ledger output (or its non-membership) at the step's pre-state. No opposing
party, competing execution trace, or timeout is intrinsically required.
Under `docs/fault-proofs/architecture.md` section 2, that is a standalone
single-party proof even when it needs several Cardano transactions.

This differs from Q43's accepted-transaction no-op ruling: Q43 recomputes the
same state transition and is therefore already total in transition-trace.
Q41 recomputes the predicate that selects _which_ withdrawal transition is
allowed. It also mirrors the native-script-decoding wrongful-rejection design:
the committed verdict is contradicted from committed bytes by a deterministic
single-party thread, while the permanent fraud token and removal path remain
the common terminal.

## 2. Fault statement and polarity

Let `claimed_valid` mean `withdrawal.info.validity == WithdrawalIsValid` and
let `actual_valid` be the deterministic predicate in section 4.

The family establishes exactly:

```text
claimed_valid != actual_valid
```

This yields both required directions:

1. **valid marked invalid**: the leaf carries any of the seven invalid
   constructors, but the authenticated UTxO exists and every owner, value,
   signature, token-count, and exact-payability condition succeeds;
2. **invalid marked valid**: the leaf carries `WithdrawalIsValid`, but at least
   one of those authenticated conditions fails. `UnpayableWithdrawalValue` is
   covered by the exact-payability condition, not by a wallet estimate.

The family intentionally adjudicates the payout/refund polarity, not priority
between two invalid reason labels. All seven invalid constructors select the
same refund/no-op semantics. A correctly invalid-tagged order is refused even
if another invalid predicate is also true because both `claimed_valid` and
`actual_valid` are false. This is the sound, consensus-relevant reading of
"both directions" in D-S8; proving an informational precedence between two
refund labels would require a separate normative priority rule that the
technical specification does not define.

## 3. Catalogue and deployment boundary

This standalone family is canonically registered as `withdrawalMistag` at
**`00000014`**. The SDK catalogue, generic Init, deployment
manifests/inspection, and watcher proof-thread topology bind the id to the
applied step-01 hash.

The five validators are applied backwards and referenced by the submitters:

| step | parameters                                                         |
| ---- | ------------------------------------------------------------------ |
| 01   | `step_02_hash`, computation-thread policy, hub-oracle policy       |
| 02   | `step_03_hash`, computation-thread policy                          |
| 03   | `step_04_hash`, computation-thread policy                          |
| 04   | `step_05_hash`, computation-thread policy                          |
| 05   | computation-thread policy, fraud-proof policy, fraud-proof address |

Reference-script publication is mandatory for all five production steps,
independent of size. Each submitter authenticates the published script hash;
inline attachment is not a production fallback.

## 4. Exact validity predicate

The proof evaluates the withdrawal at its own authenticated transition-step
pre-root. This preserves ordering: a UTxO deleted by an earlier valid
withdrawal is absent for a later withdrawal in the same block.

`actual_valid` is the conjunction below:

1. the `l2_outref` is a member of that pre-root;
2. the membership value is the canonical
   `LedgerOutputCommitmentV1` rebuilt from the supplied canonical output bytes
   at the outref's output index;
3. the output payment credential is a verification-key credential whose hash
   equals `body.l2_owner`;
4. the output value equals `body.l2_value` exactly, including lovelace, policy
   ids, asset names, quantities, order, and multiplicity;
5. `blake2b_224(verification_key) == body.l2_owner` and the Ed25519 signature
   verifies over
   `blake2b_256("MidgardWithdrawalV1" || cbor.serialise(body))`;
6. the value contains no more than 100 distinct non-Ada assets and all value
   entries are canonical positive quantities;
7. the exact Cardano Conway output `{ address: body.l1_address, value:
body.l2_value, datum: body.l1_datum, reference_script: None }` is
   representable, its Cardano value encoding is at most the canonical 5,000
   byte token-bundle limit, and its lovelace is at least
   `env.coins_per_utxo_byte * (160 + exact_serialised_output_bytes)`.

The output-length function is an on-chain closed-form encoder-length twin of
the same Conway map CML builds off-chain. It includes the address byte-string,
the exact Cardano value encoding, the datum-option wrapper (none, hash, or
inline Plutus-data tag-24 bytes), and map/key overhead. The parameter is the
compiled `env.coins_per_utxo_byte` pin (currently 4,310), never a redeemer or
wallet-provided constant. Cross-language absolute vectors cover exact minimum,
one lovelace below, each datum form, 29/57-byte addresses, and a mutated rate.

An absent trie key makes `actual_valid = false`. The proof does not pretend a
current non-membership witness can distinguish never-created from previously
spent: both are payout-invalid and both contradict a claimed-valid leaf. A
`SpentWithdrawalUtxo { l2_tx_id }` honestly carried as an invalid tag is still
refused because the family compares the consensus effect polarity, not an
unauthenticated history story.

## 5. Five-step evidence and wire contract

Every step supports `ct.Cancel`; Continue moves exactly one thread NFT and no
other family state.

### Step 01: source commitment

Inputs: thread token, authentic hub and state-queue node reference inputs,
counted `withdrawals_root` membership proof for `(WithdrawalId,
WithdrawalInfo)`.

Checks: test category id/header-hash binding; state-queue authenticity;
`WithdrawalsRootDomain`, `header.withdrawal_count`, key bytes and value bytes.

Carries: challenged header hash; withdrawal id; hash of the exact info; claimed
polarity; header event/trace roots and counts. The full leaf is reopened in the
next redeemer and rechecked against the hash, keeping the state fixed-size.

### Step 02: transition coordinate and pre-root

Inputs: exact `WithdrawalInfo` opening, counted `event_to_step_root` membership for
`WithdrawalEventKey(withdrawal_id)` and counted `transition_trace_root`
membership for the returned step index.

Checks: both domain tags and counts; key/value equality; phase is Withdrawal;
step event key is the same withdrawal; event-to-step phase/index agree.

Carries: the info hash, claimed polarity, and exact `pre_utxos_root`.

### Step 03: ledger state and non-payability-independent semantics

Inputs: the exact `WithdrawalInfo` opening (rechecked against the step-01 hash)
and one of:

- `Present { output_cbor, proof }`: rebuild the descriptor, verify MPF
  membership at `cbor(l2_outref)`, decode the canonical output, then compute
  owner equality, value equality, signature validity, canonical quantities,
  and asset count;
- `Absent { proof }`: verify MPF non-membership at the same key.

Carries only authenticated summaries and a hash of the withdrawal body. No
root, owner, value, output bytes, signature, or body supplied by the prover
survives without being bound here.

### Step 04: exact payout feasibility

Reopens the withdrawal body against the step-03 hash, recomputes the exact
target output length from it and the authenticated Cardano value size, and
applies the 100-asset and 5,000-byte constraints, then
applies the compiled min-Ada rule. It produces
`actual_valid` and requires `claimed_valid != actual_valid` before handing the
established fault to step 05.

### Step 05: permanent conclusion

Rechecks category/header identity and the established polarity mismatch, calls
`common.finalize`, burns the thread NFT, and mints exactly one permanent
fraud-proof token at the fraud-proof address. The token is never burned by the
removal workflow.

## 6. Off-chain API

SDK (`demo/midgard-sdk/src/fraud-proof/withdrawal-mistag-v1.ts`) owns the five
datum/redeemer schemas, the canonical `00000014` id, direction and evidence
types, exact output-length/min-Ada twins, and category/header asset-name
helpers. It has byte/vector twins against Aiken.

Fault-proof package owns:

- `prepare-withdrawal-mistag.ts`: admit and verify supplied retained source,
  event-to-step, trace and ledger openings; independently recompute the
  predicate; refuse preparation if there is no polarity mismatch;
- `submit-withdrawal-mistag-init.ts`: generic Init using the registered category
  membership proof;
- `submit-withdrawal-mistag-step-01.ts` through `step-05.ts`;
- `submit-withdrawal-mistag-cancel.ts`: prover-signed cancellation at any
  live step;
- `withdrawal-mistag/index.ts` and package barrels.

Submitters re-query and decode the live thread UTxO, derive the next action
from its address/datum, use validator reference inputs, run local UPLC
evaluation, and return the next outref. Resume takes only prepared evidence,
the category id/header hash, and chain state; local cursor files are advisory.

## 7. Lifecycle, recovery, and removal

Positive emulator journeys are mandatory for both polarities:

1. Init -> 01 -> 02 -> 03 -> 04 -> 05;
2. assert thread NFT burned and permanent fraud token minted;
3. call the existing fraudulent-block removal builder;
4. assert the challenged header (and any descendant fixture) is removed,
   slashing/reward shape is exact, and the permanent fraud token remains.

Crash-resume is exercised after steps 02 and 03 by discarding local state and
continuing from the chain datum. Cancellation is exercised at steps 01, 03,
and 04; it burns only the computation-thread token and can never mint a fraud
token. A cancelled thread may be re-initialized from the same evidence under
the generic duplicate-Init semantics.

## 8. Adversarial scenarios and boundary controls

The implementation is not complete without all of the following on-chain or
emulator controls:

- forged header hash, source raw root, source count, source key/value, category;
- foreign event-to-step/transition roots, counts, indices, phase, event key;
- forged ledger root/key/descriptor/output bytes, membership/non-membership
  polarity swap, wrong output index;
- script credential in place of owner vkey, wrong owner, wrong value, reordered
  or duplicate asset, zero/negative quantity;
- malformed key/signature, wrong key hash, wrong body message, signature byte;
- 100 assets vs 101; 5,000-byte value vs 5,001; exact minimum vs one below;
  29-byte vs 57-byte address; no/hash/inline datum; compiled-rate mutation
  control;
- all seven honestly invalid tags against an actually invalid withdrawal;
  `WithdrawalIsValid` against an actually valid withdrawal;
- valid->invalid for every invalid constructor, including the structured
  `SpentWithdrawalUtxo`; invalid->valid for absence, owner, value, signature,
  count, and exact-payability failures;
- replay/stale datum, wrong next-step address, extra thread-token quantity,
  wrong mint/burn, wrong final destination, and foreign fraud token policy;
- third-party cancel refusal, prover cancel success, restart/resume at both
  carried-state boundaries.

The maximum source leaf and maximum ledger witness are measured after applying
the real validators. If any step exceeds the L1 envelope even by reference
script, evidence is split or production deployment remains blocked; limits are never
silently reduced.

## 9. Verification and integration boundary

Required non-zero checks:

- five focused Aiken modules, with exact named positive and negative tests;
- `aiken check --skip-tests` and normalized-format verification for all ten
  Aiken family files;
- SDK twin tests and fault-proof prepare/submit tests;
- both emulator lifecycle polarities, honest-tag refusals, cancel/resume, and
  removal;
- TypeScript typecheck, Prettier and ESLint on touched files.

Documentation marks D-S8 implemented, registered, and emulator-proven.
Family-specific CLI, autonomous watcher detector/prover mounting, preprod/live
evidence, and any change to `l2-tx-mistag` remain out of scope.
