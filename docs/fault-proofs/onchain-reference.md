# On-Chain Reference (Aiken / Plutarch)

> Reconciled 2026-08-29 against the current working tree. Code map for
> `onchain/aiken` and `onchain/plutarch` as they implement the fault-proof
> system. Line anchors are intentionally avoided where a stable symbol suffices.
> The current generated testnet blueprint has 510 validators and SHA-256
> `ad69e8f98e49e110864cb270dd6bb731caaf43357e8459827b1659124c890de8`.

## 1. Layout

```
onchain/aiken/
  validators/
    fault-proof-catalogue.ak      # catalogue NFT: genesis mint, spend always fails
    computation-thread.ak         # Init / Success / BurnForCancellation state machine
    fault-proof.ak                # permanent fault_proof token mint; spend always fails
    state-queue.ak                # commit, merge, fraudulent removal, unattested-head timeout correction
    phas.ak, pexcludes.ak         # MPF membership / non-membership withdraw validators
    da-attestation.ak             # committee threshold attestation over header_hash
    da-params-governor.ak         # governed committee/threshold params
    fraud-proofs/<type>/step-NN.ak  # registered family step validators
  lib/midgard/
    fraud-proofs/common.ak        # step plumbing: pass/continue/finalize/cancel
    fraud-proofs/native-tx/       # canonical CBOR codec for L2 native txs
    fraud-proofs/transition-trace/proof.ak  # transition-trace verifier
    transition-trace.ak           # counted/domain-tagged root primitives
    computation-thread.ak, fraud-proof-catalogue.ak, state-queue.ak, ledger-state.ak,
    operator-directory.ak, common/utils.ak   # shared types + helpers
  env/default.ak, env/testnet.ak  # protocol parameters (see §5)
onchain/plutarch/                 # LEGACY parallel MPF validators (not deployed)
```

Family wire/state helpers and reusable predicates live under
`lib/midgard/fraud-proofs/`; transaction-context handlers live under
`validators/fraud-proofs/`. The shared native codec, field-opening/carriage,
machine-walk, transition-trace, and family engines are substantive library
logic rather than thin re-exports.

## 2. Generic machinery

| Component                    | Anchor                                                                                                                                                                                                                                           | Behavior                                                                                                                                                             |
| ---------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ | -------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Catalogue datum              | `lib/midgard/fraud-proof-catalogue.ak:13-14`                                                                                                                                                                                                     | Single `MerkleRoot<Int, ByteArray>` — `{category_id → first-step script hash}`                                                                                       |
| Catalogue mint               | `validators/fraud-proof-catalogue.ak:14-27`                                                                                                                                                                                                      | Genesis-only, coupled to hub-oracle token mint                                                                                                                       |
| Catalogue spend              | `validators/fraud-proof-catalogue.ak:7-12`                                                                                                                                                                                                       | Always fails ⇒ immutable root                                                                                                                                        |
| Thread `Init`                | `validators/computation-thread.ak:20-129`                                                                                                                                                                                                        | Proves category ∈ catalogue (`plutarch_phas`), mints NFT `category_id ‖ header_hash`, output to first step with `StepDatum{fault_prover, data: None}`, prover-signed |
| Thread `Success`             | `:130-139`                                                                                                                                                                                                                                       | Only checks own burn; comment `:132-133`: real coupling "is done in each fault proof's last step"                                                                    |
| Thread `BurnForCancellation` | `:140-149`                                                                                                                                                                                                                                       | Prover reclaims; no bookkeeping                                                                                                                                      |
| Step plumbing                | `lib/midgard/fraud-proofs/common.ak:67` (`pass_tx_to_next_step`), `:165` (`pass_native_tx_to_next_step`), `:317` (`continue`), `:391` (`finalize`), `:254` (`cancel`); tx-in-block checks `:518`, `:575`; counted-root authentication `:611-620` | Step sequencing is per-category, not generically validated                                                                                                           |
| Fault-proof mint             | `validators/fraud-proof.ak:17-63`                                                                                                                                                                                                                | Requires thread `Success` with matching asset name (`:28-40`) and exact mint `{-1 thread, +1 proof}` (`:45-54`); spend always fails (`:7-14`)                        |
| MPF membership               | `validators/phas.ak:15` — `mpf.has(from_root(root), key, value, proof)`                                                                                                                                                                          | Invoked via withdraw-zero reference scripts                                                                                                                          |
| MPF non-membership           | `validators/pexcludes.ak` — `mpf.insert(...)` must succeed; the Midgard empty-trie sentinel is translated to `mpf.empty` before `mpf.from_root`                                                                                                  | Same pattern                                                                                                                                                         |
| Delegation helpers           | `lib/midgard/common/utils.ak:629-763` (`plutarch_phas[_raw]`, `plutarch_pexcludes[_raw]`)                                                                                                                                                        |                                                                                                                                                                      |
| Counted roots                | `lib/midgard/transition-trace.ak:9-16` (`RootDomain`), `:64-80` (`commit_counted_root` = `blake2b_256(tag ‖ cbor(domain) ‖ raw_root ‖ cbor(count))`)                                                                                             | Consumed by `settlement.ak:83-118`, user-event validators, and `fraud-proofs/common.ak:611-620`                                                                      |
| Header type                  | `lib/midgard/ledger-state.ak:60-85`                                                                                                                                                                                                              | `HeaderV1`, constructor tag 0, arity 25; nine roots, seven counts, and nine metadata fields in the registry order below                                              |

The exact `HeaderV1` constructor-0 field order is:

```text
prev_utxos_root, utxos_root, withdrawals_root, forced_transactions_root,
transactions_root, deposits_root, transition_trace_root, event_to_step_root,
validation_traces_root, withdrawal_count, forced_transaction_count,
l2_transaction_count, deposit_count, total_event_count, transition_step_count,
validation_trace_count, start_time, end_time, block_slot, expected_network_id,
min_fee_a, min_fee_b, prev_header_hash, operator_vkey, protocol_version
```

## 3. State-queue removal and slashing

`validators/state-queue.ak:524-712`, redeemer `lib/midgard/state-queue.ak:93-99`
(`faulty_operator`, `faulty_blocks_header_hash`, `slashing_approach`,
`fault_proof_ref_input_index`, `block_removal_approach`).

- **Authorization**: reference input carrying the `fault_proof` token; last 28 bytes of
  asset name must equal `faulty_blocks_header_hash` (`state-queue.ak:698-708`).
  Reference-only ⇒ one token authorizes arbitrarily many removal txs.
- **`RemoveFraudulentBlocksLink`**: splices one descendant per transaction. The
  current working tree authorizes the removal from the authenticated linked-list
  successor relation and deliberately ignores the descendant's operator, so
  scheduler rotation does not deadlock structural pruning.
- **`RemoveLastFraudulentBlock`**: removes the now-tail faulty header.
- **Slashing**: mandatory same-tx `SlashActiveOperator` /
  `SlashRetiredOperator` / `OperatorAlreadySlashed`, cross-validated via
  `lib/midgard/operator-directory.ak:220-356` from the active/retired operator lists'
  `SlashOperator` mint redeemers. The removal validator obtains the prover from
  the authentic proof datum and enforces an exact ADA-only reward output,
  amount, and signer on the bond-consuming arm; already-slashed arms forbid a
  second reward-shaped payment. The compiled economics values are still zero.
  `payout.ak` is not involved; `settlement.ak` has an independent `SlashOperatorForBadSettlement` path
  (settlement disputes, not fault proofs; cf.
  `technical-spec/3-consensus-protocol/6-settlement.tex:85-110`).

The same state-queue validator implements Q61's
`RemoveUnattestedBlockAfterTimeout` path. The canonical deadline is
`header.end_time + 3_600_000` ms. Permissionless correction first removes each
authenticated descendant with `PruneTimedOutBlockDescendant`, then removes the
terminal unattested head with `RemoveTimedOutHead`; it never consumes an
operator bond. The append and attestation paths share the relevant queue inputs
and enforce the deadline boundary, so a successful competing transaction makes
the stale correction transaction invalid.

## 4. Selected proof-type validators (decisive checks)

| Type                        | Steps            | Decisive check                                                                                                                                                                                                                                 |
| --------------------------- | ---------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `zero-input`                | 2                | native counted-root membership in step 01; `bad_tx_spend_inputs_hash == blake2b_256(encode_native_byte_list([]))` (`zero-input/step-02.ak:17-25,79-82`). This deliberately does not use the PlutusData `env.empty_list_hash`.                  |
| `no-input`                  | 4                | preimage-hash check (`step-02.ak:71`); `pexcludes` vs `prev_utxos_root` (`step-03.ak:71-78`); `pexcludes` vs `transactions_root` (`step-04.ak:69-76`)                                                                                          |
| `double-spend`              | 4                | `tx1_id != tx2_id` (`step-02.ak:57`); `double_spent_input == tx2_double_spent_input` (`step-04.ak:82`); spend-input read through the §8.8 door (`lib/midgard/fraud-proofs/field-opening-v1.ak`)                                                |
| `input-no-idx`              | 4                | `producing_tx_id == bad_input_tx_id` (`step-03.ak:66`); `bad_input_output_index >= list.length(outputs_preimage)` (`step-04.ak:74`)                                                                                                            |
| `invalid-range`             | 2                | `normalize_native_validity_range` (`step-01.ak:20-44`, inline tests `:140-155`); block-range comparison incl. inverted-interval branch (`step-02.ak:82-92`)                                                                                    |
| `invalid-signature`         | 2                | field-7 opening anchored by transaction id + witness-set hash; decisive `verify_ed25519_signature(vkey, bad_tx_id, sig) == False`                                                                                                              |
| `missing-native-script-tx`  | 6                | script-hash equality via `ledger_state.hash_midgard_script(Timelock{..})` (`step-05.ak:66-69`); `pairs.has_key == False` (`step-06.ak:77-79`)                                                                                                  |
| `missing-signature`         | 4                | `get_verification_key_hash(vkey) == missing_required_signer_hash` (`step-03.ak:68`; helper `common/utils.ak:760-762`); absence check (`step-04.ak:76-78`)                                                                                      |
| `no-reference-input`        | 4                | `pexcludes` vs `prev_utxos_root` (`step-03.ak:70-76`) and vs `transactions_root` (`step-04.ak:72-79`)                                                                                                                                          |
| `withdrawn-reference-input` | 3                | `l2_outref == missing_reference_input_outref` + `phas` vs `withdrawals_root` (`step-03.ak:75-92`)                                                                                                                                              |
| `min-fee`                   | 2                | `bad_tx_body_fee < min_fee_lovelace_v1(min_fee_a, min_fee_b, canonical_tx_size)` after authenticating the compact transaction, witness compact, and all nine field preimage lengths; the same helper feeds validation-machine `reject_min_fee` |
| `transition-trace`          | route + 8 finals | dispatch in `lib/.../transition-trace/proof.ak`; header binding + category prefix `#"00000004"`; one route selects one of eight terminal validators, all deployed as mandatory authenticated reference scripts                                 |

The append-only source catalogue assigns 29 categories, IDs `00000000` through
`0000001c`. The canonical appended block `0000000b`–`0000001c` is
`fabricatedDeposit`, `fabricatedWithdrawal`, `nativeScriptDecoding`,
`missingSignature`, `missingNativeScriptTx`, `withdrawnReferenceInput`,
`canonicalDecodability`, `committedFieldShape`, `minFee`, `withdrawalMistag`,
`doubleWithdraw`, `crossBlockDuplicateEvent`, `l2TxMistag`, `withdrawnInput`,
`valueNotPreserved`, `inputSetUniqueness`, `mintAuthorization`, and
`networkId`. The network-id validators are compiled and exposed by the shared
runtime deployment map. Every step is an authenticated reference script. Catalogue
immutability makes this a fresh-genesis/redeploy
identity movement, not a migration or compatibility path. Watcher topology
registration is off-chain authority metadata; it does not mount detectors or
provers.

## 5. Environment parameters (`env/default.ak`, `env/testnet.ak`)

Two environments exist, selected via
`aiken {build,check} --env <name>` (no flag ⇒ `default`). Canonical V1 does not
select maturity by environment: `ledger_state.block_maturity_duration_v1`
fixes the challenge, merge, and operator bond-hold window at seven days.

| Param                               | default            | testnet            | Note                                                                                                    |
| ----------------------------------- | ------------------ | ------------------ | ------------------------------------------------------------------------------------------------------- |
| canonical block maturity            | seven days         | seven days         | `ledger-state.ak`; shared by merge, dispute opening, SDK/node readiness, and active-operator bond holds |
| `slashing_penalty`                  | 0                  | 0                  | TODO on fee-payment design                                                                              |
| `fraud_prover_reward`               | 0                  | 0                  | Source identifier; exact reward routing is implemented                                                  |
| `required_bond`                     | penalty+reward = 0 | 0                  |                                                                                                         |
| `inactivity_slashing_penalty`       | 0                  | 0                  |                                                                                                         |
| `empty_list_hash`                   | `default.ak:55`    | —                  | legacy PlutusData empty-list hash; not used by the native-v1 zero-input proof                           |
| `plutarch_phas_validator_hash`      | `default.ak:60-61` | present            | matches Aiken-native `phas.ak` in `plutus.json` (the deployed one)                                      |
| `plutarch_pexcludes_validator_hash` | `default.ak:63-64` | `testnet.ak:49-50` | matches `pexcludes.ak`                                                                                  |

Environment selected via `aiken build --env <name>`
(`.github/workflows/midgard-node-ci.yml:82-84`).

## 6. On-chain tests

| File                                                                             | Tests | Coverage                                                                                                                                                                                                                                                            |
| -------------------------------------------------------------------------------- | ----- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `lib/midgard/fraud-proofs/transition-trace/proof.test.ak`                        | 57    | direct transition families plus accepted validation-claim binding; both `SourcePhaseMismatch` directions, valid/invalid forced outcomes, all omitted/out-of-window source-event variants, and positive/negative pairs for all five count-fault variants are covered |
| `lib/midgard/transition-trace.test.ak`                                           | 6     | counted-root primitives                                                                                                                                                                                                                                             |
| `lib/midgard/fraud-proofs/native-tx.test.ak` (+ high-cardinality, size-balanced) | 14    | CBOR round-trips and fixture regressions                                                                                                                                                                                                                            |
| `validators/fraud-proofs/invalid-range/step-01.ak`                               | 8     | range normalization and handler controls                                                                                                                                                                                                                            |
| `validators/fraud-proofs/zero-input/step-02.ak`                                  | 3     | native empty-list encoding/literal, non-empty inequality, and direct handler controls                                                                                                                                                                               |
| `validators/computation-thread.ak:280-513`                                       | 15    | direct `Init` membership/prover/thread-NFT controls (11), `Success` exact burn accept/reject (2), and `BurnForCancellation` exact-burn accept/reject (2)                                                                                                            |
| `validators/fraud-proof-catalogue.ak:50-76`                                      | 4     | genesis-coupled mint acceptance, standalone/duplicate mint rejection, and immutable-root spend rejection                                                                                                                                                            |
| `validators/state-queue.ak`                                                      | 11    | six direct HeaderV1 commit selectors (shape/interval, validation scalars, scheduled operator, previous header, confirmed genesis, confirmed ordinary) plus five `d3_*`/`d4_*` reward-routing controls                                                               |
| `validators/da_attestation_capacity.test.ak`                                     | 3     | committee signature capacity                                                                                                                                                                                                                                        |
| `lib/midgard/common/utils.test.ak`                                               | 2     | generic folds only                                                                                                                                                                                                                                                  |

**Direct Aiken coverage remains incomplete**, but it is broader than this
representative table: the current working tree contains direct terminal
fault-token coupling and both structural removal-branch controls, including
rotated operators. Registered families also carry focused predicate selectors;
the remaining machinery is exercised by the TypeScript emulator suite
(`demo/midgard-fault-proofs/tests/submit-init-emulator*.test.ts`), which is CI-wired
through the fault-proof package job ([`testing-status.md`](testing-status.md)).

Build/test: `aiken fmt --check && aiken check` in `onchain/aiken` (CI:
`.github/workflows/aiken-ci.yml`), compiler declared `v1.1.23` and CI-pinned to
the patched fork `aiken v1.1.23+5adf783`;
blueprint via `aiken build --env testnet`.

## 7. Plutarch subproject (legacy)

`onchain/plutarch/` provides `membershipStakeValidator` / `nonMembershipStakeValidator`
(`src/Validators/Membership.hs:20-67`) over the vendored
`Plutarch.MerkleTree.PatriciaForestry` (from `catalyst-onchain-libs`, `cabal.project:16-22`),
with compiled envelopes in `generated/*.plutus.json` (`app/Main.hs:42-85`). The deployed
`phas`/`pexcludes` hashes in `env/` match the **Aiken-native** validators, not these
(`onchain/plutarch/README.md:1-8,30-36` labels the package legacy). Tests
(`tests/Testing/*`) cover MPF trie math, crypto regressions, and validator success paths;
run `cabal test helpers-tests` (`midgard-merkle.cabal:157-176`) — not CI-wired.
