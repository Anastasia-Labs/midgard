# On-Chain Reference (Aiken / Plutarch)

> Audited 2026-07-10 against branch `tx-validation` (HEAD `269bf6b3`) plus its
> contemporaneous working tree; reconstructed on clean base `55afdc54`. Code map for `onchain/aiken` and `onchain/plutarch` as they implement the
> fault-proof system. Anchors drift — re-check on each audit.

## 1. Layout

```
onchain/aiken/
  validators/
    fault-proof-catalogue.ak      # catalogue NFT: genesis mint, spend always fails
    computation-thread.ak         # Init / Success / BurnForCancellation state machine
    fault-proof.ak                # permanent fault_proof token mint; spend always fails
    state-queue.ak                # CommitBlockHeader / Merge / RemoveFaultyBlockHeader
    phas.ak, pexcludes.ak         # MPF membership / non-membership withdraw validators
    da-attestation.ak             # committee threshold attestation over header_hash
    da-params-governor.ak         # governed committee/threshold params
    fraud-proofs/<type>/step-NN.ak  # the 12 proof-type step validators (real logic)
  lib/midgard/
    fraud-proofs/common.ak        # step plumbing: pass/continue/finalize/cancel
    fraud-proofs/native-tx/       # canonical CBOR codec for L2 native txs (~2.6k lines)
    fraud-proofs/transition-trace/proof.ak  # 9-family transition-trace verifier (1710 lines)
    transition-trace.ak           # counted/domain-tagged root primitives
    computation-thread.ak, fraud-proof-catalogue.ak, state-queue.ak, ledger-state.ak,
    operator-directory.ak, common/utils.ak   # shared types + helpers
  env/default.ak, env/testnet.ak  # protocol parameters (see §5)
onchain/plutarch/                 # LEGACY parallel MPF validators (not deployed)
```

Note: `lib/midgard/fraud-proofs/<type>/step-NN.ak` files are thin type re-exports; the
logic lives under `validators/`. Exceptions with real logic in `lib/`:
`double-spend/input-witness.ak` and `native-tx/*`.

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
| MPF non-membership           | `validators/pexcludes.ak:22` — `mpf.insert(...)` must succeed                                                                                                                                                                                    | Same pattern                                                                                                                                                         |
| Delegation helpers           | `lib/midgard/common/utils.ak:597-719` (`plutarch_phas[_raw]`, `plutarch_pexcludes[_raw]`), `:739-758` (`plutarch_pdelete` — **unusable, env hash `#""`**)                                                                                        |                                                                                                                                                                      |
| Counted roots                | `lib/midgard/transition-trace.ak:9-16` (`RootDomain`), `:64-80` (`commit_counted_root` = `blake2b_256(tag ‖ cbor(domain) ‖ raw_root ‖ cbor(count))`)                                                                                             | Consumed by `settlement.ak:83-118`, user-event validators, `fraud-proofs/common.ak:611-620`. Landed in PR #458 (`5169b7f7`)                                          |
| Header type                  | `lib/midgard/ledger-state.ak:57-77`                                                                                                                                                                                                              | 8 roots + 6 counts + times + prev hash + operator vkey + protocol version                                                                                            |

## 3. State-queue removal and slashing

`validators/state-queue.ak:524-712`, redeemer `lib/midgard/state-queue.ak:93-99`
(`faulty_operator`, `faulty_blocks_header_hash`, `slashing_approach`,
`fault_proof_ref_input_index`, `block_removal_approach`).

- **Authorization**: reference input carrying the `fault_proof` token; last 28 bytes of
  asset name must equal `faulty_blocks_header_hash` (`state-queue.ak:698-708`).
  Reference-only ⇒ one token authorizes arbitrarily many removal txs.
- **`RemoveFaultyBlocksLink`** (`:624-662`): splices one descendant per tx.
  ⚠️ Decisive check `:661`: descendant's `operator_vkey == faulty_operator` — the
  _descendant's own_ operator must equal the original faulty operator, contradicting the
  comment `:633-636` ("does not need to have an associated fault proof token") in intent.
  With scheduler rotation (`lib/midgard/scheduler.ak`), cross-operator descendants make
  the cascade unvalidatable.
- **`RemoveLastFaultyBlock`** (`:662-712`): removes the (now-tail) faulty header.
- **Slashing** (`:540-619`): mandatory same-tx `SlashActiveOperator` /
  `SlashRetiredOperator` / `OperatorAlreadySlashed`, cross-validated via
  `lib/midgard/operator-directory.ak:220-356` from the active/retired operator lists'
  `SlashOperator` mint redeemers. Penalty enforced only as `fee >= env.slashing_penalty`;
  bond-remainder routing to the prover is not enforced on-chain. `payout.ak` is not
  involved; `settlement.ak` has an independent `SlashOperatorForBadSettlement` path
  (settlement disputes, not fault proofs; cf.
  `technical-spec/3-consensus-protocol/6-settlement.tex:85-110`).

## 4. Proof-type validators (decisive checks)

| Type                        | Steps                | Decisive check                                                                                                                                                                                                              |
| --------------------------- | -------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `zero-input`                | 2                    | `bad_tx_spend_inputs_hash == env.empty_list_hash` (`zero-input/step-02.ak:64`; `env/default.ak:57`)                                                                                                                         |
| `no-input`                  | 4                    | preimage-hash check (`step-02.ak:71`); `pexcludes` vs `prev_utxos_root` (`step-03.ak:71-78`); `pexcludes` vs `transactions_root` (`step-04.ak:69-76`)                                                                       |
| `double-spend`              | 4 (+`input-witness`) | `tx1_id != tx2_id` (`step-02.ak:57`); `double_spent_input == tx2_double_spent_input` (`step-04.ak:82`); witness recovery `lib/.../double-spend/input-witness.ak:7-22`                                                       |
| `input-no-idx`              | 4                    | `producing_tx_id == bad_input_tx_id` (`step-03.ak:66`); `bad_input_output_index >= list.length(outputs_preimage)` (`step-04.ak:74`)                                                                                         |
| `invalid-range`             | 2                    | `normalize_native_validity_range` (`step-01.ak:20-44`, inline tests `:140-155`); block-range comparison incl. inverted-interval branch (`step-02.ak:82-92`)                                                                 |
| `invalid-signature`         | 2                    | `verify_ed25519_signature(vkey, bad_tx_id, sig) == False` (`step-02.ak:82-87`); duplicate-vkey TODO `:75-76`                                                                                                                |
| `missing-native-script-tx`  | 6                    | script-hash equality via `ledger_state.hash_midgard_script(Timelock{..})` (`step-05.ak:66-69`); `pairs.has_key == False` (`step-06.ak:77-79`)                                                                               |
| `missing-signature`         | 4                    | `get_verification_key_hash(vkey) == missing_required_signer_hash` (`step-03.ak:68`; helper `common/utils.ak:760-762`); absence check (`step-04.ak:76-78`)                                                                   |
| `no-reference-input`        | 4                    | `pexcludes` vs `prev_utxos_root` (`step-03.ak:70-76`) and vs `transactions_root` (`step-04.ak:72-79`)                                                                                                                       |
| `withdrawn-reference-input` | 3                    | `l2_outref == missing_reference_input_outref` + `phas` vs `withdrawals_root` (`step-03.ak:75-92`)                                                                                                                           |
| `min-fee`                   | 2                    | ⚠️ `bad_tx_body_fee < get_min_transaction_fee(bad_tx)` (`step-02.ak:64`) with the stub `fn get_min_transaction_fee(_) { 0 }` (`:78-80`, TODO `:77`) — unsatisfiable                                                         |
| `transition-trace`          | single proof         | dispatch in `lib/.../transition-trace/proof.ak`; header binding + category prefix `#"00000004"`; direct unilateral transition families plus canonical accepted-validation-claim binding for normal and valid-forced effects |

## 5. Environment parameters (`env/default.ak`, `env/testnet.ak`)

Two environments exist on clean base `55afdc54`, selected via
`aiken {build,check} --env <name>` (no flag ⇒ `default`). Both still hold the
legacy 30 ms maturity value. The decided 7 day production and 10 minute testnet
values are **not yet applied** (each is compiled into script hashes, so the cut
requires redeployment). The same cut should add an explicitly non-deployable,
short-window `emulator` environment for fast tests.

| Param                               | default                    | testnet                 | Note                                                                                                                                                                              |
| ----------------------------------- | -------------------------- | ----------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `maturity_duration`                 | 30 (`default.ak:19`)       | 30 (`testnet.ak:18`)    | PosixTimeDuration (ms). **Targets (decided 2026-07-11, not yet applied): production `604_800_000` (7 days); testnet `600_000` (10 minutes), plus a separate short emulator env.** |
| `slashing_penalty`                  | 0 (`:21`)                  | 0 (`:20`)               | TODO on fee-payment design `default.ak:27-34`                                                                                                                                     |
| `fraud_prover_reward`               | 0 (`:23`)                  | 0 (`:22`)               | Historical identifier retained in source                                                                                                                                          |
| `required_bond`                     | penalty+reward = 0 (`:25`) | 0 (`:24`)               |                                                                                                                                                                                   |
| `inactivity_slashing_penalty`       | 0 (`:35`)                  | 0 (`:26`)               |                                                                                                                                                                                   |
| `empty_list_hash`                   | `default.ak:57`            | —                       | used by zero-input                                                                                                                                                                |
| `plutarch_phas_validator_hash`      | `default.ak:62-63`         | present                 | matches Aiken-native `phas.ak` in `plutus.json` (the deployed one)                                                                                                                |
| `plutarch_pexcludes_validator_hash` | `default.ak:65-66`         | `testnet.ak:50-51`      | matches `pexcludes.ak`                                                                                                                                                            |
| `plutarch_pdelete_validator_hash`   | `#""` (`default.ak:68`)    | `#""` (`testnet.ak:53`) | delete delegation unusable                                                                                                                                                        |

Environment selected via `aiken build --env <name>`
(`.github/workflows/midgard-node-ci.yml:82-84`).

## 6. On-chain tests

| File                                                                             | Tests              | Coverage                                                                                                                                               |
| -------------------------------------------------------------------------------- | ------------------ | ------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `lib/midgard/fraud-proofs/transition-trace/proof.test.ak`                        | canonical V1 suite | direct transition families plus accepted validation-claim binding; both `SourcePhaseMismatch` directions and valid/invalid forced outcomes are covered |
| `lib/midgard/transition-trace.test.ak`                                           | 4                  | counted-root primitives                                                                                                                                |
| `lib/midgard/fraud-proofs/native-tx.test.ak` (+ high-cardinality, size-balanced) | 7+1+1              | CBOR round-trips + fixture regressions                                                                                                                 |
| `validators/fraud-proofs/invalid-range/step-01.ak:140-155`                       | 3                  | range normalization (inline)                                                                                                                           |
| `validators/da_attestation_capacity.test.ak`                                     | 3                  | committee signature capacity                                                                                                                           |
| `lib/midgard/common/utils.test.ak`                                               | 2                  | generic folds only                                                                                                                                     |

**No tests exist for** `computation-thread.ak`, `state-queue.ak` removal paths,
`fault-proof.ak`, `fault-proof-catalogue.ak`, or any step validator other than
invalid-range step-01 — at the Aiken level these are exercised only indirectly via the
TypeScript emulator suite (`demo/midgard-fault-proofs/tests/submit-init-emulator.test.ts`),
which is CI-wired through the fault-proof package job
([`testing-status.md`](testing-status.md)).

Build/test: `aiken fmt --check && aiken check` in `onchain/aiken` (CI:
`.github/workflows/aiken-ci.yml:31,33`), compiler pinned `v1.1.21` (`aiken.toml:3`);
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
