//// ## Chunked MPF proof challenge thread (issue #541)
////
//// A single validator serving as both the challenge thread's minting policy
//// and the challenge UTxO's spending script.
////
//// - `mint(InitChallenge)` opens a unique challenge thread. The thread token's
////   asset name is the nonce derived from a consumed output reference, so the
////   thread is unforgeable and unique by construction. The thread's datum is
////   authenticated once, here, against the challenged block header: its
////   `(expected_root, expected_leaf_count)` pair must reproduce the counted
////   root the header commits for the named domain.
//// - `spend(FinalizeProofRedeemer)` finalizes. It references the published
////   proof chunks, concatenates their steps in the order the redeemer names,
////   verifies the complete MPF proof atomically, burns the thread token, and
////   pays the challenge's value to `proof_owner`.
////
//// Publishing proof chunks needs no validator at all — see the module header
//// of `midgard/mpf_chunked_proof_v1` for why that is safe. Both publication
//// and finalization are permissionless.
////
//// The challenge datum is never mutated between initialization and
//// finalization, so there is no intermediate state an adversary could poison.

use aiken/collection/dict
use aiken/collection/list
use aiken/crypto.{blake2b_256}
use aiken/merkle_patricia_forestry as mpf
use aiken/merkle_patricia_forestry/helpers.{combine, nibble, suffix}
use cardano/address.{Address, Script, VerificationKey}
use cardano/assets.{PolicyId}
use cardano/transaction.{
  InlineDatum, Input, NoDatum, Output, OutputReference, Transaction,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/hub_oracle as hub
use midgard/mpf_chunked_proof_v1.{
  BurnChallenge, ChallengeMintRedeemer, FinalizeProofRedeemer, InitChallenge,
  Membership, NonMembership, ProofChallengeDatum, ProofChunkDatum,
}
use midgard/state_queue
use midgard/transition_trace
use midgard/user_events

/// The single token minted or burned under `own_policy_id` in this
/// transaction. Any other shape (no entry, several asset names, several units)
/// is rejected, which is what makes the challenge thread unique and makes its
/// retirement a single event.
fn sole_thread_token(
  value: assets.Value,
  own_policy_id: PolicyId,
) -> Option<Pair<ByteArray, Int>> {
  when value |> assets.tokens(own_policy_id) |> dict.to_pairs is {
    [entry] -> Some(entry)
    _ -> None
  }
}

/// The finalizing transaction must pay the challenge UTxO's lovelace to the
/// challenge's `proof_owner`, whoever submits it.
fn reward_is_bound_to_proof_owner(
  outputs: List<Output>,
  own_lovelace: Int,
  proof_owner: ByteArray,
) -> Bool {
  let paid = list.foldr(
    outputs,
    0,
    fn(output, total) {
      let Output { address: Address { payment_credential, .. }, value, .. } = output
      when payment_credential is {
        VerificationKey(key_hash) ->
          if key_hash == proof_owner {
            total + assets.lovelace_of(value)
          } else {
            total
          }
        Script(_) -> total
      }
    },
  )
  paid >= own_lovelace
}

validator main(hub_oracle_script_hash: PolicyId) {
  mint(
    redeemer: ChallengeMintRedeemer,
    own_policy_id: PolicyId,
    tx: Transaction,
  ) {
    let Transaction { inputs, outputs, reference_inputs, mint, .. } = tx
    when redeemer is {
      InitChallenge {
        nonce_input_index,
        challenge_output_index,
        hub_oracle_ref_input_index,
        challenged_block_ref_input_index,
        challenged_root_domain,
      } -> {
        // 1. The state queue's policy comes from the authentic hub oracle.
        let hub.Datum { state_queue: state_queue_policy_id, .. } = hub.get_datum(
          reference_inputs,
          hub_oracle_script_hash,
          hub_oracle_ref_input_index,
        )

        // 2. The challenged block must be referenced from the state queue.
        let header, header_hash <- state_queue.get_block_datum_v1(
          reference_inputs,
          state_queue_policy_id,
          challenged_block_ref_input_index,
        )

        // 3. The thread token's asset name is the nonce of a consumed output
        //    reference, and exactly one unit of it is minted.
        expect Some(Input { output_reference: nonce_out_ref, .. }) = list.at(
          inputs,
          nonce_input_index,
        )
        let nonce = user_events.out_ref_to_nonce(nonce_out_ref)
        expect Some(Pair(minted_asset_name, minted_quantity)) = sole_thread_token(
          mint,
          own_policy_id,
        )

        // 4. The thread UTxO is produced at this script's own address, with an
        //    inline datum, no reference script, and the thread token.
        expect
            Some(Output {
              address: Address {
                payment_credential: Script(output_script_hash),
                ..
              },
              value: challenge_value,
              datum: InlineDatum(challenge_data),
              reference_script: None,
            })
          = list.at(outputs, challenge_output_index)
        expect challenge: ProofChallengeDatum = challenge_data

        and {
          minted_asset_name == nonce,
          minted_quantity == 1,
          output_script_hash == own_policy_id,
          assets.quantity_of(challenge_value, own_policy_id, nonce) == 1,
          // 5. The challenge is well formed and bound to the challenged header.
          mpf_chunked_proof_v1.challenge_datum_is_well_formed(challenge),
          mpf_chunked_proof_v1.challenge_matches_header(
            challenge,
            header,
            header_hash,
            challenged_root_domain,
          ),
        }
      }

      // Retiring the thread is only reachable while the thread's own UTxO is
      // being spent; the spending handler carries the finalization conditions.
      BurnChallenge { challenge_input_index } -> {
        expect
            Some(Input {
              output: Output {
                address: Address {
                  payment_credential: Script(input_script_hash),
                  ..
                },
                value: challenge_value,
                ..
              },
              ..
            })
          = list.at(inputs, challenge_input_index)
        expect Some(Pair(burnt_asset_name, burnt_quantity)) = sole_thread_token(
          mint,
          own_policy_id,
        )
        and {
          input_script_hash == own_policy_id,
          burnt_quantity == -1,
          assets.quantity_of(challenge_value, own_policy_id, burnt_asset_name) == 1,
        }
      }
    }
  }

  spend(
    datum: Option<ProofChallengeDatum>,
    redeemer: FinalizeProofRedeemer,
    own_out_ref: OutputReference,
    tx: Transaction,
  ) {
    let Transaction { inputs, outputs, reference_inputs, mint, .. } = tx
    expect Some(challenge) = datum

    // 1. Locate this input and this script's own hash, which is also the
    //    challenge thread's policy id.
    expect
        Some(Input {
          output: Output {
            address: Address { payment_credential: Script(own_script_hash), .. },
            value: own_value,
            ..
          },
          ..
        })
      = list.find(inputs, fn(input) { input.output_reference == own_out_ref })

    // 2. This input carries exactly one thread token ...
    expect Some(Pair(thread_asset_name, thread_quantity)) = sole_thread_token(
      own_value,
      own_script_hash,
    )

    // 3. ... which this transaction burns, and nothing else of this policy.
    expect Some(Pair(burnt_asset_name, burnt_quantity)) = sole_thread_token(
      mint,
      own_script_hash,
    )

    and {
      thread_quantity == 1,
      burnt_asset_name == thread_asset_name,
      burnt_quantity == -1,
      // 4. The thread token is consumed exactly once: no second input may
      //    carry a token of this policy.
      list.count(
        inputs,
        fn(input) {
          assets.quantity_of(
            input.output.value,
            own_script_hash,
            thread_asset_name,
          ) > 0
        },
      ) == 1,
      // 5. The published chunks reconstruct the challenged proof atomically.
      mpf_chunked_proof_v1.verify_published_proof(
        reference_inputs,
        challenge,
        redeemer,
      ),
      // 6. The reward stays bound to the challenge's owner.
      reward_is_bound_to_proof_owner(
        outputs,
        assets.lovelace_of(own_value),
        challenge.proof_owner,
      ),
    }
  }

  else(_) {
    fail
  }
}

// ## Fixtures
//
// The honest challenge is bound to the real native-V1 block fixture: its
// `(expected_root, expected_leaf_count)` pair is exactly what that block's
// header commits under `TransactionsV1RootDomain`, and the target is the
// single transaction that block holds. Chunked carriage of deep proofs is
// exercised at the library level in `midgard/mpf_chunked_proof_v1.test`; these
// tests exercise the validator's own guards.

const challenge_script_hash =
  #"88888888888888888888888888888888888888888888888888888888"

const challenge_lovelace: Int = 50_000_000

fn block() -> fixture.NativeBlockFixtureV1 {
  fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
}

fn thread_nonce() -> ByteArray {
  user_events.out_ref_to_nonce(fixture.own_out_ref())
}

fn honest_challenge() -> ProofChallengeDatum {
  let claimed = block()
  ProofChallengeDatum {
    proof_owner: fixture.fraud_prover_key_hash,
    challenged_header_hash: claimed.header_hash,
    target_key: claimed.tx_id,
    target_value_hash: blake2b_256(claimed.compact_cbor),
    expected_root: claimed.transactions_phas_root,
    expected_leaf_count: 1,
    mode: Membership,
  }
}

fn nonce_input() -> Input {
  Input {
    output_reference: fixture.own_out_ref(),
    output: Output {
      address: address.from_verification_key(fixture.fraud_prover_key_hash),
      value: assets.from_lovelace(100_000_000),
      datum: NoDatum,
      reference_script: None,
    },
  }
}

fn challenge_value(asset_name: ByteArray) -> assets.Value {
  assets.from_lovelace(challenge_lovelace)
    |> assets.add(challenge_script_hash, asset_name, 1)
}

fn challenge_output(challenge: ProofChallengeDatum) -> Output {
  let challenge_data: Data = challenge
  Output {
    address: address.from_script(challenge_script_hash),
    value: challenge_value(thread_nonce()),
    datum: InlineDatum(challenge_data),
    reference_script: None,
  }
}

fn init_tx(output: Output, mint: assets.Value) -> Transaction {
  Transaction {
    ..transaction.placeholder,
    inputs: [nonce_input()],
    reference_inputs: [
      fixture.hub_reference_input(),
      fixture.state_queue_reference_input(block()),
    ],
    outputs: [output],
    mint,
  }
}

fn run_init(
  output: Output,
  mint: assets.Value,
  domain: transition_trace.RootDomain,
) -> Bool {
  main.mint(
    fixture.hub_oracle_script_hash,
    InitChallenge {
      nonce_input_index: 0,
      challenge_output_index: 0,
      hub_oracle_ref_input_index: 0,
      challenged_block_ref_input_index: 1,
      challenged_root_domain: domain,
    },
    challenge_script_hash,
    init_tx(output, mint),
  )
}

fn run_honest_init(challenge: ProofChallengeDatum) -> Bool {
  run_init(
    challenge_output(challenge),
    assets.from_asset(challenge_script_hash, thread_nonce(), 1),
    transition_trace.TransactionsV1RootDomain,
  )
}

fn challenge_input(
  challenge: ProofChallengeDatum,
  asset_name: ByteArray,
) -> Input {
  let challenge_data: Data = challenge
  Input {
    output_reference: OutputReference {
      transaction_id: blake2b_256(#"c0de"),
      output_index: 0,
    },
    output: Output {
      address: address.from_script(challenge_script_hash),
      value: challenge_value(asset_name),
      datum: InlineDatum(challenge_data),
      reference_script: None,
    },
  }
}

fn reward_output(owner: ByteArray, lovelace: Int) -> Output {
  Output {
    address: address.from_verification_key(owner),
    value: assets.from_lovelace(lovelace),
    datum: NoDatum,
    reference_script: None,
  }
}

fn run_finalize(
  challenge: ProofChallengeDatum,
  inputs: List<Input>,
  outputs: List<Output>,
  mint: assets.Value,
  indices: List<Int>,
) -> Bool {
  let spent = challenge_input(challenge, thread_nonce())
  main.spend(
    fixture.hub_oracle_script_hash,
    Some(challenge),
    FinalizeProofRedeemer { ordered_chunk_reference_input_indices: indices },
    spent.output_reference,
    Transaction {
      ..transaction.placeholder,
      inputs: list.push(inputs, spent),
      outputs,
      mint,
    },
  )
}

const absent_key =
  #"4444444444444444444444444444444444444444444444444444444444444444"

const absent_value_hash =
  #"6666666666666666666666666666666666666666666666666666666666666666"

/// A permissionless publication UTxO: an inline `ProofChunkDatum` at an
/// arbitrary address, holding nothing but proof steps.
fn chunk_reference_input(steps: mpf.Proof) -> Input {
  let chunk_data: Data = ProofChunkDatum { proof_steps: steps }
  Input {
    output_reference: OutputReference {
      transaction_id: blake2b_256(#"c40c40c4"),
      output_index: 0,
    },
    output: Output {
      address: address.from_script(fixture.step_script_hash),
      value: assets.from_lovelace(2_000_000),
      datum: InlineDatum(chunk_data),
      reference_script: None,
    },
  }
}

fn run_finalize_with_chunks(
  challenge: ProofChallengeDatum,
  chunks: List<Input>,
  indices: List<Int>,
) -> Bool {
  let spent = challenge_input(challenge, thread_nonce())
  main.spend(
    fixture.hub_oracle_script_hash,
    Some(challenge),
    FinalizeProofRedeemer { ordered_chunk_reference_input_indices: indices },
    spent.output_reference,
    Transaction {
      ..transaction.placeholder,
      inputs: [spent],
      reference_inputs: chunks,
      outputs: [
        reward_output(fixture.fraud_prover_key_hash, challenge_lovelace),
      ],
      mint: assets.from_asset(challenge_script_hash, thread_nonce(), -1),
    },
  )
}

fn run_honest_finalize() -> Bool {
  run_finalize(
    honest_challenge(),
    [],
    [reward_output(fixture.fraud_prover_key_hash, challenge_lovelace)],
    assets.from_asset(challenge_script_hash, thread_nonce(), -1),
    [],
  )
}

// ## Initialization

/// Honest acceptance: a unique thread token named by a consumed output
/// reference, a datum bound to the challenged header's counted transactions
/// root and its leaf count.
test mpf_chunked_challenge_initializes_a_header_bound_thread() {
  run_honest_init(honest_challenge())
}

test mpf_chunked_challenge_init_rejects_a_foreign_header_hash() {
  !run_honest_init(
    ProofChallengeDatum {
      ..honest_challenge(),
      challenged_header_hash: fixture.step_script_hash,
    },
  )
}

test mpf_chunked_challenge_init_rejects_a_root_the_header_does_not_commit() {
  !run_honest_init(
    ProofChallengeDatum {
      ..honest_challenge(),
      expected_root: blake2b_256(#"0badc0de"),
    },
  )
}

test mpf_chunked_challenge_init_rejects_a_leaf_count_the_header_does_not_commit() {
  !run_honest_init(
    ProofChallengeDatum { ..honest_challenge(), expected_leaf_count: 2 },
  )
}

test mpf_chunked_challenge_init_rejects_a_substituted_root_domain() {
  !run_init(
    challenge_output(honest_challenge()),
    assets.from_asset(challenge_script_hash, thread_nonce(), 1),
    transition_trace.DepositsRootDomain,
  )
}

test mpf_chunked_challenge_init_rejects_a_malformed_challenge_datum() {
  !run_honest_init(
    ProofChallengeDatum {
      ..honest_challenge(),
      target_key: fixture.fraud_prover_key_hash,
    },
  )
}

test mpf_chunked_challenge_init_rejects_an_asset_name_that_is_not_the_nonce() {
  let forged_name = blake2b_256(#"f0f0f0f0")
  !run_init(
    Output {
      ..challenge_output(honest_challenge()),
      value: challenge_value(forged_name),
    },
    assets.from_asset(challenge_script_hash, forged_name, 1),
    transition_trace.TransactionsV1RootDomain,
  )
}

test mpf_chunked_challenge_init_rejects_more_than_one_minted_unit() {
  !run_init(
    challenge_output(honest_challenge()),
    assets.from_asset(challenge_script_hash, thread_nonce(), 2),
    transition_trace.TransactionsV1RootDomain,
  )
}

test mpf_chunked_challenge_init_rejects_a_thread_output_off_its_own_address() {
  !run_init(
    Output {
      ..challenge_output(honest_challenge()),
      address: address.from_script(fixture.step_script_hash),
    },
    assets.from_asset(challenge_script_hash, thread_nonce(), 1),
    transition_trace.TransactionsV1RootDomain,
  )
}

test mpf_chunked_challenge_init_rejects_a_thread_output_with_a_reference_script() fail {
  run_init(
    Output {
      ..challenge_output(honest_challenge()),
      reference_script: Some(fixture.step_script_hash),
    },
    assets.from_asset(challenge_script_hash, thread_nonce(), 1),
    transition_trace.TransactionsV1RootDomain,
  )
}

// ## Finalization

/// Honest acceptance: the challenge's proof is reconstructed from the
/// published chunks (here the zero-step proof of a one-leaf trie), the thread
/// token is burned, and the challenge's value goes to `proof_owner`.
test mpf_chunked_challenge_finalizes_a_verified_proof() {
  run_honest_finalize()
}

/// The same finalization driven from a *published chunk*: the absence witness
/// lives in a referenced UTxO's inline datum and reaches the validator only
/// through the redeemer's reference-input index.
test mpf_chunked_challenge_finalizes_a_proof_carried_by_a_published_chunk() {
  let absent_path = blake2b_256(absent_key)
  let challenge = ProofChallengeDatum {
    ..honest_challenge(),
    expected_root: combine(suffix(absent_path, 0), absent_value_hash),
    mode: NonMembership,
  }
  and {
    // The witness is a genuine divergence at the first nibble.
    nibble(blake2b_256(challenge.target_key), 0) != nibble(absent_path, 0),
    run_finalize_with_chunks(
      challenge,
      [
        chunk_reference_input(
          [mpf.Leaf { skip: 0, key: absent_path, value: absent_value_hash }],
        ),
      ],
      [0],
    ),
    // ... and the very same chunk proves nothing about a different root.
    !run_finalize_with_chunks(
      ProofChallengeDatum {
        ..challenge,
        expected_root: blake2b_256(#"0badc0de"),
      },
      [
        chunk_reference_input(
          [mpf.Leaf { skip: 0, key: absent_path, value: absent_value_hash }],
        ),
      ],
      [0],
    ),
  }
}

// ## Thread retirement (the minting-policy side of finalization)

fn run_burn(input: Input, mint: assets.Value) -> Bool {
  main.mint(
    fixture.hub_oracle_script_hash,
    BurnChallenge { challenge_input_index: 0 },
    challenge_script_hash,
    Transaction { ..transaction.placeholder, inputs: [input], mint },
  )
}

test mpf_chunked_challenge_burns_the_thread_token_with_its_own_utxo() {
  run_burn(
    challenge_input(honest_challenge(), thread_nonce()),
    assets.from_asset(challenge_script_hash, thread_nonce(), -1),
  )
}

test mpf_chunked_challenge_burn_rejects_an_input_off_the_thread_address() {
  let held = challenge_input(honest_challenge(), thread_nonce())
  let foreign = Input {
    ..held,
    output: Output {
      ..held.output,
      address: address.from_script(fixture.step_script_hash),
    },
  }
  !run_burn(
    foreign,
    assets.from_asset(challenge_script_hash, thread_nonce(), -1),
  )
}

test mpf_chunked_challenge_burn_rejects_retiring_a_token_it_does_not_hold() {
  !run_burn(
    challenge_input(honest_challenge(), thread_nonce()),
    assets.from_asset(challenge_script_hash, blake2b_256(#"f0f0f0f0"), -1),
  )
}

test mpf_chunked_challenge_burn_rejects_a_quantity_that_is_not_minus_one() {
  !run_burn(
    challenge_input(honest_challenge(), thread_nonce()),
    assets.from_asset(challenge_script_hash, thread_nonce(), -2),
  )
}

test mpf_chunked_challenge_finalize_rejects_an_unburnt_thread_token() fail {
  run_finalize(
    honest_challenge(),
    [],
    [reward_output(fixture.fraud_prover_key_hash, challenge_lovelace)],
    assets.zero,
    [],
  )
}

test mpf_chunked_challenge_finalize_rejects_a_re_minted_thread_token() {
  !run_finalize(
    honest_challenge(),
    [],
    [reward_output(fixture.fraud_prover_key_hash, challenge_lovelace)],
    assets.from_asset(challenge_script_hash, thread_nonce(), 1),
    [],
  )
}

test mpf_chunked_challenge_finalize_rejects_a_reward_paid_elsewhere() {
  !run_finalize(
    honest_challenge(),
    [],
    [reward_output(fixture.step_script_hash, challenge_lovelace)],
    assets.from_asset(challenge_script_hash, thread_nonce(), -1),
    [],
  )
}

test mpf_chunked_challenge_finalize_rejects_an_underpaid_reward() {
  !run_finalize(
    honest_challenge(),
    [],
    [reward_output(fixture.fraud_prover_key_hash, challenge_lovelace - 1)],
    assets.from_asset(challenge_script_hash, thread_nonce(), -1),
    [],
  )
}

test mpf_chunked_challenge_finalize_rejects_a_second_thread_input() {
  let challenge = honest_challenge()
  let duplicate = Input {
    ..challenge_input(challenge, thread_nonce()),
    output_reference: OutputReference {
      transaction_id: blake2b_256(#"beef"),
      output_index: 1,
    },
  }
  !run_finalize(
    challenge,
    [duplicate],
    [reward_output(fixture.fraud_prover_key_hash, challenge_lovelace)],
    assets.from_asset(challenge_script_hash, thread_nonce(), -1),
    [],
  )
}

test mpf_chunked_challenge_finalize_rejects_an_unverified_proof() {
  !run_finalize(
    ProofChallengeDatum {
      ..honest_challenge(),
      target_value_hash: blake2b_256(#"0d1ffabc"),
    },
    [],
    [reward_output(fixture.fraud_prover_key_hash, challenge_lovelace)],
    assets.from_asset(challenge_script_hash, thread_nonce(), -1),
    [],
  )
}
