use aiken/collection/list
use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{Input, OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, finalize}
use midgard/fraud_proofs/field_opening_v1.{
  FieldOpeningV1, WitnessAnchor, address_witnesses_field_index,
  fold_opened_field, opened_field_walk,
}
use midgard/fraud_proofs/missing_signature/step_04.{
  Args, Datum, SpendRedeemer, State,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/components.{
  decode_midgard_address_witness_cbor, encode_midgard_address_witness,
}
use midgard/fraud_proofs/native_tx/types.{
  MidgardAddressWitness, MidgardTransactionWitnessSet, NativeTxWitnessSetCompact,
}
use midgard/native_tx_field_access_v1.{encode_field_preimage}

validator main(
  fraud_proof_token_policy_id: PolicyId,
  fraud_proof_token_address: Address,
  computation_thread_token_policy_id: PolicyId,
  field_preimage_certificate_policy_id: PolicyId,
) {
  spend(
    datum: Option<Datum>,
    redeemer: SpendRedeemer,
    own_out_ref: OutputReference,
    tx: Transaction,
  ) {
    when redeemer is {
      ct.Cancel { input_index, computation_thread_mint_redeemer_index } ->
        cancel(
          computation_thread_token_policy_id,
          datum,
          input_index,
          own_out_ref,
          computation_thread_mint_redeemer_index,
          tx,
        )
      ct.Continue(Args {
        input_index,
        output_index,
        fraud_proof_mint_redeemer_index,
        addr_tx_wits_opening,
      }) -> {
        let Transaction { inputs, outputs, redeemers, reference_inputs, .. } = tx

        expect Some(step_datum) = datum

        // 1. Specified input must be authentic and properly reproduced at the
        //    fraud proof's spending address.
        let
            _own_script_hash,
            _computation_thread_token_asset_name,
            _fraud_prover,
            m_input_state_data
          <- finalize(
          computation_thread_token_policy_id,
          fraud_proof_token_policy_id,
          fraud_proof_token_address,
          step_datum,
          input_index,
          output_index,
          fraud_proof_mint_redeemer_index,
          own_out_ref,
          inputs,
          outputs,
          redeemers,
        )

        expect
            Some(State {
              missing_required_signer_vkey,
              verified_tx_id,
              verified_witness_set_hash,
            })
          = m_input_state_data

        // 2. Field 7's preimage is authenticated through the §8.8 door, which
        //    for a witness-set field also re-derives the supplied witness set
        //    against `verified_witness_set_hash`.
        //
        //    That hash is thread state, not something re-derived here, and the
        //    distinction is the whole security of this step. §3's transaction
        //    id preimage is the *body*: the compact structure's trailing
        //    `witness_set_hash` is outside it, so bytes carrying the genuine
        //    body and an invented tail re-derive to this very
        //    `verified_tx_id`. A door that checked only the id would accept the
        //    empty witness set against any transaction, making "the required
        //    signature is absent" true of every transaction ever committed.
        //    Step-01 read the real value off the block-committed structure and
        //    it has been carried here since.
        let addr_tx_wits_walk = opened_field_walk(
          addr_tx_wits_opening,
          WitnessAnchor {
            tx_id: verified_tx_id,
            witness_set_hash: verified_witness_set_hash,
          },
          address_witnesses_field_index,
          reference_inputs,
          field_preimage_certificate_policy_id,
        )

        // 3. Required signer's verification key must not be in the
        //    address-witness collection. An absence claim is the one shape that
        //    genuinely has to see every item, so it walks the whole field once
        //    — one pass over authenticated bytes, not a reproduction of the
        //    collection followed by a re-hash of it.
        let required_signature_is_present = fold_opened_field(
          addr_tx_wits_walk,
          False,
          fn(found, _index, item) {
            or {
              found,
              decode_midgard_address_witness_cbor(item).verification_key == missing_required_signer_vkey,
            }
          },
        )
        expect required_signature_is_present == False

        // Done.
        True
      }
    }
  }

  else(_) {
    fail
  }
}

fn challenged_block() -> fixture.NativeBlockFixtureV1 {
  fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
}

/// Runs the step with the thread anchored to `anchored_block` while the
/// redeemer's field-7 opening is taken from `opened_block`.
///
/// The two are the same fixture in every selector but the both-sides forgery.
/// Splitting them is what makes that selector expressible at all: it is the only
/// way to hand the step compact bytes and a witness set that agree with each
/// other and disagree with what the thread authenticated.
fn run_finalize_against(
  missing_required_signer_vkey: ByteArray,
  anchored_block: fixture.NativeBlockFixtureV1,
  opened_block: fixture.NativeBlockFixtureV1,
  carried_witnesses: List<MidgardAddressWitness>,
) -> Bool {
  let state = State {
    missing_required_signer_vkey,
    verified_tx_id: anchored_block.tx_id,
    verified_witness_set_hash: anchored_block.compact.witness_set_hash,
  }
  let asset_name = fixture.ct_token_asset_name(anchored_block)
  main.spend(
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(state),
      },
    ),
    ct.Continue(
      Args {
        input_index: 0,
        output_index: 0,
        fraud_proof_mint_redeemer_index: 0,
        addr_tx_wits_opening: fixture.inline_witness_opening(
          opened_block.compact_cbor,
          opened_block.witness_set_compact,
          carried_witnesses |> list.map(encode_midgard_address_witness),
        ),
      },
    ),
    fixture.own_out_ref(),
    fixture.finalize_step_l1_tx_v1(asset_name),
  )
}

/// The same step, run against an arbitrary field-7 opening and the L1
/// reference inputs the carriage it names lives in.
///
/// `run_finalize_against` builds a tier-1 opening because that is what every
/// selector above needs; this is the seam the tier-3 selector needs, and it is
/// separate rather than a widened parameter list so those selectors keep reading
/// as statements about witness sets rather than about carriage.
fn run_finalize_with_opening(
  missing_required_signer_vkey: ByteArray,
  anchored_block: fixture.NativeBlockFixtureV1,
  addr_tx_wits_opening: FieldOpeningV1,
  reference_inputs: List<Input>,
) -> Bool {
  let state = State {
    missing_required_signer_vkey,
    verified_tx_id: anchored_block.tx_id,
    verified_witness_set_hash: anchored_block.compact.witness_set_hash,
  }
  let asset_name = fixture.ct_token_asset_name(anchored_block)
  main.spend(
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(state),
      },
    ),
    ct.Continue(
      Args {
        input_index: 0,
        output_index: 0,
        fraud_proof_mint_redeemer_index: 0,
        addr_tx_wits_opening,
      },
    ),
    fixture.own_out_ref(),
    fixture.finalize_step_l1_tx_with_reference_inputs_v1(
      asset_name,
      reference_inputs,
    ),
  )
}

fn run_finalize_with(
  missing_required_signer_vkey: ByteArray,
  witness_set: NativeTxWitnessSetCompact,
  carried_witnesses: List<MidgardAddressWitness>,
) -> Bool {
  let block = challenged_block()
  run_finalize_against(
    missing_required_signer_vkey,
    block,
    fixture.NativeBlockFixtureV1 { ..block, witness_set_compact: witness_set },
    carried_witnesses,
  )
}

fn run_finalize(missing_required_signer_vkey: ByteArray) -> Bool {
  let block = challenged_block()
  run_finalize_with(
    missing_required_signer_vkey,
    block.witness_set_compact,
    block.tx.witness_set.addr_tx_wits,
  )
}

/// The authenticated field-7 preimage of the committed transaction proves the
/// required signer's key is absent, finalizing the proof.
test missing_signature_step_04_accepts_absent_signer() {
  run_finalize(
    #"7070707070707070707070707070707070707070707070707070707070707070",
  )
}

/// Valid-block negative selector: the required signer's key is present in the
/// transaction's canonical address-witness collection, so the challenge
/// cannot finalize.
test missing_signature_step_04_rejects_valid_block_challenge() fail {
  run_finalize(fixture.sample_witness_verification_key())
}

/// The absence claim is only as good as the preimage it walks: a truncated
/// collection would make any key look absent, so the door's authenticate-once
/// hash check is what stands between this rule and a trivially satisfiable one.
test missing_signature_step_04_rejects_a_truncated_witness_preimage() fail {
  let block = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  run_finalize_with(
    fixture.sample_witness_verification_key(),
    block.witness_set_compact,
    [],
  )
}

/// The witness set is authenticated too: a forged `addr_tx_wits_hash` does not
/// re-derive to the anchored `witness_set_hash`, so the door refuses before the
/// walk starts. This selector forges **one** side — the honest compact bytes are
/// left in place, so the mismatch is visible to any check at all.
test missing_signature_step_04_rejects_a_forged_witness_set() fail {
  let block = challenged_block()
  let forged = NativeTxWitnessSetCompact {
    ..block.witness_set_compact,
    addr_tx_wits_hash: #"deadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef",
  }
  run_finalize_with(
    #"7070707070707070707070707070707070707070707070707070707070707070",
    forged,
    block.tx.witness_set.addr_tx_wits,
  )
}

/// **Both sides forged — the slashing forgery.** Forging only the witness set
/// is caught by arithmetic; the attack that matters forges the compact
/// structure to match it.
///
/// §3's transaction id commits the body and nothing else, so a prover takes the
/// committed transaction's genuine body bytes, appends the `witness_set_hash` of
/// an **empty** witness set, and supplies that empty witness set together with
/// an empty field-7 preimage. The compact bytes re-derive to this very
/// `verified_tx_id`; the witness set re-derives to the `witness_set_hash` those
/// bytes carry; the preimage re-derives to the `addr_tx_wits_hash` that witness
/// set carries. Every check that starts from the transaction id is satisfied,
/// `field_item_count` is 0, the walk completes without visiting anything, and
/// "the required signature is absent" is proved against a transaction that
/// contains it — a fraud proof that slashes an honest operator.
///
/// What refuses it is the thread-anchored `verified_witness_set_hash`, which
/// step-01 read off the block-committed compact structure and which no redeemer
/// can restate. Neutralise that one `expect` in `anchored_native_tx` and this test
/// passes.
test missing_signature_step_04_rejects_a_forged_witness_set_and_compact() fail {
  let block = challenged_block()
  run_finalize_against(
    // The key IS in the committed witness set — the fixture's own signer.
    fixture.sample_witness_verification_key(),
    block,
    fixture.forged_witness_set_block_v1(
      block,
      fixture.empty_witness_set_compact(),
    ),
    [],
  )
}

/// The same forgery with a *substituted* rather than emptied witness set: the
/// prover keeps a witness collection of the right shape but drops the one key
/// it wants declared missing. It fails for the same reason, which is the point
/// — the guard is on the anchor, not on the collection being empty.
test missing_signature_step_04_rejects_a_forged_substituted_witness_set() fail {
  let block = challenged_block()
  let without_the_signer = MidgardTransactionWitnessSet {
    ..block.tx.witness_set,
    addr_tx_wits: [],
  }
  run_finalize_against(
    fixture.sample_witness_verification_key(),
    block,
    fixture.forged_witness_set_block_v1(
      block,
      fixture.witness_set_compact_v1(without_the_signer),
    ),
    [],
  )
}

/// A witness the committed transaction does **not** carry: same shape, other
/// key. 256 of these is what a prover would publish to make the fixture's own
/// required signer look absent.
fn decoy_address_witness() -> MidgardAddressWitness {
  MidgardAddressWitness {
    ..fixture.sample_address_witness(),
    verification_key: #"9090909090909090909090909090909090909090909090909090909090909090",
  }
}

/// 256 decoys — 26,371 bytes at the §5.3 address-witness stride, which is over
/// `chunk_bytes_k` and therefore admissible tier-3 carriage rather than a
/// tier-1 preimage wearing a certificate.
fn tier_three_decoy_preimage() -> ByteArray {
  encode_field_preimage(
    list.repeat(encode_midgard_address_witness(decoy_address_witness()), 256),
  )
}

/// **The tier-3 forgery, at the step.** Everything the forged-witness-set
/// selectors above establish is satisfied here on its own terms: the compact
/// bytes are the committed transaction's, the witness set is the committed
/// one, and the anchored `witness_set_hash` matches. The fraud is in the
/// *carriage*.
///
/// Under tier 3 the door has no preimage to hash — that is what tier 3 is for —
/// and the binding is a §8.6 certificate instead. That certificate is minted
/// against a `witness_set_hash` taken off the trailing bytes of the *minter's
/// own* redeemer, and §3's transaction id does not commit those bytes, so a
/// certifier can name this very transaction while certifying a field 7 it never
/// had. The 256 decoys below do not contain the fixture's required signer, so
/// the fold reports absence and this step finalizes a slashing proof against an
/// honest operator.
///
/// `field_opening_v1.carriage_reaches_the_anchor` refuses tier-3 carriage of a
/// witness-set field, and that is the only thing standing here. Neutralise it
/// and this test passes — which is how the exposure was reproduced in the first
/// place. §8.3 erratum E2 limit 3 records the cost; #606 owns the repair
/// (re-pointed from #579 via #604 by owner ruling, 2026-08-14 — see E2's
/// disposition paragraph for why the repair is a regeneration event).
test missing_signature_step_04_rejects_certified_carriage() fail {
  let block = challenged_block()
  let preimage = tier_three_decoy_preimage()
  run_finalize_with_opening(
    // The key IS in the committed witness set — the fixture's own signer.
    fixture.sample_witness_verification_key(),
    block,
    fixture.certified_witness_opening(
      block.compact_cbor,
      block.witness_set_compact,
      preimage,
    ),
    fixture.certified_reference_inputs_v1(
      block.tx_id,
      address_witnesses_field_index,
      preimage,
    ),
  )
}
