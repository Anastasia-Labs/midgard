use aiken/collection/list
use aiken/primitive/bytearray
use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, finalize}
use midgard/fraud_proofs/field_opening_v1.{
  WitnessAnchor, fold_opened_field, opened_field_walk,
  script_witnesses_field_index,
}
use midgard/fraud_proofs/missing_native_script_tx/step_06.{
  Args, Datum, SpendRedeemer, State,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/components.{
  decode_midgard_versioned_script_at, encode_midgard_versioned_script,
}
use midgard/fraud_proofs/native_tx/types.{
  MidgardTransactionWitnessSet, MidgardVersionedScript, NativeCardanoScript,
  NativeTxWitnessSetCompact,
}
use midgard/script_proof_v1

validator main(
  computation_thread_token_policy_id: PolicyId,
  fraud_proof_token_policy_id: PolicyId,
  fraud_proof_token_address: Address,
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
        script_tx_wits_opening,
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
              expected_missing_script_hash,
              bad_tx_id,
              bad_tx_witness_set_hash,
            })
          = m_input_state_data

        // 2. Field 6's preimage is authenticated through the §8.8 door, which
        //    for a witness-set field also re-derives the supplied witness set
        //    against `bad_tx_witness_set_hash`.
        //
        //    That hash is thread state, carried from step-01, and it is what
        //    makes this step's absence claim mean anything. §3's transaction id
        //    preimage is the *body*: the compact structure's trailing
        //    `witness_set_hash` is outside it, so bytes carrying the genuine
        //    body and an invented tail re-derive to this very `bad_tx_id`. A
        //    door that checked only the id would accept the empty witness set
        //    against any transaction, making "the required native script is
        //    absent" true of every transaction ever committed.
        //
        //    **Carriage limit (§8, issue #565).** Field 6 is variable-width, so
        //    step 3 below needs `field_item_count`, which is only authenticated
        //    for tiers 1–2 — under tier 3 (Certified) a variable-width field's
        //    count is the §5.1 header's self-assertion and
        //    `field_item_count` aborts rather than return it. A prover whose
        //    field-6 preimage does not fit tier-2 carriage therefore cannot
        //    finalize this family at all. Since #575's round-2 review that is
        //    over-determined: a witness-set field is refused tier-3 carriage
        //    outright at `field_opening_v1.carriage_reaches_the_anchor`,
        //    because a §8.6 certificate cannot be bound to a transaction id
        //    that does not commit the witness set. These are *known, loud*
        //    limitations, not silent clamps: both aborts are unconditional and
        //    both are recorded in `docs/spec/midgard-tx.md` §8.3 erratum E2, as
        //    limits 2 and 3. Lifting them is #565/#579's, not this step's.
        let script_tx_wits_walk = opened_field_walk(
          script_tx_wits_opening,
          WitnessAnchor {
            tx_id: bad_tx_id,
            witness_set_hash: bad_tx_witness_set_hash,
          },
          script_witnesses_field_index,
          reference_inputs,
          field_preimage_certificate_policy_id,
        )

        // 3. The bad transaction's script witnesses must not contain the
        //    required native script (canonical versioned-script hash). Field 6
        //    is variable-width, so the absence claim walks it once rather than
        //    indexing it — indexing would re-walk from item 0 every time.
        let required_script_is_present = fold_opened_field(
          script_tx_wits_walk,
          False,
          fn(found, _index, item) {
            let Pair(_offset, script_wit) = decode_midgard_versioned_script_at(
              item,
              0,
            )
            // §6.1 canonicality, re-established by re-encoding. The decoder
            // reads a *prefix*: it stops at the end of the versioned-script
            // structure and says nothing about bytes after it, and
            // `decode_definite_bytes_at` accepts a non-minimal length prefix
            // (`58 05` where `45` was canonical). Either would let two distinct
            // committed items decode to the same script, so the hash below
            // would be a statement about something the field did not commit.
            // Re-encoding pins both at once.
            expect encode_midgard_versioned_script(script_wit) == item
            or {
              found,
              script_proof_v1.versioned_script_hash(script_wit) == expected_missing_script_hash,
            }
          },
        )
        expect required_script_is_present == False

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
/// redeemer's field-6 opening is taken from `opened_block`. They are the same
/// fixture in every selector but the both-sides forgery.
fn run_finalize_against(
  expected_missing_script_hash: ByteArray,
  anchored_block: fixture.NativeBlockFixtureV1,
  opened_block: fixture.NativeBlockFixtureV1,
  carried_preimage_items: List<ByteArray>,
) -> Bool {
  let state = State {
    expected_missing_script_hash,
    bad_tx_id: anchored_block.tx_id,
    bad_tx_witness_set_hash: anchored_block.compact.witness_set_hash,
  }
  let asset_name = fixture.ct_token_asset_name(anchored_block)
  main.spend(
    fixture.computation_thread_policy_id,
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
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
        script_tx_wits_opening: fixture.inline_witness_opening(
          opened_block.compact_cbor,
          opened_block.witness_set_compact,
          carried_preimage_items,
        ),
      },
    ),
    fixture.own_out_ref(),
    fixture.finalize_step_l1_tx_v1(asset_name),
  )
}

fn run_finalize_with(
  expected_missing_script_hash: ByteArray,
  witness_set: NativeTxWitnessSetCompact,
  carried_scripts: List<MidgardVersionedScript>,
) -> Bool {
  let block = challenged_block()
  run_finalize_against(
    expected_missing_script_hash,
    block,
    fixture.NativeBlockFixtureV1 { ..block, witness_set_compact: witness_set },
    carried_scripts |> list.map(encode_midgard_versioned_script),
  )
}

fn run_finalize(expected_missing_script_hash: ByteArray) -> Bool {
  let block = challenged_block()
  run_finalize_with(
    expected_missing_script_hash,
    block.witness_set_compact,
    block.tx.witness_set.script_tx_wits,
  )
}

/// The authenticated field-6 preimage of the committed transaction proves the
/// required native script is absent, finalizing the proof.
test missing_native_script_tx_step_06_accepts_absent_script() {
  run_finalize(
    script_proof_v1.versioned_script_hash(
      MidgardVersionedScript {
        language: NativeCardanoScript,
        script_bytes: #"8201818200581c30303030303030303030303030303030303030303030",
      },
    ),
  )
}

/// Valid-block negative selector: the required native script is present in the
/// transaction's canonical script-witness collection, so the challenge cannot
/// finalize.
test missing_native_script_tx_step_06_rejects_valid_block_challenge() fail {
  run_finalize(
    script_proof_v1.versioned_script_hash(
      fixture.sample_native_script_witness(),
    ),
  )
}

/// The absence claim is only as good as the preimage it walks: a truncated
/// collection would make any script look absent, so the door's
/// authenticate-once hash check is what stands between this rule and a
/// trivially satisfiable one.
test missing_native_script_tx_step_06_rejects_a_truncated_preimage() fail {
  let block = challenged_block()
  run_finalize_with(
    script_proof_v1.versioned_script_hash(
      fixture.sample_native_script_witness(),
    ),
    block.witness_set_compact,
    [],
  )
}

/// The witness set is authenticated too: a forged `script_tx_wits_hash` does
/// not re-derive to the anchored `witness_set_hash`, so the door refuses before
/// the walk starts. This selector forges **one** side, leaving the honest
/// compact bytes in place.
test missing_native_script_tx_step_06_rejects_a_forged_witness_set() fail {
  let block = challenged_block()
  let forged = NativeTxWitnessSetCompact {
    ..block.witness_set_compact,
    script_tx_wits_hash: #"deadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef",
  }
  run_finalize_with(
    script_proof_v1.versioned_script_hash(
      MidgardVersionedScript {
        language: NativeCardanoScript,
        script_bytes: #"8201818200581c30303030303030303030303030303030303030303030",
      },
    ),
    forged,
    block.tx.witness_set.script_tx_wits,
  )
}

/// **Both sides forged — the slashing forgery.** Forging only the witness set
/// is caught by arithmetic; the attack that matters forges the compact
/// structure to match it.
///
/// §3's transaction id commits the body and nothing else, so the prover takes
/// the committed transaction's genuine body bytes, appends the
/// `witness_set_hash` of an **empty** witness set, and supplies that empty
/// witness set with an empty field-6 preimage. The compact bytes re-derive to
/// this very `bad_tx_id`; the witness set re-derives to the `witness_set_hash`
/// those bytes carry; the preimage re-derives to the `script_tx_wits_hash` that
/// witness set carries. `field_item_count` is 0, the walk completes without
/// visiting anything, and "the required native script is absent" is proved
/// against a transaction that carries it.
///
/// What refuses it is the thread-anchored `bad_tx_witness_set_hash`. Neutralise
/// that one `expect` in `resolved_sources` and this test passes.
test missing_native_script_tx_step_06_rejects_a_forged_witness_set_and_compact() fail {
  let block = challenged_block()
  run_finalize_against(
    // The script IS in the committed witness set — the fixture's own.
    script_proof_v1.versioned_script_hash(
      fixture.sample_native_script_witness(),
    ),
    block,
    fixture.forged_witness_set_block_v1(
      block,
      fixture.empty_witness_set_compact(),
    ),
    [],
  )
}

/// The same forgery with a witness set that keeps a script collection but drops
/// the one script being declared missing. It fails for the same reason: the
/// guard is on the anchor, not on the collection being empty.
test missing_native_script_tx_step_06_rejects_a_forged_substituted_witness_set() fail {
  let block = challenged_block()
  let without_the_script = MidgardTransactionWitnessSet {
    ..block.tx.witness_set,
    script_tx_wits: [],
  }
  run_finalize_against(
    script_proof_v1.versioned_script_hash(
      fixture.sample_native_script_witness(),
    ),
    block,
    fixture.forged_witness_set_block_v1(
      block,
      fixture.witness_set_compact_v1(without_the_script),
    ),
    [],
  )
}

/// §6.1 canonicality of the committed item.
///
/// Both the thread's anchor and the redeemer's opening are taken from the same
/// block here, so every binding check above passes: this preimage genuinely is
/// what that block's `script_tx_wits_hash` commits. The one defect is inside an
/// item — a byte of trailing junk after its versioned-script structure. The
/// decoder reads a prefix and would walk straight past it, hashing a script the
/// committed item does not equal; the re-encode inside the fold refuses. Without
/// that re-encode the step returns `True`, because the hash it computes is not
/// the one being looked for.
test missing_native_script_tx_step_06_rejects_a_non_canonical_item() fail {
  let honest = fixture.sample_native_script_witness()
  let with_trailing_junk = encode_midgard_versioned_script(honest)
    |> bytearray.concat(#"ff")
  let block = challenged_block()
  // A block whose field-6 collection is the single junk-carrying item, and
  // whose compact structure commits the witness set that commits it.
  let junk_block = fixture.forged_witness_set_block_v1(
    block,
    fixture.witness_set_compact_of_items([], [with_trailing_junk], []),
  )
  run_finalize_against(
    // Deliberately *not* the junk item's script: with the canonicality check
    // removed the fold finds nothing and the step succeeds.
    script_proof_v1.versioned_script_hash(
      MidgardVersionedScript {
        language: NativeCardanoScript,
        script_bytes: #"8201818200581c30303030303030303030303030303030303030303030",
      },
    ),
    junk_block,
    junk_block,
    [with_trailing_junk],
  )
}
