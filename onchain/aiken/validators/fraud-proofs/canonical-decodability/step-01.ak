//// ## Canonical-Decodability Fraud Proof — step 01 (`docs/spec/midgard-tx.md` §12.7)
////
//// Binds the disputed transaction to the block's committed `transactions_root`,
//// authenticates the **committed** preimage of the accused field against §4's
//// positionally-extracted commitment, and forwards the §5.1 verdict that
//// step 02 adjudicates.
////
//// The bytes are the prover's to carry and the block's to own: whichever §8
//// tier they arrive on, `authenticated_committed_preimage` hashes them against
//// the commitment this transaction's own committed compact structure carries,
//// so a prover supplying anything else fails here (§7.3) and what reaches the
//// verdict is undeniably what the operator committed.
////
//// Nothing on this path decodes the field. See
//// `lib/midgard/fraud-proofs/canonical-decodability/rule.ak` for the rule, the
//// violation and the total-verdict argument, and §12.7 for the boundary against
//// §12.3's `fault_item_predicate`.

use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/canonical_decodability/rule.{
  BodyFieldClaim, CommittedFieldClaimV1, CommittedFieldVerdictV1,
  WitnessFieldClaim, committed_field_verdict_v1, envelope_verdict_v1,
  miscounted_field_preimage_v1, verdict_grammatical,
  verdict_missing_array_header, verdict_missing_item_header,
  verdict_non_minimal_item_header, verdict_trailing_bytes,
  verdict_truncated_item_payload,
}
use midgard/fraud_proofs/canonical_decodability/step_01.{
  Args, Datum, SpendRedeemer,
}
use midgard/fraud_proofs/canonical_decodability/step_02
use midgard/fraud_proofs/common.{cancel, pass_native_tx_to_next_step_carried}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/compact.{verify_native_tx_compact_cbor_v1}
use midgard/native_tx_field_access_v1.{
  Inline, authenticated_whole_field_view, encode_field_preimage,
}

validator main(
  step_02_validator_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
  hub_oracle: ScriptHash,
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
      ct.Continue(Args { inclusion, claim }) -> {
        let Transaction { reference_inputs, .. } = tx

        // 1. The disputed transaction must be one this block genuinely
        //    committed, under the block registered in the state queue for this
        //    computation thread. Unlike `da-hash-preimage`, this family *does*
        //    run the native codec precondition: a leaf whose key is not its own
        //    id is that family's fault, not this one's, and the compact
        //    structure this step reads a field commitment out of only means
        //    anything once the id derived from it is the committed key.
        let
            _own_script_hash,
            _computation_thread_token_asset_name,
            _fraud_prover,
            _m_input_state_data,
            output_script_hash,
            output_state_data,
            _header,
            bad_tx_id,
            verified
          <- pass_native_tx_to_next_step_carried(
          computation_thread_token_policy_id,
          hub_oracle,
          datum,
          inclusion,
          own_out_ref,
          tx,
        )

        // 2. The UTxO must be produced at the next step's script hash carrying
        //    the exact accusation. Both members of it are derived here — the
        //    field index from the claim whose bytes the door then held to that
        //    slot's commitment, the verdict from the authenticated bytes
        //    themselves — so step 02 adjudicates committed values only and a
        //    fabricated verdict cannot be forwarded.
        expect output_script_hash == step_02_validator_script_hash
        let CommittedFieldVerdictV1 { field_index, verdict } = committed_field_verdict_v1(
          verified,
          claim,
          reference_inputs,
          field_preimage_certificate_policy_id,
        )
        let expected_output_state: Data = step_02.State {
          bad_tx_id,
          field_index,
          verdict,
        }
        expect output_state_data == expected_output_state

        // Done.
        True
      }
    }
  }

  else(_) {
    fail
  }
}

// ## Committed-preimage fixtures
//
// Every selector below commits one field of an otherwise honest transaction to
// bytes the encoders would never produce, which is the shape §12.7 exists for
// and which `committed_field_preimage_block_fixture_v1` is the only builder of.

/// §2.5 slot 2 (outputs) — a variable-width body field.
const outputs_field_index: Int = 2

/// §2.5 slot 6 (script witnesses) — a witness-set field, reachable only once the
/// supplied witness set re-derives to the committed `witness_set_hash`.
const script_witnesses_field_index: Int = 6

/// A §5.1 envelope whose header declares one item while its body carries two:
/// the walk finishes its declared items with bytes left over.
fn miscounted_preimage() -> ByteArray {
  miscounted_field_preimage_v1(1, [#"deadbeef", #"beef"])
}

/// A genuine §5.1 envelope over one item — the honest shape, for the negatives.
fn grammatical_preimage() -> ByteArray {
  encode_field_preimage([#"deadbeef"])
}

fn committed_block(
  field_index: Int,
  preimage: ByteArray,
) -> fixture.NativeBlockFixtureV1 {
  fixture.committed_field_preimage_block_fixture_v1(
    fixture.native_block_fixture_v1(fixture.valid_native_tx_v1()),
    field_index,
    preimage,
  )
}

fn body_claim(field_index: Int, preimage: ByteArray) -> CommittedFieldClaimV1 {
  BodyFieldClaim { field_index, carriage: Inline { preimage } }
}

fn witness_claim(
  block: fixture.NativeBlockFixtureV1,
  field_index: Int,
  preimage: ByteArray,
) -> CommittedFieldClaimV1 {
  WitnessFieldClaim {
    field_index,
    witness_set: block.witness_set_compact,
    carriage: Inline { preimage },
  }
}

fn expected_state(
  block: fixture.NativeBlockFixtureV1,
  field_index: Int,
  verdict: Int,
) -> Data {
  let state: step_02.State = step_02.State {
    bad_tx_id: block.tx_id,
    field_index,
    verdict,
  }
  let state_data: Data = state
  state_data
}

fn run_step_01(
  block: fixture.NativeBlockFixtureV1,
  claim: CommittedFieldClaimV1,
  output_state: Data,
) -> Bool {
  run_step_01_at(fixture.next_step_script_hash, block, claim, output_state)
}

/// `run_step_01` with the step-02 script hash the instance was parameterised
/// with left free, so a selector can show where the derived accusation is
/// allowed to land.
fn run_step_01_at(
  step_02_validator_script_hash: ScriptHash,
  block: fixture.NativeBlockFixtureV1,
  claim: CommittedFieldClaimV1,
  output_state: Data,
) -> Bool {
  main.spend(
    step_02_validator_script_hash,
    fixture.computation_thread_policy_id,
    fixture.hub_oracle_script_hash,
    fixture.test_certificate_policy_id,
    Some(fixture.step_datum_without_state()),
    ct.Continue(
      Args { inclusion: fixture.native_inclusion_carriage_v1(block), claim },
    ),
    fixture.own_out_ref(),
    fixture.continue_step_l1_tx_v1(
      block,
      output_state,
      block.transactions_phas_root,
      block.tx_id,
      block.compact_cbor,
    ),
  )
}

// ## Step-01 binding selectors

/// Positive binding selector: a block that commits a miscounted envelope at
/// field 2 binds, and the forwarded verdict names the way §5.1 was left.
test canonical_decodability_step_01_binds_a_miscounted_body_field() {
  let preimage = miscounted_preimage()
  let block = committed_block(outputs_field_index, preimage)
  run_step_01(
    block,
    body_claim(outputs_field_index, preimage),
    expected_state(block, outputs_field_index, verdict_trailing_bytes),
  )
}

/// **The stall this family removes, demonstrated at the seam.** The very same
/// committed field, opened through the §8.8 whole-materialising door, aborts —
/// so no `CanonicalDecode` step is producible for it by anyone and the dispute
/// stalls instead of rejecting. The selector above convicts it, and the door's
/// abort semantics are untouched (§12.7).
test canonical_decodability_step_01_the_door_cannot_open_the_convicted_field() fail {
  let preimage = miscounted_preimage()
  let block = committed_block(outputs_field_index, preimage)
  let view = authenticated_whole_field_view(
    verify_native_tx_compact_cbor_v1(block.tx_id, block.compact_cbor),
    block.witness_set_compact,
    outputs_field_index,
    Inline { preimage },
    [],
    fixture.test_certificate_policy_id,
  )
  view == view
}

/// The companion positive: the same door opens the same field of the same block
/// once its committed preimage is a §5.1 envelope, so the `fail` above traps
/// the grammar and not the fixture.
test canonical_decodability_step_01_the_door_opens_a_grammatical_field() {
  let preimage = grammatical_preimage()
  let block = committed_block(outputs_field_index, preimage)
  let view = authenticated_whole_field_view(
    verify_native_tx_compact_cbor_v1(block.tx_id, block.compact_cbor),
    block.witness_set_compact,
    outputs_field_index,
    Inline { preimage },
    [],
    fixture.test_certificate_policy_id,
  )
  view == view
}

/// Positive binding selector: the empty committed preimage. `#""` is the one
/// byte string shorter than §5.1's shortest admissible form (`80`), and an
/// operator can commit its hash as easily as any other.
test canonical_decodability_step_01_binds_an_empty_committed_preimage() {
  let block = committed_block(outputs_field_index, #"")
  run_step_01(
    block,
    body_claim(outputs_field_index, #""),
    expected_state(block, outputs_field_index, verdict_missing_array_header),
  )
}

/// Positive binding selector at a **witness-set** field: reachable only because
/// the supplied witness set re-derives to the `witness_set_hash` the committed
/// compact structure carries, which is what the door checks before it looks at
/// any carriage.
test canonical_decodability_step_01_binds_a_witness_set_field() {
  let preimage = miscounted_field_preimage_v1(1, [])
  let block = committed_block(script_witnesses_field_index, preimage)
  run_step_01(
    block,
    witness_claim(block, script_witnesses_field_index, preimage),
    expected_state(
      block,
      script_witnesses_field_index,
      verdict_missing_item_header,
    ),
  )
}

/// Negative selector: an honest block. The committed preimage is a §5.1
/// envelope, so the forwarded verdict is `verdict_grammatical` and step 02
/// cannot finalize. Step 01 itself is evidence carriage and binds it without
/// convicting, exactly as `da-hash-preimage`'s does.
test canonical_decodability_step_01_binds_a_grammatical_field_without_convicting() {
  let preimage = grammatical_preimage()
  let block = committed_block(outputs_field_index, preimage)
  run_step_01(
    block,
    body_claim(outputs_field_index, preimage),
    expected_state(block, outputs_field_index, verdict_grammatical),
  )
}

/// **The custody chain from derivation to adjudication.** The produced UTxO
/// must sit at step 02's script and nowhere else: run the very transaction the
/// positive selector binds against an instance parameterised with any other
/// hash and it refuses, so the thread token carrying a derived accusation
/// cannot be routed to a script that would settle it on other terms.
test canonical_decodability_step_01_rejects_a_foreign_next_step_hash() fail {
  let preimage = miscounted_preimage()
  let block = committed_block(outputs_field_index, preimage)
  run_step_01_at(
    fixture.step_script_hash,
    block,
    body_claim(outputs_field_index, preimage),
    expected_state(block, outputs_field_index, verdict_trailing_bytes),
  )
}

/// A prover cannot forward a fabricated verdict: the output state is recomputed
/// from the authenticated bytes.
test canonical_decodability_step_01_rejects_a_fabricated_verdict() fail {
  let preimage = grammatical_preimage()
  let block = committed_block(outputs_field_index, preimage)
  run_step_01(
    block,
    body_claim(outputs_field_index, preimage),
    expected_state(block, outputs_field_index, verdict_trailing_bytes),
  )
}

/// A prover cannot forward a field index other than the one it opened, so the
/// accusation cannot be re-addressed after the fact.
test canonical_decodability_step_01_rejects_a_fabricated_field_index() fail {
  let preimage = miscounted_preimage()
  let block = committed_block(outputs_field_index, preimage)
  run_step_01(
    block,
    body_claim(outputs_field_index, preimage),
    expected_state(block, 1, verdict_trailing_bytes),
  )
}

/// **The §4 hash check is what makes the bytes evidence.** A prover supplying
/// ungrammatical bytes the block never committed — against an honest field —
/// aborts rather than convicting, which is §7.3 and is the reason the door's
/// abort semantics did not have to be softened for this family to exist.
test canonical_decodability_step_01_rejects_uncommitted_bytes() fail {
  let block = committed_block(outputs_field_index, grammatical_preimage())
  run_step_01(
    block,
    body_claim(outputs_field_index, miscounted_preimage()),
    expected_state(block, outputs_field_index, verdict_trailing_bytes),
  )
}

/// A body claim may not name a witness-set slot: the door would otherwise be
/// handed the rule module's unread placeholder as if it were the transaction's
/// witness set.
test canonical_decodability_step_01_rejects_a_body_claim_at_a_witness_field() fail {
  let preimage = miscounted_preimage()
  let block = committed_block(script_witnesses_field_index, preimage)
  run_step_01(
    block,
    body_claim(script_witnesses_field_index, preimage),
    expected_state(block, script_witnesses_field_index, verdict_trailing_bytes),
  )
}

/// And a witness claim may not name a body slot, where the witness set it
/// carries would be silently discarded.
test canonical_decodability_step_01_rejects_a_witness_claim_at_a_body_field() fail {
  let preimage = miscounted_preimage()
  let block = committed_block(outputs_field_index, preimage)
  run_step_01(
    block,
    witness_claim(block, outputs_field_index, preimage),
    expected_state(block, outputs_field_index, verdict_trailing_bytes),
  )
}

/// Valid-block negative selector: evidence manufactured under a raw
/// transactions root the header never committed cannot bind, so a valid block
/// cannot be challenged with a fabricated field commitment.
test canonical_decodability_step_01_rejects_a_forged_transactions_root() fail {
  let preimage = miscounted_preimage()
  let valid_block = fixture.native_block_fixture_v1(
    fixture.valid_native_tx_v1(),
  )
  let forged_block = committed_block(outputs_field_index, preimage)
  let claimed_block = fixture.NativeBlockFixtureV1 {
    ..forged_block,
    header: valid_block.header,
    header_hash: valid_block.header_hash,
  }
  run_step_01(
    claimed_block,
    body_claim(outputs_field_index, preimage),
    expected_state(claimed_block, outputs_field_index, verdict_trailing_bytes),
  )
}

// ## Tier-3 carriage
//
// §8.4's chunked route is admissible here at every field, and the argument is
// `authenticated_whole_field_view`'s rather than the lazy door's: the chunks are
// concatenated and hashed once against the committed commitment, so the
// manifest authenticates nothing about content and a substituted chunk changes
// the hash. Without this tier the fault kind would stop at whatever fits one
// redeemer — a smaller escape hatch, but an escape hatch.

/// Just above §8.3's `K`, so tier 3 is the only admissible carriage and the
/// §8.4 partition is genuinely exercised rather than assumed.
const certified_preimage_bytes: Int = 15_200

/// Positive selector on §8.4's chunked route: the family reaches committed
/// bytes no redeemer could carry.
test canonical_decodability_step_01_binds_a_certified_carriage() {
  let preimage = fixture.ungrammatical_trailing_preimage_v1(
    certified_preimage_bytes,
  )
  let block = committed_block(outputs_field_index, preimage)
  let output_state = expected_state(
    block,
    outputs_field_index,
    verdict_trailing_bytes,
  )
  main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.hub_oracle_script_hash,
    fixture.test_certificate_policy_id,
    Some(fixture.step_datum_without_state()),
    ct.Continue(
      Args {
        inclusion: fixture.native_inclusion_carriage_v1(block),
        claim: BodyFieldClaim {
          field_index: outputs_field_index,
          carriage: fixture.certified_carriage_from_v1(2, preimage),
        },
      },
    ),
    fixture.own_out_ref(),
    fixture.continue_step_l1_tx_with_carriage_v1(
      block,
      output_state,
      fixture.certified_reference_inputs_v1(
        block.tx_id,
        outputs_field_index,
        preimage,
      ),
    ),
  )
}

/// The verdicts this validator's fixtures earn, pinned against the rule so a
/// selector cannot go green on a shape it stopped exercising.
test canonical_decodability_step_01_verdicts_match_the_committed_shapes() {
  and {
    envelope_verdict_v1(grammatical_preimage()) == verdict_grammatical,
    envelope_verdict_v1(miscounted_preimage()) == verdict_trailing_bytes,
    envelope_verdict_v1(miscounted_field_preimage_v1(1, [])) == verdict_missing_item_header,
    envelope_verdict_v1(#"") == verdict_missing_array_header,
    envelope_verdict_v1(#"815805") == verdict_non_minimal_item_header,
    envelope_verdict_v1(#"8142ff") == verdict_truncated_item_payload,
    envelope_verdict_v1(
      fixture.ungrammatical_trailing_preimage_v1(certified_preimage_bytes),
    ) == verdict_trailing_bytes,
  }
}
