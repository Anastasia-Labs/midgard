//// ## Committed-Field-Shape Fraud Proof — step 01 (`docs/spec/midgard-tx.md` §12.8)
////
//// Binds the disputed transaction to the block's committed `transactions_root`,
//// authenticates the **committed** preimage of the accused field against §4's
//// positionally-extracted commitment, and forwards the slot's §7.4/§5.4 shape
//// verdict that step 02 adjudicates.
////
//// The bytes are the prover's to carry and the block's to own: whichever §8
//// tier they arrive on, `authenticated_committed_preimage` hashes them against
//// the commitment this transaction's own committed compact structure carries,
//// so a prover supplying anything else fails here (§7.3) and what reaches the
//// verdict is undeniably what the operator committed.
////
//// Nothing on this path decodes the field beyond §5.1's header. See
//// `lib/midgard/fraud-proofs/committed-field-shape/rule.ak` for the rule, the
//// violation and the totality argument, and §12.8 for the boundary against both
//// §12.7's bytes-only verdict and §12.3's `fault_item_predicate`.

use aiken/builtin
use aiken/crypto.{ScriptHash}
use cardano/address
use cardano/assets.{PolicyId}
use cardano/transaction.{
  InlineDatum, Input, Output, OutputReference, Transaction,
}
use midgard/computation_thread as ct
use midgard/fraud_proofs/canonical_decodability/rule.{
  BodyFieldClaim, CommittedFieldClaimV1, WitnessFieldClaim,
} as canonical_decodability_rule
use midgard/fraud_proofs/committed_field_shape/rule.{
  CommittedFieldShapeVerdictV1, committed_field_shape_v1,
  committed_field_shape_verdict_v1, shape_verdict_admissible,
  shape_verdict_field_byte_bound, shape_verdict_not_an_envelope,
  shape_verdict_wrong_stride, sized_field_envelope_v1,
}
use midgard/fraud_proofs/committed_field_shape/step_01.{
  Args, Datum, SpendRedeemer,
}
use midgard/fraud_proofs/committed_field_shape/step_02
use midgard/fraud_proofs/common.{cancel, pass_native_tx_to_next_step_carried}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/compact.{verify_native_tx_compact_cbor_v1}
use midgard/native_tx_field_access_v1.{
  Inline, RawUtxo, authenticated_whole_field_view, encode_field_preimage,
  max_transaction_aggregate_field_bytes,
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
        //    computation thread. The native codec precondition runs here for
        //    §12.7's reason: a leaf whose key is not its own id is
        //    `da-hash-preimage`'s fault, not this one's, and the compact
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

        // Shape faults are ordered after the normal-leaf mistag fault.
        expect verified.tx_compact.validity_code == 0

        // 2. The UTxO must be produced at the next step's script hash carrying
        //    the exact accusation. Both members of it are derived here — the
        //    field index from the claim whose bytes the door then held to that
        //    slot's commitment, the verdict from the authenticated bytes at that
        //    slot — so step 02 adjudicates committed values only and a fabricated
        //    verdict cannot be forwarded.
        expect output_script_hash == step_02_validator_script_hash
        let CommittedFieldShapeVerdictV1 { field_index, verdict } = committed_field_shape_v1(
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
// a **well-formed §5.1 envelope** that slot's own rules refuse — which is the
// shape §12.8 exists for, and the shape §12.7's bytes-only verdict correctly
// calls grammatical. `committed_field_preimage_block_fixture_v1` is the only
// builder of such a block.

/// §2.5 slot 0 (spend inputs) — a fixed-stride body field, stride 40.
const spend_inputs_field_index: Int = 0

/// §2.5 slot 2 (outputs) — a variable-width body field, where §7.4's arithmetic
/// does not apply and this family therefore has nothing to convict.
const outputs_field_index: Int = 2

/// §2.5 slot 7 (address witnesses) — a fixed-stride *witness-set* field, stride
/// 103, reachable only once the supplied witness set re-derives to the committed
/// `witness_set_hash`.
const address_witnesses_field_index: Int = 7

/// A §5.1 envelope over one four-byte item: six bytes, and 1 + 40·1 = 41 is not
/// six, so slot 0 refuses it on §7.4's arithmetic while §12.7's verdict calls it
/// grammatical.
fn wrong_stride_preimage() -> ByteArray {
  encode_field_preimage([#"deadbeef"])
}

/// The honest shape at slot 0: one 38-byte item, `81 ‖ 58 26 ‖ 38` = 41 bytes,
/// which is exactly `header_len + 40·1`.
fn honest_spend_input_preimage() -> ByteArray {
  encode_field_preimage([fixture.repeated_bytes_v1(#"00", 38)])
}

/// The honest shape at slot 7: one 101-byte item, 1 + 2 + 101 = 104 bytes,
/// exactly `header_len + 103·1`.
fn honest_address_witness_preimage() -> ByteArray {
  encode_field_preimage([fixture.repeated_bytes_v1(#"00", 101)])
}

/// A §5.1 envelope one byte above §5.4's per-field bound, and well-formed in
/// every other respect.
fn oversize_preimage() -> ByteArray {
  sized_field_envelope_v1(max_transaction_aggregate_field_bytes + 1, #"00")
}

/// Just above §8.3's `K`, so tier 3 is the only admissible carriage and §8.4's
/// partition is genuinely exercised rather than assumed. At slot 0 it is a
/// wrong-stride envelope: 1 + 40 is not 15,200.
const certified_preimage_bytes: Int = 15_200

fn certified_wrong_stride_preimage() -> ByteArray {
  sized_field_envelope_v1(certified_preimage_bytes, #"00")
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
      block.source_cbor,
    ),
  )
}

// ## Step-01 binding selectors — the §7.4 stride shape

/// **Positive binding selector, tier 1.** A block that commits a wrong-stride
/// envelope at slot 0 binds, and the forwarded verdict names §7.4 as the rule
/// the committed bytes leave.
test committed_field_shape_step_01_binds_a_wrong_stride_body_field() {
  let preimage = wrong_stride_preimage()
  let block = committed_block(spend_inputs_field_index, preimage)
  run_step_01(
    block,
    body_claim(spend_inputs_field_index, preimage),
    expected_state(block, spend_inputs_field_index, shape_verdict_wrong_stride),
  )
}

/// **The stall this family removes, demonstrated at the seam.** The very same
/// committed field, opened through the §8.8 whole-materialising door, aborts on
/// §7.4's arithmetic — so no step of §12.7's family is producible for it either
/// (its verdict is 0) and the dispute stalls instead of rejecting. The selector
/// above convicts it, and the door's abort semantics are untouched.
test committed_field_shape_step_01_the_door_cannot_open_the_convicted_field() fail {
  let preimage = wrong_stride_preimage()
  let block = committed_block(spend_inputs_field_index, preimage)
  let view = authenticated_whole_field_view(
    verify_native_tx_compact_cbor_v1(block.tx_id, block.compact_cbor),
    block.witness_set_compact,
    spend_inputs_field_index,
    Inline { preimage },
    [],
    fixture.test_certificate_policy_id,
  )
  view == view
}

/// The companion positive: the same door opens the same slot of the same block
/// once its committed preimage is the slot's stride, so the `fail` above traps
/// §7.4's arithmetic and not the fixture.
test committed_field_shape_step_01_the_door_opens_a_right_stride_field() {
  let preimage = honest_spend_input_preimage()
  let block = committed_block(spend_inputs_field_index, preimage)
  let view = authenticated_whole_field_view(
    verify_native_tx_compact_cbor_v1(block.tx_id, block.compact_cbor),
    block.witness_set_compact,
    spend_inputs_field_index,
    Inline { preimage },
    [],
    fixture.test_certificate_policy_id,
  )
  view == view
}

/// **Positive binding selector at a fixed-stride witness-set slot**: reachable
/// only because the supplied witness set re-derives to the `witness_set_hash`
/// the committed compact structure carries, which is what the door checks before
/// it looks at any carriage.
test committed_field_shape_step_01_binds_a_wrong_stride_witness_field() {
  let preimage = wrong_stride_preimage()
  let block = committed_block(address_witnesses_field_index, preimage)
  run_step_01(
    block,
    witness_claim(block, address_witnesses_field_index, preimage),
    expected_state(
      block,
      address_witnesses_field_index,
      shape_verdict_wrong_stride,
    ),
  )
}

// ## Step-01 binding selectors — the §5.4 byte-bound shape

/// **Positive binding selector, tier 1.** A committed envelope one byte above
/// §5.4's per-field bound at a *variable-width* slot: §7.4's arithmetic never
/// runs there, so the byte bound is the only rule left and this is the shape
/// that isolates it.
test committed_field_shape_step_01_binds_an_oversize_body_field() {
  let preimage = oversize_preimage()
  let block = committed_block(outputs_field_index, preimage)
  run_step_01(
    block,
    body_claim(outputs_field_index, preimage),
    expected_state(block, outputs_field_index, shape_verdict_field_byte_bound),
  )
}

/// The same committed field through the door: `whole_view` refuses a
/// `total_length` above `max_transaction_aggregate_field_bytes`, which is the
/// second stall §12.7's residue paragraph names.
test committed_field_shape_step_01_the_door_cannot_open_an_oversize_field() fail {
  let preimage = oversize_preimage()
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

// ## Honest-block negatives

/// Valid-block negative selector: the committed preimage is the slot's stride,
/// so the forwarded verdict is `shape_verdict_admissible` and step 02 cannot
/// finalize. Step 01 itself is evidence carriage and binds it without
/// convicting, exactly as §12.7's does.
test committed_field_shape_step_01_binds_a_right_stride_field_without_convicting() {
  let preimage = honest_spend_input_preimage()
  let block = committed_block(spend_inputs_field_index, preimage)
  run_step_01(
    block,
    body_claim(spend_inputs_field_index, preimage),
    expected_state(block, spend_inputs_field_index, shape_verdict_admissible),
  )
}

/// The same, at a fixed-stride witness-set slot.
test committed_field_shape_step_01_binds_a_right_stride_witness_field_without_convicting() {
  let preimage = honest_address_witness_preimage()
  let block = committed_block(address_witnesses_field_index, preimage)
  run_step_01(
    block,
    witness_claim(block, address_witnesses_field_index, preimage),
    expected_state(
      block,
      address_witnesses_field_index,
      shape_verdict_admissible,
    ),
  )
}

/// **The boundary against §12.7, at the binding.** The very bytes §12.7's family
/// convicts — a miscounted envelope — bind here too, and forward
/// `shape_verdict_not_an_envelope`, which step 02 refuses. The two fault kinds
/// partition the committed byte strings a door refuses; they do not overlap on
/// one.
test committed_field_shape_step_01_binds_a_non_envelope_without_convicting() {
  let preimage = #"8041"
  let block = committed_block(outputs_field_index, preimage)
  run_step_01(
    block,
    body_claim(outputs_field_index, preimage),
    expected_state(block, outputs_field_index, shape_verdict_not_an_envelope),
  )
}

/// A wrong-stride envelope committed at a **variable-width** slot is admissible
/// there and forwards verdict 0 — the same bytes, a different slot, a different
/// answer, which is the whole of what makes this fault kind field-indexed.
test committed_field_shape_step_01_binds_the_same_bytes_at_a_walked_field() {
  let preimage = wrong_stride_preimage()
  let block = committed_block(outputs_field_index, preimage)
  run_step_01(
    block,
    body_claim(outputs_field_index, preimage),
    expected_state(block, outputs_field_index, shape_verdict_admissible),
  )
}

// ## Refusals

/// **The custody chain from derivation to adjudication.** The produced UTxO must
/// sit at step 02's script and nowhere else: run the very transaction the
/// positive selector binds against an instance parameterised with any other hash
/// and it refuses, so the thread token carrying a derived accusation cannot be
/// routed to a script that would settle it on other terms — including §12.7's
/// step 02, whose state has the same three members and a different code space.
test committed_field_shape_step_01_rejects_a_foreign_next_step_hash() fail {
  let preimage = wrong_stride_preimage()
  let block = committed_block(spend_inputs_field_index, preimage)
  run_step_01_at(
    fixture.step_script_hash,
    block,
    body_claim(spend_inputs_field_index, preimage),
    expected_state(block, spend_inputs_field_index, shape_verdict_wrong_stride),
  )
}

/// A prover cannot forward a fabricated verdict: the output state is recomputed
/// from the authenticated bytes at the slot the claim named.
test committed_field_shape_step_01_rejects_a_fabricated_verdict() fail {
  let preimage = honest_spend_input_preimage()
  let block = committed_block(spend_inputs_field_index, preimage)
  run_step_01(
    block,
    body_claim(spend_inputs_field_index, preimage),
    expected_state(block, spend_inputs_field_index, shape_verdict_wrong_stride),
  )
}

/// A prover cannot forward a field index other than the one it opened. This
/// refusal carries more weight here than in §12.7: the verdict *depends* on the
/// index, so a re-addressed accusation would be a verdict about one slot's rules
/// filed against another slot's bytes.
test committed_field_shape_step_01_rejects_a_fabricated_field_index() fail {
  let preimage = wrong_stride_preimage()
  let block = committed_block(spend_inputs_field_index, preimage)
  run_step_01(
    block,
    body_claim(spend_inputs_field_index, preimage),
    expected_state(block, outputs_field_index, shape_verdict_wrong_stride),
  )
}

/// **The §4 hash check is what makes the bytes evidence.** A prover supplying a
/// wrong-stride envelope the block never committed — against an honest field —
/// aborts rather than convicting, which is §7.3 and is the reason the door's
/// abort semantics did not have to be softened for this family to exist.
test committed_field_shape_step_01_rejects_uncommitted_bytes() fail {
  let block = committed_block(
    spend_inputs_field_index,
    honest_spend_input_preimage(),
  )
  run_step_01(
    block,
    body_claim(spend_inputs_field_index, wrong_stride_preimage()),
    expected_state(block, spend_inputs_field_index, shape_verdict_wrong_stride),
  )
}

/// A body claim may not name a witness-set slot: the door would otherwise be
/// handed the rule module's unread placeholder as if it were the transaction's
/// witness set.
test committed_field_shape_step_01_rejects_a_body_claim_at_a_witness_field() fail {
  let preimage = wrong_stride_preimage()
  let block = committed_block(address_witnesses_field_index, preimage)
  run_step_01(
    block,
    body_claim(address_witnesses_field_index, preimage),
    expected_state(
      block,
      address_witnesses_field_index,
      shape_verdict_wrong_stride,
    ),
  )
}

/// And a witness claim may not name a body slot, where the witness set it
/// carries would be silently discarded.
test committed_field_shape_step_01_rejects_a_witness_claim_at_a_body_field() fail {
  let preimage = wrong_stride_preimage()
  let block = committed_block(spend_inputs_field_index, preimage)
  run_step_01(
    block,
    witness_claim(block, spend_inputs_field_index, preimage),
    expected_state(block, spend_inputs_field_index, shape_verdict_wrong_stride),
  )
}

/// Valid-block negative selector: evidence manufactured under a raw transactions
/// root the header never committed cannot bind, so a valid block cannot be
/// challenged with a fabricated field commitment.
test committed_field_shape_step_01_rejects_a_forged_transactions_root() fail {
  let preimage = wrong_stride_preimage()
  let valid_block = fixture.native_block_fixture_v1(
    fixture.valid_native_tx_v1(),
  )
  let forged_block = committed_block(spend_inputs_field_index, preimage)
  let claimed_block = fixture.NativeBlockFixtureV1 {
    ..forged_block,
    header: valid_block.header,
    header_hash: valid_block.header_hash,
  }
  run_step_01(
    claimed_block,
    body_claim(spend_inputs_field_index, preimage),
    expected_state(
      claimed_block,
      spend_inputs_field_index,
      shape_verdict_wrong_stride,
    ),
  )
}

// ## Tier-2 carriage (§8.2)
//
// A single nothing-but-bytes inline datum at an arbitrary address. Nothing about
// its provenance is trusted: the door hashes what it reads against the committed
// commitment, so a substituted publication simply fails.

/// The §8.2 publication of `preimage`, as the one carriage reference input a
/// tier-2 claim names. Appended after the two the inclusion args pin, so its
/// index is 2.
fn raw_utxo_reference_input(preimage: ByteArray) -> Input {
  Input {
    output_reference: OutputReference {
      transaction_id: #"7777777777777777777777777777777777777777777777777777777777777777",
      output_index: 0,
    },
    output: Output {
      address: address.from_script(
        #"91919191919191919191919191919191919191919191919191919191",
      ),
      value: assets.from_lovelace(2_000_000),
      datum: InlineDatum(builtin.b_data(preimage)),
      reference_script: None,
    },
  }
}

/// **Positive selector on §8.2's single-UTxO route.**
test committed_field_shape_step_01_binds_a_raw_utxo_carriage() {
  let preimage = wrong_stride_preimage()
  let block = committed_block(spend_inputs_field_index, preimage)
  let output_state = expected_state(
    block,
    spend_inputs_field_index,
    shape_verdict_wrong_stride,
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
          field_index: spend_inputs_field_index,
          carriage: RawUtxo { ref_input_index: 2 },
        },
      },
    ),
    fixture.own_out_ref(),
    fixture.continue_step_l1_tx_with_carriage_v1(
      block,
      output_state,
      [raw_utxo_reference_input(preimage)],
    ),
  )
}

/// The same route with a publication of other bytes: the §4 hash check refuses
/// it, so tier 2 authenticates nothing by itself.
test committed_field_shape_step_01_rejects_a_substituted_raw_utxo_carriage() fail {
  let preimage = wrong_stride_preimage()
  let block = committed_block(spend_inputs_field_index, preimage)
  let output_state = expected_state(
    block,
    spend_inputs_field_index,
    shape_verdict_wrong_stride,
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
          field_index: spend_inputs_field_index,
          carriage: RawUtxo { ref_input_index: 2 },
        },
      },
    ),
    fixture.own_out_ref(),
    fixture.continue_step_l1_tx_with_carriage_v1(
      block,
      output_state,
      [raw_utxo_reference_input(encode_field_preimage([#"beef"]))],
    ),
  )
}

// ## Tier-3 carriage (§8.4)
//
// The chunked route is admissible here at every field, and the argument is
// `authenticated_committed_preimage`'s: the chunks are concatenated and hashed
// once against the committed commitment, so the manifest authenticates nothing
// about content and a substituted chunk changes the hash. Without this tier the
// fault kind would stop at whatever fits one redeemer — a smaller escape hatch,
// but an escape hatch.

/// **Positive selector on §8.4's chunked route**: the family reaches committed
/// bytes no redeemer could carry.
test committed_field_shape_step_01_binds_a_certified_carriage() {
  let preimage = certified_wrong_stride_preimage()
  let block = committed_block(spend_inputs_field_index, preimage)
  let output_state = expected_state(
    block,
    spend_inputs_field_index,
    shape_verdict_wrong_stride,
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
          field_index: spend_inputs_field_index,
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
        spend_inputs_field_index,
        preimage,
      ),
    ),
  )
}

/// The verdicts this validator's fixtures earn, pinned against the rule so a
/// selector cannot go green on a shape it stopped exercising.
test committed_field_shape_step_01_verdicts_match_the_committed_shapes() {
  and {
    committed_field_shape_verdict_v1(
      spend_inputs_field_index,
      wrong_stride_preimage(),
    ) == shape_verdict_wrong_stride,
    committed_field_shape_verdict_v1(
      address_witnesses_field_index,
      wrong_stride_preimage(),
    ) == shape_verdict_wrong_stride,
    committed_field_shape_verdict_v1(
      outputs_field_index,
      wrong_stride_preimage(),
    ) == shape_verdict_admissible,
    committed_field_shape_verdict_v1(
      spend_inputs_field_index,
      honest_spend_input_preimage(),
    ) == shape_verdict_admissible,
    committed_field_shape_verdict_v1(
      address_witnesses_field_index,
      honest_address_witness_preimage(),
    ) == shape_verdict_admissible,
    committed_field_shape_verdict_v1(outputs_field_index, oversize_preimage()) == shape_verdict_field_byte_bound,
    committed_field_shape_verdict_v1(
      spend_inputs_field_index,
      certified_wrong_stride_preimage(),
    ) == shape_verdict_wrong_stride,
    committed_field_shape_verdict_v1(outputs_field_index, #"8041") == shape_verdict_not_an_envelope,
  }
}
