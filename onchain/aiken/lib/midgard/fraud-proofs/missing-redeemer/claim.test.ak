use aiken/cbor
use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use midgard/fraud_proofs/direct_script_derivation/cursor
use midgard/fraud_proofs/direct_script_derivation/first_fault_seam.{
  NetworkOrderContextV1, PurposeCoordinateV1,
}
use midgard/fraud_proofs/direct_script_derivation/purpose_workers.{
  SourceSelectionV1,
}
use midgard/fraud_proofs/direct_script_derivation/redeemer
use midgard/fraud_proofs/direct_script_derivation/selection_types.{SelectionV1}
use midgard/fraud_proofs/direct_script_derivation/source.{SourceV1}
use midgard/fraud_proofs/direct_script_derivation/source_index
use midgard/fraud_proofs/direct_script_derivation/source_scan.{BoundIndexV1}
use midgard/fraud_proofs/direct_script_derivation/types.{BoundV1, PurposeV1}
use midgard/fraud_proofs/missing_redeemer/claim.{
  ClaimContextV1, ClaimedRedeemerScanV1, FinalStateV1,
}
use midgard/fraud_proofs/missing_redeemer/claim_purpose.{ClaimPurposeStateV1}
use midgard/fraud_proofs/missing_redeemer/rule.{DecisionStateV1, ScanStateV1}
use midgard/fraud_proofs/proof_thread_substrate_v1.{VerdictSubjectV1}
use midgard/rejection_reason_v1.{RedeemerMissing}

// Every field carries a distinct value, so any field or constructor reorder
// changes the encoding. The same values and digests are pinned off-chain in
// demo/midgard-sdk/tests/missing-redeemer-claim.test.ts.
fn bytes(tag: Int, length: Int) -> ByteArray {
  bytearray.from_int_big_endian(tag, length)
}

fn digest(value: Data) -> ByteArray {
  blake2b_256(cbor.serialise(value))
}

fn bound() -> BoundV1 {
  BoundV1 {
    subject: VerdictSubjectV1 {
      version: 1,
      direction: 2,
      source_kind: 3,
      transaction_id: bytes(4, 32),
      source_key: bytes(5, 3),
      rejection_reason: Some(
        RedeemerMissing { purpose_kind: 6, purpose_index: 7 },
      ),
    },
    compact_cbor: bytes(8, 5),
    network_order_context: NetworkOrderContextV1 { body_network_mismatch: True },
    event_pre_root: bytes(9, 32),
  }
}

fn index() -> source_index.StateV1 {
  source_index.StateV1 { root: bytes(10, 32), count: 11 }
}

fn selection() -> SourceSelectionV1 {
  SourceSelectionV1 {
    selection: SelectionV1 {
      bound: bound(),
      source_index: index(),
      coordinate: PurposeCoordinateV1 { kind: 12, index: 13 },
      purpose: Some(
        PurposeV1 {
          coordinate: PurposeCoordinateV1 { kind: 14, index: 15 },
          script_hash: bytes(16, 28),
          subject: bytes(17, 4),
        },
      ),
    },
    source: Some(
      SourceV1 {
        field_index: 18,
        item_index: 19,
        source_key: bytes(20, 2),
        language_tag: 21,
        script_hash: bytes(22, 28),
        total_length: 23,
        item_commitment: bytes(24, 32),
      },
    ),
  }
}

fn decision() -> DecisionStateV1 {
  DecisionStateV1 { authenticated: selection(), redeemer_missing: True }
}

fn scan() -> ScanStateV1 {
  ScanStateV1 {
    authenticated: selection(),
    scan: redeemer.PointerStateV1 {
      cursor: cursor.CursorV1 {
        bound: bound(),
        field_index: 25,
        phase: cursor.Walking,
        checkpoint_hash: bytes(26, 32),
      },
      coordinate: PurposeCoordinateV1 { kind: 27, index: 28 },
      found: True,
      complete: False,
    },
  }
}

fn observed_context() -> ClaimContextV1 {
  ClaimContextV1 {
    bound: BoundIndexV1 { bound: bound(), source_index: index() },
    observed: Some(decision()),
  }
}

fn claimed_only_context() -> ClaimContextV1 {
  ClaimContextV1 {
    bound: BoundIndexV1 { bound: bound(), source_index: index() },
    observed: None,
  }
}

fn purpose_fact(constructor: Int) -> claim_purpose.ClaimedPurposeFactV1 {
  when constructor is {
    0 -> claim_purpose.NotRequired
    1 ->
      claim_purpose.ImpossiblePurpose {
        coordinate: PurposeCoordinateV1 { kind: 29, index: -30 },
      }
    _ ->
      claim_purpose.ExistingPurpose {
        purpose: PurposeV1 {
          coordinate: PurposeCoordinateV1 { kind: 31, index: 32 },
          script_hash: bytes(33, 28),
          subject: bytes(34, 4),
        },
      }
  }
}

fn predicate_fact(constructor: Int) -> claim.ClaimedPredicateFactV1 {
  when constructor is {
    0 -> claim.NotRequired
    1 ->
      claim.ImpossiblePurpose {
        coordinate: PurposeCoordinateV1 { kind: 35, index: -36 },
      }
    2 -> claim.SourceAbsent { selection: selection() }
    3 -> claim.NativeSource { selection: selection() }
    4 -> claim.SourcePresent { selection: selection() }
    5 -> claim.RedeemerPresent { decision: decision() }
    _ -> claim.RedeemerAbsent { decision: decision() }
  }
}

fn purpose_row(constructor: Int, fact: ByteArray, state: ByteArray) -> Bool {
  let value = purpose_fact(constructor)
  and {
    digest(value) == fact,
    digest(ClaimPurposeStateV1 { context: observed_context(), purpose: value }) == state,
  }
}

fn predicate_row(constructor: Int, fact: ByteArray, state: ByteArray) -> Bool {
  let value = predicate_fact(constructor)
  and {
    digest(value) == fact,
    digest(FinalStateV1 { context: observed_context(), claim: value }) == state,
  }
}

test scan_state_encodes_as_off_chain() {
  digest(scan()) == #"136c5e40cb0ff102b446a2306ab39b555d29f3edab928797d7f5c33f479a24b5"
}

test decision_state_encodes_as_off_chain() {
  digest(decision()) == #"a12b5393258407877759bd6b7e254d2847d487c517f5285bc43895c7a4bd7d9e"
}

test observed_claim_context_encodes_as_off_chain() {
  digest(observed_context()) == #"db57e1fef74bb13a829bb9ef59a3b9a78d30387ea67bb2f10de90a4f0651d046"
}

test claimed_only_claim_context_encodes_as_off_chain() {
  digest(claimed_only_context()) == #"1c62f54d2394d22bb9272222219cde5df548b911f9c0874756c8261091fb4fa5"
}

test claimed_redeemer_scan_encodes_as_off_chain() {
  digest(
    ClaimedRedeemerScanV1 { context: claimed_only_context(), scan: scan() },
  ) == #"ce4d65208a2f267b54adc031fa71f77163ca64b14f343c2257e9e3954f49c334"
}

test purpose_not_required_encodes_as_off_chain() {
  purpose_row(
    0,
    #"923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec",
    #"4203430cd36ce9eb01ed87f16d9862e9a2975b9103635e0be1c3fa98523af4f1",
  )
}

test purpose_impossible_encodes_as_off_chain() {
  purpose_row(
    1,
    #"b3686503776d562be4716fc8f3a74f8c06343a26f6598b522b107a3979f81929",
    #"67b4185ace977401416523115a9890e5eabaabd1c3e7d1f6b183575b3ecb638e",
  )
}

test purpose_existing_encodes_as_off_chain() {
  purpose_row(
    2,
    #"575c349cfd43837b69746baf9c3fb5eaa7dd36e74378d3fb0096e717f61ceb79",
    #"82f4527d4feee3483e6d27f0167cbc99ca3c9fac5b86a339abb42973c4ffd075",
  )
}

test predicate_not_required_encodes_as_off_chain() {
  predicate_row(
    0,
    #"923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec",
    #"4203430cd36ce9eb01ed87f16d9862e9a2975b9103635e0be1c3fa98523af4f1",
  )
}

test predicate_impossible_purpose_encodes_as_off_chain() {
  predicate_row(
    1,
    #"dc85329bda3029e830f0ab2a3281f4ceda4a53454493240fb5b0149140333489",
    #"28149f174bd31a408ac24b59e8bb67a86162c99001cc022ad5aaf7f738165629",
  )
}

test predicate_source_absent_encodes_as_off_chain() {
  predicate_row(
    2,
    #"ade7342b4a72179f809938dd43d1a2d746388634b95ea1f183e5a0f8ef87a912",
    #"961e93653b19a3c3e96b3074512809a28c4f2da6ed1349c460418814c72479b1",
  )
}

test predicate_native_source_encodes_as_off_chain() {
  predicate_row(
    3,
    #"e86893bb86d51e40213d632738d87eef378ec392fa83a8259437f4d06d585539",
    #"473b0fbda43e4ff945d415e431fe401f619e038fce0ee29c4a4dc386c1725e3b",
  )
}

test predicate_source_present_encodes_as_off_chain() {
  predicate_row(
    4,
    #"87af174871a277af803d1f66925097b3ae6313ae6c131b72137ba6c0393367fa",
    #"aef193774f6f7995769397fa245045f1a6c5e88aaad00752b5e13ca963676154",
  )
}

test predicate_redeemer_present_encodes_as_off_chain() {
  predicate_row(
    5,
    #"25e34f8d63ddb1c2e9ac1912741ca6f15013decf15075fa6609124642f40320b",
    #"96b9b24dc8909def0c50044debdf9b331a3ecaba4b4c1df75ba0e87906bc2944",
  )
}

test predicate_redeemer_absent_encodes_as_off_chain() {
  predicate_row(
    6,
    #"fb887f818181a2605be3168263543ed4ac4ef8c656d4229ec51f1cd9be7cdaf2",
    #"695088c6eefa6e884b589e688d1c199a97d3e4ea39011d78e254abc7e5ec0aee",
  )
}
