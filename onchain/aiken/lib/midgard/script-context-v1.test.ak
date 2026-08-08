use aiken/builtin
use aiken/cbor
use aiken/primitive/bytearray
use midgard/bounded_item_v1
use midgard/canonical_plutus_data_v1.{is_canonical_plutus_data_v1}
use midgard/cek_data_v1
use midgard/fraud_proofs/native_tx/components.{encode_midgard_tx_output}
use midgard/fraud_proofs/native_tx/types.{
  MidgardAddress, MidgardTxOutput, MidgardValue, PubKeyCredential,
}
use midgard/ledger_output_commitment_v1
use midgard/ledger_output_v1
use midgard/script_context_v1
use midgard/script_proof_v1
use midgard/validation_merkle_v1

const output_cbor =
  #"a400581d68aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01821a0012d687a1581c11111111111111111111111111111111111111111111111111111111a1422233070246d87b9f182aff03820343010203"

const simple_output_cbor =
  #"a200581d68aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01821a0012d687a1581c11111111111111111111111111111111111111111111111111111111a142223307"

const datum_output_cbor =
  #"a300581d68aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01821a0012d687a1581c11111111111111111111111111111111111111111111111111111111a1422233070246d87b9f182aff"

const reference_output_cbor =
  #"a300581d68aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01821a0012d687a1581c11111111111111111111111111111111111111111111111111111111a14222330703820343010203"

const input_cbor =
  #"825820bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb190002"

fn exact_descriptor(output_index: Int, raw_output_cbor: ByteArray) -> ByteArray {
  expect Some(cardano_tx_out) = script_context_v1.tx_out_summary_v1(
    raw_output_cbor,
    False,
  )
  expect Some(midgard_tx_out) = script_context_v1.tx_out_summary_v1(
    raw_output_cbor,
    True,
  )
  expect Some(cardano_spend_datum) = script_context_v1.spend_datum_summary_v1(
    raw_output_cbor,
  )
  ledger_output_commitment_v1.encode(
    ledger_output_commitment_v1.LedgerOutputCommitmentV1 {
      version: ledger_output_commitment_v1.version,
      output_index,
      total_length: bytearray.length(raw_output_cbor),
      item_commitment: bounded_item_v1.from_bytes(
        2,
        output_index,
        raw_output_cbor,
      ),
      address: #"68aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
      lovelace: 1234567,
      asset_count: 0,
      asset_frontier_commitment: validation_merkle_v1.frontier_commitment(0, []),
      cardano_value_size: 0,
      reference_script_language: -1,
      reference_script_hash: #"",
      reference_script_total_length: 0,
      reference_script_item_commitment: #"",
      cardano_tx_out,
      midgard_tx_out,
      cardano_spend_datum,
    },
  )
}

fn indexed_hashes(cursor: Int, count: Int) -> List<ByteArray> {
  if cursor == count {
    []
  } else {
    [
      bytearray.from_int_big_endian(cursor, 28),
      ..indexed_hashes(cursor + 1, count)
    ]
  }
}

fn repeated_siblings(count: Int) -> List<ByteArray> {
  if count == 0 {
    []
  } else {
    [
      #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
      ..repeated_siblings(count - 1)
    ]
  }
}

fn rightmost_peak_root(current: ByteArray, height: Int) -> ByteArray {
  if height == 0 {
    current
  } else {
    rightmost_peak_root(
      validation_merkle_v1.hash_branch(
        #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
        current,
      ),
      height - 1,
    )
  }
}

test offchain_simple_output_vector_is_canonical() {
  ledger_output_v1.decode_canonical_output(simple_output_cbor) != None
}

test offchain_datum_output_vector_is_canonical() {
  ledger_output_v1.decode_canonical_output(datum_output_cbor) != None
}

test offchain_reference_output_vector_is_canonical() {
  ledger_output_v1.decode_canonical_output(reference_output_cbor) != None
}

test offchain_output_vector_is_canonical() {
  ledger_output_v1.decode_canonical_output(output_cbor) != None
}

test offchain_tx_out_semantic_roots_match() {
  expect Some(cardano) = script_context_v1.tx_out_summary_v1(output_cbor, False)
  expect Some(midgard) = script_context_v1.tx_out_summary_v1(output_cbor, True)
  and {
    cardano.root == #"e1649a619efea4d8319405dae3455e5add966785c25f269e83fd0f15352693a7",
    cardano.cbor_length == 133,
    cardano.memory == 169,
    midgard.root == #"65951e5653516caf350b2cd750bab349fbd1a48811a4c278859dd825868c2d32",
    midgard.cbor_length == 133,
    midgard.memory == 169,
  }
}

test malformed_tx_out_fails_closed() {
  script_context_v1.tx_out_summary_v1(#"a0", False) == None
}

test offchain_tx_in_info_semantic_roots_match() {
  expect Some(cardano) = script_context_v1.tx_in_info_summary_v1(
    input_cbor,
    output_cbor,
    False,
  )
  expect Some(midgard) = script_context_v1.tx_in_info_summary_v1(
    input_cbor,
    output_cbor,
    True,
  )
  and {
    cardano.root == #"66b39a3f329165f6ab15f249df38d5e8bc99230853ce5c16976b4780af1ed029",
    cardano.cbor_length == 176,
    cardano.memory == 218,
    midgard.root == #"097c6693964ba9a7448e88f4e73fcaff44b7e41663d4cd3ba2b52ff3afbc3f8f",
    midgard.cbor_length == 176,
    midgard.memory == 218,
  }
}

test authenticated_item_is_prepended_in_reverse_index_order() {
  let item = cek_data_v1.DataSummaryV1 {
    root: #"66b39a3f329165f6ab15f249df38d5e8bc99230853ce5c16976b4780af1ed029",
    cbor_length: 176,
    memory: 218,
  }
  let item_leaf = script_proof_v1.context_item_leaf_hash(
    0,
    0,
    item.root,
    item.cbor_length,
    item.memory,
  )
  expect Some(sequence) = script_context_v1.prepend_authenticated_list_item_v1(
    0,
    1,
    [validation_merkle_v1.FrontierPeak { height: 0, hash: item_leaf }],
    0,
    item,
    [],
    cek_data_v1.empty_data_list_summary_v1(),
  )
  let list_summary = cek_data_v1.list_data_summary_v1(sequence)
  and {
    item_leaf == #"d2b8985450e215893013635f7403ff16d8ee18a991747ca8428bfcf2ddb22546",
    sequence.root == #"f5df83aaebfb31fabe0c3758026af9b87655bdfe0b07fe4755a47e8dd345cbca",
    sequence.length == 1,
    sequence.payload_cbor_length == 176,
    sequence.memory == 218,
    list_summary.root == #"7f0329cc3ab6a71c6452a353ca7328124ef675be5ec1f035856943453baa732e",
    list_summary.cbor_length == 178,
    list_summary.memory == 222,
  }
}

test raw_resolved_input_is_converted_and_prepended_in_one_bounded_step() {
  let leaf = script_proof_v1.resolved_context_item_leaf_hash(
    0,
    0,
    input_cbor,
    output_cbor,
  )
  expect Some(sequence) = script_context_v1.prepend_resolved_tx_in_info_v1(
    1,
    [validation_merkle_v1.FrontierPeak { height: 0, hash: leaf }],
    1,
    0,
    0,
    input_cbor,
    output_cbor,
    [],
    False,
    cek_data_v1.empty_data_list_summary_v1(),
  )
  let list_summary = cek_data_v1.list_data_summary_v1(sequence)
  and {
    sequence.root == #"f5df83aaebfb31fabe0c3758026af9b87655bdfe0b07fe4755a47e8dd345cbca",
    list_summary.root == #"7f0329cc3ab6a71c6452a353ca7328124ef675be5ec1f035856943453baa732e",
    script_context_v1.prepend_resolved_tx_in_info_v1(
      1,
      [validation_merkle_v1.FrontierPeak { height: 0, hash: leaf }],
      1,
      1,
      0,
      input_cbor,
      output_cbor,
      [],
      False,
      cek_data_v1.empty_data_list_summary_v1(),
    ) == None,
  }
}

test authenticated_output_is_converted_and_prepended_in_one_bounded_step() {
  let leaf = script_proof_v1.output_leaf_hash(0, output_cbor)
  expect Some(sequence) = script_context_v1.prepend_output_v1(
    1,
    [validation_merkle_v1.FrontierPeak { height: 0, hash: leaf }],
    0,
    output_cbor,
    [],
    True,
    cek_data_v1.empty_data_list_summary_v1(),
  )
  let item = cek_data_v1.list_data_summary_v1(sequence)
  and {
    sequence.length == 1,
    sequence.payload_cbor_length == 133,
    sequence.memory == 169,
    item.cbor_length == 135,
    item.memory == 173,
  }
}

test authenticated_descriptor_drives_spend_and_reference_context_items() {
  let descriptor_cbor = exact_descriptor(2, output_cbor)
  let spend_leaf = script_proof_v1.resolved_context_item_leaf_hash(
    0,
    0,
    input_cbor,
    descriptor_cbor,
  )
  let reference_leaf = script_proof_v1.resolved_context_item_leaf_hash(
    1,
    0,
    input_cbor,
    descriptor_cbor,
  )
  expect Some(spend) = script_context_v1.prepend_resolved_descriptor_tx_in_info_v1(
    1,
    [validation_merkle_v1.FrontierPeak { height: 0, hash: spend_leaf }],
    1,
    0,
    0,
    input_cbor,
    descriptor_cbor,
    [],
    False,
    cek_data_v1.empty_data_list_summary_v1(),
  )
  expect Some(reference) = script_context_v1.prepend_resolved_descriptor_tx_in_info_v1(
    1,
    [validation_merkle_v1.FrontierPeak { height: 0, hash: reference_leaf }],
    0,
    1,
    0,
    input_cbor,
    descriptor_cbor,
    [],
    True,
    cek_data_v1.empty_data_list_summary_v1(),
  )
  and {
    spend.length == 1,
    reference.length == 1,
    spend.root != reference.root,
    script_context_v1.prepend_resolved_descriptor_tx_in_info_v1(
      1,
      [validation_merkle_v1.FrontierPeak { height: 0, hash: spend_leaf }],
      1,
      0,
      0,
      input_cbor,
      exact_descriptor(2, simple_output_cbor),
      [],
      False,
      cek_data_v1.empty_data_list_summary_v1(),
    ) == None,
  }
}

test complete_output_cbor_cannot_substitute_for_a_resolved_descriptor() fail {
  let leaf = script_proof_v1.resolved_context_item_leaf_hash(
    0,
    0,
    input_cbor,
    output_cbor,
  )
  script_context_v1.prepend_resolved_descriptor_tx_in_info_v1(
    1,
    [validation_merkle_v1.FrontierPeak { height: 0, hash: leaf }],
    1,
    0,
    0,
    input_cbor,
    output_cbor,
    [],
    False,
    cek_data_v1.empty_data_list_summary_v1(),
  ) != None
}

test output_descriptor_membership_selects_exact_maximum_semantic_summary() {
  let maximum = 16_384
  let uint64_max = 18_446_744_073_709_551_615
  let summary = cek_data_v1.DataSummaryV1 {
    root: #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
    cbor_length: maximum,
    memory: maximum,
  }
  let descriptor_cbor = ledger_output_commitment_v1.encode(
    ledger_output_commitment_v1.LedgerOutputCommitmentV1 {
      version: ledger_output_commitment_v1.version,
      output_index: 0,
      total_length: 16_384,
      item_commitment: #"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
      address: #"68aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
      lovelace: uint64_max,
      asset_count: 0,
      asset_frontier_commitment: validation_merkle_v1.frontier_commitment(0, []),
      cardano_value_size: 5000,
      reference_script_language: -1,
      reference_script_hash: #"",
      reference_script_total_length: 0,
      reference_script_item_commitment: #"",
      cardano_tx_out: summary,
      midgard_tx_out: cek_data_v1.DataSummaryV1 {
        ..summary,
        root: #"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
      },
      cardano_spend_datum: cek_data_v1.DataSummaryV1 {
        ..summary,
        root: #"cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc",
      },
    },
  )
  let leaf = script_proof_v1.output_descriptor_leaf_hash(0, descriptor_cbor)
  expect Some(output) = script_context_v1.prepend_output_descriptor_v1(
    1,
    [validation_merkle_v1.FrontierPeak { height: 0, hash: leaf }],
    0,
    descriptor_cbor,
    [],
    False,
    cek_data_v1.empty_data_list_summary_v1(),
  )
  expect Some(spend) = script_context_v1.cardano_spend_script_info_from_descriptor_v1(
    #"825820bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb190000",
    descriptor_cbor,
  )
  and {
    output.length == 1,
    output.payload_cbor_length == maximum,
    output.memory == maximum,
    bytearray.length(spend.root) == 32,
    spend.root != summary.root,
  }
}

test maximum_signer_frontier_item_fits_one_step() {
  let signer = #"11111111111111111111111111111111111111111111111111111111"
  let leaf = script_proof_v1.signer_leaf_hash(signer)
  let peaks = [
    validation_merkle_v1.FrontierPeak {
      height: 6,
      hash: rightmost_peak_root(leaf, 6),
    },
  ]
  expect Some(sequence) = script_context_v1.prepend_signer_v1(
    64,
    validation_merkle_v1.frontier_commitment(64, peaks),
    peaks,
    63,
    signer,
    repeated_siblings(6),
    cek_data_v1.empty_data_list_summary_v1(),
  )
  sequence.length == 1
}

test observer_context_collections_are_not_capped_at_16() {
  let observers = indexed_hashes(0, 17)
  expect Some(cardano_observers) = script_context_v1.observer_collection_summary_v1(
    observers,
    False,
  )
  expect Some(midgard_observers) = script_context_v1.observer_collection_summary_v1(
    observers,
    True,
  )
  and {
    cardano_observers.cbor_length > midgard_observers.cbor_length,
    midgard_observers.cbor_length > 400,
  }
}

test composed_transaction_info_and_context_match_direct_plutus_data() {
  let empty_list = builtin.list_data([])
  let empty_map = builtin.map_data([])
  let none = builtin.constr_data(1, [])
  let validity = builtin.constr_data(
    0,
    [
      builtin.constr_data(
        0,
        [builtin.constr_data(0, []), builtin.constr_data(1, [])],
      ),
      builtin.constr_data(
        0,
        [builtin.constr_data(0, []), builtin.constr_data(0, [])],
      ),
    ],
  )
  let tx_id = #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
  let empty_items = cek_data_v1.empty_data_list_summary_v1()
  let empty_pairs = cek_data_v1.empty_data_pair_summary_v1()
  let cardano_tx_info = script_context_v1.tx_info_summary_v1(
    False,
    empty_items,
    empty_items,
    empty_items,
    0,
    -1,
    -1,
    cek_data_v1.map_data_summary_v1(empty_pairs),
    empty_items,
    cek_data_v1.map_data_summary_v1(empty_pairs),
    empty_pairs,
    tx_id,
  )
  let midgard_tx_info = script_context_v1.tx_info_summary_v1(
    True,
    empty_items,
    empty_items,
    empty_items,
    0,
    -1,
    -1,
    cek_data_v1.list_data_summary_v1(empty_items),
    empty_items,
    cek_data_v1.map_data_summary_v1(empty_pairs),
    empty_pairs,
    tx_id,
  )
  let direct_cardano = builtin.constr_data(
    0,
    [
      empty_list,
      empty_list,
      empty_list,
      builtin.i_data(0),
      empty_map,
      empty_list,
      empty_map,
      validity,
      empty_list,
      empty_map,
      empty_map,
      builtin.b_data(tx_id),
      empty_map,
      empty_list,
      none,
      none,
    ],
  )
  let direct_midgard = builtin.constr_data(
    0,
    [
      empty_list,
      empty_list,
      empty_list,
      builtin.i_data(0),
      validity,
      empty_list,
      empty_list,
      empty_map,
      empty_map,
      builtin.b_data(tx_id),
    ],
  )
  let redeemer = cek_data_v1.semantic_data_summary_v1(builtin.i_data(0))
  expect Some(script_info) = script_context_v1.script_purpose_summary_v1(
    1,
    #"11111111111111111111111111111111111111111111111111111111",
    #"",
    False,
  )
  let context = script_context_v1.script_context_summary_v1(
    cardano_tx_info,
    redeemer,
    script_info,
  )
  and {
    cardano_tx_info == cek_data_v1.semantic_data_summary_v1(direct_cardano),
    midgard_tx_info == cek_data_v1.semantic_data_summary_v1(direct_midgard),
    context == cek_data_v1.semantic_data_summary_v1(
      builtin.constr_data(
        0,
        [
          direct_cardano,
          builtin.i_data(0),
          builtin.constr_data(
            0,
            [
              builtin.b_data(
                #"11111111111111111111111111111111111111111111111111111111",
              ),
            ],
          ),
        ],
      ),
    ),
  }
}

// ---------------------------------------------------------------------------
// The spend-datum gate: the §6.2 predicate against the pin it retired
// ---------------------------------------------------------------------------
//
// `spend_datum_summary_v1` used to gate an output's inline datum on a round
// trip through Aiken stdlib's CBOR decoder — `deserialise`, then `serialise`
// and compare bytes. AC4 replaced that with the §6.2 predicate
// `is_canonical_plutus_data_v1`, which decides membership in `serialiseData`'s
// image by walking the bytes. The `round_trips` family in
// `canonical_plutus_data_v1.test` proves the new predicate accepts everything
// the builtin emits; what it cannot show is how the *gate* moved, and the
// predicate is now solely load-bearing here. So the retired pin is restated
// below and the two run side by side on the same vectors — including every
// structural rejection the retired pin used to be trusted for.
//
// One thing the differential turns up immediately: the retired pin was never
// a total function. `cbor.deserialise` does not decline a canonical tag-2/3
// bignum or an unknown tag — it drives the stdlib decoder into
// `constr_data` with a negative alternative, which is a machine failure, not
// a `None`. Those vectors are therefore held to the predicate and the gate
// only, and the reason is recorded at each site rather than left as a gap in
// the vector list.

/// The retired gate, restated verbatim so the difference is measured rather
/// than asserted. Only safe on inputs the stdlib decoder can reach a verdict
/// on: see the module note above.
fn retired_round_trip_pin(datum_cbor: ByteArray) -> Bool {
  when cbor.deserialise(datum_cbor) is {
    None -> False
    Some(datum) -> cbor.serialise(datum) == datum_cbor
  }
}

fn output_with_datum(datum_cbor: ByteArray) -> ByteArray {
  encode_midgard_tx_output(
    MidgardTxOutput {
      address: MidgardAddress {
        protected: False,
        network_id: 0,
        payment_credential: PubKeyCredential(
          #"0102030405060708090a0b0c0d0e0f101112131415161718191a1b1c",
        ),
        stake_credential: None,
      },
      value: MidgardValue { lovelace: 2000000, assets: [] },
      datum_cbor: Some(datum_cbor),
      script_ref: None,
    },
  )
}

/// True when the gate admits the datum — `spend_datum_summary_v1` answers with
/// a summary rather than declining.
fn datum_gate_admits(datum_cbor: ByteArray) -> Bool {
  when
    script_context_v1.spend_datum_summary_v1(output_with_datum(datum_cbor))
  is {
    Some(_summary) -> True
    None -> False
  }
}

/// Both predicates accept and the gate admits: the ordinary path is untouched
/// by the swap.
test datum_predicates_agree_on_ordinary_datums() {
  let agrees = fn(datum_cbor: ByteArray) -> Bool {
    and {
      is_canonical_plutus_data_v1(datum_cbor),
      retired_round_trip_pin(datum_cbor),
      datum_gate_admits(datum_cbor),
    }
  }
  and {
    agrees(#"00"),
    agrees(#"1818"),
    agrees(#"20"),
    agrees(#"4401020304"),
    agrees(#"80"),
    agrees(#"9f0001ff"),
    agrees(#"a0"),
    agrees(#"a10102"),
    agrees(#"d87980"),
    agrees(#"d87f9f00ff"),
    agrees(cbor.serialise(builtin.constr_data(127, [builtin.b_data(#"ff")]))),
  }
}

/// The direction that matters for soundness: the new predicate is not weaker.
/// Every structural form the retired pin refused is still refused — by the
/// predicate, and by the gate.
///
/// Note what the middle column costs the retired pin. `1817`, `8100`,
/// `bf0000ff`, `9fff`, `5fff` and `d8798100` all *decode* under the stdlib
/// decoder; the round trip caught them only because re-serialising produced
/// different bytes. The new predicate refuses them structurally, without
/// materialising anything.
test datum_predicates_agree_on_every_non_canonical_form() {
  let refuses = fn(datum_cbor: ByteArray) -> Bool {
    and {
      !is_canonical_plutus_data_v1(datum_cbor),
      !retired_round_trip_pin(datum_cbor),
      !datum_gate_admits(datum_cbor),
    }
  }
  and {
    // Non-minimal integer head.
    refuses(#"1817"),
    // Definite non-empty list; the indefinite form is the only spelling.
    refuses(#"8100"),
    // Indefinite empty list; `80` is the only spelling.
    refuses(#"9fff"),
    // Empty indefinite byte string.
    refuses(#"5fff"),
    // Constructor with a definite argument list.
    refuses(#"d8798100"),
    // Indefinite map.
    refuses(#"bf0000ff"),
    // Text and simple values are outside Plutus Data.
    refuses(#"6161"),
    refuses(#"f5"),
    // Trailing bytes after a complete item.
    refuses(#"0000"),
    // Truncated head, and no input at all.
    refuses(#"18"),
    refuses(#""),
  }
}

/// An unknown tag is the vector the retired pin cannot be asked about: tag 1
/// drives the stdlib decoder into a constructor with alternative `1 - 121`,
/// which fails the machine rather than returning `None`. The predicate and
/// the gate both refuse it without materialising anything, which is the whole
/// point of deciding canonicity on bytes.
test unknown_tags_are_refused_without_materialising_them() {
  and {
    !is_canonical_plutus_data_v1(#"c11a514b67b0"),
    !datum_gate_admits(#"c11a514b67b0"),
  }
}

/// The divergence the re-pin exists for, on the class where both columns can
/// be evaluated: tag-102 constructors with alternatives at or above 128. The
/// predicate accepts them; the retired round trip refused them, because the
/// stdlib decoder declines the form outright.
test datum_predicates_diverge_on_high_constructor_alternatives() {
  let diverges = fn(datum_cbor: ByteArray) -> Bool {
    and {
      is_canonical_plutus_data_v1(datum_cbor),
      !retired_round_trip_pin(datum_cbor),
    }
  }
  and {
    diverges(#"d86682188080"),
    diverges(#"d8668218809f00ff"),
    diverges(#"d8668219ffff80"),
  }
}

/// What that divergence does *not* do: change this gate's verdict. Canonicity
/// and materialisability are separate questions now, and the stdlib decoder
/// still declines tag 102, so the gate declines at the second step instead of
/// the first. Recording it is what keeps the deferral honest — these datums
/// become spendable when the Canonical-Data Acceptor lands (spec §11, issue
/// 571), and not before.
test high_alternative_datums_are_still_declined_by_this_gate() {
  and {
    is_canonical_plutus_data_v1(#"d86682188080"),
    !datum_gate_admits(#"d86682188080"),
    is_canonical_plutus_data_v1(#"d8668219ffff80"),
    !datum_gate_admits(#"d8668219ffff80"),
  }
}

/// The bignum class, first column: a canonical tag-2/3 bignum datum is
/// §6.2-canonical and the output decoder admits the output that carries it, so
/// it is a legal L2 output.
///
/// What the middle column still cannot say, and why. The stdlib decoder does
/// not decline a bignum the way it declines tag 102: it reads `c2` as a
/// constructor tag, computes alternative `2 - 121`, and applies `constr_data`
/// to it. Whether that is a failure is *protocol-version-dependent*. Under
/// PlutusV3 builtin semantics variant E — mainnet protocol major version 11,
/// enacted 2026-07-18 — the alternative is a `Word64`, so a negative one fails
/// the machine on real L1. So `retired_round_trip_pin` cannot be called on a
/// bignum at all: not "returns False", but aborts. That is why this test holds
/// the class to the first column only, and why the retired pin is absent from
/// the assertions below.
///
/// The runner reports that abort as RC=101 from a Rust `panic!` at
/// `crates/uplc/src/machine/runtime.rs:945` (`TryFromBigIntError`). That panic
/// is aiken-lang/aiken#1359 — a runner defect where a clean evaluation error
/// belongs. It is not a Midgard failure, and the *verdict* it delivers (script
/// fails) is the one mainnet delivers at PV11.
///
/// The third column moved: `spend_datum_summary_v1` and `tx_out_data_v1` now
/// screen the head byte and decline, which is what
/// `bignum_datums_are_declined_by_the_materialisation_path` below pins. Full
/// materialisation still waits on the Canonical-Data Acceptor (spec §11, issue
/// 571).
test bignum_datums_are_canonical_and_reach_the_output_decoder() {
  and {
    is_canonical_plutus_data_v1(#"c249010000000000000000"),
    is_canonical_plutus_data_v1(#"c349010000000000000000"),
    ledger_output_v1.decode_canonical_output(
      output_with_datum(#"c249010000000000000000"),
    ) != None,
    ledger_output_v1.decode_canonical_output(
      output_with_datum(#"c349010000000000000000"),
    ) != None,
  }
}

/// The head-byte screen, proved. Before it this test could not be written at
/// all: every call below reached `constr_data(2 - 121, ..)` and failed the
/// machine, so there was no verdict to assert — only an abort. Both
/// materialisation entry points now answer `None`.
///
/// The first conjunct is the distinction the screen must not blur.
/// `is_canonical_plutus_data_v1` still accepts these bytes, as AC4 requires;
/// declining to materialise a datum is not declaring it non-canonical.
test bignum_datums_are_declined_by_the_materialisation_path() {
  let declines = fn(datum_cbor: ByteArray) -> Bool {
    let output = output_with_datum(datum_cbor)
    and {
      is_canonical_plutus_data_v1(datum_cbor),
      !datum_gate_admits(datum_cbor),
      script_context_v1.tx_out_data_v1(output, False) == None,
      script_context_v1.tx_out_data_v1(output, True) == None,
    }
  }
  and {
    declines(#"c249010000000000000000"),
    declines(#"c349010000000000000000"),
  }
}
