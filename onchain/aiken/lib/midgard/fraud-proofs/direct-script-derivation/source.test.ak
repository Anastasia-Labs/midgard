use aiken/builtin
use aiken/collection/list
use aiken/crypto.{blake2b_224}
use aiken/merkle_patricia_forestry as library_mpf
use aiken/primitive/bytearray
use cardano/transaction.{OutputReference}
use midgard/cek_data_v1.{DataSummaryV1}
use midgard/fraud_proofs/direct_script_derivation/source
use midgard/fraud_proofs/native_tx/components.{encode_midgard_tx_input}
use midgard/fraud_proofs/native_tx/types.{MidgardTxInput}
use midgard/fraud_proofs/transition_trace/proof.{ledger_outref_key}
use midgard/ledger_output_commitment_v1 as descriptor

const h28 = #"22222222222222222222222222222222222222222222222222222222"

const h32 = #"1111111111111111111111111111111111111111111111111111111111111111"

fn output_descriptor(language: Int) -> ByteArray {
  let summary = DataSummaryV1 { root: h32, cbor_length: 1, memory: 1 }
  descriptor.encode(
    descriptor.LedgerOutputCommitmentV1 {
      version: 1,
      output_index: 7,
      total_length: 50,
      item_commitment: h32,
      address: #"6011111111111111111111111111111111111111111111111111111111",
      lovelace: 5000000,
      asset_count: 0,
      asset_frontier_commitment: h32,
      cardano_value_size: 5,
      reference_script_language: language,
      reference_script_hash: if language == -1 {
        #""
      } else {
        h28
      },
      reference_script_total_length: if language == -1 {
        0
      } else {
        4
      },
      reference_script_item_commitment: if language == -1 {
        #""
      } else {
        h32
      },
      cardano_tx_out: summary,
      midgard_tx_out: summary,
      cardano_spend_datum: summary,
    },
  )
}

fn out_ref() -> ByteArray {
  encode_midgard_tx_input(MidgardTxInput { tx_id: h32, output_index: 7 })
}

fn root(value: ByteArray) -> ByteArray {
  library_mpf.insert(
    library_mpf.empty,
    ledger_outref_key(OutputReference { transaction_id: h32, output_index: 7 }),
    value,
    [],
  )
    |> library_mpf.root
}

test spent_and_reference_inputs_open_the_same_authenticated_script() {
  let value = output_descriptor(0)
  let witness = source.ResolvedWitnessV1 { descriptor_cbor: value, proof: [] }
  expect Some(spent) = source.resolved_source_v1(
    root(value),
    0,
    2,
    out_ref(),
    witness,
  )
  expect Some(reference) = source.resolved_source_v1(
    root(value),
    1,
    3,
    out_ref(),
    witness,
  )
  and {
    spent.script_hash == h28,
    reference.script_hash == h28,
    spent.field_index == 0,
    reference.field_index == 1,
    spent.source_key == reference.source_key,
  }
}

test output_without_a_script_is_an_authenticated_absence() {
  let value = output_descriptor(-1)
  source.resolved_source_v1(
    root(value),
    0,
    0,
    out_ref(),
    source.ResolvedWitnessV1 { descriptor_cbor: value, proof: [] },
  ) == None
}

test wrong_event_pre_root_is_refused() fail {
  let value = output_descriptor(0)
  source.resolved_source_v1(
    h32,
    0,
    0,
    out_ref(),
    source.ResolvedWitnessV1 { descriptor_cbor: value, proof: [] },
  ) != None
}

test substituted_descriptor_is_refused() fail {
  let value = output_descriptor(0)
  source.resolved_source_v1(
    root(value),
    1,
    0,
    out_ref(),
    source.ResolvedWitnessV1 {
      descriptor_cbor: output_descriptor(3),
      proof: [],
    },
  ) != None
}

test another_input_field_is_refused() fail {
  let value = output_descriptor(0)
  source.resolved_source_v1(
    root(value),
    2,
    0,
    out_ref(),
    source.ResolvedWitnessV1 { descriptor_cbor: value, proof: [] },
  ) != None
}

test witness_source_hash_is_of_the_actual_versioned_bytes() {
  let opened = source.witness_source_v1(3, #"820042aabb")
  and {
    opened.field_index == 6,
    opened.item_index == 3,
    opened.total_length == 5,
    opened.script_hash == blake2b_224(#"00aabb"),
  }
}

test witness_outer_envelope_substitution_is_refused() fail {
  source.witness_source_v1(0, #"820042aabb00").total_length > 0
}

// These source-boundary controls keep ledger membership authentic while the
// descriptor spelling changes, so an unrelated root check cannot hide a
// permissive decoder. The nearest source tests already own this setup.
fn nonminimal_descriptor() -> ByteArray {
  let bytes = output_descriptor(0)
  builtin.slice_bytearray(0, 2, bytes)
    |> bytearray.concat(#"1807")
    |> bytearray.concat(
        builtin.slice_bytearray(3, bytearray.length(bytes) - 3, bytes),
      )
}

fn resolve_descriptor(bytes: ByteArray) -> Option<source.SourceV1> {
  source.resolved_source_v1(
    root(bytes),
    0,
    2,
    out_ref(),
    source.ResolvedWitnessV1 { descriptor_cbor: bytes, proof: [] },
  )
}

test supported_reference_languages_preserve_original_source_facts() {
  list.all(
    [0, 3, 128],
    fn(language) {
      let bytes = output_descriptor(language)
      let original = descriptor.decode(bytes)
      let direct = descriptor.decode_canonical_schema(bytes)
      expect Some(selected) = resolve_descriptor(bytes)
      and {
        original == direct,
        descriptor.encode(original) == bytes,
        selected.source_key == out_ref(),
        selected.language_tag == language,
        selected.script_hash == original.reference_script_hash,
        selected.total_length == original.reference_script_total_length,
        selected.item_commitment == original.reference_script_item_commitment,
      }
    },
  )
}

test generic_decoder_rejects_reencoded_nonminimal_descriptor() fail {
  let decoded: Data = descriptor.decode(nonminimal_descriptor())
  bytearray.length(builtin.serialise_data(decoded)) > 0
}

test authenticated_source_rejects_reencoded_nonminimal_descriptor() fail {
  resolve_descriptor(nonminimal_descriptor()) != None
}

test generic_decoder_rejects_trailing_descriptor_bytes() fail {
  let decoded: Data = descriptor.decode(
    output_descriptor(0) |> bytearray.concat(#"00"),
  )
  bytearray.length(builtin.serialise_data(decoded)) > 0
}

test authenticated_source_rejects_trailing_descriptor_bytes() fail {
  resolve_descriptor(output_descriptor(0) |> bytearray.concat(#"00")) != None
}
