use aiken/builtin
use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use midgard/fraud_proofs/native_tx/compact
use midgard/fraud_proofs/native_tx/transaction

const canonical: ByteArray =
  #"83018c4180418041801720181841804180418058201111111111111111111111111111111111111111111111111111111111111111582022222222222222222222222222222222222222222222222222222222222222220183418041804180"

const compact: ByteArray =
  #"83018c582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c017201818582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582011111111111111111111111111111111111111111111111111111111111111115820222222222222222222222222222222222222222222222222222222222222222201582052408b440567f475c3eee21c9a8be91ed05a31e81e175c69fcaec5ccd187258c"

const witness: ByteArray =
  #"83582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0"

const lengths: ByteArray = #"89010101010101010101"

const commitment: ByteArray =
  #"29f0c824abc1283bf70e2d1774261027d7de033e16b28dac14681c8ba9035c06"

const tx_id: ByteArray =
  #"c27090b9b20b3e6da5906b85969adad0a7c8a8f01a899925b16b93ad8be476f4"

test forced_source_golden_vectors() {
  let decoded = compact.decode_forced_tx_compact_v1(compact)
  let full = transaction.decode_submitted_forced_transaction_v1(canonical)
  and {
    compact.encode_forced_tx_compact_v1(decoded) == compact,
    transaction.encode_submitted_forced_transaction_v1(full) == canonical,
    transaction.submitted_forced_transaction_to_compact(full) == decoded,
    compact.forced_tx_proof_commitment_v1(compact, witness, lengths) == commitment,
    bytearray.length(
      compact.encode_native_tx_proof_source_v1(compact, witness, lengths),
    ) == 434,
  }
}

test forced_source_rejects_obsolete_envelope() fail {
  let obsolete = #"84"
    |> bytearray.concat(bytearray.drop(compact, 1))
    |> bytearray.concat(#"00")
  expect _decoded = compact.decode_forced_tx_compact_v1(obsolete)
}

test forced_source_rejects_noncanonical_encoding() fail {
  let nonminimal = #"831801" |> bytearray.concat(bytearray.drop(compact, 2))
  expect _decoded = compact.decode_forced_tx_compact_v1(nonminimal)
}

test forced_source_binds_witness_and_lengths() {
  let Pair(verified, _) = compact.verify_forced_tx_proof_source_v1(
    tx_id,
    compact,
    witness,
    lengths,
  )
  and {
    verified.tx_id == tx_id,
    compact.forced_tx_proof_commitment_v1(
      compact,
      witness,
      #"89020101010101010101",
    ) != commitment,
    compact.forced_tx_proof_commitment_v1(compact, #"80", lengths) != commitment,
  }
}

test forced_source_preserves_id_and_fee_size() {
  let tx = compact.decode_forced_tx_compact_v1(compact)
  and {
    compact.native_tx_id_for_version(
      1,
      compact.encode_native_tx_body_compact(tx.body),
    ) == tx_id,
    compact.ledger_tx_size_v1(
      tx.body,
      compact.decode_native_tx_field_preimage_lengths_v1(lengths),
    ) == 96,
    bytearray.length(canonical) == 95,
  }
}

test forced_source_verdict_independence() {
  // The committed source has no decision; only the enclosing adjudicated value changes.
  let accepted_leaf = builtin.serialise_data(
    (tx_id, (compact, witness, lengths), 0),
  )
  let rejected_leaf = builtin.serialise_data(
    (tx_id, (compact, witness, lengths), 1),
  )
  and {
    blake2b_256(accepted_leaf) != blake2b_256(rejected_leaf),
    compact.forced_tx_proof_commitment_v1(compact, witness, lengths) == commitment,
    compact.forced_tx_proof_commitment_v1(compact, witness, lengths) != compact.native_tx_proof_commitment_v1(
      compact,
      witness,
      lengths,
    ),
  }
}
