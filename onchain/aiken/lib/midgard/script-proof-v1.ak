use aiken/cbor
use aiken/builtin
use aiken/crypto.{blake2b_224, blake2b_256}
use aiken/primitive/bytearray
use midgard/bounded_item_v1
use midgard/fraud_proofs/native_tx/codec.{encode_definite_bytes}
use midgard/fraud_proofs/native_tx/components.{
  encode_midgard_redeemer_witness, encode_midgard_versioned_script,
}
use midgard/fraud_proofs/native_tx/types.{
  CertRedeemer, MidgardRedeemerPurpose, MidgardRedeemerWitness,
  MidgardScriptLanguage, MidgardV1Script, MidgardVersionedScript, MintRedeemer,
  NativeCardanoScript, PlutusV3Script, ProposeRedeemer, ReceiveRedeemer,
  RewardRedeemer, SpendRedeemer, VoteRedeemer,
}

/// Canonical Midgard V1 script-proof commitments.
const source_leaf_domain = #"4d696467617264536372697074536f757263654c6561665631"

const inline_source_leaf_domain =
  #"4d696467617264496e6c696e65536372697074536f757263654c6561665631"

const redeemer_leaf_domain = #"4d69646761726452656465656d65724c6561665631"

const purpose_leaf_domain =
  #"4d696467617264536372697074507572706f73654c6561665631"

const signer_leaf_domain = #"4d6964676172645369676e65724c6561665631"

const output_leaf_domain = #"4d6964676172644f75747075744c6561665631"

const execution_leaf_domain =
  #"4d696467617264536372697074457865637574696f6e4c6561665631"

const script_context_item_leaf_domain =
  #"4d696467617264536372697074436f6e746578744974656d4c6561665631"

const resolved_context_item_leaf_domain =
  #"4d6964676172645265736f6c766564436f6e746578744974656d4c6561665631"

pub fn language_tag(language: MidgardScriptLanguage) -> Int {
  when language is {
    NativeCardanoScript -> 0
    PlutusV3Script -> 3
    MidgardV1Script -> 128
  }
}

pub fn versioned_script_hash(script: MidgardVersionedScript) -> ByteArray {
  #""
    |> bytearray.push(language_tag(script.language))
    |> bytearray.concat(script.script_bytes)
    |> blake2b_224
}

/// `origin_kind` is 0 for an inline witness and 1 for a reference input.
/// `source_key` is the inline index encoding or canonical reference out-ref.
pub fn inline_source_leaf_hash(
  source_index: Int,
  item_commitment: ByteArray,
) -> ByteArray {
  expect source_index >= 0
  expect bytearray.length(item_commitment) == 32
  inline_source_leaf_domain
    |> bytearray.concat(cbor.serialise(source_index))
    |> bytearray.concat(encode_definite_bytes(item_commitment))
    |> blake2b_256
}

pub fn source_leaf_hash(
  origin_kind: Int,
  source_key: ByteArray,
  script: MidgardVersionedScript,
) -> ByteArray {
  expect or {
      origin_kind == 0,
      origin_kind == 1,
    }
  if origin_kind == 0 {
    expect Some(source_index_data) = cbor.deserialise(source_key)
    let source_index = builtin.un_i_data(source_index_data)
    expect source_index >= 0
    expect cbor.serialise(source_index) == source_key
    inline_source_leaf_hash(
      source_index,
      bounded_item_v1.from_bytes(
        7,
        source_index,
        encode_midgard_versioned_script(script),
      ),
    )
  } else {
    source_leaf_domain
      |> bytearray.concat(cbor.serialise(origin_kind))
      |> bytearray.concat(encode_definite_bytes(source_key))
      |> bytearray.concat(cbor.serialise(language_tag(script.language)))
      |> bytearray.concat(encode_definite_bytes(versioned_script_hash(script)))
      |> blake2b_256
  }
}

pub fn redeemer_purpose_tag(purpose: MidgardRedeemerPurpose) -> Int {
  when purpose is {
    SpendRedeemer -> 0
    MintRedeemer -> 1
    CertRedeemer -> 2
    RewardRedeemer -> 3
    VoteRedeemer -> 4
    ProposeRedeemer -> 5
    ReceiveRedeemer -> 6
  }
}

pub fn redeemer_leaf_hash(redeemer: MidgardRedeemerWitness) -> ByteArray {
  redeemer_leaf_domain
    |> bytearray.concat(
        encode_definite_bytes(encode_midgard_redeemer_witness(redeemer)),
      )
    |> blake2b_256
}

pub fn signer_leaf_hash(signer_hash: ByteArray) -> ByteArray {
  expect bytearray.length(signer_hash) == 28
  signer_leaf_domain
    |> bytearray.concat(encode_definite_bytes(signer_hash))
    |> blake2b_256
}

pub fn output_leaf_hash(output_index: Int, output_cbor: ByteArray) -> ByteArray {
  expect output_index >= 0
  output_leaf_domain
    |> bytearray.concat(cbor.serialise(output_index))
    |> bytearray.concat(encode_definite_bytes(output_cbor))
    |> blake2b_256
}

/// Commits the exact purpose/source/redeemer tuple discovered for one script
/// execution. Native scripts use the empty byte string for `redeemer_leaf`.
pub fn execution_leaf_hash(
  language_tag: Int,
  purpose_leaf: ByteArray,
  source_leaf: ByteArray,
  redeemer_leaf: ByteArray,
) -> ByteArray {
  expect or {
      language_tag == 0,
      language_tag == 3,
      language_tag == 128,
    }
  expect bytearray.length(purpose_leaf) == 32
  expect bytearray.length(source_leaf) == 32
  expect or {
      redeemer_leaf == #"",
      bytearray.length(redeemer_leaf) == 32,
    }
  execution_leaf_domain
    |> bytearray.concat(cbor.serialise(language_tag))
    |> bytearray.concat(encode_definite_bytes(purpose_leaf))
    |> bytearray.concat(encode_definite_bytes(source_leaf))
    |> bytearray.concat(encode_definite_bytes(redeemer_leaf))
    |> blake2b_256
}

/// Purpose kinds are consensus ordered: spend=0, mint=1, observe=2,
/// protected receive=3. The subject is the canonical out-ref for spend, the
/// policy id for mint, and the credential hash for observe/receive.
pub fn purpose_leaf_hash(
  purpose_kind: Int,
  purpose_index: Int,
  script_hash: ByteArray,
  subject: ByteArray,
) -> ByteArray {
  expect purpose_kind >= 0
  expect purpose_kind <= 3
  expect purpose_index >= 0
  expect bytearray.length(script_hash) == 28
  purpose_leaf_domain
    |> bytearray.concat(cbor.serialise(purpose_kind))
    |> bytearray.concat(cbor.serialise(purpose_index))
    |> bytearray.concat(encode_definite_bytes(script_hash))
    |> bytearray.concat(encode_definite_bytes(subject))
    |> blake2b_256
}

pub fn context_item_leaf_hash(
  collection_kind: Int,
  item_index: Int,
  semantic_root: ByteArray,
  cbor_length: Int,
  memory: Int,
) -> ByteArray {
  expect collection_kind >= 0 && collection_kind <= 7
  expect item_index >= 0
  expect bytearray.length(semantic_root) == 32
  expect cbor_length >= 0
  expect memory >= 0
  script_context_item_leaf_domain
    |> bytearray.concat(cbor.serialise(collection_kind))
    |> bytearray.concat(cbor.serialise(item_index))
    |> bytearray.concat(encode_definite_bytes(semantic_root))
    |> bytearray.concat(cbor.serialise(cbor_length))
    |> bytearray.concat(cbor.serialise(memory))
    |> blake2b_256
}

pub fn resolved_context_item_leaf_hash(
  source_kind: Int,
  item_index: Int,
  key: ByteArray,
  output_cbor: ByteArray,
) -> ByteArray {
  expect or {
      source_kind == 0,
      source_kind == 1,
    }
  expect item_index >= 0
  resolved_context_item_leaf_domain
    |> bytearray.concat(cbor.serialise(source_kind))
    |> bytearray.concat(cbor.serialise(item_index))
    |> bytearray.concat(encode_definite_bytes(key))
    |> bytearray.concat(encode_definite_bytes(output_cbor))
    |> blake2b_256
}
