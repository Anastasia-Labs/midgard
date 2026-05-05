# Native Transaction Model

`lucid-midgard` must build the implemented Midgard native transaction format,
not a close approximation.

## Implemented Format

The current implementation uses:

- `MIDGARD_NATIVE_TX_VERSION = 1`
- `MIDGARD_POSIX_TIME_NONE = -1`
- `MIDGARD_NATIVE_NETWORK_ID_NONE = 255`
- A full transaction tuple:
  `[version, transaction_compact, transaction_body_full, transaction_witness_set_full]`

The compact transaction contains:

- version
- transaction body hash
- transaction witness-set hash
- validity code

The body full contains root/preimage pairs for:

- spend inputs
- reference inputs
- outputs
- required observers
- required signers
- mint

The witness full contains root/preimage pairs for:

- address witnesses
- script witnesses
- redeemer witnesses

## Hashing

All roots and compact commitments use Blake2b-256. `Hash32` must be exactly
32 bytes. The transaction id is the compact transaction body hash. It is not a
hash of the full native transaction bytes.

`lucid-midgard` must compute:

- Preimage CBOR bytes.
- Root hashes for every preimage.
- Compact body bytes.
- Compact body hash.
- Compact witness bytes.
- Compact witness hash.
- Full transaction bytes.

## Strict CBOR

The builder must produce canonical RFC 8949 CBOR compatible with the node
codec. It must not emit indefinite values, undefined, duplicate map keys, or
trailing bytes. For any helper that accepts user-supplied CBOR, strict decode
and re-encode rules must be specified.

## Empty Values

The current implementation uses:

- Empty list preimage: CBOR `[]`
- Empty null hash for auxiliary data and empty script-integrity cases:
  Blake2b-256 of CBOR `null`

The builder should expose constants for these but not allow callers to mutate
them.

## Outputs

Outputs are serialized as byte strings inside the native outputs preimage.
They must be Babbage/Conway map-form transaction outputs with a Shelley payment
credential. Output order is authored order and must be preserved.

Protected outputs set bit `0x08` on the first address byte before inclusion in
the native output preimage. Validation clears that bit for ordinary
address decoding, but Phase B uses the marker to require:

- A matching vkey witness for protected pubkey outputs.
- A MidgardV1 receive execution for protected script outputs.

## Script Integrity

`scriptIntegrityHash` is not a legacy Cardano `script_data_hash` marker. For
transactions requiring non-native scripts it is:

```text
blake2b256(cbor([
  redeemerTxWitsRoot,
  scriptLanguageViews
]))
```

`scriptLanguageViews` is a canonical CBOR map from stable numeric language tags
to frozen ordered integer cost-model arrays. Initial tags are `2` for
`PlutusV3` and `128` for `MidgardV1`. The map contains only languages actually
required by the transaction after Phase B resolves inline and reference script
sources. Native-only and no-script transactions use the empty null hash.

## Cardano Bridge

The existing node can attempt Cardano-to-native conversion during ingress, but
`lucid-midgard` must not depend on that path. Its normal output is native v1
bytes. Cardano conversion may be exposed only as a diagnostic/import tool and
must reject lossy conversion.

## Doc Drift

Older design notes may still describe a datum witness bucket. The current
implementation uses three buckets: address, script, and redeemer witnesses.
This design follows the implementation until an explicit protocol migration
changes it.
