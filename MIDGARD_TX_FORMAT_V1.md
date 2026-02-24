# Midgard Native Transaction Format v1

## Objectives
- Use a Midgard-owned wire format instead of Cardano transaction body/witness encoding.
- Keep the fields required by current Aiken fraud proofs directly accessible.
- Optimize serialization/deserialization with fixed-order arrays and no string-key maps.
- Separate compact commitments from full data-availability preimages.
- Enforce native-only ledger acceptance: Cardano transaction payloads are invalid at runtime.

## Canonical Structures
- `MidgardTxCompactV1`
  - `[version, tx_body_hash, tx_witness_set_hash, validity_code]`
  - `version = 1`
  - `validity_code` maps to Aiken `MidgardTxValidity` variants:
    - `0 TxIsValid`
    - `1 NonExistentInputUtxo`
    - `2 InvalidSignature`
    - `3 FailedScript`
    - `4 FeeTooLow`
    - `5 UnbalancedTx`

- `MidgardTxBodyCompactV1`
  - `[spend_inputs_root, reference_inputs_root, outputs_root, fee, valid_from, valid_to, required_observers_root, required_signers_root, mint_root, script_integrity_hash, auxiliary_data_hash, network_id]`
  - `valid_from = -1` or `valid_to = -1` means unbounded interval side.

- `MidgardTxWitnessSetCompactV1`
  - `[addr_tx_wits_root, script_tx_wits_root, redeemer_tx_wits_root]`

- `MidgardTxBodyFullV1`
  - Same scalar/root fields as compact body, plus preimages for each root:
  - `(spend_inputs_root, spend_inputs_preimage_cbor)`, `(reference_inputs_root, reference_inputs_preimage_cbor)`, `(outputs_root, outputs_preimage_cbor)`, `(required_observers_root, required_observers_preimage_cbor)`, `(required_signers_root, required_signers_preimage_cbor)`, `(mint_root, mint_preimage_cbor)`.

- `MidgardTxWitnessSetFullV1`
  - Three `(root, preimage_cbor)` pairs:
  - `addr_tx_wits`, `script_tx_wits`, `redeemer_tx_wits`.

- `MidgardTxFullV1`
  - `[version, compact, body_full, witness_set_full]`

## Why This Fits Current Aiken Proofs
- Existing proof steps consume compact roots from tx body (`spend_inputs`, `outputs`, validity info, etc.).
- This format keeps those roots first-class in compact and full payloads.
- Full payload carries corresponding preimages for offchain reconstruction/proof generation.

## Performance Notes
- Arrays remove map key parsing and key lookup overhead in hot paths.
- Fixed arity allows cheap structural validation.
- Compact payload can be decoded/hashed without decoding full preimages.

## Compatibility
- Legacy Cardano-derived codec paths are retained as migration bridges.
- A conversion helper is provided in node codec to map Cardano tx bytes into `MidgardTxFullV1`.
- New integrations should treat `MidgardTx*V1` as canonical.
