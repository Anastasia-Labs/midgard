# Midgard Phase-1 Transaction Validation Rules

## Purpose

This document defines the phase-1 validation rules that `midgard-node` enforces before a transaction is accepted into mempool state and eligible for inclusion in a committed block.

Scope of this phase:

- Include: CBOR/structure checks, stateless checks, UTxO/stateful checks, signature and witness availability checks.
- Include: supported Plutus witness-bundle execution through the configured evaluator when a tx carries Plutus scripts/redeemers/datum witnesses.

## Pipeline in `basic-deposit`

Transactions are queued by `POST /submit` and validated by `tx-queue-processor` before they are inserted into `MempoolDB`.

- Accepted transactions are inserted into mempool tables.
- Rejected transactions are persisted in `tx_rejections` with reject code and details.
- Batches are triggered on `VALIDATION_BATCH_SIZE` or `VALIDATION_MAX_QUEUE_AGE_MS`, whichever comes first.

## Rule Set

## R1. Decodable Transaction CBOR

Rule:
The submitted `tx_cbor` must decode into a supported Midgard transaction structure.

Reject code:
`E_CBOR_DESERIALIZATION`

## R2. Transaction Hash Integrity

Rule:
The transaction identifier must match the hash recomputed from the decoded transaction body.

Reject code:
`E_TX_HASH_MISMATCH`

## R3. Supported Field Set Only

Rule:
Fields outside Midgard's supported subset must be absent or empty.
Currently enforced unsupported fields include:

- `collateral_inputs`
- `collateral_return`
- `total_collateral`
- `voting_procedures`
- `proposal_procedures`
- `current_treasury_value`
- `donation`
- witness-set fields: `bootstrap_witnesses`

The native tx codec can preserve several additional Cardano-shaped fields for
round-tripping and observability:

- zero-ADA script withdrawals normalized into `required_observers`
- non-empty native-script mint maps
- `script_data_hash`
- redeemer bytes
- datum witness bytes
- Plutus script witness bytes

Phase-1 validation support is narrower than full Cardano semantics:

- zero-ADA script withdrawals are validated only after normalization into
  `required_observers`
- non-empty mint is validated only for native-script policies
- Plutus witness material is supported only as one coherent bundle:
  - `script_data_hash`
  - redeemers
  - Plutus scripts
  - datum witnesses
  - exact spent/reference-input UTxO context
- collateral and related Cardano phase-2 side channels remain unsupported

Reject code:
`E_UNSUPPORTED_FIELD_NONEMPTY`

## R4. At Least One Input

Rule:
A transaction must contain at least one spend input.

Reject code:
`E_EMPTY_INPUTS`

## R5. No Duplicate Inputs Within a Transaction

Rule:
Spend inputs must be unique by outref within a single transaction.

Reject code:
`E_DUPLICATE_INPUT_IN_TX`

## R6. Output Structure Validity

Rule:
Each output must be structurally valid and serializable, with a valid address and valid non-negative value encoding.

Reject code:
`E_INVALID_OUTPUT`

## R7. Input Existence

Rule:
Every consumed input must exist in available pre-state, or be produced by an earlier accepted transaction in the same deterministic batch.

Reject code:
`E_INPUT_NOT_FOUND`

## R8. No Double Spend Across State/Batch

Rule:
No outref may be consumed more than once across accepted transactions in the current validation window.

Reject code:
`E_DOUBLE_SPEND`

## R8b. Reference Input Existence

Rule:
Reference inputs are allowed. Every reference input must exist in current UTxO state at validation time and must not be spent by the same transaction.

Reject code:
`E_INPUT_NOT_FOUND`

## R9. Validity Interval Well-Formed

Rule:
Validity bounds must be coherently encoded and ordered.

Reject code:
`E_INVALID_VALIDITY_INTERVAL_FORMAT`

## R10. Validity Interval Compatible With Inclusion Interval

Rule:
The transaction validity interval must be compatible with the current Cardano
slot used for inclusion-time validation.

Reject code:
`E_VALIDITY_INTERVAL_MISMATCH`

## R11. Minimum Fee

Rule:
`fee(tx)` must satisfy configured minimum fee requirements.

Reject code:
`E_MIN_FEE`

## R12. Value Preservation

Rule:
Value conservation is checked as:

`totalInputs - fee + mintedValue - burnedValue == totalOutputs`

Reject code:
`E_VALUE_NOT_PRESERVED`

## R13. Required Witness Presence

Rule:
Required signers in the tx body must have corresponding witness material present.

Reject code:
`E_MISSING_REQUIRED_WITNESS`

## R14. VKey Signature Validity

Rule:
Provided VKey signatures must verify against the transaction body hash.

Reject code:
`E_INVALID_SIGNATURE`

## R15. Native Script Availability and Validation

Rule:
If native scripts are used, required scripts must be present and validate under
phase-1 semantics. This includes script-credential inputs, declared observers,
and declared native minting policies.

Reject code:
`E_NATIVE_SCRIPT_INVALID`

## R16. Input-Witness Consistency

Rule:
For each consumed input:

- PubKey inputs must have a corresponding VKey witness.
- Script inputs must have a corresponding native script witness.

This is enforced statefully in phase-B against the input UTxO set, not only via `required_signers`.

Reject code:
`E_MISSING_REQUIRED_WITNESS`

## R17. No Dependency Cycles

Rule:
Transactions in the same validation batch must not form a dependency cycle through spent/reference relations.

Reject code:
`E_DEPENDENCY_CYCLE`

## R18. Cascaded Dependency Rejection

Rule:
If a transaction is rejected, all descendants that depend on its outputs are rejected without additional stateful work.

Reject code:
`E_DEPENDS_ON_REJECTED_TX`

## Midgard-Specific Rules

## R19. `is_valid` Must Be `true`

Rule:
Transactions with `is_valid = false` are rejected in this pipeline.

Reject code:
`E_IS_VALID_FALSE_FORBIDDEN`

## R20. No Auxiliary Data Payload

Rule:
Auxiliary data payloads are not carried in the native tx format. The codec may
preserve `auxiliary_data_hash` for round-trip/export purposes, but phase-1
admission rejects non-empty auxiliary-data hashes for user transactions because
the corresponding payload is unavailable to validate or reconstruct.

Reject code:
`E_AUX_DATA_FORBIDDEN`

## R21. Certificates Empty

Rule:
Certificates must be absent/empty for Midgard L2 user transactions.

Reject code:
`E_CERTIFICATES_FORBIDDEN`

## R22. Withdrawals Must Be Zero-ADA Script Observers Only

Rule:
Any withdrawal entry other than a zero-ADA withdrawal from a script reward
account is invalid. Valid zero-ADA script withdrawals are normalized into
Midgard `required_observers` and must be satisfied by native-script witness
material during validation.

Implementation note:
For Cardano-shaped ingress, invalid withdrawals are rejected during submit-path
normalization before phase-1 native validation begins. Phase-1 itself validates
only the normalized `required_observers` representation.

## R23. Mint Witnesses Must Be Satisfied And Value Must Be Preserved

Rule:
Non-empty mint is supported only when every declared policy is satisfiable by
the witness material declared in the tx and the transaction preserves value
under the mint-aware accounting rule.

For native policies, phase-1 verifies the native script directly against witness
signers and validity bounds.

For Plutus policies, phase-1 requires the full Plutus evaluator path described
in `R24`; the mint policy is considered supported only when the evaluator is
configured and the reconstructed Cardano tx validates successfully.

Reject code:
`E_MISSING_REQUIRED_WITNESS`, `E_NATIVE_SCRIPT_INVALID`,
`E_PLUTUS_EVALUATION_UNAVAILABLE`, `E_PLUTUS_SCRIPT_INVALID`,
`E_VALUE_NOT_PRESERVED`, or `E_INVALID_FIELD_TYPE` depending on the failure mode

## R24. Plutus Witness Bundle Requires Evaluator Validation

Rule:
When a tx carries Plutus witness material, phase B reconstructs the Cardano tx,
derives the exact spent/reference-input UTxO context from the effective ledger
state, and validates the tx through the configured evaluator. The node does not
accept Plutus witness bundles without executing that path.

Compatibility note:
Because the evaluator runs on reconstructed Cardano tx bytes, native txs whose
VKey witnesses were signed against the Midgard body hash are not eligible for
this path. Phase-1 accepts evaluator-backed Plutus validation only when the tx
either carries no VKey witnesses or arrived with a Cardano witness-body hash
override from Cardano ingress normalization.

Supported bundle members:

- non-empty `script_data_hash`
- redeemers
- datum witnesses
- inline Plutus scripts
- reference-input Plutus scripts
- Plutus-governed spend, observer, and mint purposes as represented by the
  reconstructed Cardano tx and its redeemer pointers

Reject code:
`E_PLUTUS_EVALUATION_UNAVAILABLE`, `E_PLUTUS_SCRIPT_INVALID`,
`E_INVALID_FIELD_TYPE`, or `E_MISSING_REQUIRED_WITNESS` depending on the
failure mode

## R25. Network Consistency

Rule:
When `network_id` is present, it must match node configuration.

Reject code:
`E_NETWORK_ID_MISMATCH`

## Determinism and Parallelization Notes

The validation pipeline runs in grouped stages:

1. Parse/hash checks (parallel-safe).
2. Structural checks (parallel-safe).
3. Witness/crypto checks (parallel-safe).
4. Dependency graph filtering and cascade rejection.
5. Stateful checks by dependency waves with deterministic arrival-order merge.

Deterministic ordering is required so validation outcomes are reproducible and consistent across runs.
