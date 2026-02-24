# Midgard Phase-1 Transaction Validation Rules

## Purpose

This document defines the phase-1 validation rules that `midgard-node` enforces before a transaction is accepted into mempool state and eligible for inclusion in a committed block.

Scope of this phase:

- Include: CBOR/structure checks, stateless checks, UTxO/stateful checks, signature and witness availability checks.
- Exclude: Plutus phase-2 script execution.

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
- `script_data_hash`
- `voting_procedures`
- `proposal_procedures`
- `current_treasury_value`
- `donation`
- witness-set fields: `bootstrap_witnesses`, `plutus_v1_scripts`, `plutus_v2_scripts`, `plutus_v3_scripts`, `plutus_datums`, `redeemers`

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
The transaction validity interval must be compatible with the block/event interval used for inclusion.

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
If native scripts are used, required scripts must be present and validate under phase-1 semantics.

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

## R20. No Auxiliary Data

Rule:
Auxiliary data must be null/absent according to Midgard constraints.

Reject code:
`E_AUX_DATA_FORBIDDEN`

## R21. Certificates Empty

Rule:
Certificates must be absent/empty for Midgard L2 user transactions.

Reject code:
`E_CERTIFICATES_FORBIDDEN`

## R22. Withdrawals Must Be Zero/Absent

Rule:
Any withdrawal entry other than zero-equivalent is invalid.

Reject code:
`E_NONZERO_WITHDRAWAL`

## R23. No Minting in L2 User Transaction Path

Rule:
Mint field must be absent/empty for transactions in this path.

Reject code:
`E_MINT_FORBIDDEN`

## R24. Network Consistency

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
