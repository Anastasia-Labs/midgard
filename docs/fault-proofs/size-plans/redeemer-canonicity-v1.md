# Redeemer canonicity V1 physical and fit plan

Category `redeemerCanonicity` has frozen ID `00000028` and covers only
`RedeemerMalformed { redeemer_index }`.

## Physical chain

1. `fraud_proofs/redeemer_canonicity/step_01.main.spend` binds the accepted or
   forced transaction, exact direction, exact typed reason, and redeemer index.
   It imports only the native transaction source verifier and the narrow
   redeemer-canonicity coordinate rule.
2. `fraud_proofs/redeemer_canonicity/step_02.main.spend` authenticates the exact
   field-8 item through a `FieldOpeningV1`, total-decodes its narrow four-field
   envelope, and applies the canonical Plutus-Data predicate to the exact
   embedded byte string. Malformed input returns a verdict rather than aborting
   and only the terminal state can proceed to step 3.
3. `fraud_proofs/redeemer_canonicity/step_03.main.spend` applies the terminal
   contradiction and burns the computation thread while permanently minting
   the fraud proof.

Application order is `(step_02_hash, computation_thread_policy, hub_oracle)`,
`(step_03_hash, computation_thread_policy,
field_preimage_certificate_policy)`, then `(fraud_policy, fraud_address,
computation_thread_policy)`.

## Maximum evidence and fit

The maximum case is the consensus redeemer-item frontier carried through
certified field-8 bytes, with the most expensive nested Plutus Data walk.
The fit suite publishes every applied reference script and records signed byte,
memory, and CPU margins for init, opening/first decode, the worst resume,
cancel, final proof mint, and state-queue removal. No positive path raises a
protocol limit or disables local UPLC evaluation. The machine-readable ledger
is `redeemer-canonicity-v1-fit-ledger.json`.
