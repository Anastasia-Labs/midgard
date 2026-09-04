# `zeroInput` wrongful-rejection extension V1 size plan

## Scope and semantics

This extension preserves the deployed two-step `zeroInput` topology. The
decisive predicate remains the canonical field-0 item-count rule:
`input_count == 0`. Direction A convicts an accepted transaction only when the
predicate is true; direction B convicts a forced `EmptyInputs` rejection only
when the authenticated field-0 count is strictly positive. The prover never
supplies a verdict, reason, count, or actuator decision.

## Physical validators

1. `fraud_proofs_zero_input_step_01`: authenticates either the existing normal
   transaction inclusion carriage or an exact forced-root leaf, binds the
   forced leaf to the computation-thread header hash, and forwards the
   canonical `VerdictSubjectV1`. It imports only the shared proof-thread
   substrate and native transaction verifier.
2. `fraud_proofs_zero_input_step_02`: re-authenticates field 0 through the
   field-opening door and applies the terminal polarity to the on-chain item
   count. It imports only the zero-input rule, field-opening door, and shared
   computation-thread finalizer.

Maximum dynamic evidence is one native transaction inclusion carriage at step
01 and one complete field-0 opening at step 02. There is no resumable loop: a
single authenticated item count proves non-emptiness, while the field door
authenticates the complete preimage and therefore also proves emptiness.

## State and transitions

`Init -> step01 -> step02 -> permanent proof mint`, with cancellation available
at step01 and step02. Step01 produces
`StateV1 { subject: VerdictSubjectV1 }`; step02 consumes it and burns the thread
token through the shared finalizer. Restarts reconstruct the next action from
the datum plus authenticated L1/reference inputs.

## Fit test

Build with the pinned Aiken compiler and `--env testnet`, publish both applied
scripts as real reference scripts under the 16,384-byte L1 envelope, and run
real Lucid Evolution transactions for maximum direct/published carriage,
cancel/restart, terminal mint, and leased state-queue removal. The ledger must
record positive margins against 16,384 signed bytes, 15,872 publication bytes,
16,500,000 memory units, and 10,000,000,000 CPU units.
