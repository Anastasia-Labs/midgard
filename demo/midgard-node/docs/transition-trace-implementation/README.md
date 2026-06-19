# Transition Trace Commitments Implementation Plan

This directory breaks `TRANSITION_TRACE_COMMITMENTS.md` into implementation tasks
for introducing transition trace commitments across the Midgard codebase.

The target model is:

```text
source event roots
  withdrawals_root
  forced_transactions_root
  transactions_root
  deposits_root

event_to_step_root
transition_trace_root
  step_index -> TransitionStep

final utxos_root
```

`transition_trace_root` proves the ordered state-machine execution. The source
roots prove what events exist. `event_to_step_root` proves exact-once coverage
between source events and trace steps.

The trace schema is intentionally minimal:

```text
TransitionStep {
  schema_version
  step_index
  event_key
  phase
  pre_utxos_root
  post_utxos_root
}
```

The step does not contain a claimed effect commitment. Fault proofs open the
source event by `event_key`, derive its expected effect from the event payload
and `pre_utxos_root`, and show the claimed `post_utxos_root` is wrong.

## Task Dependency Order

1. [Protocol schemas and ABI](./01-protocol-schemas-and-abi.md)
2. [Root primitives and proof witnesses](./02-root-primitives-and-proof-witnesses.md)
3. [Forced transaction source root](./03-forced-transaction-source-root.md)
4. [Trace and event-to-step builders](./04-trace-and-event-to-step-builders.md)
5. [Header, state queue, and settlement wiring](./05-header-state-queue-and-settlement-wiring.md)
6. [DA payloads and retained proof data](./06-da-payloads-and-retained-proof-data.md)
7. [Aiken transition fault proof validators](./07-aiken-transition-fault-proofs.md)
8. [Challenger tooling and node APIs](./08-challenger-tooling-and-node-apis.md)
9. [Conformance, budget, integration, and spec completion](./09-conformance-budget-integration-and-spec.md)

Tasks 1 and 2 define the ABI and proof surface. Tasks 3 through 6 wire block
production and data availability. Task 7 makes fraud proofs enforceable on L1.
Task 8 makes proofs buildable by challengers. Task 9 is the launch gate that
proves TypeScript, Aiken, DA, and the technical specification agree.

## Global Constraints

- Treat this as a clean `HeaderV2` redeploy. Do not preserve old header shapes
  through compatibility adapters in production paths.
- Preserve deterministic CBOR across TypeScript and Aiken.
- Preserve the protocol phase order:

  ```text
  withdrawals -> forced inclusion transactions -> normal L2 transactions -> deposits
  ```

- Disallow same-block deposit spending under this phase order.
- Keep `transactions_root` as normal accepted L2 transactions keyed by
  `MidgardTxId`.
- Add `forced_transactions_root` for L1 tx-order events keyed by the L1 order
  identity, not by the L2 transaction id.
- Do not use `inclusion_time` as a source-root key. Due-window and ordering
  checks use opened event payloads and L1 evidence.
- Every committed source event must have exactly one trace step.
- Every trace step must bind to exactly one committed source event.

