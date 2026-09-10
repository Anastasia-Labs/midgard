# Midgard L2 Transaction Evaluation

Status: Active

Last reviewed: 2026-09-07 (ingress and validation source map).

This is an implementation guide to admission and validation, not another wire
specification. [Midgard transaction format](../../../docs/spec/midgard-tx.md)
owns the concrete format; the [consensus profile](../../../docs/consensus-profile-v1.md)
owns the profile reference. Encoding details belong there and in the shared
codec, rather than in a copied field-count or line-number table here.

## Admission to mempool

1. `POST /submit` checks the required submission media type, body limits,
   and ingress capacity, then decodes the canonical submission envelope.
2. The shared native decoder verifies the submitted transaction and supplies its
   transaction ID and canonical bytes. Invalid native input is rejected; ingress
   does not fall back to converting a Cardano transaction.
3. Durable admission records or matches the transaction. A new admission returns
   HTTP 202; a duplicate returns HTTP 200 with its existing status. This is queue
   admission, not successful ledger validation or settlement.
4. The queue processor leases admissions in arrival order, runs Phase A, and
   passes surviving transactions to stateful Phase B.
5. Phase B resolves ledger dependencies, validates against the selected state,
   and returns accepted/rejected outcomes and a UTxO patch. The node's validation
   commit path reconciles that result with durable admission and mempool state.

Transport/backlog refusal, validation rejection, and retryable operational
failure are distinct outcomes. Use the handler's error response and admission
status rather than treating every failure as an invalid transaction.

## Source map

| Responsibility                                         | Implementation                                                                                                                             |
| ------------------------------------------------------ | ------------------------------------------------------------------------------------------------------------------------------------------ |
| HTTP media type, bounded body read, capacity, response | [`postSubmitHandler`](../src/commands/listen-router.ts)                                                                                    |
| Canonical submission normalization                     | [`normalizeSubmitTxCanonicalCborToNative`](../src/commands/listen-utils.ts)                                                                |
| Durable admission and leases                           | [`txAdmissions.ts`](../src/database/txAdmissions.ts)                                                                                       |
| Batch sequencing and validation orchestration          | [`tx-queue-processor.ts`](../src/fibers/tx-queue-processor.ts)                                                                             |
| Native format and output codecs                        | [`midgard-core/src/codec/`](../../midgard-core/src/codec)                                                                                  |
| Stateless validation                                   | [`validatePhaseASingle` / `runPhaseAValidation`](../../midgard-validation/src/phase-a.ts)                                                  |
| Dependency-aware state validation                      | [`runPhaseBValidationWithPatch`](../../midgard-validation/src/phase-b.ts)                                                                  |
| Script execution and budget handling                   | [`local-script-eval.ts`](../../midgard-validation/src/local-script-eval.ts)                                                                |
| Script source and context construction                 | [`script-source.ts`](../../midgard-validation/src/script-source.ts), [`script-context.ts`](../../midgard-validation/src/script-context.ts) |
| Redeemer purpose/index semantics                       | [`midgard-redeemers.ts`](../../midgard-validation/src/midgard-redeemers.ts)                                                                |
| Canonical ledger-output material                       | [`ledger-output-descriptor.ts`](../../midgard-validation/src/ledger-output-descriptor.ts)                                                  |

Protocol-info language advertisement is the exact
`MIDGARD_SUPPORTED_SCRIPT_LANGUAGES` set in the core codec. Native-script handling
inside validation does not add `NativeCardano` to that advertised set.

## Focused verification

From `demo/midgard-node`:

```sh
pnpm exec vitest run tests/listen-admission-auth.test.ts tests/native-transaction-integration.test.ts tests/validation-parallelization.test.ts
```

These scenarios exercise ingress, transaction integration, and validation
sequencing. For a codec or script semantic change, also run the affected core
and validation package tests; an ingress test cannot establish script correctness.
Check that the selected files collect tests and report their passing counts.

Acceptance into the mempool is not block inclusion, L1 confirmation, merge,
or withdrawal payability. Those boundaries have their own workflows and
acceptance evidence.
