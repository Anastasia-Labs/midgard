# Dependency patches

`@lucid-evolution__lucid@0.6.5.patch` fixes delayed-redeemer bootstrap for
transactions whose inputs fund an explicit fee. The unpatched builder assigns
the entire maximum transaction execution budget before the delayed redeemers
exist. That provisional fee exceeds the exact funding required by availability
challenge validators, so construction fails before actual evaluation.

For an explicitly supplied minimum fee only, bootstrap now uses provisional
zero execution units to construct the canonical redeemer context. The subsequent
resolved transaction still runs local UPLC evaluation and the existing fee and
execution checks. Automatic fee selection retains its conservative bootstrap.
Both published module formats are patched through pnpm's locked patch mechanism.
The external Lucid checkout and generated dependency files are not source
authorities for this repository change.

Canonical completion preserves inline datums, witness datums, and redeemer data
while sorting ledger containers. Redeemers keep compact canonical encodings when
their ordered Plutus value is unchanged; otherwise their original data is retained.
This preserves existing proof-size measurements. Recursively canonicalizing Plutus maps changes
their ordered on-chain values and datum hashes: for example, it reverses the
lexical token order `alpha`, `beta`. Output construction and minimum ADA use the
preserved datum encoding. Delayed callbacks, evaluation, final completion, and
canonical serialization use the same preservation helper; convergence checks
also distinguish changes in ordered redeemer data. Both published formats are
covered by `midgard-sdk/tests/lucid-inline-datum-preservation.test.ts`.

The Lucid patch also selects collateral from an explicit wallet
`getCollateral()` method when supplied. Wallets without that method retain the
existing selection from wallet inputs; an explicitly empty collateral list does
not fall back to ordinary spendable inputs. The
`@lucid-evolution__core-types@0.3.0.patch` declares this optional wallet method,
and `@lucid-evolution__wallet@0.2.2.patch` exposes a CIP-30 wallet's collateral
through it. Both JavaScript module formats and both type declaration formats
are covered by their respective patches.

The Lucid patch also reuses the last successful default Aiken evaluation within
one public transaction completion, including its internal fee, collateral, and
delayed-redeemer replays. Reuse requires exact transaction bytes, ordered input
and output CBOR bytes, cost models, CPU and memory budgets, and slot parameters.
The retained request and result bytes are detached; each hit decodes fresh
redeemer values. A changed request clears the entry, failures are not retained,
and separate completions do not share results. Explicit custom evaluators and
provider evaluation keep their existing behavior. All convergence checks and
application of the evaluated execution units still run.

Static transactions with explicit funding (`coinSelection: false`) and the
default local evaluator use one provisional evaluation before collateral
selection. The complete fee/change/collateral context still runs the full
convergence loop. The final built body's collateral must cover the exact ceiling
of its fee times the protocol collateral percentage; insufficient coverage fails
completion and reports the amount needed for a rebuild with `setCollateral`.
Automatic coin selection, delayed redeemers, and custom evaluators retain their
existing evaluation paths. Both formats are covered by
`midgard-sdk/tests/lucid-static-completion.test.ts`, including a real UPLC
fee-dependent execution-cost fixture and insufficient collateral refusal/retry.

The production SDK lifecycle tests in
`midgard-node/tests/availability-challenge-sdk-lifecycle.test.ts` verify signed
transactions with nonzero measured execution, fee refusal, and mainnet transaction
limits. These tests use the canonical delayed-redeemer callbacks throughout.

The existing `blake2b@2.1.4.patch` remains independently managed.

`@lucid-evolution__provider@0.2.4.patch` fixes emulator verification of native
reference scripts. The attached-script pass has already freed its signer list;
the reference-script pass now creates and frees its own list from the same
verified key hashes. Signature and timelock checks are unchanged. Both ESM and
CommonJS builds are patched. The reclaim builder test in
`midgard-fault-proofs/tests/submit-init-emulator-history-reclaim-builders.test.ts`
verifies deposit and withdrawal reclamation through reference-only native owner
authorization, including mismatched script and reference refusals.
