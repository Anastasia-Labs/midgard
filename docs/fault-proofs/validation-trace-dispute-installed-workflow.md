# Validation trace dispute workflow

`validationTraceDispute` is the interactive fault-proof family. Its on-chain
validators are deployed, but this release installs no off-chain workflow for
it, and none for the trace-relative families listed in
`UNINSTALLED_FRAUD_PROOF_CATEGORIES`
([installed categories](../../demo/midgard-fault-proofs/src/workflow/installed-categories.ts)).

Their evidence is read from per-transaction validation trace states. The DA
payload does not carry them: its `validation_trace_witnesses` section is always
empty, and each validation trace descriptor is a placeholder that binds the
event key, the transaction commitment and the verdict. The watcher reports these
categories in `WATCHER_MISSING_WORKFLOW_CATEGORIES`, the catalogue's complement
of the
[family application registry](../../demo/midgard-fault-proofs/src/workflow/family-application-registry.ts).

## Deployed chain

The deployed interactive chain is `VALIDATION_TRACE_DISPUTE_STEP_COUNT` steps
long. The SDK family builder composes the `steps` array (control validators,
proof item, semantic resolvers, the shared ScriptSources item chain, the CEK
context and item stages, the canonical decode item stages and the prepare
resolvers) and fails the build if its length differs from that constant.

## Verification

[Resolver publication](../../demo/midgard-fault-proofs/tests/validation-trace-resolver-publication.test.ts)
measures every current semantic resolver and compares the complete saved ledger
on ordinary runs. It establishes publication only.
