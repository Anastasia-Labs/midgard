# Validation trace dispute workflow

`validationTraceDispute` is the interactive fault-proof family. Its on-chain
validators are deployed and this release installs its off-chain workflow: the
[family application record](../../demo/midgard-fault-proofs/src/workflow/family-application-registry.validation-trace-dispute-family-application-record.ts)
binds the
[manifest-bound runner](../../demo/midgard-fault-proofs/src/validation-trace-dispute/workflow.ts)
to the deployment the host loaded, and the classifier's
[catalogue replay](../../demo/midgard-fault-proofs/src/workflow/catalogue-replay.ts)
re-executes each committed block's scripts on its admitted predecessor. A
script re-execution that throws (rather than deciding) holds only its event;
the block's objective names `validation_trace_replay_held`, or
`validation_trace_replay_stuck` once the same failure repeats, and retries
after a backoff. Neither is a verdict.

No off-chain workflow is installed for the trace-relative families listed in
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
