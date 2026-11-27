# Contributor verification features

Use this map when verifying the contributor CLI or changing its behavior. The
CLI is a user surface; these checks do not establish live rollup acceptance.
For that surface, use the [acceptance skill](../../midgard-e2e-acceptance/SKILL.md). [review]

| Feature                                                       | Observable proof                                         |
| ------------------------------------------------------------- | -------------------------------------------------------- |
| [Preparation planning](preparation-planning.md)               | Correct prerequisites and freshness without building     |
| [Focused test evidence](focused-test-evidence.md)             | Selected assertions run and a verifiable receipt remains |
| [Compiled package boundaries](compiled-package-boundaries.md) | Declared compiled entrypoints load under ordinary Node   |

Launch from the selected checkout with its declared Node version; this CLI
needs no long-lived server. `node scripts/contrib.mjs --help` is the control
surface doctor. Before executing suites or builds, use the environment doctor
in the [owning skill](../SKILL.md#start-with-doctor). Missing build capabilities
do not prevent inspecting a preparation plan. [review]

Capture the exact command, checkout/ref, exit code and JSON output in a unique
evidence directory outside disposable fixture state. For an execution, preserve
its receipt and referenced logs too. Plans prove only selection, and receipts
prove only the assertions actually run. [review]

The CLI exits after each action. Leave shared services alone; remove only
scratch fixtures created by this verification. Confirm the evidence files still
exist after cleanup. [review]
