# Autonomous run

Read [the host adapter](../references/host-adapter.md) before host-dependent operations.

1. State the observable exit predicate, authorized scope, and any user-defined budget or stop condition.

2. Drive one verifiable increment at a time. Use native bounded waits for external events. Durable scheduling uses only a requested host automation; otherwise leave an explicit checkpoint.

3. Keep successful increments, remove only owned unsuccessful experiments, and log decisions with show-me-your-work.

4. Continue through routine fixable failures while preserving the predicate. Report a genuine dependency on user input or external state accurately.

5. Check the final predicate on the artifact and report what finished, what was discarded, checks, and remaining blockers.
