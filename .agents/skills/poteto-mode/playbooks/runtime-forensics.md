# Runtime forensics

Read [the host adapter](../references/host-adapter.md) before host-dependent operations.

1. Capture a real signal from the affected owned process: CPU profile, heap snapshot, trace, logs, or transaction evidence.

2. Reduce it to a concrete hot path, retainer chain, retry loop, or timing mechanism. Use bounded read-only analysis for large artifacts.

3. Confirm the mechanism in an isolated test instance. Instrumentation that changes protocol behavior follows [production-L2 guidance](../../../../docs/agents/production-l2.md).

4. Map the finding to source and return a cited diagnosis, evidence paths, and gaps. Implement a fix only when within the requested scope.
