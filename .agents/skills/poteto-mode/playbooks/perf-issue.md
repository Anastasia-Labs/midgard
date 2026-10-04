# Performance

Read [the host adapter](../references/host-adapter.md) before host-dependent operations.

1. Read [benchmark-checklist](../../benchmark-checklist/SKILL.md). Define the metric, workload, sample method, baseline SHA, and target before changing code.

2. Capture and reduce a real profile or measurement. Confirm the mechanism and rule out measuring setup, cache, or unrelated load.

3. Change the smallest demonstrated bottleneck using the perf-issue role. Preserve correctness and liveness invariants.

4. Remeasure using the same method and comparable environment. Verify required behavior checks independently of performance.

5. Report baseline and candidate SHAs, method, measured change, variance, and correctness evidence.
