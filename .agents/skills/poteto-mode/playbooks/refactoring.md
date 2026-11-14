# Refactoring

Read [the host adapter](../references/host-adapter.md) before host-dependent operations.

1. Pin the existing behavior and the structural improvement. Identify every caller and protocol invariant.

2. Read the owning cleanup or module-split skill. For a pure split, [splitting-oversized-modules](../../splitting-oversized-modules/SKILL.md) owns the proof and separate commit.

3. Move in small verifiable steps. Migrate obsolete undeployed APIs in place while preserving deployed-state obligations.

4. Run the behavior pin and required checks. Review missed string, fixture, docs, and generated references.

5. Report the structural change and evidence that behavior stayed the same.
