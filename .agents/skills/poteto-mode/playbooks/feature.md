# Feature

Read [the host adapter](../references/host-adapter.md) before host-dependent operations.

1. State the user-visible outcome, input/state shape, boundaries, and failure behavior. Trace existing callers before designing.

2. Design consequential interfaces with architect. For multi-step work, state a throughput checkpoint: independent slices, the critical dependency, and the next verifiable increment.

3. Implement the smallest working end-to-end increment. Delegate only across proven independent seams, using feature, refactoring roles.

4. Run narrow behavior checks and required Midgard checks. Exercise the real CLI, service, emulator, or devnet when that is the affected surface.

5. Review the final diff and report the outcome, verification, and limits.
