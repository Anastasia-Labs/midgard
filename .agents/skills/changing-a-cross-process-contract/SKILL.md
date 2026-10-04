---
name: changing-a-cross-process-contract
description: Change a serialized Midgard interface shared by parent/worker or TypeScript/native processes. Use for lease-owner identities, worker payloads, message schemas, native protocol versions or source/dist behavior differences that require compiled producer-consumer conformance.
---

# Changing a cross-process contract

Read [the boundary and discovery commands](../../../docs/agents/contrib.md#integration-and-program-resumes)
before guessing producer/consumer filenames or compiling temporary probes.

Use `contrib locate --symbol` and declared package exports to find both owners.
Put constructors/parsers in their existing shared boundary module. For commitment
owners, that module is `@al-ft/midgard-core/commit-lease-owner`; independent
prefix construction would recreate the parent/worker mismatch.

Run the focused source test for malformed payloads, then `contrib boundary`
and the `process-boundaries` gate against fresh compiled artifacts. Assert the
intended state transition, not merely message acceptance. [review]
For native changes, use `contrib native` so the binary and compiler identity are
recorded; retain the existing protocol's caller conformance tests.

Finish with the shared schema/owner, exact compiled receipts and any consumer
that could not be exercised. A source resolver pass alone does not close a
compiled boundary change. [review]
