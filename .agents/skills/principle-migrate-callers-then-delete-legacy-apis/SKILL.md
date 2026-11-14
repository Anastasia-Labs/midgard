---
name: principle-migrate-callers-then-delete-legacy-apis
description: "Apply when introducing a new internal API while old callers still exist. Migrate callers and delete the old API in the same wave instead of preserving compatibility layers."
disable-model-invocation: true
---

# Migrate Callers Then Delete Legacy APIs

When we decide a new API is the right design, migrate callers and remove the old API in the same refactor wave instead of preserving compatibility layers.

**Rule:**
- Do not keep legacy API paths only because internal callers still exist [review]
- Inventory callers, migrate them, and delete the old API immediately [review]
- Treat temporary adapters as exceptional and time-boxed, not default architecture [review]
- Update tests to assert the new contract, and delete tests that only protect pre-refactor implementation details [review]

**When this applies:**
- No external users depend on backward compatibility [review]
- The project can absorb coordinated breaking changes [review]
- The new API is part of a simplification or refactor initiative [review]

Keeping both old and new APIs creates dual-path complexity, slows cleanup, and makes the codebase feel append-only.
