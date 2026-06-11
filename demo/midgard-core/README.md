# midgard-core

Shared Midgard protocol primitives.

This package owns the Midgard native transaction v1 codec used by
`midgard-node` and `lucid-midgard`. Downstream packages may keep compatibility
re-export paths, but codec semantics should be changed here first and verified
through shared conformance tests.
