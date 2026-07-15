# midgard-core

Shared Midgard protocol primitives.

This package owns the Midgard native transaction v1 codec used by
`midgard-node` and `lucid-midgard`. It also owns the stable DA libp2p transport
envelopes, protocol identifiers, and manifest identity types used by the node
and DA committee service. Change codec semantics here first and verify them
through the shared conformance tests; pre-launch legacy codec compatibility is
not a package goal.
