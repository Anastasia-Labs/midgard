# Midgard Node Compatibility

This file applies to `demo/midgard-node`.

Backward compatibility is never a goal for `demo/midgard-node`. Midgard has not
launched, so prefer the current canonical production design over support for
older in-repo behavior.

Do not add:

- Compatibility modes
- Fallback paths
- Alias lookups
- Dual-ID behavior
- Legacy-format support
- Operator toggles intended to preserve old behavior
- Migration shims, unless the user explicitly asks for an isolated migration
  tool

Tests should prove the intended current canonical behavior, not compatibility
with abandoned pre-launch shapes.
