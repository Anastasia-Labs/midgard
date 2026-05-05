# Execution Decisions

This document records the initial decisions needed to execute the first
implementation slice. Revisit it before starting provider, script, or
migration work.

## Initial Package Location

Create the package at:

```text
demo/lucid-midgard
```

Add it to `demo/pnpm-workspace.yaml` beside `midgard-node` and `midgard-sdk`.
Use package name:

```text
@al-ft/lucid-midgard
```

Rationale: the first migration target is `demo/midgard-node` L2 transfer
construction, and the existing workspace already groups TypeScript Midgard
packages under `demo/`.

## Initial Scope

The first implementation slice covers:

- T01 package scaffold.
- T02 native codec surface.
- T03 core types.

It does not yet implement provider calls, wallet signing, transaction
completion, fee balancing, local validation, or submission.

## Codec Strategy

Decision: native v1 codec source belongs in `@al-ft/midgard-core`.

`midgard-node` and `@al-ft/lucid-midgard` must consume the shared codec source
instead of carrying independent implementations. Compatibility re-export shims
may remain at the previous package-local paths to keep existing imports stable,
but protocol serialization, root hashing, transaction id derivation, output
normalization, and Cardano bridge logic must live in shared core.

Conformance tests must exercise the shared package directly and through the
node/library consumers. The builder package should remain an ergonomic
construction layer, not the owner of protocol serialization.

## Provider API Gap

Decision: T04 must use stable node endpoints instead of encoding production
defaults in `lucid-midgard`.

Confirmed endpoint:

- `GET /tx-status?tx_hash=...` resolves canonical node transaction status.

Required endpoint to add or keep stable:

- `GET /protocol-info` returns API version, Midgard-native tx version,
  network, current slot, protocol fee parameters, submission limits, and
  validation strictness profile.

Until `GET /protocol-info` is available in the target node, any provider
implementation must require explicit fallback configuration and expose that
fallback source in diagnostics. No production values may be hard-coded.

## Local Validation Packaging

Decision: extract Phase A and Phase B into shared validation packages before
`lucid-midgard` exposes serious local validation.

The production path is shared source, not a mirrored implementation. Conformance
tests remain required, but they verify shared behavior across node and library
consumers. The library must preserve node reject codes and must describe local
validation as preflight only, never as final validity.

## Script Integrity Policy

Decision: `scriptIntegrityHash` commits to `redeemerTxWitsRoot` and canonical
`scriptLanguageViews`.

```text
scriptLanguageViews =
  Map<ScriptLanguageTag, CanonicalCostModelView>

scriptIntegrityHash =
  blake2b256(cbor([
    redeemerTxWitsRoot,
    scriptLanguageViews
  ]))
```

Initial stable numeric tags are:

- `2`: `PlutusV3`.
- `128`: `MidgardV1`.

The tag defines the semantic branch: hash domain, context shape, allowed
purposes, and evaluator rules. The map value is only the canonical ordered
integer cost-model view. `contextAbi` is not encoded separately.

Initial `PlutusV3` and `MidgardV1` views are frozen protocol constants matching
Harmonic `defaultV3Costs` at the time of adoption. Dependency bumps must not
change protocol cost models unless an explicit protocol migration changes these
constants.

Phase A checks presence or absence only. Phase B resolves inline and reference
scripts, derives the required non-native language set, rebuilds the canonical
map, and rejects if the body hash does not match. Empty, no-script, and
native-only transactions continue to use
`EMPTY_NULL_ROOT = blake2b256(cbor(null))`.

## Release Scope

The first usable release should target deterministic pubkey L2 transfers. Script
spends, minting, observers, and protected script receive outputs are subsequent
milestones unless explicitly promoted.
