# T26 API Hardening and Regression Guards

## Scope

Add regression guards that keep the public `lucid-midgard` API aligned with
Midgard-native transaction semantics after the parity, provider, observer,
signing, compose, chain, safe, Effect, submitted, and documentation work lands.

This task is about preventing drift, not adding new feature behavior.

## Dependencies

- T13 API parity map.
- T14 provider switching and overrides.
- T15 provider convenience methods.
- T16 observer validator surface.
- T17 fromTx round trip.
- T18 sign builder layer.
- T19 partial signing.
- T20 compose builders.
- T21 chain local outputs.
- T22 safe and Effect APIs.
- T23 submitted and signed object ergonomics.
- T24 provider conformance and hardening.
- T25 documentation and examples.
- T09 script and redeemer builders.
- T10 local validation.

## Deliverables

- Public API snapshot or type-level export guard.
- Type tests for parity-map supported and unsupported API entries.
- Type tests for staged transaction objects and safe/Effect APIs.
- Golden or fixture tests for compose determinism.
- Golden or fixture tests for local output derivation from tx id, output index,
  and exact output CBOR.
- Regression tests for protected output marker preservation.
- Static guard against public Cardano canonical/noncanonical options.
- Static guard against normal-path Cardano transaction submission or
  Cardano-to-native conversion.
- Static guard against unsupported Cardano provider exports.
- Static guard against hard-coded production protocol defaults, including fee
  policy, network, native transaction version, maximum submit size, fallback
  protocol info, and dependency default cost models.
- Static guard against compatibility toggles, legacy aliases, and hidden
  fallback modes.
- Static guard banning Lucid's Cardano local-evaluation option from the
  `lucid-midgard` public option surface, package source, tests, docs, snippets,
  and Lucid-compatibility helpers. Midgard APIs must use `localValidation`
  instead.
- Error and reject-code snapshot tests.
- Documentation/example scan for forbidden shortcuts.

## Acceptance Criteria

- CI fails when public exports change without an intentional snapshot update.
- CI fails if a public option exposes Cardano canonical/noncanonical behavior.
- Cardano canonical/noncanonical options are allowed only if Midgard itself has
  two explicit encodings introduced by a protocol migration and covered by golden
  vectors.
- Compose remains deterministic for repeated runs with identical ordered
  fragments.
- Local chaining continues to use tx id, native transaction-body output indexes,
  and exact output CBOR. Authored output indexes are allowed only as explicit
  metadata mapped to body indexes.
- Protected output markers cannot be lost through decode/re-encode helpers.
- Safe and Effect APIs preserve structured errors and node reject codes.
- Observer validator APIs continue to reject `PlutusV1`, `PlutusV2`, withdrawal
  emulation, and reward-account observer shims.
- No `lucid-midgard` source, public type, example, test, doc snippet, adapter,
  or compatibility helper exposes or uses Lucid's Cardano local-evaluation
  option; Midgard local validation is controlled only through `localValidation`.

## Required Tests

- API snapshot or `tsd` tests for public exports.
- Type tests proving unsigned transactions are not submit-capable.
- Property or fixture tests for compose determinism and associativity.
- Fixture tests for local output derivation byte equality.
- Static scan for forbidden public options and compatibility toggles.
- Static scan for Cardano conversion as a normal submission path.
- Static scan for unsupported provider exports.
- Static scan for hard-coded production protocol defaults and dependency default
  cost-model reads.
- Static scan banning Lucid's Cardano local-evaluation option across source,
  public types, tests, docs, snippets, and compatibility helpers.
- Reject-code preservation tests for local validation and provider status.
- Documentation snippet compile tests.

## Current Implementation Notes

- API type guards cover supported Midgard entry points, unsupported Cardano
  staking/governance/provider surfaces, observer validator types,
  safe/Effect methods, sign-builder completion, staged transaction accessors,
  and provider interfaces.
- The parity-map snapshot now tracks implemented `addSignerKey`, `setMinFee`,
  builder `config`/`rawConfig`, sign-builder completion, and
  `toCBOR`/`toJSON`/`toHash` accessors.
- Regression tests cover compose determinism, exact local-output CBOR
  derivation, protected-output marker preservation, forbidden unsafe Effect
  helpers, provider language-profile hardening, and documentation/example
  shortcut scans.
- `demo/lucid-midgard` source, tests, README, examples, and local design docs no
  longer expose Lucid's Cardano local-evaluation completion option.

## Non-Goals

- No protocol serialization migration.
- No new compatibility mode.
- No benchmark shortcut that weakens validation or determinism.
- No broad refactor outside the public API and regression guard surface.
