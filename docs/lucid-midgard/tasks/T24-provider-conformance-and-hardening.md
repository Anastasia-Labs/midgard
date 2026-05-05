# T24 Provider Conformance and Hardening

## Scope

Harden the `MidgardProvider` contract and add conformance coverage proving that
providers expose Midgard-native facts, bytes, errors, and diagnostics. The
provider interface must remain Midgard-aware and must not collapse into a
generic Cardano or Lucid provider abstraction.

## Dependencies

- T04 provider client.
- T09 script and redeemer builders.
- T11 submit status chain.
- T14 provider switching and overrides.
- T15 provider convenience methods.
- T16 observer validator surface.
- Architecture docs `04`, `07`, and `09`.

## Deliverables

- Provider conformance test suite reusable by `MidgardNodeProvider` and future
  provider implementations.
- Hardened `MidgardProvider` documentation describing required Midgard-native
  semantics for UTxOs, protocol info, submission, status, diagnostics, and local
  validation pre-state.
- Strict validation that provider protocol info supplies API version, Midgard
  native transaction version, network, current slot, fee parameters, submission
  limits, validation strictness profile, and supported non-native script
  language tags.
- Stable source-of-truth rule for supported script languages:
  `GET /protocol-info` must advertise supported language tags and their stable
  numeric protocol tags, or an explicitly named shared protocol profile from
  `@al-ft/midgard-core` must be selected. Omitted, extra, `PlutusV1`,
  `PlutusV2`, unknown, or future language tags fail closed unless a protocol
  migration updates the shared constants and fixtures.
- Network and native transaction version mismatch checks during
  `LucidMidgard.new(...)` or first provider use.
- Submit-side validation for hex encoding and
  `submissionLimits.maxSubmitTxCborBytes`.
- Provider diagnostics that distinguish node-derived protocol facts from
  explicit fallback facts.
- Error classification rules for payload, capability, HTTP, transport,
  retryable, and non-retryable failures.
- Secret-safe diagnostics and error messages for endpoints containing API keys or
  credentials.
- Conformance fixtures covering observer-bearing native transactions and node
  reject-code preservation.

## Acceptance Criteria

- `MidgardProvider` remains typed around `MidgardUtxo`, Midgard native
  transaction CBOR, Midgard protocol info, and Midgard transaction status.
- Providers do not hard-code production fee policy, slot, network id, native tx
  version, language support, or maximum transaction size.
- `GET /protocol-info` remains the canonical source of node-derived provider
  facts for `MidgardNodeProvider`.
- Missing `GET /protocol-info` fails closed unless an explicit fallback is
  supplied.
- Fallback protocol info is validated, exposed in diagnostics, and never silently
  treated as node-derived production state.
- Address UTxO queries reject substituted outputs for other addresses.
- Outref queries reject substituted, malformed, duplicate, or unexpected UTxOs.
- Submission preserves durable admission semantics and does not label `202` as
  final transaction validity.
- Transaction status preserves node reject codes and details verbatim, including
  observer-related reject codes.
- Provider and builder surfaces recognize only `PlutusV3` and `MidgardV1` as
  supported non-native language tags.
- Provider language support is validated from protocol info or the explicit
  shared protocol profile; it is not hard-coded in provider convenience methods.
- Provider failures needed for local validation pre-state are surfaced as
  structured insufficiency or provider errors, not warnings.

## Required Tests

- Shared provider conformance suite run against `MidgardNodeProvider`.
- Protocol info success, malformed payload, unavailable-without-fallback, and
  explicit-fallback tests.
- Missing, malformed, or incompatible `apiVersion` tests.
- Supported-language protocol info/profile tests for exact
  `PlutusV3`/`MidgardV1` support, omitted tags, extra tags, `PlutusV1`,
  `PlutusV2`, and unknown/future tags.
- Network mismatch and native transaction version mismatch rejection tests.
- Submit hex validation and maximum CBOR size rejection tests.
- UTxO address-query substitution rejection test.
- Single and batch outref substitution, duplicate, malformed, and missing UTxO
  tests.
- Transaction status unknown-status, wrong-tx-id, rejected-code, and transport
  failure tests.
- Retry classification tests for conflict, backlog, HTTP failure, and transport
  failure.
- Diagnostics redaction test for endpoints with credentials or API keys.
- Observer-bearing transaction submission/status fixture proving reject codes are
  preserved without Cardano-domain translation.
- Language hardening tests proving `PlutusV1` and `PlutusV2` are rejected at
  provider/builder boundaries where untyped payloads can enter.
- Unknown/future language tag rejection tests at provider/builder ingress.
- Local validation pre-state tests for missing spend pre-state, missing
  reference pre-state, script-source fetch failures, and diagnostics for each.
- Explicit capability-error and non-retryable-error classification tests.

## Current Implementation Notes

- `MidgardProtocolInfo.supportedScriptLanguages` is mandatory and validated
  against the shared `MIDGARD_SUPPORTED_SCRIPT_LANGUAGES` protocol constants.
- `MidgardNodeProvider.create(...)` rejects omitted, extra, `PlutusV1`,
  `PlutusV2`, unknown, and tag/name-mismatched language payloads.
- `demo/midgard-node` `GET /protocol-info` now advertises the same shared
  language profile used by `lucid-midgard` provider validation.
- Provider tests cover protocol-info fallback diagnostics, UTxO substitution,
  durable admission validation, tx-status reject-code preservation, transport
  classification, and language-profile fail-closed behavior.

## Non-Goals

- No generic Lucid/Cardano provider compatibility layer.
- No `PlutusV1` or `PlutusV2` support.
- No silent defaults for production protocol values.
- No direct dependency on node database modules.
- No browser wallet or signing work.
- No local durable state reset, redeploy flow, or benchmark shortcut behavior.
