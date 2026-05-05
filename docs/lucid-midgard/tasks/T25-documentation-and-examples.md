# T25 Documentation and Examples

## Scope

Update user-facing documentation and runnable examples for the hardened
`lucid-midgard` API after provider switching, convenience methods, observer
validators, signing, composition, local chaining, safe/Effect APIs, and staged
transaction objects are implemented.

Documentation must teach Midgard-native semantics, not Cardano transaction
compatibility.

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
- Architecture docs `00` through `09`.

## Deliverables

- Updated package README with production L2 constraints and quickstart.
- API documentation for provider switching and UTxO overrides.
- API documentation for provider convenience methods.
- API documentation for observer validators.
- API documentation for `fromTx`, sign builder, and partial signing.
- API documentation for builder composition.
- API documentation for local produced-output chaining.
- API documentation for `CompleteTx`, the signed/witness-complete `CompleteTx`
  compatibility shape, `PartiallySignedTx`, and `SubmittedTx`.
- API documentation for Promise, safe-result, and Effect variants.
- Runnable examples for:
  - simple balanced transfer
  - provider switching and override UTxOs
  - querying UTxOs and status through convenience methods
  - observer validator attachment and observer redeemer use
  - importing an existing Midgard native tx
  - partial signing and assembly
  - composing builder fragments
  - local two-transaction chaining
  - signing, submission, and status polling
  - Promise error handling
  - safe-result error handling
  - Effect error handling, including cancellation where relevant
  - protected output construction
- Notes explaining durable admission versus validation acceptance.
- Notes explaining that local chained outputs are optimistic until node status
  confirms acceptance.
- Structured error taxonomy reference.
- Node reject-code preservation reference.
- Provider fallback diagnostics reference.
- Exact output-CBOR local chaining rules, including body output index versus
  authored output metadata.
- Protected-output marker preservation reference.
- Migration notes from older examples.

## Acceptance Criteria

- Examples build and run against the package test harness.
- Documentation never describes Cardano transaction CBOR as the normal output.
- Documentation never relies on node Cardano-to-native conversion as the normal
  submission path.
- Documentation includes no Cardano canonical/noncanonical options unless an
  explicit Midgard protocol migration introduces two Midgard encodings.
- Examples do not weaken local validation, fee, or signature defaults.
- Status examples distinguish queued, accepted, rejected, and committed states.
- Safe and Effect examples preserve structured error information.
- Promise, safe-result, and Effect examples are all present; no API family may be
  skipped by documenting only one alternative.
- Effect examples include cancellation or timeout behavior for polling.
- Failure-mode docs cover structured errors, node reject codes, provider
  fallback diagnostics, exact output-CBOR chaining, and protected-output marker
  preservation.
- Observer examples use `ObserverValidator` and do not model observers as
  withdrawals or reward-account operations.
- Provider examples use only `MidgardProvider`/`MidgardNodeProvider` or
  explicitly marked test doubles.

## Required Tests

- Typecheck all documentation snippets.
- Smoke-test runnable examples.
- Link check package-local documentation references.
- Snapshot test example output where deterministic.
- Lint or static scan for forbidden Cardano conversion examples.
- Lint or static scan for unsupported provider adapters.
- Test that README quickstart compiles.
- Static scan proving docs include Promise, safe-result, and Effect examples.
- Documentation test for reject-code, provider-fallback, exact-chaining, and
  protected-marker sections.

## Current Implementation Notes

- `demo/lucid-midgard/README.md` documents production L2 constraints,
  provider switching, UTxO overrides, provider convenience methods, observer
  validators, reference-script metadata, partial signing, composition, local
  chaining, safe-result APIs, Effect APIs, durable admission semantics,
  protected outputs, and supported script language tags.
- `demo/lucid-midgard/examples/usage.ts` provides runnable in-memory examples
  for balanced transfers, provider switching, overrides, observer validators,
  importing, composition, local chaining, partial signing, submission, status,
  safe-result errors, Effect errors, and protected outputs.
- `documentation-examples.test.ts` smoke-runs the examples and guards package
  docs/examples against unsupported Cardano provider adapters, normal-path
  conversion shortcuts, and Lucid's Cardano local-evaluation option.
- The currently exported signed transaction shape is a witness-complete
  `CompleteTx`; there is no public `SignedTx` class. Documentation and examples
  must describe `CompleteTx.submit(...)`, `SubmittedTx.tx`, and
  `SubmittedTx.awaitStatus({ until: ... })`.

## Non-Goals

- No marketing landing page.
- No full protocol tutorial.
- No L1 protocol builder migration docs.
- No legacy compatibility guide unless it clearly says old behavior is removed.
