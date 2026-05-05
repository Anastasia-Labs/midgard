# T09 Script and Redeemer Builders

## Scope

Add script, datum, mint, observer, and protected receive support with correct
redeemer pointer derivation.

## Dependencies

- T06 builder API.
- T07 canonicalization.
- T08 balancing.
- Architecture doc `06`.

## Deliverables

- `attach.Script`, `attach.NativeScript`, `attach.Datum`.
- `mintAssets`.
- `observe`.
- Spend redeemer support.
- Receive redeemer support for protected script outputs.
- Redeemer pointer derivation.
- Script-integrity derivation using shared `@al-ft/midgard-core`
  `scriptLanguageViews` constants.

## Acceptance Criteria

- Spend indexes use sorted spent outrefs.
- Mint indexes use sorted policy ids.
- Observer indexes use sorted observer hashes.
- Receive indexes use sorted unique protected receiving script hashes.
- PlutusV3 receive is rejected locally.
- Duplicate redeemer pointers fail.
- Finalization derives redeemer witness preimages, `redeemerTxWitsRoot`,
  required language tags, canonical `scriptLanguageViews`, and
  `scriptIntegrityHash`.
- Required reference-script languages must be known locally before script
  finalization, unless the caller provides explicit trusted script metadata.
- `readFrom(..., { trustedReferenceScripts })` and
  `attach.ReferenceScriptMetadata(...)` provide the explicit metadata path.
  Metadata is keyed by reference outref and declares the selected
  `NativeCardano`, `PlutusV3`, or `MidgardV1` hash domain. When local
  script-ref bytes are available, finalization verifies the declaration before
  using it.
- Dependency default cost models are never read as protocol values during
  finalization; Midgard protocol constants are used instead.

## Required Tests

- Spend redeemer index test.
- Mint redeemer index test.
- Observer redeemer index test.
- Protected receive redeemer index test.
- Missing datum witness failure.
- Extraneous script witness failure.
- Mixed PlutusV3/MidgardV1 hash-domain test.
- No-script and native-only transactions use `EMPTY_NULL_ROOT`.
- PlutusV3-only, MidgardV1-only, and mixed script-integrity hash fixture tests.
- Redeemer data and ex-unit changes alter `scriptIntegrityHash`.
- Missing and extraneous language views reject.
- Reference-script transaction recomputes correctly in Phase B.

## Non-Goals

- No new script semantics in node validators.
- No compatibility with old redeemer tags.
