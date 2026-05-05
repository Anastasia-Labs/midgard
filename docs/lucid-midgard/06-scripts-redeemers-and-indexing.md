# Scripts, Redeemers, and Indexing

Scripts are the highest-risk part of `lucid-midgard`. The API must make
script purpose construction explicit and derive redeemer pointers from Midgard
validation rules.

## Script Source Forms

The node recognizes:

- Native Cardano scripts.
- Typed PlutusV3 scripts.
- Raw native script bytes.
- Raw UPLC bytes that can expose PlutusV3 and MidgardV1 hash domains.

The builder should expose typed wrappers so callers state whether a script is
intended for native, PlutusV3, MidgardV1, or both hash domains.

## Supported Purposes

Midgard Phase B discovers executions for:

- Spend: script payment credential on a spent output.
- Mint: policy id in the native mint map.
- Observe: required observer hash in the body.
- Receive: protected script output.

Receive purposes require MidgardV1 context. PlutusV3 receive must be rejected
locally before submission.

## Redeemer Tags

Supported redeemer tags:

- Spend: CML spend tag.
- Mint: CML mint tag.
- Observe: CML reward tag.
- Receive: tag `6`.

The builder should support both array and map redeemer encodings only if tests
cover both. Internally, a canonical map representation is easier to audit.

## Index Derivation

Redeemer indexes must be derived as follows:

- Spend index: position of spent outref in lexicographically sorted spent-input
  list.
- Mint index: position of policy id in sorted policy id list.
- Observe index: position of observer hash in sorted observer list.
- Receive index: position of protected receiving script hash in sorted unique
  receiving hash list.

Outputs are never sorted for context construction. They remain in authored
order.

## Datums

The builder must support inline datum outputs and datum witnesses needed for
script spends. Current validation rejects unsupported MidgardV1 datum-hash
spends and expects datum witnesses to decode as CML Plutus data.

Datum witness de-duplication must use datum hash and reject duplicates.

## Script Integrity Hash

`scriptIntegrityHash` commits to the redeemer witness root and the required
non-native script language views:

```text
scriptLanguageViews =
  Map<ScriptLanguageTag, CanonicalCostModelView>

scriptIntegrityHash =
  blake2b256(cbor([
    redeemerTxWitsRoot,
    scriptLanguageViews
  ]))
```

`redeemerTxWitsRoot` is the existing 32-byte redeemer witness-set root.
`scriptLanguageViews` is a canonical CBOR map. Keys are stable numeric Midgard
protocol tags, never free-form strings:

- `2`: `PlutusV3`, Cardano-compatible PlutusV3 script hash domain, context
  shape, allowed purposes, and evaluator branch.
- `128`: `MidgardV1`, MidgardV1 script hash domain, context shape, allowed
  purposes, and evaluator branch.

The map value is only the canonical ordered integer cost-model view for that
language. It does not encode `contextAbi`; Phase B dispatches from the language
tag. Initial `PlutusV3` and `MidgardV1` cost-model views are frozen Midgard
protocol constants matching Harmonic `defaultV3Costs` at the time this rule was
introduced. Dependency bumps must not alter these arrays unless an explicit
protocol migration changes the constants.

The map includes only non-native script languages actually required by the
transaction after Phase B resolves inline and reference script sources.
No-script and native-only transactions keep:

```text
EMPTY_NULL_ROOT = blake2b256(cbor(null))
```

Phase A checks presence only: if redeemers or non-native inline witnesses are
present, `scriptIntegrityHash` must be non-empty. Phase A does not fully verify
language views because reference scripts may require Phase B state resolution.

Phase B must resolve inline and reference scripts, discover required
executions, derive the required language set, rebuild `scriptLanguageViews`,
recompute `scriptIntegrityHash`, and reject when the body hash does not match.
Missing and extraneous language views are both hash mismatches.

## Reference Scripts

Reference script resolution depends on reference inputs existing in Phase B
state. The builder may attach reference inputs, but it cannot guarantee node
state has not changed before validation. Builders must refuse script
finalization if a required reference-script language cannot be known locally,
unless the caller supplies explicit trusted script metadata. Local Phase B
simulation should catch snapshot-level errors only.

Trusted metadata is keyed by the reference outref and declares the exact script
language branch plus the hash in that branch. If local output CBOR contains a
script ref, the builder must verify the declaration against the decoded bytes
and reject mismatches. A CML PlutusV3 reference script is treated as PlutusV3 by
default; using the same bytes in the MidgardV1 hash/domain requires explicit
trusted `MidgardV1` metadata. Metadata without local script bytes is accepted
only as caller-trusted input and remains subject to node Phase B recomputation.
