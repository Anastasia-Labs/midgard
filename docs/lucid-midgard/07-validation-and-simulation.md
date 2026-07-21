# Validation and Simulation

`lucid-midgard` should provide local validation as a preflight tool, not as a
replacement for node validation.

## Phase A Preflight

Phase A preflight should be available after unsigned completion and after
signing:

- Unsigned preflight catches encoding, fee, input, output, script witness,
  redeemer, datum, network, and auxiliary-data issues that do not require
  signatures.
- Signed preflight must verify required witnesses and vkey signatures over the
  Midgard-native body hash.

Phase A must come from shared validation source before this library exposes it
as production preflight. A mirrored validator is acceptable only as an explicitly
experimental interim artifact and must not be the default implementation.

## Phase B Simulation

Phase B simulation requires:

- Current slot.
- UTxO pre-state for every spend and reference input.
- Script source resolution data.
- Budget enforcement configuration.

The provider may supply UTxO state by outref. If any dependency is missing,
simulation should fail with a structured "insufficient pre-state" error unless
the caller explicitly chooses partial validation.

Default budget enforcement must be true.

Phase B must also come from shared validation source before this library exposes
it as production preflight. Node validation and `lucid-midgard` preflight should
consume the same reject-code definitions and rule implementation.

## Error Mapping

Local validation should preserve node reject codes:

- `E_CBOR_DESERIALIZATION`
- `E_TX_HASH_MISMATCH`
- `E_IS_VALID_FALSE_FORBIDDEN`
- `E_AUX_DATA_FORBIDDEN`
- `E_NETWORK_ID_MISMATCH`
- `E_MIN_FEE`
- `E_DUPLICATE_INPUT_IN_TX`
- `E_MISSING_REQUIRED_WITNESS`
- `E_INVALID_SIGNATURE`
- `E_PLUTUS_SCRIPT_INVALID`
- `E_VALUE_NOT_PRESERVED`

Library-specific errors should wrap these codes rather than translate them
away.

## Simulation Result

Expose:

```ts
type LocalValidationReport = {
  readonly phase: "phase-a" | "phase-b";
  readonly acceptedTxIds: readonly string[];
  readonly rejected: readonly {
    readonly txId: string;
    readonly code: string;
    readonly detail: string | null;
  }[];
  readonly preStateSource?:
    | "explicit"
    | "instance-override"
    | "completion-preset";
  readonly preStateAuthoritative?: false;
  readonly statePatch?: {
    readonly deletedOutRefs: readonly string[];
    readonly upsertedOutRefs: readonly (readonly [string, string])[];
  };
};
```

If a condition would reject in the node, local validation returns the shared
reject code and detail rather than downgrading it to a warning.

Local validation cannot claim final validity. Submission followed by node status
remains authoritative.

## Current Surface

`lucid-midgard` imports `runPhaseAValidation` and
`runPhaseBValidationWithPatch` from `@al-ft/midgard-validation`. The node queue
processor imports the same shared package directly; the former node-local
validation compatibility modules have been removed.

`complete({ localValidation: "phase-a" | "phase-b" })` fails completion when
the shared validator rejects. `CompleteTx.validate(...)` returns the same
reject-preserving report without mutating completion metadata, with
`validateSafe(...)` and `validateProgram(...)` wrappers for safe-result and lazy
Effect callers.

## Determinism

Validation must be deterministic for a fixed provider snapshot. Any provider
data used for simulation should be included in debug metadata:

- Slot.
- Protocol parameters.
- Pre-state outrefs and output hashes.
- Native tx id and byte hash.

If a caller supplies local pre-state that was derived from UTxO overrides or
completion preset wallet inputs, the validation report must mark that source as
non-authoritative. Override-derived pre-state is useful for local simulation,
but node Phase B remains the authority for whether those inputs actually exist
and are unspent.
