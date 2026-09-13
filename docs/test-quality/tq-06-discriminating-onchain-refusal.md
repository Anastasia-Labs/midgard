# TQ-06 — Attribute validator refusals where the scenario requires it

Status: Proposed
Last reviewed: 2026-09-07 (helper and caller review)

Audit: §12, §5. Rule: R3. Related: TQ-04, TQ-05, TQ-17.

## Problem and current evidence

`tests/support/emulator/expect-onchain-refusal.ts` requires the error text to
contain `failed script execution`. It rejects success and errors without that
marker, and returns the text to its caller. It does not itself bind the exact
refusing validator. Callers may add checks, so this limitation must not be
restated as absence of discrimination throughout the entire fault-proof suite.

The invalid-signature lifecycle example additionally requires a `Spend[index]`
marker and excludes `Mint`. That distinguishes purposes, not an exact validator
hash or a unique failed guard. The old claim that it identifies the exact
validator was too strong.

`support/withdrawal-mistag-emulator.ts` interpolates `JSON.stringify(error)` in
an error wrapper. Native Error objects can serialize to `{}`, while other
structured errors retain fields. Inspect the actual error type and preserve
its cause rather than assuming every failure has the same representation.

## Proposed work

1. Inventory the observable error data supplied by the actual evaluator and
   Lucid adapter: redeemer pointer, purpose, script identity, trace and cause
   where present. Do not promise that every adapter exposes all these fields.
2. For tests naming a particular validator, bind the available refusal
   identity to the correctly applied contract used by the fixture. Resolve
   identity from the current deployment/application rather than transcribing
   a historical script hash.
3. Where exact attribution is unavailable, state the narrower observed
   contract and combine relevant successful controls and state assertions.
   Extend the adapter only with a separately reviewed, justified boundary.
4. Preserve structured causes through test helpers. A readable formatter may
   supplement the cause; it should not discard it.
5. Reuse attribution logic across helpers where their boundaries match.
   Keep deliberately broad on-chain refusal tests labeled as such, rather
   than requiring arbitrary exact identities to satisfy a mechanical rule.

## Acceptance

- Tests claiming an exact refuser establish that identity; tests proving only
  general on-chain refusal do not claim more.
- Helper checks reject unrelated builder/setup errors and preserve useful
  underlying diagnostics.
- Unit tests for the attribution adapter cover mismatched expected identity
  and malformed error data; the applicable emulator controls still run.
- Migration proceeds by family with actual test outcomes, not an assumed
  current count of 200 callers or a claim that all old cases were ineffective.
