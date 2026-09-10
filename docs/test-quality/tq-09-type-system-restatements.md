# TQ-09 — Review runtime shape checks and strengthen their meaning

Status: Proposed
Last reviewed: 2026-09-07

- **Audit sections**: §9
- **Rules**: R1
- **Blocked by**: —

## Why review this surface

TypeScript `string` and branded hex types do not enforce a string's length or
contents at runtime. A producer, cast, deserialiser, or package export can be
wrong while the program still typechecks. A non-null assertion (`!`) is erased;
it does not replace `toBeDefined()`. The earlier proposal to mechanically delete
these assertions was based on an incorrect type-system guarantee.

## Scope

Review hash and unit shape checks in `operator-lifecycle-emulator.test.ts`,
`initialization-emulator.test.ts`, the `submit-init-emulator-*` suites,
`midgard-contracts.test.ts`, watcher adapter/storage/replay tests, and lucid's
native fixture tests. Also review runtime export checks in
`execution-native-script-invalid.test.ts`, `canonical-evidence-source.test.ts`
and `da-hash-preimage.test.ts`, and the datum check in
`tx-order-carriage-l1-observation.test.ts`.

These are candidates for stronger assertions, not confirmed redundant checks.
`listen-admission-auth.test.ts` checks that a router exists under a name
promising routes; existence alone does not establish that broader claim (TQ-18).

## What to do

1. Identify the producer and the runtime contract at each site. Keep format,
   existence, and package-export assertions when they detect a possible
   regression, including on statically typed values.
2. Where the test promises successful submission, add or use the transaction's
   observed ledger effect. A 64-character hash alone does not prove submission.
3. Remove a shape check only if a retained assertion subsumes it in the same
   relevant execution path. Document that assertion in the review.
4. Keep type-level tests for public signatures separate from runtime tests.

## Candidate search

From the repository root:

```bash
rg -n 'toHaveLength\((64|56|120)\)|toBeDefined\(|typeof.*function' demo -g '*.test.ts'
```

This is a candidate search, not an exact count or a defect detector; it misses
multiline forms and includes valid checks.

## Acceptance criteria

- Every changed site retains a runtime oracle for the claimed behavior.
- Relevant package tests and typechecks pass; removal is justified by a named
  surviving check, not the variable's static type.
- No blanket lint ban on hash lengths, `typeof`, or `toBeDefined()` is added.
