# Compressed-prefix MPF consumer repair

The pinned upstream Aiken forestry implementation loses the skipped path prefix
when excluding a terminal Fork and selects the wrong neighbor nibble in a
nonterminal Leaf with a positive skip. The shared canonical wrappers already
correct both cases. This repair moves the remaining production exclusion and
root mutation consumers onto those wrappers:

- `pexcludes.exclusion.withdraw`;
- `transition_trace.verify_root_non_membership_raw`;
- legacy transition-trace ledger non-membership, insertion, and deletion.

Midgard empty-root sentinel normalization remains in each caller. Membership
uses the unaffected upstream `has` implementation: both captured vectors verify
membership before deletion and refuse membership after deletion. Remaining
upstream insertion calls construct test fixtures.

The captured Fork and Leaf vectors reproduce ten failures before this repair;
the two membership controls already passed. All twelve pass after the repair.
Four additional negative cases reject the root computed by dropping the Fork
prefix. The JavaScript forestry verifier independently reproduces both canonical
exclusion roots, and Lucid publishes and executes the real exclusion reference
script for both vectors, refusing membership roots and the dropped-prefix root.

## Verification

Run from `onchain/aiken`, with the pinned Aiken binary and shared Aiken lock:

```sh
flock /tmp/midgard-nip-aiken.lock /home/gumbo/.aiken/bin/aiken check --env testnet \
  -m 'midgard/transition_trace.{..}' \
  -m 'midgard/fraud_proofs/transition_trace/proof.{..}' \
  -m 'pexcludes.{..}'
flock /tmp/midgard-nip-aiken.lock /home/gumbo/.aiken/bin/aiken check --env testnet
```

Results: **111/111** targeted and **3,766/3,766** full-suite tests pass.
Using the freshly built testnet blueprint and declared Node/pnpm toolchain,
`pnpm --dir demo/midgard-fault-proofs exec vitest run` passes **59/59** across:

- `tests/mpf-prefix-consumers-emulator.test.ts`;
- `tests/submit-init-emulator-no-reference-input-lifecycle.test.ts`;
- `tests/no-reference-input-wrongful-rejection-lifecycle.test.ts`;
- `tests/non-existent-input-wrongful-rejection-lifecycle.test.ts`;
- `tests/transition-trace-challenger.test.ts`.

The fault-proofs package typecheck and the new test's ESLint check pass.

## Artifact impact

The exclusion script grows from 3,058 to 3,659 raw bytes. Both `default` and
`testnet` pins now equal
`074ca4d6263082ec32a5715e4685981188fdab23aeb8f2c6d536843e`.
The chunked verifier hash remains
`1dbd8f5ad1314ebf9e4bcbf814fb7f44eea574ed20b3a5b42c7e0767`.

This isolated final testnet blueprint SHA-256 is
`d31a201a30cf8b074d3f7717dacb588eddd868df82efbe5b645953171f832a54`.
Changing the exclusion parameter also changes dependent compiled validators.
Existing fit ledgers bind their recorded blueprint digest and must be regenerated
when integrating this repair into a new consolidated blueprint. The completed
value-conservation family and its ledger remain unchanged in this repair.
