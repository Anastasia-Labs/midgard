# Cardano capability P2 nested-Value boundary checkpoint — 2026-07-26

Branch: `codex/tx-validation-capability-checkpoint`

Authority:

- `cardano-capability-proof-completion.md`;
- decision `0001-cardano-l1-transaction-capability-floor.md`.

This checkpoint closes the P2 nested output `Value` row. It does not change
canonical V1 schemas, protocol limits, activation, compatibility, deployment,
or any P3–P6 surface.

## Exact maximum-cardinality Value

The accepted fixture spends one deterministic funded key input and creates:

1. one fixed 30,000,000-lovelace multi-asset output; and
2. one Ada-only change output, so fee convergence cannot change the measured
   multi-asset `Value`.

The target `Value` contains seven canonical 28-byte policy IDs and 1,592
distinct assets at quantity one. Three policies contain 228 assets and four
contain 227. Within each policy, the first asset name is empty and the
remaining names are the canonical one-byte values beginning at `00`.

The exact 5,000-byte derivation is:

- Value array header, five-byte lovelace, and outer map header: 7 bytes;
- seven policy byte strings plus seven 227/228-entry map headers:
  `7 × (30 + 2) = 224` bytes;
- seven empty-name entries plus 1,585 one-byte-name entries:
  `7 × 2 + 1,585 × 3 = 4,769` bytes.

Total: `7 + 224 + 4,769 = 5,000`.

This is also the maximum asset cardinality for a Cardano-valid 5,000-byte
Value with a valid multi-million-lovelace coin:

- with seven policies, 1,593 assets require at least 5,003 bytes;
- with six or fewer policies, at most 1,542 empty/one-byte names are
  available; the remaining two-byte names give a best lower bound of 5,029
  bytes;
- with eight or more policies, even the optimistic one-byte map-header lower
  bound is at least 5,026 bytes.

The immediately adjacent fixture preserves all seven policies and all 1,592
asset identities. It changes only the final quantity from `1` to `24`, adding
one canonical integer byte and producing an exact 5,001-byte Value.

## Cardano and production validation

| Measurement | Accepted | Adjacent |
| --- | ---: | ---: |
| Output Value bytes | 5,000 | 5,001 |
| Policies | 7 | 7 |
| Assets | 1,592 | 1,592 |
| Complete signed Cardano bytes | 5,233 | 5,234 |
| Cardano snapshot rule | passes | `OutputTooBigUTxO` |
| Midgard consensus parity | passes | `E_VALUE_SIZE` |

The accepted transaction has one input, two outputs, one exact vkey witness,
and no withdrawals, mint, Plutus/native scripts, redeemers, datums, or
collateral. The fixture enumerates and compares every policy ID, asset name,
and quantity before and after the semantic Midgard-to-Cardano bridge.

The pinned Cardano snapshot uses protocol major `11`, `maxValueSize = 5,000`,
and `maxTxSize = 16,384`. The official Dijkstra/Conway UTxO path preserves
the Babbage/Alonzo `validateOutputTooBigUTxO` check: serialize the `Value`
canonically at the protocol major, then emit `OutputTooBigUTxO` exactly when
the serialized size is greater than `ppMaxValSize`. The fixture therefore
binds the accepted and adjacent Cardano results directly: `5,000` is not
greater than `5,000`, while `5,001` is, and neither case is confounded by the
whole-transaction size limit (`5,233` and `5,234` bytes respectively).

Lucid's emulator validates the accepted transaction but does not implement
the `maxValSize` rejection, so it is not cited as the adjacent authority.
Separately, the production Midgard consensus validator applies the same
boundary and rejects the adjacent candidate with `E_VALUE_SIZE` and feature
`output_value`. That is independent parity evidence, not a substitute for
the Cardano snapshot rule.

## Retained and bounded proof path

The exact accepted canonical transaction survives both mandatory DA
classifications:

- normal `transactions` plus `transaction_preimages`;
- forced `forced_transactions` plus
  `forced_transaction_preimages`, retaining `TxIsValid`.

Each independently decoded path reconstructs the same 5,329-byte canonical
Midgard transaction. The outputs field is 5,085 bytes and the target output
item is 5,034 bytes, so output bytes are revealed in two independently
verified chunks of at most 4,095 bytes.

The nested output proof performs:

- 1,592 authenticated reverse-membership asset steps;
- six exact policy-boundary transitions;
- 1,594 Value-fold steps including close and finalization;
- 3,198 complete typed output-proof steps.

The largest policy/asset/quantity/Merkle-sibling witness payload is 358 bytes.
The terminal output descriptor binds the exact address, lovelace, asset count,
asset frontier, 5,000-byte Cardano Value size, and semantic Cardano/Midgard
TxOut summaries.

## Cross-language evidence

`ledger-output-value.max-cardano.test.ak` consumes exact TypeScript controls,
frontier peaks, and membership siblings from the maximum fixture.

| Applied transition | Aiken memory | Aiken CPU |
| --- | ---: | ---: |
| Finalize maximum Value summary | 1,921,700 | 766,243,938 |
| Cross-policy authenticated asset step | 2,332,654 | 856,442,965 |

The policy transition moves from policy `12…12` to `11…11` at remaining count
228, authenticates asset name `e2` at quantity one through ten Merkle
siblings, and reaches the exact TypeScript successor. Replacing the quantity
with two fails closed.

## Focused verification

From `demo/midgard-validation`:

```sh
NODE_OPTIONS=--max-old-space-size=3072 \
./node_modules/.bin/vitest run \
  tests/nested-value-boundary-v1.test.ts \
  --pool=forks --poolOptions.forks.singleFork=true \
  --testTimeout=300000 --hookTimeout=300000 --reporter=verbose

./node_modules/.bin/tsc --noEmit

../node_modules/.bin/eslint \
  tests/helpers/ordered-collection-boundary-v1.ts \
  tests/nested-value-boundary-v1.test.ts \
  --max-warnings=0
```

Result: **PASS** (one Vitest fixture, 27.298 seconds); typecheck and lint pass.

From `onchain/aiken`:

```sh
aiken fmt --check \
  lib/midgard/ledger-output-value.max-cardano.test.ak

aiken check -m maximum_cardano_value
```

Result: **PASS** (two exact applied vectors).

P2 remains open on maximum nested datum/redeemer Data shapes,
script-envelope/program material, maximum incremental canonical-CBOR scans,
and remaining ordered-field Aiken terminal vectors. Unsupported activation
remains fail closed.
