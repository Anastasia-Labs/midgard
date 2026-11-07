# Open dependency: bounded authentication for maximum forced orders

Status: reproduced; implementation not yet designed or approved as a protocol change.
Consumer: Task 2/3 capability assessment. This is the concrete dependency report
permitted by Task 1 M1, not a replacement for a passing maximum-order fit gate.

The validity-free source preserves the existing admission grammar and 32,768-byte
aggregate field bound. At that bound, dense variable-width item envelopes make
the current order mint scan far too expensive. Publishing and certifying the
bytes succeeds; the complete order mint cannot fit the L1 execution budget.

## Evidence

Pinned compiler: `v1.1.23+5adf783`, testnet environment. Provisional dirty-tree
measurements; rerun against the final implementation revision and artifact.

- Aiken test `midgard/user_events/tx_order_v1.test` /
  `tx_order_mint_variable_width_aggregate_maximum` uses exactly 32,768 aggregate
  bytes and all nine nonempty fields. The diagnostic predicate succeeds with
  **349,051,704 memory units and 103,527,527,646 CPU units**. This exceeds the
  13,200,000-memory / 8,000,000,000-CPU fit limits by about 26.4x / 12.9x.
- Its adjacent aggregate case uses 32,769 bytes and is refused at the aggregate
  bound: 2,182,233 memory / 823,992,139 CPU. This tests the admission boundary;
  it is not an admitted maximum transaction.
- `demo/midgard-fault-proofs/tests/forced-submission-capacity.test.ts` constructs
  a corresponding SDK-admitted 32,768-byte shape with a canonical output and
  versioned native script, filling the remaining field space with dense opaque
  redeemer items. It publishes three chunks and mints their real field
  certificate, then verifies that order construction fails on execution budget.
  Inner semantic invalidity cannot exempt an admitted envelope from material
  authentication; ledger rejection remains the later verdict decision.

The latest emulator rerun used untraced blueprint SHA-256
`e91e45ab81fc779a7b404cdbfde347c4f148aadd7bbf5a12160392640bb419e0`.
It reproduced the same publication/certificate measurements and mint refusal:
one test, exit 0, 17.63 seconds, `/tmp/forced-capacity-14.log`.

| Submitted transaction    | Signed bytes |  Memory |         CPU |
| ------------------------ | -----------: | ------: | ----------: |
| First chunk publication  |       15,872 |       0 |           0 |
| Second chunk publication |       15,872 |       0 |           0 |
| Tail chunk publication   |        2,491 |       0 |           0 |
| Field-8 certificate      |        5,243 | 508,842 | 226,222,584 |

No order was minted or settled in this diagnostic. The intermediate full Aiken
run passed all 4,151 cases, including the exact maximum and adjacent vectors:
`/tmp/forced-aiken-batches-11/summary.json`. The current full Aiken rerun is
recorded in `/tmp/forced-aiken-batches-13/summary.json`.

## Required follow-up

Design a bounded material-authentication route before claiming support for every
admitted maximum. It must authenticate the same immutable forced source and all
nine declared field lengths, enforce the full existing envelope grammar and
chunk availability, and bind every intermediate certificate/checkpoint to the
source kind, transaction ID, field index, commitment and exact scan position.
The order mint must consume a completed authenticated result whose own creation
and every intermediate step fit the fixed transaction limits. Partial, foreign,
replayed or substituted certificates must not authorize an order.

This may require a staged field-grammar certificate or equivalent authenticated
bounded scan. Choose that design in an ADR amendment before implementation;
merely moving an unbounded scan to another validator does not resolve the gap.
Include publication, cancellation/recovery, completed order mint, settlement,
maximum/adjacent refusal, and full signed-transaction measurements in its gate.
Do not lower the frozen bound, weaken the grammar, trust off-chain certification,
or present smaller existing M1 ledger rows as maximum-capacity evidence.
