# §3.2 Necessity artifact — total header decode of the committed field-8 redeemer collection

## Binding

- Families / items: the three narrow total rules that adjudicate one accepted
  or forced native transaction's complete field-8 redeemer collection —
  `missingRedeemer` (`0000002e`), `redeemerCanonicity` (`00000028`), and
  `unusedRedeemer` — one committed 32,768-byte-capped aggregate field per
  accusation, batched pointer/canonicity walk over its items.
- Consumers bound by this artifact (the `canonical_cbor_scan_v1` imports this
  file justifies):
  - `onchain/aiken/lib/midgard/fraud-proofs/missing-redeemer/rule.ak`
    (`pointer_from_item_v1`: outer array head, purpose tag, purpose index);
  - `onchain/aiken/lib/midgard/fraud-proofs/redeemer-canonicity/rule.ak`
    (`item_is_canonical_v1`: the full exact item walk — outer head, purpose,
    index, data span, ex-units pair);
  - `onchain/aiken/lib/midgard/fraud-proofs/unused-redeemer/rule.ak`
    (pointer and ex-units head reads over the same item shape).
- Blueprint provenance: `onchain/aiken/plutus.json` SHA-256
  `caaf9849fe9d66b1bba2a2ba082c18857e9c16059351e70fcdc6f61781191652`
  (1,131 validators / 1,844 definitions), built by the pinned fork
  `aiken v1.1.23+5adf783` under the declared construction
  `aiken build --env testnet`. The three consumer modules are library rules
  applied through their families' step validators; the family fit ledgers
  cited below carry the per-family applied identities.
- Fixtures / gates: the family lifecycle ledgers
  `docs/fault-proofs/size-plans/missing-redeemer-v1-fit-ledger.json` (42
  entries), `redeemer-canonicity-v1-fit-ledger.json` (14 entries), and
  `unused-redeemer-v1-fit-ledger.json`, produced by the families' emulator
  suites; plans in the sibling `*-v1.md` files.

## Measurements (§3.2 order — stop at the first representation that fits)

The byte-fit half of this argument is the shared flat-carriage table, cited
rather than re-derived, exactly as
`docs/exec-plans/evidence/necessity/redeemer-item-traversal-v1.md` cites it:
`docs/exec-plans/evidence/necessity/transaction-field-chunk-v1.md`'s
"Measurements — flat `FieldCarriageV1` scheme". The adjudicated item here is
the **whole field-8 collection** (legal aggregate bound
`MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES` = 32,768), not one redeemer
item, so both complete-carriage routes sit far above their frontiers at the
maximum shape.

| Representation                                                                                                                  | Tx bytes / maxTxSize                                                                                                                                                                                                                                                                                                                      | Fits §3.3?            |
| ------------------------------------------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | --------------------- |
| 1. Complete redeemer collection direct in proof tx                                                                              | the shared table's measured publication framing reaches 35,186/16,384 at the 32,768-byte aggregate                                                                                                                                                                                                                                        | NO above 13,282 bytes |
| 2. Complete collection as inline-datum publication + reference                                                                  | 32,768 → 35,186/16,384 measured; frontier 14,396                                                                                                                                                                                                                                                                                          | NO above 14,396 bytes |
| 3. Certified field carriage (§8 tiers) + batched pointer walk, `canonical_cbor_scan_v1.head_at_v1` total header decode per item | every lifecycle row within limits — worst signed row 15,872/16,384 (`accepted-carriage-chunk-0`, full 15,148-byte chunk of the 17-item 32,768-byte certified field), worst execution rows 5,019,938 mem / 2,182,994,737 CPU (missing-redeemer), 1,678,487 mem / 571,611,300 CPU (redeemer-canonicity) against 16,500,000 / 10,000,000,000 | YES                   |

## Exact limiting constraint

Byte fit. A `RedeemerMissing`, `RedeemerMalformed`, or unused-redeemer
accusation binds the _complete_ field-8 collection — absence and
exhaustiveness claims are only sound against every item — and a collection
legally approaching 32,768 bytes cannot enter one proof transaction directly
(measured 35,186 at the aggregate cap) nor one inline-datum publication
(same figure; frontier 14,396). The families therefore stage the walk:
step 03 opens the committed field through published raw or certified §8
carriage, step 04 resumes fixed-size batches and self-loops until a match is
found or `cursor == item_count`.

## Why no simpler authenticated representation closes the gap

Within each batch the rule has one complete item's bytes in hand, and the
remaining question is structural: reading the item's purpose tag, purpose
index, data span, and ex-units heads. A typed Aiken `expect`-decode of the
item is **partial** — malformed bytes abort the script — while these rules
must be **total** over adversarial bytes in both polarities: the
wrongful-rejection direction of `redeemerCanonicity` has to adjudicate a
malformed item to an exact `False` verdict (`item_is_canonical_v1`'s doc
comment: "malformed adversarial bytes return False"), and the presence scans
must step past items without crashing the accusation thread. Aiken's
`cbor`/`expect` machinery cannot express that; re-implementing a total
header reader per family would triplicate the one already measured and
frozen in `lib/midgard/canonical-cbor-scan-v1.ak`. `head_at_v1` — an O(1)
bounded read of a single canonical CBOR head at an offset — is the minimum
machinery over the complete-item read; no incremental multi-transaction
byte-revelation is introduced by these consumers at all.

## Preserved complete-item path

Collections that fit the §8 tier-1/tier-2 complete-preimage doors reach the
walk as one carriage step (the `accepted-carriage-chunk-0` rows), and each
batched step still total-decodes complete items — the scanner reads heads of
items it fully possesses; it never reveals partial item bytes across
transactions. The families' emulator suites prove both polarities at the
maximum shape: honest absence/malformedness convicts, and skipped, reordered,
substituted, prematurely-terminal, or suffix-omitting walks are refused.

## Necessity conclusion

**YES.** The complete field-8 collection at its legal 32,768-byte bound fits
neither complete route (representations 1–2, NO above 13,282 / 14,396
bytes), so the batched walk is required; and within the walk, totality over
adversarial item bytes rules out the typed partial decode, making the frozen
`canonical_cbor_scan_v1.head_at_v1` reader the minimal authenticated
representation for these three rules' header reads.
