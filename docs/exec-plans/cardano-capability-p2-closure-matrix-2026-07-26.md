# Cardano capability P2 closure matrix — 2026-07-26

Authority:

- `cardano-capability-proof-completion.md`, especially P2;
- `../midgard/decisions/0001-cardano-l1-transaction-capability-floor.md`.

This is a live evidence matrix, not an activation claim. `PASS` means the
family has an exact retained source for both normal and forced
classification, committed typed count or length, individually bounded
reveals, an exact terminal result agreed by TypeScript and Aiken, and an
accepted Cardano-derived maximum plus an immediately adjacent rejection where
the family has an applicable Cardano boundary. `PARTIAL` means useful
machinery or vectors exist but at least one of those requirements is missing.

The canonical V1 activation gate remains fail closed until every required P2
row is `PASS`. P3 through P6, live deployment, release-digest construction,
and compatibility work are outside this checkpoint.

## Retained-DA ordered-field checkpoint

The retained-DA boundary harness stores the same canonical native transaction
in both V1 classification paths:

- normal: `transactions` plus `transaction_preimages`;
- forced: `forced_transactions` plus
  `forced_transaction_preimages`, retaining `TxIsValid`.

It round-trips the actual SDK `DaPayloadV1` codec and mandatory identity
envelope. After decoding, each path independently re-derives the transaction
identity, proof-source fields, and proof commitment, verifies every typed
field/item chunk, completes the terminal reconstruction fold, and requires
byte-exact equality with the retained canonical transaction.

This harness deliberately stays below full-node and P3 scope. It does not
invent header, root, validation-trace, or ledger state. The existing strict DA
payload suite separately keeps mandatory envelope, header binding, roots,
counts, preimage coverage, and trace coverage fail closed.

The harness is exercised against:

- every existing Cardano-derived maximum ordered-field fixture for fields
  `0` through `8`, in both normal and forced classification;
- the collateral-free schema-parallel form of the exact maximum redeemer
  fixture; the genuine collateralized Cardano transaction continues to reject
  with `E_CONVERSION_UNSUPPORTED_FEATURE` and detail `collateral_inputs`;
- the existing mixed canonical transaction fixture at `16,126` bytes.

The maximum redeemer fixture contains neither withdrawals nor mint. Its exact
spend pointers remain `1` through `296`, its Data remains constructor tag
`121` (`d87980`) for every item, and its execution units remain exact through
the semantic reverse bridge.

## Whole-P2 dynamic-content matrix

| Dynamic-content family | Normal/forced retained source | Typed count/length | Individually bounded reveal | TypeScript terminal result | Aiken terminal agreement | Cardano maximum + adjacent reject | Gate |
| --- | --- | --- | --- | --- | --- | --- | --- |
| Ordered transaction fields `0`–`8` | **PASS** — exact proof source and canonical preimage retained in both paths | **PASS** — field directory plus exact item counts/lengths | **PASS** — every item/chunk verified; complete fold step counts checked | **PASS** — byte-exact canonical reconstruction for all maximum fixtures | **PARTIAL** — exact maximum field-8 applied terminal vector passes; maximum-specific agreement is not yet recorded for fields `0`–`7` | **PASS** — accepted maxima and immediately adjacent signed-Cardano rejection fixtures exist for all fields | **PARTIAL** |
| General byte blobs/chunks | **PARTIAL** — canonical field bytes are retained, but no dedicated maximum blob fixture is carried through both classifications | **PASS (machinery)** — commitment binds field domain and total byte length | **PASS (machinery)** — TS/Aiken chunk proofs cover multi-chunk content and tampering | **PARTIAL** — generic reconstruction tests exist, not the applicable maximum retained fixture | **PARTIAL** — generic bounded-blob vectors exist, not an exact maximum retained terminal vector | **OPEN** — no Cardano-derived maximum blob and adjacent rejection pair is recorded | **PARTIAL** |
| Nested output `Value` policy/asset content | **PARTIAL** — transactions containing Values are retained, but not an exact maximum nested-Value proof source in both classifications | **PARTIAL** — authenticated asset-map machinery exists | **PARTIAL** — value/mint mutation and chunk-boundary tests exist; the full maximum nested Value reveal is not evidenced | **OPEN** — no retained maximum-Value terminal fixture | **OPEN** — no exact maximum-Value TypeScript/Aiken terminal vector | **OPEN** — the `maxValueSize = 5,000` accepted shape and immediate adjacent rejection are not yet proven end to end | **PARTIAL** |
| Datum and redeemer Plutus `Data` maximum shapes | **PARTIAL** — maximum redeemer *cardinality* is retained, but its Data is the minimal `d87980`; no maximum datum/redeemer Data shape is retained | **PARTIAL** — Data scan frames bind child counts and blob lengths in generic tests | **PARTIAL** — bounded Data-node/leaf scan machinery exists | **PARTIAL** — generic Data scans pass, not maximum retained datum and redeemer shapes | **PARTIAL** — cross-language scan vectors exist, not maximum-shape terminal vectors | **OPEN** — no accepted maximum Data-shape transaction plus adjacent rejection fixture | **PARTIAL** |
| Script envelopes and content-addressed program material | **PARTIAL** — maximum native-script cardinality is retained, but the harness has no Plutus program-material sidecar entries | **PARTIAL** — envelope and program-material schemas bind kinds, roots, and byte lengths | **PARTIAL** — content-addressed traversal and bounded-node tests exist | **PARTIAL** — generic material traversal passes, not a maximum retained envelope/material terminal fixture | **PARTIAL** — envelope/material limits and nodes have Aiken tests, not an exact maximum retained terminal vector | **OPEN** — no applicable maximum script envelope/program-material fixture and adjacent rejection path | **PARTIAL** |
| Incremental canonical-CBOR scan states | **PARTIAL** — canonical transaction bytes are retained, but no maximum semantic scan-state fixture is selected from each path | **PASS (machinery)** — scan controls bind cursor, source, and terminal state | **PARTIAL** — generic control-step scans exist; maximum source traversal is not evidenced | **PARTIAL** — generic terminal scan vectors exist | **PARTIAL** — generic cross-language control hashes exist, not a maximum retained terminal scan | **OPEN** — no maximum/adjacent source pair is tied to the complete incremental scan | **PARTIAL** |

## Evidence at this checkpoint

Focused TypeScript checks pass:

- lint and typecheck for the retained harness plus all ordered-field boundary
  fixtures;
- `8` Vitest files / `8` tests, covering retained normal/forced
  reconstruction and all ordered-field maximum fixtures.

Focused Aiken checks pass:

- the exact `296`-redeemer field commitment/codec vector;
- the exact applied terminal-fold vector, at `2,068,700` memory and
  `856,755,810` CPU.

The aggregate field codec fixture uses `124,272,878` memory and
`50,949,000,952` CPU. It is diagnostic construction evidence, not the
production one-step path. The strict DA payload suite also passes `6` focused
tests.

## Next P2 sequence

Reuse the retained-DA harness rather than build a node lifecycle:

1. close general maximum blob/chunk retention and exact TS/Aiken terminal
   agreement;
2. close the `5,000`-byte nested Value boundary;
3. close maximum datum and redeemer Data shapes;
4. close script-envelope and program-material nodes;
5. close maximum incremental CBOR scans;
6. backfill any remaining maximum-specific ordered-field Aiken terminal
   vectors, then rerun the complete matrix.

No row may be promoted from `PARTIAL` based only on generic unit machinery or
on an ordered-field cardinality fixture.
