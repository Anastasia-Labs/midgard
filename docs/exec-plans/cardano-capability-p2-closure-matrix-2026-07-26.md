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

The original retained-DA boundary harness stores the same canonical native
transaction in both V1 classification paths:

- normal: `transactions` plus `transaction_preimages`;
- forced: `forced_transactions` plus
  `forced_transaction_preimages`, retaining `TxIsValid`.

It round-trips the actual SDK `DaPayloadV1` codec and mandatory identity
envelope. It is now used as a checked boundary-corpus producer, not as evidence
that production DA reconstruction accepted the payload. Its synthetic payload
has empty committed roots and no validation traces and therefore cannot pass
`reconstructDaPayloadV1`.

The corrected vertical integration stays in
`@al-ft/midgard-fault-proofs`, preserving package dependency direction. For
each checked corpus entry it builds an internally consistent identity-wrapped
payload with real transaction, forced-transaction, transition-trace,
event-to-step, and validation-trace roots; exact member/header counts; and an
exact header hash. It then calls the production `reconstructDaPayloadV1`.
Only after that succeeds does it run every bounded item/chunk proof and the
complete generic field/item chunk reconstruction from:

- `reconstruction.transactions[].fullTransactionCbor` for normal retention;
- `reconstruction.forcedTransactions[].fullTransactionCbor` for forced
  retention.

The strict corpus contains every existing Cardano-derived maximum
ordered-field fixture for fields `0` through `8`, the maximum inline-datum
blob, maximum nested Value, balanced nested datum and redeemer fixtures, and
the existing mixed canonical transaction fixture at `16,126` bytes. Both
classifications reconstruct byte exactly for all `12` entries. Deliberate
transaction-root and validation-trace-count mismatches reject with
`rootMismatch` and `countMismatch`; substituting a different valid canonical
transaction under the committed forced-preimage key rejects with
`malformedPayload` before forced bytes are returned.

The production corpus test does not itself replay every row-specific
ordered-item, output/Value, or Data semantic finalizer. Those typed tests run
as the `12` corpus producers, and their output is byte-identical to the checked
corpus consumed by production reconstruction. Matrix typed-terminal cells
remain calibrated to that separately run evidence rather than being promoted
solely by the strict corpus test.

The maximum redeemer fixture contains neither withdrawals nor mint. Its exact
spend pointers remain `1` through `296`, its Data remains constructor tag
`121` (`d87980`) for every item, and its execution units remain exact through
the semantic reverse bridge.

## Whole-P2 dynamic-content matrix

| Dynamic-content family | Normal/forced retained source | Typed count/length | Individually bounded reveal | TypeScript terminal result | Aiken terminal agreement | Cardano maximum + adjacent reject | Gate |
| --- | --- | --- | --- | --- | --- | --- | --- |
| Ordered transaction fields `0`–`8` | **PASS** — production reconstruction authenticates exact proof sources and canonical preimages in both paths | **PASS** — field directory plus exact item counts/lengths | **FAIL (active transition)** — retained proofs reveal every item/chunk boundedly, but `validation-machine-v1.ak` still decodes complete spend/reference-input preimages at `8464`–`8467` and `16194`–`16195`, and the complete observer preimage at `15045` | **PARTIAL** — byte-exact generic reconstruction passes for all maximum fixtures, but the three active whole-field consumers remain | **PARTIAL** — exact maximum field-8 applied terminal vector passes; maximum-specific agreement is not yet recorded for fields `0`–`7`, and the active whole-field consumers are not replaced | **PASS** — accepted maxima and immediately adjacent signed-Cardano rejection fixtures exist for all fields | **PARTIAL** |
| General byte blobs/chunks | **PASS** — production reconstruction authenticates the exact maximum inline-datum transaction and proof source in both classifications | **PASS** — one field-2 item binds exact item count `1`, item length `16,221`, field length `16,225`, and chunk count `4` | **PASS** — four exact reveals use at most `4,095` bytes and reconstruct the authenticated source | **PASS** — the field terminal advances to the exact next canonical-scan state and the complete fold reconstructs byte exactly | **PASS** — the applied Aiken terminal step agrees with the TypeScript roots, proof, terminal chunk, and next-field successor | **PASS** — `15,680` datum payload bytes produce `16,383` signed bytes; the adjacent `15,681`-byte payload produces `16,385` signed bytes and rejects | **PASS** |
| Nested output `Value` policy/asset content | **PASS** — production reconstruction authenticates the exact 5,000-byte maximum-Value transaction and canonical output in both classifications | **PASS** — seven policies and the maximum 1,592 distinct policy/asset entries are bound by the output descriptor and asset frontier | **PASS** — two output chunks and 1,592 authenticated reverse-membership steps; the largest asset witness payload is 358 bytes | **PASS** — 3,198 typed output-proof steps reach the exact terminal Value summary and descriptor | **PASS** — exact maximum terminal and cross-policy membership transitions match TypeScript; substituted quantity rejects | **PASS** — at protocol major `11`, exact canonical Value sizes `5,000`/`5,001` satisfy/violate the official `validateOutputTooBigUTxO` snapshot rule against `maxValueSize = 5,000`, while both signed transactions remain below `maxTxSize`; independent Midgard parity rejects the adjacent case with `E_VALUE_SIZE` | **PASS** |
| Datum and redeemer Plutus `Data` maximum shapes | **PARTIAL** — production reconstruction authenticates the balanced maximum-cardinality datum and schema-parallel redeemer in both classifications, and the genuine collateralized redeemer remains fail closed; broad and unary-depth shapes are not yet retained fixtures | **PARTIAL** — exact lengths and `10,776`/`10,650` semantic-node counts are bound for the balanced family, not yet for maximum constructor/list breadth, map-pair breadth, or unary depth | **PARTIAL** — the balanced datum is chunk-bound inside the output proof, the redeemer field is four bounded chunks, and every Data-machine source span is at most `14` bytes; the active challenged-transition redeemer scanner still carries whole `raw_cbor` and an obsolete `9,215`-byte cap | **PARTIAL** — every action kind and the exact terminal reconstruct for the balanced maximum-cardinality trace; broad, unary-depth, and active scanner paths remain open | **PARTIAL** — real balanced-trace HeadLargeConstructor, HeadSequence, HeadMap, HeadScalar, FoldList, FoldMap, internal FinalizeFrame, and terminal controls agree with TypeScript and mutations reject; broad, unary-depth, and active scanner vectors remain open | **PARTIAL** — balanced datum `5,387` leaves / `16,382` signed bytes and redeemer `5,324` leaves / `16,382` signed bytes pass, and their one-leaf adjacent shapes are `16,385` bytes; separate broad and unary-depth envelope boundaries are not yet evidenced | **PARTIAL** |
| Script envelopes and content-addressed program material | **PARTIAL** — maximum native-script cardinality is retained, but the harness has no Plutus program-material sidecar entries | **PARTIAL** — envelope and program-material schemas bind kinds, roots, and byte lengths | **PARTIAL** — content-addressed traversal and bounded-node tests exist | **PARTIAL** — generic material traversal passes, not a maximum retained envelope/material terminal fixture | **PARTIAL** — envelope/material limits and nodes have Aiken tests, not an exact maximum retained terminal vector | **OPEN** — no applicable maximum script envelope/program-material fixture and adjacent rejection path | **PARTIAL** |
| Incremental canonical-CBOR scan states | **PARTIAL** — canonical transaction bytes are retained, but no maximum semantic scan-state fixture is selected from each path | **PASS (machinery)** — scan controls bind cursor, source, and terminal state | **PARTIAL** — generic control-step scans exist; maximum source traversal is not evidenced | **PARTIAL** — generic terminal scan vectors exist | **PARTIAL** — generic cross-language control hashes exist, not a maximum retained terminal scan | **OPEN** — no maximum/adjacent source pair is tied to the complete incremental scan | **PARTIAL** |

## Evidence at this checkpoint

Focused TypeScript checks pass:

- `12` validation boundary files / `12` tests generate the checked corpus
  from the established ordered, blob, Value, balanced Data, and mixed
  fixtures;
- fault-proofs typecheck;
- `2` production retained-DA tests: all `12` entries pass strict
  `reconstructDaPayloadV1` in both classifications before generic bounded
  field/item chunk reconstruction, and deliberate
  root/count/forced-preimage mismatches reject.

Focused Aiken checks pass:

- the exact `296`-redeemer field commitment/codec vector;
- the exact applied terminal-fold vector, at `2,068,700` memory and
  `856,755,810` CPU;
- the exact maximum Cardano inline-datum blob terminal step, at `1,921,043`
  memory and `808,043,728` CPU;
- the maximum Cardano Value finalization, at `1,921,700` memory and
  `766,243,938` CPU;
- an exact maximum-Value cross-policy membership transition, at `2,332,654`
  memory and `856,442,965` CPU.
- the maximum nested inline-datum Data terminal, at `2,170,067` memory and
  `871,272,899` CPU;
- the maximum nested redeemer Data terminal, at `2,165,655` memory and
  `870,105,005` CPU.
- four exact maximum-cardinality Data head controls plus per-kind mutations,
  at `7,441,306` memory and `2,843,553,783` CPU;
- exact FoldList, FoldMap, and internal FinalizeFrame controls plus mutated
  fold proofs, at `8,608,081` memory and `3,527,543,832` CPU.

The aggregate field codec fixture uses `124,272,878` memory and
`50,949,000,952` CPU. It is diagnostic construction evidence, not the
production one-step path.

## Next P2 sequence

Reuse the retained-DA harness rather than build a node lifecycle:

1. replace the active whole-field spend/reference-input decoders in script
   source scheduling and Value/mint initialization with bounded retained item
   folds;
2. replace the active whole-field observer decoder in CEK context construction
   with a bounded retained observer fold;
3. replace the active whole-`raw_cbor` challenged-transition redeemer scan
   and its obsolete `9,215`-byte cap with retained bounded Data traversal;
4. add distinct constructor/list breadth, map-pair breadth, and iterative
   unary-depth envelope fixtures for both datum and genuine-redeemer paths;
5. close script-envelope and program-material nodes;
6. close maximum incremental CBOR scans;
7. backfill any remaining maximum-specific ordered-field Aiken terminal
   vectors, then rerun the complete matrix.

No row may be promoted from `PARTIAL` based only on generic unit machinery or
on an ordered-field cardinality fixture.
