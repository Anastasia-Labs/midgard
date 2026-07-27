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
| Ordered transaction fields `0`–`8` | **PASS** — production reconstruction authenticates exact proof sources and canonical preimages in both paths | **PASS** — field directory plus exact item counts/lengths | **PASS** — spend/reference scheduling retains its authenticated linked schedule, and CEK observer construction now authenticates one field-3 item and one 28-byte chunk per step; no active challenged transition decodes a complete ordered field | **PARTIAL** — byte-exact generic reconstruction passes for all maximum fixtures; exhaustive TypeScript folds pass for the `434`-spend, mixed `1`-spend/`433`-reference, and exact tracked `224`-observer controls | **PARTIAL** — exact terminal vectors now agree for maximum fields `0`, `1`, `3`, and `8`, including the tracked observer source, commitment, and Data summary; a full applied `224`-transition TypeScript/Aiken observer lifecycle and the other maximum-specific field vectors remain open | **PASS** — accepted maxima and immediately adjacent signed-Cardano rejection fixtures exist for all fields; the exact observer/native-script pair is `224` at `16,338` bytes and `225` at `16,410` bytes, with the latter rejected only by `maxTxSize = 16,384` | **PARTIAL** |
| General byte blobs/chunks | **PASS** — production reconstruction authenticates the exact maximum inline-datum transaction and proof source in both classifications | **PASS** — one field-2 item binds exact item count `1`, item length `16,221`, field length `16,225`, and chunk count `4` | **PASS** — four exact reveals use at most `4,095` bytes and reconstruct the authenticated source | **PASS** — the field terminal advances to the exact next canonical-scan state and the complete fold reconstructs byte exactly | **PASS** — the applied Aiken terminal step agrees with the TypeScript roots, proof, terminal chunk, and next-field successor | **PASS** — `15,680` datum payload bytes produce `16,383` signed bytes; the adjacent `15,681`-byte payload produces `16,385` signed bytes and rejects | **PASS** |
| Nested output `Value` policy/asset content | **PASS** — production reconstruction authenticates the exact 5,000-byte maximum-Value transaction and canonical output in both classifications | **PASS** — seven policies and the maximum 1,592 distinct policy/asset entries are bound by the output descriptor and asset frontier | **PASS** — two output chunks and 1,592 authenticated reverse-membership steps; the largest asset witness payload is 358 bytes | **PASS** — 3,198 typed output-proof steps reach the exact terminal Value summary and descriptor | **PASS** — exact maximum terminal and cross-policy membership transitions match TypeScript; substituted quantity rejects | **PASS** — at protocol major `11`, exact canonical Value sizes `5,000`/`5,001` satisfy/violate the official `validateOutputTooBigUTxO` snapshot rule against `maxValueSize = 5,000`, while both signed transactions remain below `maxTxSize`; independent Midgard parity rejects the adjacent case with `E_VALUE_SIZE` | **PASS** |
| Datum and redeemer Plutus `Data` maximum shapes | **PARTIAL** — production reconstruction authenticates the balanced maximum-cardinality datum and schema-parallel redeemer in both classifications, and the genuine collateralized redeemer remains fail closed; broad and unary-depth shapes are not yet retained fixtures | **PARTIAL** — the retained field-8 descriptor binds exact item index/count/length/commitment, purpose/index, Data offset/length, and execution units; exact `10,776`/`10,650` semantic-node counts are bound for the balanced family, not yet for maximum constructor/list breadth, map-pair breadth, or unary depth | **PARTIAL** — the balanced datum is chunk-bound inside the output proof; every active challenged redeemer path now authenticates one retained field-8 item, exact outer header/tail spans, and at most one adjacent `4,095`-byte chunk for each Data-machine source span of at most `132` bytes; no production auxiliary carries a whole redeemer or whole Data preimage | **PARTIAL** — the accepted validation trace uses the retained item control for frontier ingestion, discovery match, unused-redeemer audit, CEK execution-limit selection, and CEK-context Data selection; the checked `15,982`-byte maximum retained Data payload reaches its exact terminal summary; broad and unary-depth paths remain open | **PARTIAL** — exact outer descriptor/header/tail controls and the maximum balanced nested-Data terminal agree with TypeScript; malformed descriptor, chunk, traversal, successor, and summary relations reject; broad and unary-depth vectors remain open | **PARTIAL** — balanced datum `5,387` leaves / `16,382` signed bytes and redeemer `5,324` leaves / `16,382` signed bytes pass, and their one-leaf adjacent shapes are `16,385` bytes; separate broad and unary-depth envelope boundaries are not yet evidenced | **PARTIAL** |
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
- exhaustive retained-schedule folds pass twice, deterministically, for the
  `434`-spend maximum and the mixed `1`-spend/`433`-reference maximum;
- the accepted deterministic trace keeps its exact `22` ScriptSources and
  `8` ValueAndMint states, carries one immutable resolution-schedule head at
  every ResolveInputs/ScriptSources/NativeScripts/ValueAndMint array position,
  and emits no transaction-field pair-preimage witness.
- the bounded observer test crosses the tracked signed-Cardano fixture through
  the production bridge and exhaustively folds its exact `224` committed
  hashes in reverse for both Cardano withdrawals-map and Midgard list
  encodings; the exact `225` adjacent signed shape exceeds `maxTxSize`, while
  duplicate, descending, non-28-byte, and derived-guardrail-plus-one inputs
  reject, and the exact `24`-field CEK context-control ABI remains unchanged;
- all `21` deterministic validation-machine regressions pass; the real
  PlutusV3 observer trace carries one field-3 `TransactionFieldChunkWitness`,
  then a `NoAuxiliaryWitness` finalizer, and no CEK whole-field preimage.
- the checked balanced-redeemer transaction opens its only field-8 item
  through the production retained descriptor and traverses the exact
  `15,982`-byte Data payload with source spans no larger than `132` bytes;
  removing the required adjacent chunk proof at a boundary rejects;
- the accepted validation trace and fault-proof ABI contain retained
  redeemer begin/step witnesses for all five active consumers and contain no
  `redeemer`, `rawCbor`, or `dataCborHex` payload field.

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
- maximum spend and reference-input terminal schedule vectors agree with the
  exhaustive TypeScript results; each Aiken argument carries only the last
  item and logarithmic frontier peaks;
- the NoAux ScriptSources stage-two handoff preserves the immutable original
  schedule head, while duplicate, altered head/source/key/tail, premature
  terminal, and terminal count/accumulator controls reject.
- the exact tracked maximum-`224` observer source accepts its bounded last-item
  proof and terminal fold; changing the proof/successor count to `225` against
  that `224`-item commitment, non-NoAux empty finalization, malformed
  successor/index/count/field/item/chunk/order, and a well-formed but wrong
  final summary reject. This does not make authenticated count `225`
  intrinsically invalid;
- the retained redeemer descriptor authenticates the canonical outer header,
  tail, purpose/index, Data bounds, and execution units; mutated header, tail,
  chunk evidence, descriptor/control bindings, and cross-language terminal
  summaries reject or disagree exactly;
- the maximum nested redeemer Data terminal still agrees with TypeScript after
  the production path switches from the removed whole-`raw_cbor` scanner to
  the retained field-8 item control.

The aggregate field codec fixture uses `124,272,878` memory and
`50,949,000,952` CPU. It is diagnostic construction evidence, not the
production one-step path.

## Next P2 sequence

Reuse the retained-DA harness rather than build a node lifecycle:

1. add distinct constructor/list breadth, map-pair breadth, and iterative
   unary-depth envelope fixtures for both datum and genuine-redeemer paths;
2. close script-envelope and program-material nodes;
3. close maximum incremental CBOR scans;
4. backfill any remaining maximum-specific ordered-field Aiken terminal
   vectors, then rerun the complete matrix.

No row may be promoted from `PARTIAL` based only on generic unit machinery or
on an ordered-field cardinality fixture.
