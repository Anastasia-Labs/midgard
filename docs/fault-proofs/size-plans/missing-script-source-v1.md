# `missingScriptSource` V1 size plan

- Frozen category: `0000002d`; typed reason: `ScriptSourceMissing {
purpose_kind, purpose_index }`.
- Physical scripts: `fraud_proofs/missing_script_source/step_01.main.spend`
  through `step_06.main.spend`. Step 1 binds the exact purpose coordinate and
  accepted/forced direction; step 2 authenticates the committed header trace
  state; step 3 authenticates the exact ScriptSources stage-9
  purpose and source frontiers plus the transaction-source partition; step 4
  authenticates the resolved-reference partition; step 5 scans the complete
  frontier for absence, or the authenticated prefix through the exact matching
  source for forced presence, with a domain-separated resumable checkpoint;
  step 6 finalizes universal absence or the forced-rejection presence
  contradiction, burns the computation thread, and permanently mints the proof
  token.
- Semantic engine: the family-local `missing_script_source/rule.ak` and the
  shared proof-thread substrate. Applied validators import no unrelated subject
  adapter. Purpose kinds use the consensus order spend=0, mint=1, observe=2,
  receive=3, and source locations use inline witness=0 and resolved reference
  input=1. Spent inputs never contribute script sources in the canonical
  validation machine.
- Maximum evidence: all four purpose kinds, the maximum supported authenticated
  purpose frontier, and the combined transaction/resolved source frontier. The
  frontier is Merkle-authenticated, so nothing in the thread bounds it; the
  maximum supported shape is the largest universe a canonical transaction can
  commit under the consensus field bounds — 2,520 inline script witnesses
  (field 6 at its 32 KiB preimage bound with 13-byte items) plus 819
  reference inputs (field 1 at its bound with 40-byte items), 3,339 sources in
  all — pinned in `tests/support/missing-script-source-shapes.ts` and
  re-derived from the encoders by the lifecycle suite. A step-5 transaction
  advances at most 24 sources (`staged_source_budget`) and carries the prior
  source identity, cursor, total count, found accumulator, and next-script
  hash in its checkpoint; the successor is the finalizer exactly when the
  walk reaches the authenticated scan limit (`advance_scan_v1`), which for a
  wrongful rejection stops at the matching source before the frontier's end.
  The off-chain driver (`missingScriptSourceDriverBatch`) folds 24 sources per
  batch up to a 1,024-source frontier and 20 above it, because every source
  costs its sibling depth on chain: the 24-source batch measured 13,211,702
  memory at depth 12, over the 13,200,000 reserve, and 20 sources stay under
  it (167 batches walk the maximum frontier). A same-hash source at any
  alternate location proves presence; a different hash cannot substitute for
  the selected purpose's required hash.
- Fit tests: `missing-script-source-publication-fit.test.ts` publishes every
  applied reference script; `missing-script-source-lifecycle.test.ts` runs
  both directions over every purpose kind and both source locations, the
  honest refusals, every seam substitution, cancel from every nonterminal
  step, interruption and resumption at a real checkpoint, and the maximum
  frontier in both directions, measuring every transaction under the
  repository's Van Rossem parameters with local UPLC evaluation and the 20%
  execution reserve. `missing-script-source-fit-ledger.test.ts` pins the
  signed bytes, ExUnits, compiler version, and positive margins in the
  machine-readable family ledger. No raised limits, oversized completion, or
  disabled evaluation route is permitted.

The decisive predicate is universal absence of the exact required script hash
across every authenticated source location. Wrongful acceptance succeeds only
when the complete scan finds no match; wrongful forced rejection succeeds only
when the stage-9 scan prefix includes and finds a matching source. The latter
prefix may stop at the first match, while every membership remains bound to the
complete committed source frontier.

Latest signed reference-publication measurement (testnet blueprint
`172c72d3…`, compiler `v1.1.23+5adf783`, real Van Rossem limits): `15117,
10238, 10772, 2322, 5522, 2672` bytes for steps 1–6 respectively, leaving
positive margins of `755, 5634, 5100, 13550, 10350, 13200` bytes against the
15,872-byte reliability target.

Maximum-frontier lifecycle (3,339 sources, both directions): the thinnest
signed-byte margin is the widest scan batch at 10,276 bytes (6,108 bytes of
margin); the thinnest memory margin is the deepest all-reference batch at
11,395,818 memory (1,804,182 under the 13,200,000 reserve; 5,104,182 under the
limit); the thinnest CPU margin is the same batch at 4,069,610,967 CPU
(3,930,389,033 under the reserve). Every other step, cancel and removal
transaction stays below 2,600 signed bytes and 1.8M memory.
