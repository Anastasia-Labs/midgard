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
  purpose frontier, and the combined transaction/resolved source frontier. A
  step-5 transaction advances at most 24 sources and carries the prior source
  identity, cursor, total count, found accumulator, and next-script hash in its
  checkpoint. A same-hash source at any alternate location proves presence; a
  different hash cannot substitute for the selected purpose's required hash.
- Fit tests: publish every applied reference script and measure maximum-frontier
  purpose authentication, all source carriage publications, the most expensive
  scan-resume branch, cancel, final mint, and canonical leased removal under the
  repository's Van Rossem parameters with local UPLC evaluation. Record signed
  bytes, ExUnits, compiler version, and positive margins in the machine-readable
  family ledger. No raised limits, oversized completion, or disabled evaluation
  route is permitted.

The decisive predicate is universal absence of the exact required script hash
across every authenticated source location. Wrongful acceptance succeeds only
when the complete scan finds no match; wrongful forced rejection succeeds only
when the stage-9 scan prefix includes and finds a matching source. The latter
prefix may stop at the first match, while every membership remains bound to the
complete committed source frontier.

Latest signed reference-publication measurement (testnet blueprint, real
Van Rossem limits): `15117, 10238, 10772, 2336, 5534, 2672` bytes for steps
1–6 respectively, leaving positive margins of `755, 5634, 5100, 13536, 10338,
13200` bytes against the 15,872-byte reliability target.
