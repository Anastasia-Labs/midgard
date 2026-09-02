# `observerOrderInvalid` V1 size plan

- Frozen category: `00000025`; typed reason: `ObserverOrderInvalid { observer_index }`.
- Physical scripts: `fraud_proofs/observer_order_invalid/step_01.main.spend`
  through `step_04.main.spend`. Step 1 binds the accepted/forced source and exact
  reason coordinate; step 2 authenticates fixed-stride field 3 (including its
  certified carriage) and initializes its canonical walk; step 3 compares authenticated
  adjacent 28-byte observer items and self-loops with a domain-separated walk
  checkpoint; step 4 finalizes the direction-dependent contradiction and burns
  the computation thread while permanently minting the proof token.
- Semantic engine: the family-local `observer_order_invalid/rule.ak`, plus the
  shared proof-thread substrate, field-opening door, and native field-walk
  checkpoint engine. No unrelated subject adapter is imported by an applied
  validator.
- Maximum evidence: the maximum valid field-3 preimage, carried via the shared
  inline/raw/certified planner; fixed 30-byte item stride makes its item count
  immediately derivable under every tier. Each scan transaction advances at most 24 items
  and supplies one authenticated field opening plus its prior checkpoint.
- Fit tests: publish every applied reference script, exercise the maximum-shape
  certified opening and scan-resume transaction under repository protocol
  parameters, and record signed bytes, ExUnits, compiler version, and margins in
  the family fit ledger. No raised transaction or ExUnit limit is permitted.

The decisive predicate is `previous >= current` at the exact later-item ordinal.
The scan proves all earlier adjacent pairs strictly ascending, so a terminal
result cannot silently select a later violation after an earlier one.
