# `observerOrderInvalid` V1 size plan

- Frozen category: `00000025`; typed reason: `ObserverOrderInvalid { observer_index }`.
- Machine predicate: the phase-A observer walk of
  `validation-machine/phase-a-script-preconditions.ak`. Whenever field 3 is
  non-empty the machine visits every item in order and rejects at the first
  later item that is not strictly greater than its predecessor
  (`bytearray.compare(previous, current) != Less`, so a duplicate offends). The
  walk runs before the integrity-hash and network-scalar arms of the same
  function and is not gated by them: an out-of-order field is rejected whether
  or not Plutus evaluation is required, which is why this family, unlike its
  `observersForbiddenOnUntaggedNetwork` sibling, needs no precondition twin.
  `observer_index` is the ordinal of the offending later item.
- Family twin (`observer_order_invalid/rule.ak`): the scan proves every
  adjacent pair before the cited ordinal strictly ascending, decides at the
  ordinal itself, and, when the authenticated walk ends first, exhausts as
  ordered because no offending item exists at any ordinal (`exhaust_scan_v1`).
  Wrongful acceptance closes on a violation at the ordinal; wrongful rejection
  closes when the walk reaches the ordinal ordered, when it exhausts the field
  ordered (an ordinal past the end, or the empty field), or when the ordinal
  is 0, which is never the later member of a pair. A rejection whose ordinal
  lies past an earlier offending pair is refused by the prefix walk: the
  transaction is invalid and the family never convicts over it.
- Physical scripts: `fraud_proofs/observer_order_invalid/step_01.main.spend`
  through `step_04.main.spend`. Step 1 binds the accepted/forced source and exact
  reason coordinate; step 2 authenticates fixed-stride field 3 (including its
  certified carriage) and initializes its canonical walk; step 3 compares authenticated
  adjacent 28-byte observer items and self-loops with a domain-separated walk
  checkpoint, taking the engine's own end before spending budget; step 4
  finalizes the direction-dependent contradiction and burns the computation
  thread while permanently minting the proof token.
- Semantic engine: the family-local `observer_order_invalid/rule.ak`, plus the
  shared proof-thread substrate, field-opening door, and native field-walk
  checkpoint engine. No unrelated subject adapter is imported by an applied
  validator.
- Maximum evidence: the largest field 3 the §5.4 aggregate field bound admits,
  a three-byte array header plus 1,092 fixed-stride 30-byte items (32,763 of
  32,768 bytes), carried as three certified chunks. One more item has no
  carriage tier and its fault belongs to the committed-field-shape families;
  the decisive predicate carries no numeric bound of its own. Each scan
  transaction advances at most 24 items and supplies one authenticated field
  opening plus its prior checkpoint, so the maximum shape takes 46 scans in
  either direction.
- Fit tests: publish every applied reference script, exercise the maximum-shape
  certified opening and every scan-resume transaction under repository protocol
  parameters, and record signed bytes, ExUnits, compiler version, and margins in
  the family fit ledger. No raised transaction or ExUnit limit is permitted.

## Parameters and reference roles

- Step 01 parameters, in order: step-02 script hash, computation-thread policy
  ID, hub-oracle script hash.
- Step 02 parameters, in order: step-03 script hash, computation-thread policy
  ID, field-preimage certificate policy ID.
- Step 03 parameters, in order: step-04 script hash, computation-thread policy
  ID, field-preimage certificate policy ID.
- Step 04 parameters, in order: permanent fraud-proof policy ID, permanent
  token address data, computation-thread policy ID.
- Family publications: steps 01 through 04. Shared references: computation-thread
  mint, fraud-proof mint, PHAS-membership withdrawal, chunk verification
  withdrawal, field-preimage certificate mint, and the canonical removal roster.

## Lifecycle gate

The real testnet-blueprint Lucid gate executes, on the registered SDK chain
(`expectRegisteredChainParity`), in
`demo/midgard-fault-proofs/tests/observer-order-invalid-lifecycle.test.ts`:

- accepted maximum field with its last pair descending (ordinal 1,091),
  driven by the production actuator from its admitted artifact through 46
  resumed scans;
- accepted first-pair (ordinal 1) and middle-duplicate (ordinal 2)
  convictions through the production submitters;
- forced maximum strictly ascending field cited at ordinal 1,091, and forced
  contradictions at a middle ordinal, at an ordinal past the field's end, over
  the empty field, and at ordinal 0;
- honest accepted refusal (ordered field cited at ordinal 1) and honest forced
  refusal (duplicate at the cited ordinal), both at the terminal step; a
  rejection cited past an earlier offending pair refused by the scan itself;
- typed-reason mutation (a leaf typed `ObserversForbiddenOnUntaggedNetwork`,
  the other reason behind the same machine code, claimed as this family), a
  leaf naming ordinal 1,091 claimed at 1,090, subject and direction mutation;
- every authentication seam: transaction membership, forced leaf header and
  membership, successor script, compact source, published carriage,
  certificate, chunk order, resumed checkpoint bytes, successor accumulator,
  scanning state sent to the terminal script, item budget over 24, a decision
  before the ordinal, and a flipped terminal decision;
- cancellation at all four physical stages;
- permanent mint and removal in every successful direction;
- signed publication and lifecycle measurements under ordinary Van Rossem
  limits with local evaluation enabled, closed by
  `assertCompleteLifecycleCoverage`.

## Measured implementation status

Measured against testnet blueprint
`569daa74f2f35c97fcfa3f541a123ca4c7dde3ff113b8fa8dd1bfba8c5499182`
(`aiken v1.1.23+5adf783`), 154 ledger rows, every margin positive. Applied
step 01 publishes at 14,709 bytes (1,163-byte reserve margin), step 02 at
7,644, step 03 at 9,185 and step 04 at 2,214 bytes. The maximum field's three
certified chunks publish at 15,872, 15,872 and 2,795 bytes in both directions;
the smallest signed-byte margin of the ledger is that 512-byte reserve on the
two full chunks. The largest lifecycle transaction is the accepted step 01 at
2,134 bytes. The scan is the execution frontier: the most expensive resume of
the accepted maximum walk uses 6,747,343 memory units (9,752,657 remaining)
and the most expensive resume of the forced maximum walk 3,230,932,654 CPU
units (6,769,067,346 remaining). Terminal proof mints are 916 bytes; removal
is 2,048 bytes in every direction. The final machine-readable artifact is
`docs/fault-proofs/size-plans/observer-order-invalid-v1-fit-ledger.json` in
the shared `midgard-van-rossem-fit-ledger-v1` schema, written by the lifecycle
suite under `MIDGARD_WRITE_FIT_LEDGER=1` and pinned by
`observer-order-invalid-fit-ledger.test.ts`.
