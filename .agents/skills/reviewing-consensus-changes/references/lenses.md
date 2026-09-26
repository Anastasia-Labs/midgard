# Review lenses and calibration

Hand this file, whole, to every reviewer in pass 1 and pass 2. Each lens is a
question to hunt with, not a box to tick. Each one comes from a defect or a
false alarm this repository has already had; the history is cited so that you
can check it, and so that nobody relaxes a lens without knowing what it
closes. Facts are as of 2026-09-25.

## Calibration

A finding is reported only when all of these hold:

1. **A source.** Something an adversary controls: a redeemer, a datum they
   create, an output they add, a transaction they order, a prover-supplied
   byte string, a block the operator commits, a parameter a deployer could
   get wrong through the SDK.
2. **A trace.** Each step from that source to the outcome, with `path:line`.
3. **An outcome that matters.** An honest block removed or an honest party
   slashed; a fraudulent block that cannot be proven; value created, taken or
   frozen; liveness lost; a transaction that no longer fits its budget in a
   supported shape; a guard that cannot fail.
4. **Reachability.** Why nothing earlier in the transaction, the thread, or
   the SDK refuses the input first.

Drop a finding that lacks one of them, and record the drop and its reason.
Two real findings beat twelve speculative ones.

These are **not** findings:

- A validator that does not re-check its own `validator main(...)`
  parameters. On-chain code trusts deployment parameters by owner ruling; see
  the parameter-trust lens.
- "`tx_id` should hash `witness_set_hash`." The two commitments are separate
  by design; see the anchoring lens for what the real question is.
- Legacy fault-proof families that still attach step scripts inline. The
  reference-script ruling governs new work; see that lens.
- "Consider more validation" with no input that gets through.
- "This function is complex and could have bugs."
- A cost concern with no measurement. Measure it, or report it as PLAUSIBLE
  with the measurement that would decide it.
- Style, naming or comments, unless the comment states an invariant the code
  breaks.
- Anything you would not defend as a real defect to the person who wrote the
  code.

## The lenses

### 1. Parameter trust

On-chain code trusts deployment parameters: a validator never re-checks a
`validator main(...)` parameter's width, cardinality or domain
([contracts.md](../../../../docs/agents/contracts.md)). The owner ruled this on
2026-09-04; commit `fa52b3844` removed about 22 parameter-only checks from
fifteen validators and libraries, because they spent execution units on every
spend re-proving a fixed fact. The shape checks moved to the SDK, into
`assertParameterShapes`, which `applyBlueprintParams` calls on every
application
`[runtime: assertParameterShapes, demo/midgard-sdk/src/fraud-proof/contracts/blueprint.ts:364]`.

Ask:

- Does the change add an on-chain check that can only be false for a
  dishonestly deployed script? That check is the defect `[review]`.
- Does a new parameter reach the chain through `applyBlueprintParams`, with
  every builder, fixture and emulator scenario updated in the same change
  ([contracts.md](../../../../docs/agents/contracts.md))? `[review]`
- Is the value a **parameter**, or something read from a datum, redeemer or
  reference input? Only parameters are trusted. A datum field that looks like
  configuration is adversary input.

### 2. Always-succeeds scripts

Under Plutus V3 a validator that receives too few parameters reduces to a
lambda value, and the ledger reads "no error" as success. Ten validation-trace
semantic resolvers shipped that way after #592 added a parameter that the SDK
did not apply: a challenger could defeat any honest block (#605, found by the
symmetric-soundness emulator test; fixed in #609, commit `ac54d01a1`).
`applyBlueprintParams` now refuses too few and too many parameters
`[runtime: applyBlueprintParams, demo/midgard-sdk/src/fraud-proof/contracts/blueprint.ts:211]`.

Blind spot: a site that deploys `compiledCode` bare never reaches that guard.
#610 (open) tracks that class; `f26292930` and `9d84bf552` closed the known
bare loaders, and as of 2026-09-25 there are 36 other `compiledCode` reads in
`demo/*/src`.

Ask:

- Does every script this change deploys or references go through
  `applyBlueprintParams`, or through a loader that refuses a validator with
  declared parameters?
- Can any arm of the validator return `True` without inspecting the
  transaction: an `else` handler, a wildcard redeemer arm, an early exit, a
  `when` branch that forgets its conjunction?
- Does a spending validator that delegates to a rewarding script perform the
  whole yield handshake (NFT-authenticated reference input, script hash from
  its `reference_script`, the exact zero withdrawal)? Skip one step and the
  delegate can be any script
  ([withdraw-zero-yielding.md](../../../../docs/agents/withdraw-zero-yielding.md)).

### 3. Decoders and the pinned compiler

Every `expect` over `Data` is a decoder, and its domain is the set of values
it accepts. Stock Aiken v1.1.22 named generated decoders by the local type
name only, so two same-named types in different modules shared one decoder,
and the shipped blueprint had under-strict decoders on the adjudication path
(#521; `MintRedeemer` alone is declared in 12 modules). The fork pinned in CI
fixes it
`[ci: aiken-ci.yml/Assert the pinned compiler identity and put it on PATH]`,
and local entry points assert the same pin
`[script: onchain/aiken/scripts/pinned-compiler.mjs]` `[hook: pre-commit]`.
Blind spot: the hook skips the check when no `aiken` binary is found, and a
hand-typed `aiken check` asserts nothing; `aikup` can repoint the local
`aiken` at stock.

Ask:

- For every datum, redeemer and prover-supplied `Data` the change decodes:
  what does the decoder accept that the protocol does not? Extra constructor
  fields, trailing list items, non-canonical integers, an unexpected
  constructor index?
- Does a decode that can fail on adversary input make an honest party's
  transaction fail? A prover who can make the refusal path unreachable, or a
  datum an attacker can make undecodable to strand an honest spend, is a
  finding.
- Where the protocol must adjudicate an out-of-domain input rather than
  refuse it (a fault proof about malformed bytes), is there an explicit arm
  for it?
- Were the change's Aiken results produced by the pinned fork?

### 4. Value conservation

Every lovelace and token a transaction moves must be accounted for exactly.
The fraud-prover reward is paid to the prover's enterprise address, ADA only,
with no datum or reference script, and over- and under-payment are refused
alike
`[aiken-test: state-queue/]` (predicate `fraud_prover_reward_output_is_exact_v1`,
`onchain/aiken/validators/state-queue.ak:189`).

Ask:

- For each input the change lets a validator spend: where does its whole
  value go, and what check pins each destination? "At least" where the
  protocol means "exactly" is a finding when the surplus can be redirected.
- Can an output that satisfies the check also carry extra tokens, a datum or
  a reference script the check does not look at?
- Does an equality such as `fee == penalty` survive the off-chain builder?
  Lucid charges a change output's own fee on top of `setMinFee`, so an exact
  fee needs a builder with no Lucid-managed change
  (`demo/midgard-sdk/src/operator-lifecycle/exact-fee.ts`). The bad-settlement
  slash builder at `demo/midgard-sdk/src/settlement.ts:744` still uses a bare
  `setMinFee` (see
  [invariants-user-events.md](invariants-user-events.md) UE7).
- Mint and burn: is each minted token's quantity, name and destination
  pinned, and can a burn be skipped?

### 5. Anchoring, not preimage

When a proof reads bytes, the question is whether those bytes are tied to a
commitment the block authenticated. A preimage that hashes correctly to a
hash the prover also supplied proves nothing `[review]`. For witness-set
fields the anchor equality is pinned by tests
`[aiken-test: midgard/fraud-proofs/field-opening-v1.test/]`
([invariants-fraud-proofs.md](invariants-fraud-proofs.md) FP5); a new reader
of committed bytes has no such test until its author writes one.

Precision example, a finding that looked real and was not: "`tx_id` does not
commit `witness_set_hash`." The transaction id is the Level-2 commitment over
the body and the witness-set hash is a separate Level-1 commitment, by design.
Adding the witness-set hash to the id's preimage would be the bug. The real
question was whether the witness-set hash a proof reads is still tied to the
block-authenticated commitment.

The real defect of that shape: a field-preimage certificate could be minted
over a fabricated witness set, because the consuming door selected the
certificate by token name without comparing its field hash to the anchored
commitment. `d14c3e9aa` (#606) welded `field_hash` into the mint-verified
datum and added the equality at the door. The decision record states the rule:
"Token naming or an anchored witness-set hash without that comparison cannot
substitute for content authentication"
([field-carriage decision](../../../../docs/midgard/decisions/field-carriage-authentication-and-budgeting.md)).

Ask:

- For every byte string a step reads, which authenticated commitment does it
  trace back to, and at which `path:line` is the equality checked?
- Does a rebinding or threading step carry the authenticated commitment
  forward, or only a hash the prover can choose?
- Before calling something a preimage defect, has the anchoring been checked?

### 6. Reference scripts for fault-proof steps

Fault proofs and their supporting witness scripts deploy as published
reference scripts, never attached inline, whatever their size. The in-tree
record of this owner ruling (2026-08-26) is the header of
`demo/midgard-fault-proofs/src/witness-reference-scripts.ts:7-12`; `d3df3323e`
(2026-08-29, "require published reference scripts") removed the inline
fallback. Submitters hash-check each reference script against the script the
transaction executes, and a missing entry fails closed
`[runtime: requireWitnessReferenceScriptUtxo]`
(`witness-reference-scripts.ts:47`, throw at `:96`; linear families use
`requireLinearFaultReferenceScript`,
`demo/midgard-fault-proofs/src/linear-fault-family.ts:69`). Older families
that still attach inline are not a precedent for new work.

Blind spot: the runtime check covers only the builders that call it. No
scan refuses a new `attach.*` in a fault-proof builder, and
`inline_emulator_only` certificate witnesses
(`demo/midgard-sdk/src/fraud-proof/field-preimage-carriage.ts:859`, `:920`)
attach inline by design `[review]`.

Ask:

- Does a new or changed family publish its steps as reference scripts, and
  do its submitters go through one of the two functions above?
- Does a new inline attach sit on an emulator-only path, and is that path
  unreachable from production submitters?

### 7. Both polarities

Every contract has emulator scenarios for the honest path and for the
refusal the validator must make
([contracts.md](../../../../docs/agents/contracts.md)) `[review]`. For a fault
proof that means two suites: the proof succeeds when the fault is really in
the commitment, and a realistic adversary running the same flow against an
honest commitment is refused at the intended check. The
`submit-init-emulator-*-adversarial.test.ts` suites in
`demo/midgard-fault-proofs/tests` are the pattern. The fault-proof gap
register's closure rule asks for the same pair
([remaining-gaps.md](../../../../docs/fault-proofs/remaining-gaps.md)).

Ask:

- Does each changed validator arm have a negative that fails **at that arm**?
  A negative that fails earlier, in the builder or at another check, passes
  for the wrong reason and keeps passing when the arm is deleted.
- Does the positive scenario use the real deployment path (applied
  parameters, reference scripts), not a hand-built script?

### 8. Gates that cannot fail

A gate that cannot fail looks exactly like a healthy one. #519 confirmed 24 of
them in one sweep. Recurring shapes:

- a zero-collection Aiken selector that exits 0 (#523;
  `[script: onchain/aiken/scripts/run-focused-check.mjs]` and
  `guard-focused-selector.mjs` now fail closed on zero);
- an Aiken `test ... fail` over an `and { }`, which passes as soon as any one
  conjunct is false, including the one the test meant to exercise (fixed in
  `7d01f2b71`, #485);
- a runtime gate whose condition is always met in production: the watcher's
  predecessor-ledger gate checked `replayContext === undefined`, which a
  launch-scope family always set (`1b53eafd8`);
- a check whose expected value comes from the artifact under test;
- a fix applied to one copy of duplicated code while a twin keeps the defect:
  the empty-payload bind was fixed family by family over five commits, and a
  hand-parsing copy with no caller remains
  ([invariants-fraud-proofs.md](invariants-fraud-proofs.md) FP6).

Ask:

- Can each new or changed test fail? Name the mutation that turns it red,
  then run it (step 5 of the skill).
- Does each `fail` test pin one disposition, not a conjunction?
- Does a gate compare against an independent value, or against something the
  thing it checks supplies?
- When a fix lands in one copy, where are the others (`grep` the function
  name and the parsed field), and does each get the same fix?

### 9. Execution and size budgets

Every flat-reversion measurement is judged against 13,200,000 memory units:
the 20% reserve off Cardano's 16,500,000 `maxTxExUnits` memory cap
([GOAL_SPEC §3.3](../../../../docs/exec-plans/GOAL_SPEC.md)). Execution
ledgers pin measured units for several lanes
`[ci: aiken-ci.yml/Pin the Q1x family execution ledger]` and its sibling
`Pin the ... execution ledger` steps, judged by
`[script: onchain/aiken/scripts/exec-ledger-within-basis-v1.mjs]`. Blind spot:
`aiken check` reports units but compares them to nothing, so a validator
without a ledger can grow past the budget with every suite green.

Ask:

- Does the change add work proportional to an adversary-chosen size (inputs,
  items, bytes, list length)? What is the largest supported shape, and was it
  measured?
- Did a ledger row move? A moved row needs a reason in the change, not a
  silent re-pin.
- Does the transaction still fit `maxTxSize` with its evidence, once it
  carries every witness and datum?

### 10. TypeScript and Aiken twins

Canonical codecs and several proof computations exist twice, in TypeScript
and in Aiken, tied by generated golden channels: a TypeScript generator emits
a JSON fixture and an Aiken test module, and `:check` fails when either
differs from what the generator produces
`[ci: midgard-node-ci.yml/Check native V1 field-access golden vectors]` and
the sibling `Check ... golden vectors` steps. Blind spots as of 2026-09-25:
`fixtures:transaction-root-v1:check` and `fixtures:native-compact:check` are
not run by any workflow, and editing a producer then regenerating re-greens
both suites, so the channel shows drift only when the goldens are not
regenerated.

History: on-chain `parse_script_ref`
(`onchain/aiken/lib/midgard/ledger-output-v1.ak:334`) accepts tag-0
reference-script bytes as opaque, while the TypeScript codec parses them and
throws on malformed bytes (`demo/midgard-core/src/codec/versioned-script.ts:94`),
so the two sides disagree about the honest post-state root for a transaction
only one side can decode (#633, open). The general form is worse for
liveness: the reference challenger declines a whole transaction when its codec
refuses any one output item, so an operator can shield a real fault by
committing one refused item beside it (#635, open).

Ask:

- Does the change edit one twin without the other?
- If goldens changed in the diff, which producer change explains each changed
  vector, and does the Aiken side agree for a reason other than having been
  regenerated?
- Does the TypeScript side accept anything the Aiken side refuses, or the
  reverse? The watcher decides from the TypeScript side and the chain from the
  Aiken side; a disagreement is a false accusation or a missed fault.

### 11. Ledger facts, not builder assumptions

A validator can rely only on what the ledger guarantees about the script
context ([script-context-invariants.md](../../../../docs/script-context-invariants.md)).
Inputs arrive ordered by output reference, not in builder order; withdrawals by
credential; redeemers by purpose `[review]`. The emulator enforces only the
ledger rules its version implements, so an emulator pass is evidence, not
proof.

History: native-script-decoding step-03 built positional reference-input
indices from the carriage UTxOs alone, while the ledger sorts the complete
reference-input set, so whenever the step's own reference script sorted first
the index named the wrong UTxO (`fc635c8f9`). The emulator itself once let
a transaction's reference inputs overlap its inputs, which the ledger refuses
when a PlutusV3 script runs; lucid-evolution 0.6.5 added the rule, and a
duplicate-operator slash that proved itself with the node being removed is
now refused as it would be on chain (`967b1698b`).

Ask:

- Does the change assume an order, a uniqueness or an absence that only the
  honest builder produces? An adversary builds their own transaction.
- Does an index in a redeemer point into a list whose order the adversary
  controls, and is the indexed element authenticated after lookup?
- Does a test pass only because the emulator accepts a transaction the ledger
  would refuse?

### 12. Replacement paths keep every guard

When a change replaces or retires a path (a new redeemer arm, a rewritten
step, a family moved onto a new door), list the guards the old path enforced
and find each one on the new path `[review]`.

History:

- The first #575 rebind moved the Q1x families onto the authenticated field
  door. It re-derived the witness-set hash but dropped the MPF anchoring the
  old path had; review reproduced an end-to-end forgery, and `2fec6b0fb`
  restored the anchor.
- `c07ac1326` (#424, 2026-04-07), an off-chain PR about queuing block
  submission, deleted the state queue's `end_time` equality; it stayed gone
  until `66d2d5d54` (2026-07-29).
- `3e3090aa1` (2026-08-31) removed the link-removal arm's operator check on a
  sound argument (a descendant's operator legitimately differs after
  rotation) without binding the anchor's operator in its place. See
  [invariants-state-queue.md](invariants-state-queue.md) SQ5; the relaxation
  awaits an owner ruling in #643.

The counter-example is also instructive: `d14c3e9aa` (#606) deleted
`carriage_reaches_the_anchor` rather than keep a guard that could no longer
fail once the certificate mint welded the field hash.

Ask:

- For every check the old code made, where does the new code make it, or
  what makes it unnecessary? Write the pairing down.
- When a check is removed because it was too strong, what weaker check
  replaces it?
- Do the rarely taken exits (timeouts, cancellation, removal, reclaim) keep
  the same invariants as the main path?
