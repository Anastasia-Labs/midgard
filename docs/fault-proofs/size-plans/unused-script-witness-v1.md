# `unusedScriptWitness` V1 state and fit plan

Category `0000002f` proves that one canonical field-6 inline script witness is
not selected by any authenticated script purpose. It remains a narrow family:
no missing-source, redeemer, script-decoding, or execution-validity predicate
enters an applied validator.

## State and transitions

1. `step_01` binds accepted/forced source, direction, exact
   `UnusedScriptWitness { script_index }` reason when forced, transaction
   identity, validation-trace root/count, and the field-6 coordinate.
2. `step_02` authenticates the exact producer-committed ScriptSources control
   and opens the named field-6 inline-source leaf. Wrongful acceptance binds
   the stage-11 audit state at the offending coordinate; wrongful rejection
   binds the complete stage-12 audit. The work-root-bound 31-field control is
   decoded on-chain and binds language, hash, encoded length, item commitment,
   and complete purpose frontier. A fabricated committed frontier is convicted
   through header trace invalidity rather than trusted as canonical.
3. `step_03` freezes the authenticated complete purpose frontier. Its
   domain-separated checkpoint binds transaction,
   witness coordinate/hash, cursor zero, total count, peaks, and the next
   script.
4. `step_04` authenticates every earlier inline source descriptor. Any equal
   hash proves the named witness is shadowed (and therefore unused); a changed
   alternate-source descriptor or membership is refused.
5. `step_05` resumably authenticates purpose leaves in canonical order. The
   cursor advances only on the committed next leaf. A target-hash match records
   `used`; exhausting the exact committed count records `unused`. The self-loop
   preserves the domain-separated checkpoint and its own applied script.
6. `step_06` applies accepted/forced polarity, burns the computation-thread
   token, and permanently mints the proof token. Generic leased state-queue
   removal follows the existing canonical removal path.

## Semantic engine and maximum evidence

The family imports the frozen validation-trace, validation-Merkle, and
`script_proof_v1` engines. Every control is bound through header trace root,
event/source/direction/verdict/transaction identity, machine state, work root,
stage, and coordinate. A malicious producer cannot escape accountability by
fabricating the frontier: doing so changes the committed work root and opens
the trace-invalid arm. Maximum evidence is the purpose frontier and script-source frontier, with
one membership proof per submitted scan item. The reverse scan is bounded per transaction and must resume until
`cursor == purpose_count`; an empty or partial scan cannot finalize absence.
Spend, mint, observe, and receive purpose kinds are exercised. Inline and
reference source descriptors are exercised as alternatives; only an inline
field-6 coordinate may be the accused witness.

## Fit test

Apply all six validators backwards from the freshly built testnet blueprint.
Publish each complete reference script with the shared Van Rossem parameters,
then measure init, all steps including the maximum scan/resume branch, cancel
from every nonterminal step, permanent proof mint, and leased canonical
removal. The machine-readable ledger must retain positive margins against
15,872 publication bytes and the Cardano hard byte/memory/CPU limits. No
positive route may use oversized transactions, raised limits, or disabled
local evaluation.

The final testnet blueprint digest is
`398b776c68680afdab31416688135c1f653a1bd9c372481cd901b48b53108ac1`.
Fully applied publication sizes for steps 1 through 6 are respectively 14,978,
14,251, 1,969, 3,369, 3,745, and 2,211 bytes, leaving ordinary publication
reserve margins of 894, 1,621, 13,903, 12,503, 12,127, and 13,661 bytes.
