# State queue invariants

Scope: `onchain/aiken/validators/state-queue.ak`,
`onchain/aiken/lib/midgard/state-queue.ak`, the removal and merge tests next
to them, and the operator-directory arms the removal path calls into.

Each entry gives the rule, where it is enforced, the history that earned it,
how many times it has been broken or nearly broken ("recurrence"), and a
status:

- **VERIFIED**: the code site and at least one refusing test were read at the
  cited lines, and the provenance was checked in git or the issue tracker.
- **PARTIAL**: the code site was read, but a test, the provenance, or part of
  the rule is missing. The missing part is named.

Line numbers are as of the commit this skill was written against; re-find the
symbol if they drift.

## SQ1. A header's end time is the commit's inclusive upper bound

Status: VERIFIED.

Rule: a committed header's `end_time` equals the inclusive upper bound of the
commit transaction's validity range, and `start_time < end_time`.

Enforced: `commit_bound_header_time_is_valid`
(`lib/midgard/state-queue.ak:60`), called from the commit arm
(`validators/state-queue.ak:1113-1119`)
`[aiken-test: midgard/state-queue.test/]`. The boundary tests at
`lib/midgard/state-queue.test.ak:245-322` refuse one millisecond above the
bound, an end strictly inside the window, and every value past or inside it.

Provenance: `18c3c56ba` (#400) and `bdded5380` set the bound. `c07ac1326`
("Queue Block Submission in `midgard-node`", #424, 2026-04-07), an off-chain
PR, deleted the on-chain equality without replacement
(`git log -S'output_end_time == inclusive_tx_upper_bound'`). `66d2d5d54`
(2026-07-29) restored it. `df2f149b5` then found the Q60 tests had been
selected with an `-m` filter that collected nothing, and `b02b25cd1` killed a
surviving interval mutant.

Recurrence: 5 (wrong sign, silent deletion, vacuous selector, surviving
mutant, original missing bound).

## SQ2. The node key is the hash of the serialised header

Status: PARTIAL (no validator-level test pins the derivation).

Rule: a state-queue node's key is `blake2b_224` of the serialised header, the
same derivation at commit and at every reader.

Enforced: commit at `validators/state-queue.ak:1106-1108`; readers at
`:171`, `:741`, `:761`, `:900` and `lib/midgard/state-queue.ak:457`, `:495`,
`:544` `[review]`.

Provenance: `ab96b2457` (PR #437, "Fix output_header_hash calculation in
state queue policy") replaced a derivation that applied `un_b_data` to a
constructor.

Recurrence: 1. Blind spot: a new reader that hashes a different encoding of
the header would pass every existing test.

## SQ3. Appends continue the previous head exactly

Status: VERIFIED.

Rule: an appended header carries the previous head's hash, its UTxO root, a
`start_time` equal to the previous `end_time`, and the same protocol version.
The genesis protocol sentinel is never copied into an ordinary header.

Enforced: `validators/state-queue.ak:72-101` and `:1186-1226`
`[aiken-test: state-queue/]` (`q49_l295_previous_header_control_and_independent_mutations`
`:1936`, `q49_l295_confirmed_genesis_control_and_independent_mutations`
`:1980`, `q49_l295_confirmed_ordinary_control_and_independent_mutations`
`:2032`); sentinel refusal at `lib/midgard/state-queue.test.ak:52`
`[aiken-test: midgard/state-queue.test/]`.

Provenance: #313, #314, and the `66d2d5d54` watcher-checkpoint hardening.

Recurrence: 3.

## SQ4. Merge takes only a matured, available, unproven head

Status: PARTIAL (merge refusals for DA status and the correction lock are
tested only off-chain).

Rule: merge consumes only the head that has matured, whose DA status is
Attested or Published, whose `proven_fraud` marker is `None`, and whose
redeemer roots equal the header. A settlement is spawned if and only if the
header carries L2 material. The correction lock must be Idle.

Enforced: `validators/state-queue.ak:386-496` (unproven and matured `:426-432`, DA status
`:441-442`, roots `:452-472`, settlement `:474-493` with
`header_carries_l2_material` at `:377`), lock at `:1611-1616`
`[aiken-test: state-queue-merge.test/]` (`:148`, `:155`, `:260`, `:336`,
`:359`). Off-chain readiness refusals:
`demo/midgard-node/tests/merge-readiness.test.ts:102`, `:160`.

Provenance: #314; `bf79ece94` (no settlement for empty trees); `668673e9f`
(merge trusted the redeemer's roots without equating them to the header);
`090436fc3` (merge could race a terminal fraud proof; the completed-fraud
marker, SQ8, closes it).

Recurrence: 4.

## SQ5. Every slash names the operator of a block it removes

Status: PARTIAL. **Suspected soundness gap, established by reading only; no
test or transaction has exercised it.** Open owner question; see #643 item 2.

Rule (intended): the operator slashed by `RemoveFraudulentBlockHeader` is the
operator of the fraud-proved block.

What the code does:

- The terminal arm binds it:
  `expect removed_header_view.operator_vkey == fraudulent_operator`
  (`validators/state-queue.ak:717`), refused by
  `state_queue_removal_rejects_wrong_target_operator`
  (`validators/state-queue-removal.test.ak:377`)
  `[aiken-test: state-queue-removal.test/]`.
- The link arm does not. `remove_fraudulent_blocks_link_v1`
  (`validators/state-queue.ak:637-675`) takes no operator and discards the
  anchor's own node data (`_fraudulent_header_data`).
- `fraudulent_operator` is a redeemer field
  (`validate_remove_fraudulent_v1`, `:1376`). The slashing arms
  (`:1420-1530`) only require the operator-directory redeemer's
  `slashed_operator` to equal it (`cross_validate_slashing_reason`,
  `lib/midgard/operator-directory.ak:381`), and the directory's
  `SlashOperatorForBadState` arm only checks the same equality back
  (`:343-356`). Neither directory validator reads the state queue.

Consequence if the reading is right: with a valid fraud proof for a block
that still has a descendant, the link-removal transaction can name any
active or retired operator, slash that operator's bond, and pay the prover
reward. The real operator is slashed again at the terminal removal, with a
second reward.

Provenance: `3e3090aa1` (2026-08-31) removed the descendant-operator equality
from the link arm, with the rationale at its doc comment: the descendant's
operator legitimately differs after scheduler rotation, so requiring it would
deadlock correction. The anchor in the link arm is the fraud-proved block
itself, whose operator is available and does not rotate; binding that was not
added. #643 item 2 (OPEN) asks for an owner ruling on the relaxation.

Recurrence: 1 (this relaxation), and the same shape as SQ9 and the miss
patterns below: a check present in one arm and absent from its sibling.

Review action: any change to the removal or slashing path must state whether
it closes, keeps or widens this gap. Do not fix it inside an unrelated change;
the fix shape is an owner decision.

## SQ6. The prover reward is exact and paid once per bond

Status: VERIFIED (subject to SQ5).

Rule: the arm that consumes an operator bond pays exactly
`env.fraud_prover_reward` to the prover named in the fraud-proof datum. The
already-slashed arm pays nothing.

Enforced: `fraud_prover_reward_output_is_exact_v1` and routing at
`validators/state-queue.ak:189-302`; arms at `:1455-1460`, `:1488-1493`,
`:1508-1512` `[aiken-test: state-queue/]`
(`d3_reward_output_exactness_control_and_mutations` `:2115`,
`d3_reward_routing_control_and_mutations` `:2174`,
`d3_reward_routing_rejects_out_of_range_index` `:2207`,
`d4_reward_exclusivity_control_and_double_claim` `:2225`,
`d3_reward_routing_does_not_require_the_prover_signature` `:2249`); SDK side
`demo/midgard-sdk/tests/state-queue.test.ts:112-162`.

Provenance: `573a536fc` (#603). #643 item 1 (OPEN) records that the prover
signature binding from #603 was removed; the test at `:2249` pins the
current behaviour.

Recurrence: 1.

## SQ7. Fraud-proof tokens are permanent

Status: PARTIAL (code and tests verified; the design rationale is recorded in
commit messages only).

Rule: a fraud-proof token can never be spent. Proving fraud mints exactly one
proof token and burns exactly one computation-thread token.

Enforced: `validators/fraud-proof.ak:9-16` (spend always fails), `:50-58`
(mint shape) `[aiken-test: fraud-proof/]` (`:108`, `:121`, `:134`, `:151`,
`:166`).

Provenance: #603.

## SQ8. The completed-fraud marker is written once and blocks progress

Status: VERIFIED.

Rule: `proven_fraud` is set once, from `None`, in the same transaction that
mints the proof, and a head carrying it can neither be appended to nor
merged.

Enforced: `validate_completed_fraud_record`
(`lib/midgard/state-queue.ak:470-512`, which requires `None` before and
`Some(proof_asset)` after at `:496` and `:510`) and its reader at `:519-548`;
the append guard `head.proven_fraud == None` at `validators/state-queue.ak:117`,
the merge guard at `:426`, and the commit output at `:1101`
`[aiken-test: state-queue-removal.test/]` (`:1199-1496`, including `:1248`,
`:1372`, `:1485`).

Provenance: `090436fc3` (2026-09-23, commit only; no issue).

## SQ9. Cross-validator redeemer lookups use the right purpose

Status: PARTIAL (no test would fail on a wrong purpose).

Rule: a validator reading another script's redeemer looks it up under that
script's actual purpose; `CommitBlockHeader` is a Mint redeemer.

Enforced: `[review]`.

Provenance: `67b1b4d86` (2026-05-06) and `4e42e3f5e` (2026-06-05), both
"state queue redeemer lookup" fixes; the second found
`UpdateBondHoldNewState` using the spend purpose.

Recurrence: 2.

## SQ10. Every state-queue element output carries no reference script

Status: PARTIAL (tests and mutation runs read on the #693 worktree, not yet
committed; no provenance commit to cite).

Rule: every block node and the confirmed-state root is created with
`reference_script == None`, and every arm that continues one pins `None`
again, so by induction no element ever carries a script. It matters because
`validate_completed_fraud_record` requires `None` on the fraud node, before
and after: a node carrying a script made fraud unprovable, and the fraudulent
block would merge.

Enforced: one predicate, `element_output_has_no_reference_script`
(`lib/midgard/state-queue.ak:88`). Creation: the `InitV1` genesis root
(`validators/state-queue.ak:1724`) and the commit arm's new node (`:1099`).
Continuation: the commit arm's anchor (`:1098`), the merge root (`:428`),
the fraud link and last removals (`:671`, `:718`), the unattested prune and
last removals (`:832`, `:885`), the unavailable prune and head removals
(`:970`, `:1018`), and the DA core used by Apply, Open and Close
(`da_availability_status_transition`, `lib/midgard/state-queue.ak:522`).
`validate_completed_fraud_record` pins `None` itself (`:583-584`). Refused,
each beside its honest control, all fail:

- commit `[aiken-test: state-queue-commit.test/]`:
  `state_queue_commit_rejects_new_node_reference_script_on_root_anchor` `:860`,
  `..._on_node_anchor` `:872`,
  `state_queue_commit_rejects_continued_root_anchor_reference_script` `:884`,
  `..._continued_tail_anchor_reference_script` `:897`,
  `state_queue_init_rejects_root_reference_script` `:1273`;
- merge `[aiken-test: state-queue-merge.test/]`:
  `merge_rejects_continued_root_reference_script` `:618`;
- removals `[aiken-test: state-queue-removal.test/]`: the six
  `*_rejects_continued_anchor_reference_script` tests (fraud link `:1726`,
  fraud last `:1764`, unattested prune `:1808`, unattested last `:1844`,
  unavailable prune `:1888`, unavailable head `:1924`);
- the DA core `[aiken-test: midgard/state-queue.test/]`:
  `da_core_apply_rejects_node_output_reference_script` `:667`,
  `da_core_open_...` `:675`, `da_core_close_...` `:683`, and through each
  user (Apply, Open, Close) as listed under DA6 in
  [invariants-da.md](invariants-da.md).

Provenance: #693, orchestrator ruling P2. The hole was pre-existing since
`090436fc3`, which added the `None` requirement to the fraud record while the
commit arm and the DA core left the script free. Review action: a new arm
that creates or continues an element must call the predicate `[review]`.

## SQ11. Every queued node holds the lovelace floor, and a committed header has a bounded width

Status: PARTIAL (tests and mutation runs read on the #693 worktree, not yet
committed; no provenance commit to cite).

Rule: every state-queue node, from creation until it leaves the queue, holds
at least `state_queue_node_min_lovelace_v1` (5 ADA), and that floor covers the
ledger minimum of the largest admissible node (`Challenged` status, a
proven-fraud mark, a link, a header at every width bound). It is inductive: a
node is created at or above the floor, a continued node never loses lovelace,
and the DA status transitions keep the node's value exactly. It matters
because Open grows a node's datum (`Attested` to `Challenged`) and must keep
its value exactly: a node below the minimum of its `Challenged` shape cannot
be opened, and an unavailable attested block would merge unslashed. The floor
only has a largest shape to cover if the committed header's width is bounded,
so the commit arm bounds every header field it does not pin by carry-over.
The confirmed-state root carries no DA status and is outside the floor, but a
commit on the root links it, which grows its datum, so the commit arm lets a
root anchor gain lovelace: merge and the unavailable-head removal leave the
root's lovelace free, and an exact pin there would let whoever empties the
queue leave the root too poor to relink and halt every later commit.

Enforced: the constant (`lib/midgard/state-queue.ak:110`). Creation: the
commit arm's new node `output_header_lovelace >=
state_queue_node_min_lovelace_v1` (`validators/state-queue.ak:1128`).
Continuation, per arm that continues an element:

- commit anchor (`:1129-1133`): a node anchor `== 0`, a root anchor `>= 0`;
- fraud link and last removals: `>= 0` (`:692`, `:739`), because their
  builders fold the removed node's lovelace into the continued anchor;
- unattested prune and last removal: `== 0` (`:853`, `:904`);
- unavailable prune: `== 0` (`:991`);
- merge root and unavailable-head removal: free (`_confirmed_state_lovelace_change`
  `:433`, `_root_lovelace_change` `:1022`); both continue the root only;
- the DA core used by Apply, Open and Close: value equality
  (`lib/midgard/state-queue.ak:521`, DA6 in [invariants-da.md](invariants-da.md)).

Width: `commit_block_header_width_is_bounded_v1` (`validators/state-queue.ak:77`,
called at `:1163`) requires a 32-byte `utxos_root` and `block_slot`,
`min_fee_a`, `min_fee_b` below `commit_header_int_exclusive_bound_v1` (2^64,
`:63`). Tests `[aiken-test: state-queue-commit.test/]`: the constant pinned at
5 ADA `:969` and the bound at 2^64 `:974`; fixtures at exactly the floor
`:980`; above the floor accepted `:999`; floor − 1 refused on a root anchor
`:1012` and a node anchor `:1025`; a root anchor losing one lovelace `:1038`,
a node anchor losing one `:1052` and a tail anchor gaining one `:1066`, all
refused; a root anchor topped up after a drain accepted `:1083`; every bounded
Int at 2^64 − 1 accepted `:1109`; a 33- and a 31-byte `utxos_root` `:1127`,
`:1140` and `block_slot`, `min_fee_a`, `min_fee_b` at the bound `:1153`,
`:1169`, `:1185`, all refused. Removals `[aiken-test: state-queue-removal.test/]`:
fraud link and last gains accepted and losses refused (`:1955-1974`),
unattested and unavailable prune loss and gain refused (`:1981-2002`). The
SDK codec test `demo/midgard-sdk/tests/state-queue-node-min-lovelace.test.ts`
measures the largest admissible node against the floor with CML min-UTxO and
pins the SDK constant at `5_000_000n`.

Provenance: #693, orchestrator ruling P3 (T688R4P0N1); the root-anchor
allowance is the #693 review fix for a permanent commit halt (T693R1consensus1).
The gap was pre-existing (base `59b633d1d`): the commit arm and the fraud
removal discarded both lovelace values. Review action: a new arm that creates
a node must require the floor; a new arm that continues a node must pin its
lovelace change to `== 0` or `>= 0`; a new arm that continues the root must
not forbid the gain a relink needs; a new header field needs a width bound in
the commit arm `[review]`.

## Miss patterns in this subtree

- A check present in one arm and missing from its sibling (SQ5, SQ9).
- A consensus check deleted inside an unrelated PR (SQ1 via `c07ac1326`).
- "Trust the other validator's redeemer" with no equality on the other side
  (`668673e9f`; SQ5's two-sided equality that binds only a redeemer field).
- Tests that collect nothing or leave a mutant alive (SQ1).
- An on-chain invariant tested only off-chain (SQ4).
