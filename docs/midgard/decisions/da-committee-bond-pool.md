# One pooled bond backs the whole DA committee

Status: Implemented in #686–#691, with the timeout slash in #693 (spec
[#685](https://github.com/Anastasia-Labs/midgard/issues/685)). The implementation
amended the spec in places; see
[Amendments during implementation](#amendments-during-implementation).

Recorded: 2026-09-27. Decided: 2026-09-27.

## Context

Before this decision, every attestation locked its own DA bond (10,000 ADA). When
the attestation applied, that bond became a per-block bond UTxO, funded from one
shared bond-owner credential. An honest block's bond had no release path, so it
stayed locked forever. That is 10,000 ADA per block with nothing in return. The
[deployment economics](0002-canonical-v1-goal-economics-and-margins.md) also force
the DA bond to equal the challenger bond, and the challenger bond must cover the
reserve-coverage floor (about 9,635 ADA at current fee ceilings). So even a
testing profile could not use a small DA bond.

## Decision

The DA committee as a whole is backed by one DA bond pool, instead of one bond
per block or one bond per member.

- **Top-ups.** Anyone can top up the pool, above a minimum increment. Contributors
  have no share and no claim.
- **Applying an attestation.** An attestation applies only while the pool is not
  being withdrawn and holds at least one DA bond of backing. Starting and signing
  an attestation need no bond.
- **Attested commitment.** Applying an attestation binds the block to the hash of
  the commitment the committee signed. An availability challenge opens against
  that hash, before a fixed window after the block's end time closes.
- **Lost challenges.** When an availability challenge times out, the same
  transaction takes up to one DA bond from the pool. The penalty share is paid as
  the transaction fee and the rest goes to the challenger, following the operator
  slash rule. If the pool is short, the penalty share is filled first. The timeout
  must spend the pool, even when it is empty.
- **Withdrawal.** Only the DA governance owner quorum can withdraw, in two steps.
  The delay covers the latest possible slash for any block the pool still backs.
- **Challenger bond.** It is no longer tied to the DA bond. It alone carries the
  reserve-coverage floor.

The normative design is [#685](https://github.com/Anastasia-Labs/midgard/issues/685).

## Considered options

- **Keep the per-block bond and add a time-based reclaim.** Superseded by this
  decision. This was the ReclaimBond plan (`da-bond-reclaim-plan.md`), accepted on
  2026-09-22 and never implemented. It releases an honest block's bond after a DA
  window. But it keeps one bond per block in flight, and it keeps funding with the
  shared credential. It had itself superseded an earlier plan to release the bond
  when the block binds terminally (`da-bond-release-plan.md`). Both plans are
  historical working notes and were never in this repository.
- **One bond per committee member.** Superseded by this decision, never
  implemented. The bond would be posted at registration like the
  [operator bond](../../../CONTEXT.md). Governance would seat only bonded keys,
  and a lost challenge would slash every signer. Rejected: slashing had to remove
  members without stranding attestations already in progress. That forced a
  member registry, a bond check when seating, and a mark-then-compact removal of
  slashed keys that paused new attestations, all for per-member accountability.
  Governance already provides that accountability, because it seats the committee.

## Consequences

- **No per-member accountability.** Deterrence falls on whoever funds the pool,
  and on governance's power to rotate the committee. A member who contributed
  nothing loses nothing.
- **Liability is one bond per withholding episode.** A timeout removes the
  withheld block while it is the queue head, and prunes every block queued after
  it, without a further slash. So a committee that withholds N queued blocks pays
  one DA bond per timeout, and the pool is never slashed twice for blocks applied
  before the first slash. Apply needs a full bond of backing, so an earlier slash
  never leaves a later withheld block facing a partly funded pool. Only a late
  timeout, landing after the owners completed a withdrawal, can meet a pool below
  one bond. The withdrawal delay ensures that a timely challenger always meets a
  full bond.
- **A slash can pause the committee.** A slash that leaves the pool short blocks
  applying attestations until someone tops it up. Attestations that cannot apply
  in time lapse, and their blocks time out unattested.
- **The pool is shared state.** Every attestation that applies reads the pool, so
  top-ups, slashes and withdrawal steps force pending applies to be rebuilt. The
  minimum top-up puts a price on that churn.
- **The pool outlives committees.** It belongs to the committee role and carries
  across rotations.
- **Testing profiles can use a small DA bond,** now that the reserve-coverage
  floor binds only the challenger bond.
- **Deployment identity changes.** Under
  [prelaunch format replacement](prelaunch-format-replacement.md), the status and
  commitment formats are replaced in place. This changes validator hashes and
  deployment identity, and needs a fresh deployment.

## Amendments during implementation

The tickets amended spec #685 as follows. Each amendment only tightens a check
or fixes a gap the spec left open.

- **A6, withdrawal delay (#686).** The delay also covers the wait until the
  withheld block is the queue head. It is at least the maximum validity range,
  plus the larger of the block maturity and the challenge window with the full
  response window, plus the slash grace. That is 778,080,000 ms on public profiles
  and 2,340,000 ms on testing profiles.
- **B4, Slash binding (#687).** The pool's Slash arm also requires the correction
  lock to be `Idle`. Without it, the resume steps of a multi-step removal, which
  reuse the same state-queue redeemer, could drain the pool.
- **C3, Apply refund (#688).** Apply names a refund output that returns the
  attestation's exact lovelace to its rescue beneficiary, so whoever submits Apply
  cannot take it.
- **D3, one challenger output (#693).** The challenger's reward and record refund
  are one output. A separate reward below the minimum UTxO would otherwise make
  every timeout unbuildable.
- **G2, Slash while withdrawing (#687).** A `Withdrawing` pool can be slashed and
  keeps its datum and `unlock_at`. Otherwise a committee could escape a slash by
  starting a withdrawal.
- **G5 and P1, Open after a late Apply (#686).** Public profiles must leave
  the challenge window minus the attestation timeout at least twice the maximum
  validity range plus the doubled L1 finality budget, which is 2,160,000 ms.
  Testing profiles are exempt.
- **P2, node reference scripts (#693).** Every state-queue node is created, and
  continued, without a reference script. A node carrying one would make fraud
  unprovable.
- **P3, node lovelace floor (#693).** Every state-queue node holds at least
  `state_queue_node_min_lovelace_v1` (5 ADA) while it is in the queue. The commit
  arm bounds the width of every header field, so no node grows past what the floor
  covers. Without the floor, an attested block could be too poor to challenge.
- **P4, root anchor top-up (#693).** A commit may add lovelace to the root anchor
  but not take it. A node anchor keeps its lovelace exactly. An exact root pin
  would halt every commit after the queue empties.
- **P7, liability (#689).** Liability is one bond per withholding episode, as
  described under Consequences. The spec's "liability cap" wording, which says
  later challengers are paid from what is left, does not match the code.
- **P9, one challenge per header (#690).** A watcher's challenge journal holds one
  workflow per actor, deployment and header, so one live challenge no longer
  blocks opening another.
- **P10, lost timeout race (#690).** A watcher whose timeout lost the race to
  another watcher's timeout on the same header releases its pending intent once
  the other spend is final.
