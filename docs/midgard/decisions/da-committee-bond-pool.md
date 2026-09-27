# One pooled bond backs the whole DA committee

Status: Accepted; implementation pending in
[#686–#693](https://github.com/Anastasia-Labs/midgard/issues/685).

Recorded: 2026-09-27. Decided: 2026-09-27.

## Context

Today every attestation locks its own DA bond (10,000 ADA). When the attestation
applies, that bond becomes a per-block bond UTxO, funded from one shared bond-owner
credential. An honest block's bond has no release path, so it stays locked
forever. That is 10,000 ADA per block with nothing in return. The
[deployment economics](0002-canonical-v1-goal-economics-and-margins.md) also force
the DA bond to equal the challenger bond, and the challenger bond must cover the
reserve-coverage floor (about 9,635 ADA at current fee ceilings). So even a
testing profile cannot use a small DA bond.

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

- **Keep the per-block bond and add a time-based reclaim.** This was the
  ReclaimBond plan, accepted on 2026-09-22 and never implemented. It releases an
  honest block's bond after a DA window. But it keeps one bond per block in
  flight, and it keeps funding with the shared credential.
- **One bond per committee member,** posted at registration like the
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
- **Liability is capped at one pool.** A committee that withholds N blocks pays
  what the pool holds. Later challengers still get the block removed, but may
  receive a smaller reward or none.
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
