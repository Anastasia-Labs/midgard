/**
 * A replaced block of this node holds the slot of a base that a sibling built
 * on the same base already finalized locally, or already landed. Neither side
 * can be reconciled from here without writing over the other, so the node
 * cannot decide yet: it leaves the signed intent in place, raises this
 * readiness reason, holds block commitment, and decides again on every later
 * evaluation, so a rollback or a landing that settles it lets the node
 * proceed.
 */
export const SIGNED_INTENT_UNDECIDED = "signed_intent_undecided";

/** The L1 blocks an undecided signed intent may stay raised before it logs
 * once at error. Escalation never stops the node. */
export const SIGNED_INTENT_UNDECIDED_ESCALATION_L1_BLOCKS = 30;

/** The nominal L1 block interval, which turns the block bound into the wall
 * time readiness compares against. */
export const NOMINAL_L1_BLOCK_INTERVAL_MS = 20_000;

export const SIGNED_INTENT_UNDECIDED_ESCALATION_MS =
  SIGNED_INTENT_UNDECIDED_ESCALATION_L1_BLOCKS * NOMINAL_L1_BLOCK_INTERVAL_MS;
