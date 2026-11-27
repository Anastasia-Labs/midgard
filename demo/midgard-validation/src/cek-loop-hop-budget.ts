/**
 * The budget every hop of an arm 31 loop (the staged product loop, the
 * serialise loop) is planned against: the ex-units a hop's semantics
 * validator has after the fixed overhead of the step transaction, and the
 * share of it a planner keeps free on top of its calibrated cost model.
 */

/** Execution units. */
export type MidgardCekExUnits = {
  readonly memory: bigint;
  readonly cpu: bigint;
};

/**
 * The ex-units a hop's semantics validator has after the fixed overhead of
 * the step transaction.
 */
export const MIDGARD_CEK_LOOP_HOP_BUDGET: MidgardCekExUnits = {
  memory: 11_100_000n,
  cpu: 7_200_000_000n,
};

/** The share of the budget a planner keeps free, in percent. */
export const MIDGARD_CEK_LOOP_HOP_MARGIN_PERCENT = 20n;

/** Whether `cost` fits `budget` with the margin kept free. */
export const midgardCekLoopHopFits = (
  cost: MidgardCekExUnits,
  budget: MidgardCekExUnits = MIDGARD_CEK_LOOP_HOP_BUDGET,
): boolean => {
  const kept = 100n - MIDGARD_CEK_LOOP_HOP_MARGIN_PERCENT;
  return (
    cost.memory * 100n <= budget.memory * kept &&
    cost.cpu * 100n <= budget.cpu * kept
  );
};
