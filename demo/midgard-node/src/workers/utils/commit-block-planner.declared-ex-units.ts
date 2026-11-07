import { decodeMidgardSubmittedTxFromCanonicalCbor } from "@al-ft/midgard-validation";

import type { ExUnitsBudget } from "./commit-block-planner.commit-scheduler-evidence-key.js";

const NO_EX_UNITS: ExUnitsBudget = { memory: 0n, steps: 0n };

/**
 * The sum of a normal transaction's declared redeemer ex-units. Bytes that do
 * not decode count as none: Phase A refuses them when the block is built.
 */
export const declaredTxExUnits = (txCbor: Uint8Array): ExUnitsBudget => {
  let redeemers;
  try {
    redeemers =
      decodeMidgardSubmittedTxFromCanonicalCbor(txCbor).ledgerTx.redeemers;
  } catch {
    return NO_EX_UNITS;
  }
  return redeemers.reduce(
    (total, { exUnits }) => ({
      memory: total.memory + exUnits.memory,
      steps: total.steps + exUnits.steps,
    }),
    NO_EX_UNITS,
  );
};

/** Running sum of the declared ex-units of the normal transactions selected
 * for one block, held within the block budget in each component. */
export class DeclaredExUnitsTally {
  private selected: ExUnitsBudget = NO_EX_UNITS;

  constructor(private readonly budget: ExUnitsBudget) {}

  /** Adds the transaction when the sum stays within budget; else false. */
  admit(txCbor: Uint8Array): boolean {
    const tx = declaredTxExUnits(txCbor);
    const next = {
      memory: this.selected.memory + tx.memory,
      steps: this.selected.steps + tx.steps,
    };
    if (next.memory > this.budget.memory || next.steps > this.budget.steps) {
      return false;
    }
    this.selected = next;
    return true;
  }
}
