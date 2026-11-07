import { Config } from "effect";

import { DEFAULT_BLOCK_EX_UNITS_BUDGET } from "../workers/utils/commit-block-planner.commit-scheduler-evidence-key.js";

const exUnitsComponent = (name: string, fallback: bigint) =>
  Config.string(name).pipe(
    Config.withDefault(fallback.toString()),
    Config.mapAttempt((value) => {
      if (!/^[1-9][0-9]*$/.test(value.trim())) {
        throw new Error(`${name} must be a positive integer`);
      }
      return BigInt(value.trim());
    }),
  );

/**
 * The per-block ex-units budget: the commit planner stops adding normal
 * transactions once their declared redeemer ex-units would exceed either
 * component. Defaults to twice the per-transaction cap (33,000,000 memory,
 * 20,000,000,000 steps). Forced transactions are never budget-gated.
 */
export const blockExUnitsBudgetConfig = Config.all({
  memory: exUnitsComponent(
    "BLOCK_EX_UNITS_BUDGET_MEMORY",
    DEFAULT_BLOCK_EX_UNITS_BUDGET.memory,
  ),
  steps: exUnitsComponent(
    "BLOCK_EX_UNITS_BUDGET_STEPS",
    DEFAULT_BLOCK_EX_UNITS_BUDGET.steps,
  ),
});
