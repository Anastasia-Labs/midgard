/**
 * Execution-unit ceilings for Aiken tests whose cost the protocol relies on.
 *
 * `script-budgets.json` is the one table CI judges and production reads. Every
 * entry is a ceiling: CI fails when a fresh reading is above it, and a reading
 * that shrinks needs no edit. `onchain/aiken/scripts/script-budgets.mjs`
 * checks the table (`check`), lowers ceilings to the current readings plus 1%
 * (`tighten`), and raises one ceiling with a recorded reason (`raise`).
 *
 * An entry names one Aiken test as `module/test`, the way `aiken check`
 * reports it. With `minusControl` the reading is that test minus another test
 * of the same module (a cost net of its fixture, or the step between two
 * sizes). With `env` the tests run under that Aiken environment. An entry
 * whose ceiling is above {@link SCRIPT_BUDGET_BASIS} must say why it is above
 * the basis (`aboveBasis`), and CI then also fails if the reading falls inside
 * the basis: those entries record work that does not fit an L1 transaction.
 *
 * Production code reads a ceiling as a conservative cost. Keep this module
 * free of other imports: the Node-run judge imports it directly.
 */
import table from "./script-budgets.json" with { type: "json" };

/** Memory and CPU execution units. */
export type ScriptExecutionUnits = {
  readonly mem: number;
  readonly cpu: number;
};

/**
 * The execution basis every protocol cost is held to: 20% under the supported
 * L1 per-transaction limits (`MIDGARD_CONSENSUS_LIMITS`), the GOAL_SPEC §3.3
 * reliability reserve.
 */
export const SCRIPT_BUDGET_BASIS: ScriptExecutionUnits = Object.freeze({
  mem: 13_200_000,
  cpu: 8_000_000_000,
});

export type ScriptBudgetName = keyof typeof table.budgets;

/** The ceiling of one budget. A name the table lacks does not typecheck. */
export const scriptBudget = (name: ScriptBudgetName): ScriptExecutionUnits =>
  table.budgets[name].ceiling;
