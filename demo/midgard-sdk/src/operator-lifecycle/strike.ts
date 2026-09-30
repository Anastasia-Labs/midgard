/**
 * Stalled-operator strike and takeover.
 *
 * L1 never acts on its own: when the scheduled operator misses its shift,
 * somebody else has to submit a transaction that spends the scheduler with a
 * skipped-operator redeemer *and* spends the inactive operator's active node
 * with `StrikeForInactivity`. That single transaction reproduces the node with
 * one more inactivity strike and hands the shift to the next operator.
 *
 * This module carries the two halves of that endpoint:
 *
 * - `computeInactivityThreshold` / `planInactivityTakeover`: a pure mirror of
 *   the on-chain timing rules (`validators/scheduler.ak`,
 *   `validate_operator_inactivity_and_get_its_link`) so a watchdog can decide
 *   *whether* a strike is possible, and report why not when it is not.
 * - `buildStrikeInactiveOperatorTxProgram`: the transaction builder, which
 *   resolves every redeemer index from the final transaction context rather
 *   than guessing at it.
 */

import "@lucid-evolution/lucid";
import "effect";
import "../active-operators.js";
import "../linked-list.js";
import "../protocol-parameters.js";
import "../scheduler.js";
import "../scheduler-refresh.js";
import "../tx-completion.js";
import "../tx-context-redeemer.js";
import "./directory.js";
import "./output-selectors.js";
import "./strike.compute-inactivity-threshold.js";
import "./strike.plan-inactivity-takeover.js";
import "./strike.derive-strike-layout.js";
import "./strike.build-strike-inactive-operator-tx-program.js";
export { buildStrikeInactiveOperatorTxProgram } from "./strike.build-strike-inactive-operator-tx-program.js";
export {
  computeInactivityThreshold,
  type ComputeInactivityThresholdInput,
  DEFAULT_INACTIVITY_TIMING_PARAMETERS,
  DEFAULT_STRIKE_VALIDITY_WINDOW_MS,
  type InactivityDirectoryView,
  type InactivityTakeoverBlockedReason,
  type InactivityTakeoverPlan,
  type InactivityTakeoverTier,
  type InactivityTakeoverValidity,
  type InactivityTakeoverWitnesses,
  type InactivityThreshold,
  type InactivityThresholdSource,
  type InactivityThresholdUnsatisfiableReason,
  type InactivityTimingParameters,
  type NeglectedUserEventClaim,
  type NeglectedUserEventKind,
  type PlanInactivityTakeoverInput,
} from "./strike.compute-inactivity-threshold.js";
export {
  type BuildStrikeInactiveOperatorTxConfig,
  planInactivityTakeover,
  type StrikeInactiveOperatorAdversarialOverrides,
  type StrikeInactiveOperatorLayout,
  type StrikeInactiveOperatorTxResult,
  type StrikeNeglectedUserEvent,
} from "./strike.plan-inactivity-takeover.js";
