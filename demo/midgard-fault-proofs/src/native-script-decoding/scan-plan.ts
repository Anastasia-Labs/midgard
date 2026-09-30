/**
 * `native-script-decoding` segment planner (offchain plan §5.2/§5.3).
 *
 * Cuts the engine twin's whole-item trace into an ordered list of Scan
 * transaction plans plus one final Verdict plan, such that every plan's
 * on-chain fold provably lands exactly where the plan says it does:
 *
 * - a segment carries at most `maxStepsPerTx` primitive steps, and its
 *   `stepBudget` equals its exact step count so `budgeted_scan_v1` stops at
 *   the planned cut by budget exhaustion, never by a window/frame stall;
 * - a segment never spans a window change: every token step in a segment
 *   reads from the same authenticated chunk, and the mandatory
 *   chunk-plus-next window shape then makes the §33-byte safe-read margin
 *   unconditional (a full following chunk is ≥ 4,095 bytes of margin, and a
 *   window ending at the item's last byte satisfies the end-of-item arm);
 * - direction A (wrongful acceptance) folds every advanced step of the trace
 *   and leaves the refusing primitive step to a budget-1 Verdict fold, which
 *   carries the window only when the refusing control is token-stage — the
 *   frozen twin's frame steps can only advance or abort (witness error), so
 *   a refusal is always exhibited by a token or finalize step and the
 *   Verdict plan never needs a frame witness;
 * - direction B (wrongful rejection) folds through finalize to the exact
 *   terminal and the Verdict plan is windowless.
 *
 * Bind-level short circuits skip the machine entirely: an undecodable
 * wrapper closes for direction A (`bindMalformed`), a non-zero language tag
 * closes for direction B (`descriptorContradiction`). Either short circuit
 * requested with the opposite direction — like a machine trace whose outcome
 * contradicts the requested direction — throws: the fault does not exist in
 * the claimed polarity, and the planner refuses rather than letting a
 * submitter discover that on-chain.
 *
 * ExUnits discipline (§5.3): every plan carries a prediction derived from
 * the pinned exec ledger
 * (`onchain/aiken/scripts/native-script-decoding-engine-exec-ledger-v1.json`),
 * and the planner throws on any plan predicted over the 13.2M-mem/8B-cpu
 * GOAL_SPEC §3.3 basis. Scan predictions price every primitive step at the
 * ledger's deep per-NODE fold slope, which over-prices roughly 2–3× (a deep
 * node spends 2–3 primitive steps to earn one node's slope): the prediction
 * is a conservative ceiling, so "refuse to submit" can only fire early,
 * never lie low. Divergence between prediction and an emulator reading
 * beyond the fixture share is a finding, never a reason to raise a budget.
 */

import "@al-ft/midgard-core";
import "./contracts.js";
import "./scan-plan.native-script-decoding-exec-pins.js";
import "./scan-plan.build-native-script-decoding-scan-plan.js";
export { buildNativeScriptDecodingScanPlan } from "./scan-plan.build-native-script-decoding-scan-plan.js";
export {
  NATIVE_SCRIPT_DECODING_DEFAULT_MAX_STEPS_PER_TX,
  NATIVE_SCRIPT_DECODING_EXEC_PINS,
  type NativeScriptDecodingPlanControl,
  type NativeScriptDecodingPlanRoute,
  NativeScriptDecodingPlanRoutes,
  type NativeScriptDecodingPlanWindow,
  type NativeScriptDecodingScanPlan,
  type NativeScriptDecodingScanSegmentPlan,
  type NativeScriptDecodingVerdictPlan,
} from "./scan-plan.native-script-decoding-exec-pins.js";
