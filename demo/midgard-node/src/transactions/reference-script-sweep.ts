/**
 * Reclaims the ADA locked in reference-script UTxOs that belong to a retired
 * deployment's reference-script auth policy.
 *
 * The sweep is scoped to exactly one retired policy and fails closed when that
 * policy, or any script it would spend, belongs to the live deployment. It
 * batches inputs under the protocol's per-transaction reference-script and
 * size limits, and re-plans from chain before every batch so an interrupted
 * run can simply be started again.
 */

import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "effect";
import "../tx-context.js";
import "./reference-scripts.js";
import "./utils.js";
import "./wallet-hygiene.js";
import "./reference-script-sweep.reference-script-fee.js";
import "./reference-script-sweep.select-retired-reference-script-utxos.js";
import "./reference-script-sweep.build-reference-script-sweep-plan.js";
import "./reference-script-sweep.sweep-retired-reference-scripts-program.js";
export {
  buildReferenceScriptSweepPlan,
  type ReferenceScriptSweepBatchSummary,
  type ReferenceScriptSweepPlanSummary,
  summarizeReferenceScriptSweepPlan,
} from "./reference-script-sweep.build-reference-script-sweep-plan.js";
export {
  assertRetiredPolicyIsNotLive,
  ledgerMinimumFee,
  type LiveReferenceScriptDeployment,
  liveReferenceScriptDeployment,
  REFERENCE_SCRIPT_SWEEP_BURN_VALIDITY_SLOTS,
  REFERENCE_SCRIPT_SWEEP_MAX_INPUTS_PER_BATCH,
  referenceScriptFee,
  referenceScriptLedgerBytes,
  type ReferenceScriptSweepBatch,
  type ReferenceScriptSweepLimits,
  referenceScriptSweepLimitsFromProtocolParameters,
  type ReferenceScriptSweepPlan,
  ReferenceScriptSweepRefusal,
  type ReferenceScriptSweepRefusalCheck,
  type RetiredAuthPolicyDisposition,
} from "./reference-script-sweep.reference-script-fee.js";
export {
  decideRetiredAuthPolicyDisposition,
  selectRetiredReferenceScriptUtxos,
  valueCborBytes,
} from "./reference-script-sweep.select-retired-reference-script-utxos.js";
export {
  type ReferenceScriptSweepOptions,
  type ReferenceScriptSweepResult,
  type ReferenceScriptSweepSubmittedBatch,
  sweepRetiredReferenceScriptsProgram,
} from "./reference-script-sweep.sweep-retired-reference-scripts-program.js";
