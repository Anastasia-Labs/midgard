/**
 * Process-level evidence for the pooled DA bond journey (ticket #692,
 * program rulings P16 and P18): what a real `da-committee-node` process and
 * real `midgard-node da-bond` processes must show, and the pure checks the
 * journey driver applies to it.
 *
 * - P16: the committee node's pool readiness reasons come from its `GET
 *   /readyz` body and its pool transitions from the JSON event lines it
 *   writes to stderr (`createDaBondPoolWiring` in
 *   `da-committee-node/src/coordinator/pool-monitor.ts`).
 * - P18: every top-up and withdraw step is submitted by the real CLI
 *   (`da-bond top-up`; `da-bond withdraw <step> --build-unsigned`, one
 *   `da-bond witness` per signer, `da-bond assemble`), bracketed by real
 *   `da-bond status` processes. The status printed by the submitting process
 *   must already be the post-transaction pool; one that still shows the old
 *   pool means the installed submit did not wait for confirmation (P15).
 *
 * Nothing here spawns a process: an adapter records what it ran, and these
 * functions judge the record, so both polarities are testable without a chain.
 */

/** One finished process, as the adapter ran it. */

import "./da-bond-pool-process-evidence.command-mismatches.js";
import "./da-bond-pool-process-evidence.check-da-bond-cli-submit-evidence.js";
import "./da-bond-pool-process-evidence.create-da-bond-pool-stderr-cursor.js";
export {
  checkDaBondCliSubmitEvidence,
  type DaBondPoolReadyz,
  type DaBondPoolStderrEvent,
  isDaBondPoolEvent,
  parseDaBondPoolReadyz,
} from "./da-bond-pool-process-evidence.check-da-bond-cli-submit-evidence.js";
export {
  type DaBondCliChainRead,
  type DaBondCliExpectation,
  type DaBondCliStatus,
  type DaBondCliSubmitEvidence,
  type DaBondPoolProcessRun,
  type DaBondProcessEvidenceCheck,
  parseDaBondCliStatus,
} from "./da-bond-pool-process-evidence.command-mismatches.js";
export { createDaBondPoolStderrCursor } from "./da-bond-pool-process-evidence.create-da-bond-pool-stderr-cursor.js";
