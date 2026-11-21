/**
 * State-queue merge transaction for advancing committed blocks into confirmed
 * state.
 * This module owns the off-chain merge flow that builds and submits the merge
 * transaction for the oldest queued block.
 *
 * It performs the following tasks:
 *
 * 1. Fetches the confirmed state and the block it points to (i.e. the oldest
 *    block in the queue).
 * 2. Fetches the transactions of that block by querying BlocksDB and its
 *    associated inputs table.
 * 3. Builds, signs and submits the merge transaction, and waits for its L1
 *    confirmation.
 *
 * The merge changes no local ledger: landed-block processing folds the block
 * into `confirmed_ledger` once the follower's landed queue shows the merge
 * (`landed-blocks/fold.ts`).
 */

import "@al-ft/midgard-core/error-format";
import "@al-ft/midgard-sdk";
import "@effect/sql";
import "@lucid-evolution/lucid";
import "effect";
import "../../commands/command-utils.js";
import "../../database/index.js";
import "../../database/utils/common.js";
import "../../fibers/queue-metrics.js";
import "../../fibers/slot-aware-due-work.js";
import "@al-ft/midgard-core/ogmios-slot";
import "../../services/follower-write-gate.js";
import "../../services/index.js";
import "../../services/mpf-native-owner/index.js";
import "../../utils.js";
import "../reference-scripts.js";
import "../submit-timing-due-work.js";
import "../utils.js";
import "../utils.wallet-view.js";
import "./confirmed-ledger-snapshot.js";
import "./merge-readiness.js";
import "./merge-to-confirmed-state.merge-options.js";
import "./merge-to-confirmed-state.fetch-canonical-merge-candidate-readiness.js";
import "./merge-to-confirmed-state.capture-merge-local-ledger-gate.js";
import "./merge-to-confirmed-state.build-and-submit-merge-tx.js";
export { buildAndSubmitMergeTx } from "./merge-to-confirmed-state.build-and-submit-merge-tx.js";
export {
  captureMergeLocalLedgerGate,
  mergeNoInlineSubmitDueWorkFromDefer,
} from "./merge-to-confirmed-state.capture-merge-local-ledger-gate.js";
export {
  type CanonicalMergeCandidateReadiness,
  fetchCanonicalMergeCandidateReadiness,
  mergeSemanticSkipResult,
  type MergeTxResult,
  preflightDecodeBlockTxs,
} from "./merge-to-confirmed-state.fetch-canonical-merge-candidate-readiness.js";
export {
  diagnoseMissingBlockTxs,
  MERGE_CONFIRMATION_PROVIDER_RETRIES,
  type SubmittedMergeTx,
} from "./merge-to-confirmed-state.merge-options.js";
