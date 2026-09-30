/**
 * State-queue merge transaction for advancing committed blocks into confirmed
 * state.
 * This module owns the off-chain merge flow that replays the oldest queued
 * block into the confirmed ledger and then submits the corresponding merge tx.
 *
 * It performs the following tasks:
 *
 * 1. Fetches the confirmed state and the block it points to (i.e. the oldest
 *    block in the queue).
 * 2. Fetches the transactions of that block by querying BlocksDB and its
 *    associated inputs table..
 * 3. Apply those transactions to ConfirmedLedgerDB and update the table to
 *    store the updated UTxO set.
 * 4. Remove all header hashes from BlocksDB associated with the merged block.
 * 5. Build and submit the merge transaction.
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
import "../../local-ledger-slot.js";
import "../../operator-wallet-view.js";
import "../../services/event-history-producer.js";
import "../../services/index.js";
import "../../services/mpf-native-owner/index.js";
import "../../utils.js";
import "../reference-scripts.js";
import "../submit-timing-due-work.js";
import "../utils.js";
import "./confirmed-ledger-snapshot.js";
import "./merge-readiness.js";
import "./merge-to-confirmed-state.finalize-confirmed-merge-program.js";
import "./merge-to-confirmed-state.landed-unfinalized-merges.js";
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
  type ConfirmedMergeNativeOwnerObservation,
  finalizeConfirmedMergeProgram,
  finalizeConfirmedMergeTransaction,
  observeNativeOwnerAfterConfirmedMerge,
} from "./merge-to-confirmed-state.finalize-confirmed-merge-program.js";
export {
  type ConfirmedMergeFinalization,
  diagnoseMissingBlockTxs,
  finalizeLandedMergesProgram,
  finalizeMergesLandedThrough,
  MERGE_CONFIRMATION_PROVIDER_RETRIES,
} from "./merge-to-confirmed-state.landed-unfinalized-merges.js";
