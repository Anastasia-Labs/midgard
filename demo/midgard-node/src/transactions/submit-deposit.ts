/**
 * Deposit submission flow for projecting deposit observations into Midgard
 * state.
 * This module owns node/API concerns and delegates production transaction
 * construction to the SDK user-event builders.
 */

import "@al-ft/midgard-core/hex";
import "@al-ft/midgard-core/plutus-data-cbor";
import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "effect";
import "../asset-specs.js";
import "../database/index.js";
import "../services/index.js";
import "./event-history-submission.js";
import "./submit-deposit.deposit-submission-attempt-from-completed-tx.js";
import "./submit-deposit.reconcile-deposit-submission-attempt-program.js";
import "./submit-deposit.parse-funding-utxos.js";
import "./submit-deposit.parse-build-deposit-request.js";
export {
  type BuildDepositRequest,
  type BuiltUnsignedDepositTx,
  type DepositBuildMetadata,
  DepositConfirmationUnknownError,
  depositSubmissionAttemptFromCompletedTx,
  type DepositSubmissionReconciliationResult,
  matchesDepositSubmissionIntent,
  type SubmitDepositConfig,
  SubmitDepositError,
  type SubmitDepositReferenceScripts,
  type SubmittedDeposit,
} from "./submit-deposit.deposit-submission-attempt-from-completed-tx.js";
export { parseBuildDepositRequest } from "./submit-deposit.parse-build-deposit-request.js";
export { parseSubmitDepositConfig } from "./submit-deposit.parse-funding-utxos.js";
export {
  buildUnsignedDepositTxFromFundingContextProgram,
  buildUnsignedDepositTxProgram,
  depositSubmissionIntentHash,
  reconcileDepositSubmissionAttemptProgram,
  submitDepositWithMetadataProgram,
} from "./submit-deposit.reconcile-deposit-submission-attempt-program.js";
