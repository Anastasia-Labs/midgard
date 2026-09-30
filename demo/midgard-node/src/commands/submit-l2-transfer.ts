/**
 * Native user-transfer command support for Midgard L2.
 * This module derives a wallet from a seed phrase, queries the live Midgard
 * ledger view for spendable UTxOs, constructs a balanced Midgard-native
 * transaction with explicit change, and submits it through the node's public
 * `/submit` endpoint.
 */

import "@al-ft/midgard-core/assets";
import "@al-ft/midgard-core/cek-proof";
import "@al-ft/midgard-core/codec";
import "@lucid-evolution/lucid";
import "effect";
import "../asset-specs.js";
import "../services/index.js";
import "../sleep.js";
import "../tx-context.js";
import "./command-utils.js";
import "./transfer-build-core.js";
import "./submit-l2-transfer.compare-assets-by-coverage.js";
import "./submit-l2-transfer.submit-native-transfer-tx.js";
import "./submit-l2-transfer.prepare-l2-terminal-drain-program.js";
export {
  FANOUT_NATIVE_TRANSFER_SUBMIT_RETRY_POLICY,
  fetchNodeUtxos,
  type NativeTransferSubmitRetryPolicy,
  parseSubmitL2TransferConfig,
  type PreparedL2TerminalDrain,
  type PreparedL2Transfer,
  selectTransferInputs,
  type SubmitL2TransferConfig,
  type SubmitL2TransferResult,
} from "./submit-l2-transfer.compare-assets-by-coverage.js";
export {
  prepareL2TerminalDrainProgram,
  submitL2TransferProgram,
} from "./submit-l2-transfer.prepare-l2-terminal-drain-program.js";
export {
  prepareL2TransferProgram,
  submitNativeTransferTx,
} from "./submit-l2-transfer.submit-native-transfer-tx.js";
export {
  buildTerminalDrainTx,
  buildTransferTx,
  buildTransferTxWithMinFee,
  type BuiltTransferTx,
  makeStaticMidgardProvider,
  makeTransferMidgard,
  type PrivateKeyInput,
  type TransferNetworkName,
} from "./transfer-build-core.js";
