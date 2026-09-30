/**
 * `da-hash-preimage` step-01 submitter (Goal task `Q44`, §9.1 output 8).
 *
 * Unlike the retired RF-043 step routes, nothing in the prepared JSON is
 * trusted. Before a transaction is built this module re-derives, from the
 * **on-chain** state-queue block header:
 *
 * - the counted `transactions_root` over the supplied raw PHAS root and the
 *   header's own `l2TransactionCount`, which must equal the committed
 *   `transactionsRoot`; and
 * - the violation itself, by re-running the Q44 rule over the committed leaf
 *   bytes.
 *
 * A prepared file that claims a violation the chain does not support is
 * therefore rejected locally, before any submission.
 */

import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "effect";
import "./json-file.js";
import "./runtime.js";
import "./step-support.js";
import "./tx-layout.js";
import "./witness-reference-scripts.js";
import "./workflow/transaction-boundary.js";
import "./submit-da-hash-preimage-step-01.parse-submit-da-hash-preimage-tx-inclusion.js";
import "./submit-da-hash-preimage-step-01.submit-da-hash-preimage-step01.js";
import "./submit-da-hash-preimage-step-01.submit-da-hash-preimage-step01-from-files.js";
export {
  parseSubmitDaHashPreimageTxInclusion,
  type SubmitDaHashPreimageStep01CliConfig,
  type SubmitDaHashPreimageStep01Result,
  type SubmitDaHashPreimageTxInclusion,
} from "./submit-da-hash-preimage-step-01.parse-submit-da-hash-preimage-tx-inclusion.js";
export { submitDaHashPreimageStep01 } from "./submit-da-hash-preimage-step-01.submit-da-hash-preimage-step01.js";
export { submitDaHashPreimageStep01FromFiles } from "./submit-da-hash-preimage-step-01.submit-da-hash-preimage-step01-from-files.js";
