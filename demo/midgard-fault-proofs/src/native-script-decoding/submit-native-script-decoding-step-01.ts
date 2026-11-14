/**
 * `native-script-decoding` step-01 submitters (offchain plan §4.2).
 *
 * Two arms, two entry points:
 *
 * - `submitNativeScriptDecodingStep01BindNormal` — direction A over a Normal
 *   source. The accepted transaction is bound through the header's counted
 *   `transactions_root` via the shared inclusion machinery, on either
 *   carriage (redeemer-carried proof, or the #545 published chunks for a
 *   proof the 16,384-byte envelope cannot hold). The §2.4.3(d) predicate is
 *   re-checked locally before anything is paid for: a leaf whose embedded
 *   validity scalar does not claim acceptance can never bind on-chain.
 * - `submitNativeScriptDecodingStep01RecordForced` — either direction over a
 *   Forced source. Only the direction is recorded; the forced leaf itself is
 *   opened at step-02 under the thread NFT's header, so this transaction
 *   needs no reference inputs at all.
 */

import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "../proof-chunk-carriage.js";
import "../runtime.js";
import "../step-support.js";
import "../tx-layout.js";
import "../witness-reference-scripts.js";
import "../workflow/transaction-boundary.js";
import "./submit-common.js";
import "./submit-native-script-decoding-step-01.source-step-script.js";
import "./submit-native-script-decoding-step-01.submit-native-script-decoding-step01-bind-normal.js";
import "./submit-native-script-decoding-step-01.submit-native-script-decoding-step01-record-forced.js";
export { type SubmitNativeScriptDecodingStep01Result } from "./submit-native-script-decoding-step-01.source-step-script.js";
export { submitNativeScriptDecodingStep01BindNormal } from "./submit-native-script-decoding-step-01.submit-native-script-decoding-step01-bind-normal.js";
export { submitNativeScriptDecodingStep01RecordForced } from "./submit-native-script-decoding-step-01.submit-native-script-decoding-step01-record-forced.js";
