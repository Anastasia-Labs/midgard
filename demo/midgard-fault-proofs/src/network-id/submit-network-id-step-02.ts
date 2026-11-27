/** Open the committed output field (when needed) and mint the proof token. */

import "@al-ft/midgard-sdk/core";
import "@al-ft/midgard-sdk/state-queue";
import "@al-ft/midgard-sdk/fraud-proof";
import "@lucid-evolution/lucid";
import "../field-opening.js";
import "../proof-chunk-carriage.js";
import "../runtime.js";
import "../spend-input-witness.js";
import "../step-support.js";
import "../tx-layout.js";
import "../witness-reference-scripts.js";
import "../workflow/transaction-boundary.js";
import "./submit-common.js";
import "./wrongful-rejection.js";
import "./submit-network-id-step-02.same-fault.js";
import "./submit-network-id-step-02.submit-network-id-step02.js";
export { type SubmitNetworkIdStep02Result } from "./submit-network-id-step-02.same-fault.js";
export { buildNetworkIdStep02 } from "./submit-network-id-step-02.submit-network-id-step02.js";
