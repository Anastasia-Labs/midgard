/**
 * `mint-authorization` step-03 submitters — the direction dispatch.
 *
 * Two arms, two entry points:
 *
 * - `submitMintAuthorizationStep03WitnessAbsence` — direction A's inline
 *   half. Opens the whole committed field 6 through the §8.8 door and lets
 *   the validator's fold prove no inline script witness (of any language)
 *   hashes to the claimed policy id, then routes into step-04's
 *   reference-input scan at cursor 0.
 * - `submitMintAuthorizationStep03EvaluateUnsatisfied` — direction B. Pins
 *   the policy's native payload by hash, opens the committed field 7 for
 *   the signer set, and lets the machine-twin evaluator refute the script
 *   against the committed signers and validity interval. Closes straight to
 *   step-05.
 *
 * Every check the validator makes that this process can make locally is
 * made locally first: the wrong-direction thread, an inline script that
 * DOES hash to the policy, a payload that does not hash-pin, a payload the
 * committed signer set actually satisfies — all refused before anything is
 * paid for.
 */

import "@al-ft/midgard-core";
import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "effect";
import "../field-opening.js";
import "../runtime.js";
import "../spend-input-witness.js";
import "../step-support.js";
import "../tx-layout.js";
import "../witness-reference-scripts.js";
import "../workflow/raw-datum-preimage-prerequisite.js";
import "../workflow/transaction-boundary.js";
import "./evaluate.js";
import "./submit-common.js";
import "./submit-mint-authorization-step-03.submit-prepared-step03.js";
import "./submit-mint-authorization-step-03.submit-mint-authorization-step03-witness-absence.js";
import "./submit-mint-authorization-step-03.submit-mint-authorization-step03-evaluate-unsatisfied.js";
export { submitMintAuthorizationStep03EvaluateUnsatisfied } from "./submit-mint-authorization-step-03.submit-mint-authorization-step03-evaluate-unsatisfied.js";
export { submitMintAuthorizationStep03WitnessAbsence } from "./submit-mint-authorization-step-03.submit-mint-authorization-step03-witness-absence.js";
export { type SubmitMintAuthorizationStep03Result } from "./submit-mint-authorization-step-03.submit-prepared-step03.js";
