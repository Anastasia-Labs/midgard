/**
 * Node-side programs behind the operator exit verbs: voluntary and forced
 * retirement, bond recovery, and duplicate-registration slashing. Each program
 * reads the live directory, refuses locally with a plain reason when the
 * ledger would refuse anyway, runs the funding preflight, then builds, signs,
 * and submits through the SDK builder.
 */

import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "effect";
import "../../commands/contract-deployment-info.js";
import "../../lucid-time.js";
import "../../workers/utils/commit-end-time.js";
import "../reference-scripts.js";
import "../register-active-operator/clock.js";
import "../utils.js";
import "./funding-preflight.js";
import "./exit.resolve-operator-script-refs-program.js";
import "./exit.retire-operator-program.js";
import "./exit.slash-duplicate-operator-program.js";
export {
  configuredOperatorEconomicsProgram,
  EXIT_VALIDITY_LOOKBACK_SLOTS,
  type ExitValidityWindow,
  exitValidityWindow,
  OPERATOR_TX_VALIDITY_WINDOW_MS,
  type OperatorEconomics,
  type OperatorExitError,
  OperatorExitRefusal,
  type OperatorScriptFamily,
  type OperatorScriptRefs,
  resolveOperatorScriptRefsProgram,
  type RetirementSubmission,
} from "./exit.resolve-operator-script-refs-program.js";
export {
  type BondRecoverySubmission,
  type DuplicateSlashSubmission,
  recoverOperatorBondProgram,
  retireOperatorProgram,
} from "./exit.retire-operator-program.js";
export {
  selectDuplicateRegistration,
  slashDuplicateOperatorProgram,
} from "./exit.slash-duplicate-operator-program.js";
