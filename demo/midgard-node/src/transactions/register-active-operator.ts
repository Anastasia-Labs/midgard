/**
 * Operator lifecycle transaction orchestration for register/activate flows.
 * This module is the main off-chain entrypoint for operator-set updates and
 * composes the shared layout and clock helpers extracted from the monolith.
 */

import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "effect";
import "../services/index.js";
import "../tx-context.js";
import "../workers/utils/commit-end-time.js";
import "./operators/exit.js";
import "./operators/funding-preflight.js";
import "./reference-scripts.js";
import "./register-active-operator/activation.js";
import "./register-active-operator/clock.js";
import "./utils.js";
import "./register-active-operator.fetch-hub-oracle-ref-input.js";
import "./register-active-operator.to-lifecycle-result.js";
import "./register-active-operator.operator-lifecycle-program.js";
import "./register-active-operator.activate-registered-operator-program.js";
export type { ReferenceScriptCommandName } from "./reference-scripts.js";
export {
  deployReferenceScriptCommandProgram,
  REFERENCE_SCRIPT_COMMAND_NAMES,
} from "./reference-scripts.js";
export {
  activateOperatorProgram,
  activateProgram,
  activateRegisteredOperatorProgram,
  deregisterOperatorProgram,
  program,
  registerAndActivateOperatorProgram,
  registerOperatorProgram,
} from "./register-active-operator.activate-registered-operator-program.js";
export { OperatorRegistrationRefusal } from "./register-active-operator.to-lifecycle-result.js";
