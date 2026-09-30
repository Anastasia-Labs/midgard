/**
 * Shared emulator fixture and strike helpers for stalled-operator recovery.
 *
 * The authenticated protocol deployment is expensive, so it is built once per
 * operator-count and deep-cloned per scenario: every test gets its own ledger,
 * slots, datum table and wallets, and nothing bleeds between scenarios.
 *
 * Both the inactivity-strike tests and the forced-retire tests build on this
 * module: `strikeOperatorToMaxStrikes` is the setup a forced retirement needs.
 */
import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "effect";
import "../../src/transactions/initialization.js";
import "../../src/transactions/operators/takeover.js";
import "../../src/transactions/reference-scripts.js";
import "../../src/transactions/register-active-operator.js";
import "../../src/transactions/script-reward-registration.js";
import "../../src/workers/utils/commit-end-time.js";
import "./real-midgard-contracts.js";
import "./operator-inactivity.build-deployment-snapshot.js";
import "./operator-inactivity.prepare-inactivity-strike.js";
import "./operator-inactivity.strike-operator-to-max-strikes.js";

import { alignedUnixTimeAtOrAfter } from "../../src/transactions/operators/takeover.js";
export {
  advanceEmulatorPastUnixTime,
  alignedUnixTimeAtOrBefore,
  EMPTY_FRAUD_PROOF_CATALOGUE_ROOT,
  EMULATOR_PROTOCOL_PARAMETERS,
  EMULATOR_REFERENCE_SCRIPT_AUTH_TIMELOCK_MS,
  EMULATOR_REQUIRED_BOND_LOVELACE,
  type InactivityOperatorAccount,
  initOperatorInactivityFixture,
  type OperatorInactivityFixture,
  REGISTRATION_ACTIVATION_DELAY_SLOTS,
  STRIKE_VALIDITY_WINDOW_MS,
} from "./operator-inactivity.build-deployment-snapshot.js";
export {
  appointFirstSchedulerOperator,
  fetchInactivityDirectorySnapshot,
  fetchSchedulerDatum,
  type PreparedStrike,
  prepareInactivityStrike,
  type StrikeAttemptOptions,
  type StrikeSubmission,
  submitInactivityStrike,
} from "./operator-inactivity.prepare-inactivity-strike.js";
export {
  activeOperatorNodeUnit,
  expectInactivityStrikeRefusal,
  strikeOperatorToMaxStrikes,
  submitNeglectedDeposit,
  submitNeglectedWithdrawal,
  submitUnauthenticatedHistoryNodeCopy,
} from "./operator-inactivity.strike-operator-to-max-strikes.js";

// ---------------------------------------------------------------------------
// Slot-aligned time helpers
// ---------------------------------------------------------------------------

export { alignedUnixTimeAtOrAfter };
