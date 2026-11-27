/**
 * Operator exit: retirement (voluntary or forced by inactivity), bond
 * recovery, and duplicate-registration slashing.
 *
 * These are the removal counterparts of `buildRegisterOperatorTx` /
 * `buildActivateOperatorTx`, and they follow the same shape: a synchronous
 * `TxBuilder` builder whose redeemers may be resolved from the redeemer
 * context, plus an Effect program that builds twice — once with callbacks to
 * learn the layout, once with a static layout — because the redeemer's own
 * size moves the fee and therefore the indices.
 *
 * The forced retirement and the slashing pin their fee to a protocol penalty,
 * so their programs also balance the transaction themselves; see
 * `./exact-fee.ts`.
 */

import "@lucid-evolution/lucid";
import "effect";
import "../active-operators.js";
import "../common.js";
import "../linked-list.js";
import "../registered-operators.js";
import "../retired-operators.js";
import "../scheduler.js";
import "../scheduler-refresh.js";
import "../tx-completion.js";
import "../tx-context-redeemer.js";
import "./directory.js";
import "./exact-fee.js";
import "./layout.js";
import "./output-selectors.js";
import "./exit.complete-in-two-passes.js";
import "./exit.derive-retire-scheduler-sync-layout.js";
import "./exit.derive-retire-layout.js";
import "./exit.build-retire-operator-tx.js";
import "./exit.derive-retire-operator-witnesses.js";
import "./exit.build-slash-duplicate-operator-tx.js";
export {
  buildRetireOperatorTx,
  buildUnsignedRetireOperatorTxProgram,
  type RetireOperatorTxResult,
  type RetireOperatorWitnesses,
} from "./exit.build-retire-operator-tx.js";
export {
  buildSlashDuplicateOperatorTx,
  buildUnsignedSlashDuplicateOperatorTxProgram,
  type SlashDuplicateOperatorTxConfig,
  type SlashDuplicateOperatorTxResult,
} from "./exit.build-slash-duplicate-operator-tx.js";
export {
  activeOperatorNodeUnit,
  type OperatorBondParameters,
  OperatorExitError,
  retiredOperatorBondTranche,
  retiredOperatorNodeUnit,
  type RetirementMode,
  type RetireSchedulerSync,
} from "./exit.complete-in-two-passes.js";
export {
  buildRecoverOperatorBondTx,
  buildUnsignedRecoverOperatorBondTxProgram,
  deriveRecoverOperatorBondWitnesses,
  deriveRetireOperatorWitnesses,
  type DuplicateProof,
  type RecoverOperatorBondRedeemerLayout,
  type RecoverOperatorBondTxConfig,
  type RecoverOperatorBondTxResult,
  type SlashDuplicateOperatorRedeemerLayout,
} from "./exit.derive-retire-operator-witnesses.js";
export {
  type RetireOperatorTxConfig,
  type RetireRedeemerLayout,
  type RetireSchedulerSyncLayout,
} from "./exit.derive-retire-scheduler-sync-layout.js";
