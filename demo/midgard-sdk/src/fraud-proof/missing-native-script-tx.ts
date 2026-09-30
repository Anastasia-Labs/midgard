/**
 * `missing-native-script-tx` fault-proof family (Goal task `Q17`).
 *
 * **Rule.** Every committed transaction that spends an output locked by a
 * native-script credential must carry that script in its script-witness
 * collection (witness field 6): for every spend input whose producing output's
 * payment credential is `ScriptCredential(h)` with
 * `h = versioned_script_hash(NativeCardanoScript, bytes)`, some item of
 * `script_tx_wits` must hash to `h`.
 *
 * **Violation.** A block commits a transaction spending such an output while
 * no script-witness item hashes to the credential. The rule is unconditional
 * over the block's transactions.
 *
 * The proof is an eight-script computation thread. Steps 1–5 bind and
 * classify the accused credential; step 6 either finalizes the bounded direct
 * route or starts the authenticated grammar walk; steps 7–8 own the grammar
 * and semantic resumptions for larger fields.
 *
 * 1. bind the bad transaction to the block's counted `transactions_root` and
 *    forward its id together with the block-committed `witness_set_hash`;
 * 2. open the bad transaction's spend-input field (body field 0) and forward
 *    the accused input;
 * 3. bind the *producing* transaction (the accused input's tx id) the same
 *    way;
 * 4. open the producing transaction's outputs field (body field 2), read the
 *    output the accused input names, and require a script payment credential;
 * 5. lift the credential to a native script: the prover supplies the script
 *    bytes and the step equates `versioned_script_hash` (language tag 0) with
 *    the credential; and
 * 6. open the bad transaction's script-witness field (witness field 6)
 *    against the thread-anchored witness-set hash, finalizing directly at or
 *    below 64 items or starting a 32-item grammar-certification batch;
 * 7. resume grammar certification until terminal, then start the semantic
 *    absence scan; and
 * 8. resume the semantic scan in bounded batches and finalize only at the
 *    authenticated terminal with the presence accumulator still false.
 *
 * This module is the strict TypeScript twin of
 * `onchain/aiken/lib/midgard/fraud-proofs/missing-native-script-tx/step-0{1..8}.ak`.
 * Field order in every `Data.Object` mirrors the aiken record declarations
 * 1:1 — the PlutusData encoding is positional, so re-ordering here would
 * silently produce redeemers the validators reject.
 */

import "@al-ft/midgard-core";
import "@al-ft/midgard-core/lucid-data";
import "@lucid-evolution/lucid";
import "../common.js";
import "./field-opening.js";
import "./native.js";
import "./missing-native-script-tx.missing-native-script-is-absent.js";
import "./missing-native-script-tx.missing-native-script-tx-step06-args-schema.js";
import "./missing-native-script-tx.missing-native-script-tx-step04-state.js";
export {
  MISSING_NATIVE_SCRIPT_TX_DIRECT_WITNESS_LIMIT,
  MISSING_NATIVE_SCRIPT_TX_SCRIPT_TX_WITS_FIELD_INDEX,
  MISSING_NATIVE_SCRIPT_TX_STAGED_BATCH_LIMIT,
  MISSING_NATIVE_SCRIPT_TX_VIOLATION_ID,
  missingNativeScriptIsAbsent,
  MissingNativeScriptTxStep01Args,
  MissingNativeScriptTxStep01ArgsSchema,
  MissingNativeScriptTxStep01Datum,
  MissingNativeScriptTxStep01DatumSchema,
  MissingNativeScriptTxStep01SpendRedeemer,
  MissingNativeScriptTxStep01SpendRedeemerSchema,
  MissingNativeScriptTxStep02Args,
  MissingNativeScriptTxStep02ArgsSchema,
  MissingNativeScriptTxStep02Datum,
  MissingNativeScriptTxStep02DatumSchema,
  MissingNativeScriptTxStep02SpendRedeemer,
  MissingNativeScriptTxStep02SpendRedeemerSchema,
  MissingNativeScriptTxStep02State,
  MissingNativeScriptTxStep02StateSchema,
  MissingNativeScriptTxStep03Args,
  MissingNativeScriptTxStep03ArgsSchema,
  MissingNativeScriptTxStep03Datum,
  MissingNativeScriptTxStep03DatumSchema,
  MissingNativeScriptTxStep03SpendRedeemer,
  MissingNativeScriptTxStep03SpendRedeemerSchema,
  MissingNativeScriptTxStep03State,
  MissingNativeScriptTxStep03StateSchema,
  MissingNativeScriptTxStep04Args,
  MissingNativeScriptTxStep04ArgsSchema,
  MissingNativeScriptTxStep04Datum,
  MissingNativeScriptTxStep04DatumSchema,
  MissingNativeScriptTxStep04SpendRedeemer,
  MissingNativeScriptTxStep04SpendRedeemerSchema,
  MissingNativeScriptTxStep04State,
  MissingNativeScriptTxStep04StateSchema,
  MissingNativeScriptTxStepCancel,
  MissingNativeScriptTxStepCancelSchema,
  missingNativeScriptTxVersionedScriptHash,
} from "./missing-native-script-tx.missing-native-script-is-absent.js";
export {
  missingNativeScriptTxStep04State,
  missingNativeScriptTxStep05State,
  missingNativeScriptTxStep06ReadyState,
} from "./missing-native-script-tx.missing-native-script-tx-step04-state.js";
export {
  MissingNativeScriptTxPhase,
  MissingNativeScriptTxPhaseSchema,
  missingNativeScriptTxStep02StateFromBadTx,
  missingNativeScriptTxStep03State,
  MissingNativeScriptTxStep05Args,
  MissingNativeScriptTxStep05ArgsSchema,
  MissingNativeScriptTxStep05Datum,
  MissingNativeScriptTxStep05DatumSchema,
  MissingNativeScriptTxStep05SpendRedeemer,
  MissingNativeScriptTxStep05SpendRedeemerSchema,
  MissingNativeScriptTxStep05State,
  MissingNativeScriptTxStep05StateSchema,
  MissingNativeScriptTxStep06Args,
  MissingNativeScriptTxStep06ArgsSchema,
  MissingNativeScriptTxStep06Datum,
  MissingNativeScriptTxStep06DatumSchema,
  MissingNativeScriptTxStep06SpendRedeemer,
  MissingNativeScriptTxStep06SpendRedeemerSchema,
  MissingNativeScriptTxStep06State,
  MissingNativeScriptTxStep06StateSchema,
  MissingNativeScriptTxStep07Args,
  MissingNativeScriptTxStep07ArgsSchema,
  MissingNativeScriptTxStep07Datum,
  MissingNativeScriptTxStep07DatumSchema,
  MissingNativeScriptTxStep07SpendRedeemer,
  MissingNativeScriptTxStep07SpendRedeemerSchema,
  MissingNativeScriptTxStep07State,
  MissingNativeScriptTxStep07StateSchema,
  MissingNativeScriptTxStep08Args,
  MissingNativeScriptTxStep08ArgsSchema,
  MissingNativeScriptTxStep08Datum,
  MissingNativeScriptTxStep08DatumSchema,
  MissingNativeScriptTxStep08SpendRedeemer,
  MissingNativeScriptTxStep08SpendRedeemerSchema,
  MissingNativeScriptTxStep08State,
  MissingNativeScriptTxStep08StateSchema,
} from "./missing-native-script-tx.missing-native-script-tx-step06-args-schema.js";
