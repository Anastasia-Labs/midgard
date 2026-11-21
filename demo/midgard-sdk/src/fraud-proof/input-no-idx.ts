/**
 * `input-no-idx` (`nonExistentInputNoIndex`) fault-proof family (Goal task
 * `Q13`).
 *
 * **Rule.** Every spend input of a committed transaction must name an output
 * that its producing transaction actually created: for an input
 * `(tx_id, output_index)` whose producer `tx_id` is itself committed in the
 * same block, `output_index` must be strictly less than the number of outputs
 * that producer commits.
 *
 * **Violation `input-no-idx`.** A committed transaction spends
 * `(producing_tx_id, output_index)` where `producing_tx_id` *is* committed in
 * the same block — so the preimage of the transaction id exists — yet
 * `output_index >= |producer.outputs|`. The UTxO therefore never existed, and
 * no other family can convict it: `non-existent-input` proves exclusion from
 * the previous block's ledger, which says nothing about an output index of a
 * transaction produced inside this block.
 *
 * The proof is a four-step computation thread:
 *
 * 1. bind the bad transaction to the block's counted `transactions_root` and
 *    forward its §2.5 anchor — the transaction **id**;
 * 2. open field 0 through the §8.8 door with the prover's chosen carriage and
 *    forward the challenged `(tx_id, output_index)`;
 * 3. bind the producing transaction to the *same* block and forward **its**
 *    anchor alongside the challenged index; and
 * 4. open that transaction's field 2 through the door and require
 *    `output_index >= |outputs|`, on the door's authenticated item count.
 *
 * This module is the strict TypeScript twin of
 * `onchain/aiken/lib/midgard/fraud-proofs/input-no-idx/step-0{1..4}.ak` and of
 * the `MidgardTxOutput` shape in
 * `onchain/aiken/lib/midgard/fraud-proofs/native-tx/types.ak`. Field order in
 * every `Data.Object` mirrors the aiken record declarations 1:1 — the
 * PlutusData encoding is positional, so re-ordering here would silently produce
 * redeemers the validators reject.
 *
 * Thread state carries the §2.5 transaction anchor rather than a per-field
 * collection commitment, and a step redeemer carries `FieldOpeningV1` rather
 * than a reproduced `..._preimage: List<…>`. See
 * `docs/fault-proofs/decisions/0001-reference-input-field-evidence.md`.
 */
import "@al-ft/midgard-core";
import "@al-ft/midgard-core/lucid-data";
import "@lucid-evolution/lucid";
import "../common.js";
import "./field-opening.js";
import "./native.js";
import "./input-no-idx.input-no-idx-evidence-from-committed-transactions.js";
import "./input-no-idx.encode-midgard-value-canonical.js";
import "./input-no-idx.input-no-idx-outputs-commitment.js";

import {
  FaultProofStepCancel,
  FaultProofStepCancelSchema,
  MidgardTxInput,
  MidgardTxInputSchema,
  NativeTxInclusionCarriage,
  NativeTxInclusionCarriageSchema,
} from "./native.js";
export {
  encodeMidgardAddressCanonical,
  encodeMidgardTxInputCanonical,
  encodeMidgardTxOutputCanonical,
  encodeMidgardValueCanonical,
  encodeMidgardVersionedScriptCanonical,
  INPUT_NO_IDX_OUTPUTS_FIELD_INDEX,
  INPUT_NO_IDX_SPEND_INPUTS_FIELD_INDEX,
  inputNoIdxSpendInputsCommitment,
  inputNoIdxStep02StateFromBadTx,
  inputNoIdxStep03StateFromEvidence,
  InputNoIdxStep04Args,
  InputNoIdxStep04ArgsSchema,
  InputNoIdxStep04SpendRedeemer,
  InputNoIdxStep04SpendRedeemerSchema,
  inputNoIdxStep04StateFromEvidence,
} from "./input-no-idx.encode-midgard-value-canonical.js";
export {
  INPUT_NO_IDX_CATALOGUE_CATEGORY,
  INPUT_NO_IDX_STEP02_DIRECT_INPUT_LIMIT,
  INPUT_NO_IDX_VIOLATION_ID,
  type InputNoIdxEvidence,
  inputNoIdxEvidenceFromCommittedTransactions,
  InputNoIdxStep01Datum,
  InputNoIdxStep01DatumSchema,
  InputNoIdxStep01SpendRedeemer,
  InputNoIdxStep01SpendRedeemerSchema,
  InputNoIdxStep02Args,
  InputNoIdxStep02ArgsSchema,
  InputNoIdxStep02Datum,
  InputNoIdxStep02DatumSchema,
  inputNoIdxStep02ExecutionMode,
  InputNoIdxStep02SpendRedeemer,
  InputNoIdxStep02SpendRedeemerSchema,
  InputNoIdxStep02State,
  InputNoIdxStep02StateSchema,
  InputNoIdxStep03Datum,
  InputNoIdxStep03DatumSchema,
  InputNoIdxStep03SpendRedeemer,
  InputNoIdxStep03SpendRedeemerSchema,
  InputNoIdxStep03State,
  InputNoIdxStep03StateSchema,
  InputNoIdxStep04Datum,
  InputNoIdxStep04DatumSchema,
  InputNoIdxStep04State,
  InputNoIdxStep04StateSchema,
  isInputNoIdxViolation,
  MidgardAddress,
  MidgardAddressSchema,
  MidgardCredential,
  MidgardCredentialSchema,
  MidgardScriptLanguage,
  MidgardScriptLanguageSchema,
  MidgardTxOutput,
  MidgardTxOutputList,
  MidgardTxOutputListSchema,
  MidgardTxOutputSchema,
  MidgardValue,
  MidgardValueSchema,
  MidgardVersionedScript,
  MidgardVersionedScriptSchema,
} from "./input-no-idx.input-no-idx-evidence-from-committed-transactions.js";
export { inputNoIdxOutputsCommitment } from "./input-no-idx.input-no-idx-outputs-commitment.js";

export {
  MidgardTxInput as InputNoIdxSpendInput,
  MidgardTxInputSchema as InputNoIdxSpendInputSchema,
  FaultProofStepCancel as InputNoIdxStepCancel,
  FaultProofStepCancelSchema as InputNoIdxStepCancelSchema,
  NativeTxInclusionCarriage as InputNoIdxTxInclusionArgs,
  NativeTxInclusionCarriageSchema as InputNoIdxTxInclusionArgsSchema,
};
