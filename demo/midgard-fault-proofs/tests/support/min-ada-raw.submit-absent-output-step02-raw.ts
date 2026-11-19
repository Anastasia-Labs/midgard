import {
  MIDGARD_FIELD_INDEX,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
  scriptRewardAddress,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  faultProofFieldOpening,
  planFaultProofFieldOpening,
} from "../../src/field-opening.js";
import {
  linearFaultStepLabel,
  requireLinearFaultReferenceScript,
} from "../../src/linear-fault-family.js";
import type { MinAdaContracts } from "../../src/min-ada/contracts.js";
import {
  encodeMinAdaGrammarCheckpoint,
  resolveMinAdaGrammarCheckpoint,
} from "../../src/min-ada/field-walk.js";
import {
  Redeemer,
  requireStep02,
  Step03Datum,
} from "../../src/min-ada/submit-step-02.require-step02.js";
import type { ResolvedProverSigner } from "../../src/runtime.js";
import {
  completeStep,
  finishStep,
  fundStep,
} from "../../src/step-completion.js";
import { computationThreadOutputPredicate } from "../../src/tx-layout.js";
import { submitUnjournaled } from "../../src/workflow/transaction-boundary.js";

/**
 * Min-ada step 02 built by hand on a thread whose field-2 grammar is
 * complete: field 2 is opened from `outputItemCbors` and the thread moves to
 * step 03 carrying `successor` (by default `MinAdaTxAbsent`, the
 * absent-output close), whatever the field holds. No builder check runs on
 * whether the bound output index is past the field's last output, or on
 * which step-03 state the walk would produce.
 */
export const submitRawMinAdaAbsentOutputStep02 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  nativeTxCompactCbor,
  outputItemCbors,
  publishedCarriageUtxos,
  referenceScriptUtxo,
  yieldReferenceScriptUtxo,
  successor = (direction) => ({ MinAdaTxAbsent: { direction } }),
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: MinAdaContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly nativeTxCompactCbor: string;
  readonly outputItemCbors: readonly string[];
  readonly publishedCarriageUtxos: readonly UTxO[];
  readonly referenceScriptUtxo: UTxO;
  readonly yieldReferenceScriptUtxo: UTxO;
  readonly successor?: (direction: bigint) => Step03Datum["data"];
}): Promise<string> => {
  const { stepIndex, threadUtxo, threadToken, state } = await requireStep02({
    lucid,
    contracts,
    categoryId,
    signer,
    threadOutRef,
  });
  const label = `raw ${linearFaultStepLabel("min-ada", stepIndex)} absent`;
  if (!state.grammar_complete)
    throw new Error(`${label}: field 2 grammar is not complete`);
  const items = outputItemCbors.map((item) => Buffer.from(item, "hex"));
  const planned = planFaultProofFieldOpening({
    anchorSourceKind: state.source_kind === 1n ? 1n : 0n,
    fieldIndex: MIDGARD_FIELD_INDEX.outputs,
    anchorTxId: state.bad_tx_id,
    nativeTxCompactCbor,
    itemCbors: items,
    owner: signer.paymentKeyHash,
    publish: false,
    label,
  });
  const grammarBytes = encodeMinAdaGrammarCheckpoint(
    resolveMinAdaGrammarCheckpoint({
      txId: state.bad_tx_id,
      items,
      committedHash: state.grammar_checkpoint_hash,
    }),
  ).toString("hex");
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[stepIndex].spendingScriptHash,
    family: "min-ada",
    stepIndex,
  });
  const yieldReference = requireLinearFaultReferenceScript({
    utxo: yieldReferenceScriptUtxo,
    expectedScriptHash: contracts.yields.tx.withdrawalScriptHash,
    family: "min-ada",
    stepIndex,
  });
  const nextDatum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: successor(state.direction),
    },
    Step03Datum,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[2].spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, label);
    return Data.to(
      {
        Continue: [
          {
            grammar_checkpoint_bytes: grammarBytes,
            walk_checkpoint_bytes: "",
            input_index: requireInputIndex(ctx, threadUtxo, label),
            output_index: requireUniqueOutputIndex(
              ctx.outputs,
              outputMatches,
              label,
            ),
            yield_to_ref_input_index: requireReferenceInputIndex(
              ctx,
              yieldReference,
              label,
            ),
            outputs_opening: faultProofFieldOpening({
              planned,
              referenceInputs: ctx.referenceInputs,
              certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
              label,
            }),
          },
        ],
      },
      Redeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const network = lucid.config().network;
  if (network === undefined) throw new Error(`${label}: Lucid network missing`);
  const references = [stepReference, ...publishedCarriageUtxos, yieldReference];
  const funding = await fundStep(lucid, { signer, excluding: references });
  const tx = funding.tx
    .collectFrom([threadUtxo], redeemer)
    .readFrom(references)
    .withdraw(
      scriptRewardAddress(network, contracts.yields.tx.withdrawalScript),
      0n,
      Data.void(),
    )
    .pay.ToContract(
      contracts.steps[2].spendingScriptAddress,
      { kind: "inline", value: nextDatum },
      { lovelace: threadUtxo.assets.lovelace ?? 0n, [threadToken.unit]: 1n },
    );
  const signed = await completeStep(tx, funding);
  return await submitUnjournaled(
    lucid,
    finishStep(signed, { referenceScripts: [] }),
    { label, awaitConfirmation: true },
  );
};
