import {
  encodeMidgardTxInputCanonicalV1,
  type FieldOpeningV1,
  MIDGARD_FIELD_INDEX_V1,
  type MidgardTxInput,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
  WithdrawnInputStep02Datum,
  WithdrawnInputStep02SpendRedeemer,
  WithdrawnInputStep03Datum,
  withdrawnInputStep03StateV1,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  faultProofFieldOpeningV1,
  planFaultProofFieldOpeningV1,
  publishFaultProofFieldCarriageV1,
} from "../field-opening-v1.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  type ResolvedProverSigner,
} from "../runtime.js";
import { excludeUtxo } from "../spend-input-witness.js";
import { selectFeeInput } from "../submit-step-01.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import {
  WITHDRAWN_INPUT_CATEGORY_LABEL,
  type WithdrawnInputContractsV1,
} from "./contracts-v1.js";
import {
  requireWithdrawnInputReferenceScriptV1,
  requireWithdrawnInputStepStateV1,
  requireWithdrawnInputThreadUtxoV1,
  withdrawnInputSubmitError,
} from "./submit-common-v1.js";

export type WithdrawnInputSpendInputsEvidenceV1 = {
  readonly inputs: readonly MidgardTxInput[];
  readonly badInputIndex: number;
  readonly nativeTxCompactCbor: string;
};

export type SubmitWithdrawnInputStep02Result = {
  readonly txHash: string;
  readonly nextThreadOutRef: string;
  readonly thirdStepAddress: string;
  readonly withdrawnInput: MidgardTxInput;
  readonly computationThreadUnit: string;
  readonly carriageTier: string;
};

export const submitWithdrawnInputStep02 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  evidence,
  referenceScriptUtxo,
  publishCarriage = false,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: WithdrawnInputContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: WithdrawnInputSpendInputsEvidenceV1;
  readonly referenceScriptUtxo: UTxO;
  readonly publishCarriage?: boolean;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitWithdrawnInputStep02Result> => {
  const { threadUtxo, threadToken } = await requireWithdrawnInputThreadUtxoV1({
    lucid,
    contracts,
    categoryId,
    stepIndex: 1,
    threadOutRef,
  });
  const state = requireWithdrawnInputStepStateV1({
    threadUtxo,
    signer,
    schema: WithdrawnInputStep02Datum,
    stepIndex: 1,
  });
  const stepReference = requireWithdrawnInputReferenceScriptV1({
    utxo: referenceScriptUtxo,
    contracts,
    stepIndex: 1,
  });
  const withdrawnInput = evidence.inputs[evidence.badInputIndex];
  if (withdrawnInput === undefined) {
    throw withdrawnInputSubmitError(
      `badInputIndex ${evidence.badInputIndex.toString()} is out of range for ${evidence.inputs.length.toString()} spend inputs.`,
    );
  }
  const planned = planFaultProofFieldOpeningV1({
    fieldIndex: MIDGARD_FIELD_INDEX_V1.spendInputs,
    anchorTxId: state.bad_tx_id,
    nativeTxCompactCbor: evidence.nativeTxCompactCbor,
    itemCbors: evidence.inputs.map(encodeMidgardTxInputCanonicalV1),
    owner: signer.paymentKeyHash,
    publish: publishCarriage,
    label: `${WITHDRAWN_INPUT_CATEGORY_LABEL} spend inputs`,
  });
  signer.selectWallet(lucid);
  const carriageUtxos = await publishFaultProofFieldCarriageV1({
    lucid,
    signer,
    planned,
    publisherAddress: signer.address,
    label: `${WITHDRAWN_INPUT_CATEGORY_LABEL} spend inputs`,
  });
  const referenceInputs = [...carriageUtxos, stepReference];
  const opening: FieldOpeningV1 = faultProofFieldOpeningV1({
    planned,
    referenceInputs,
    certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
    label: `${WITHDRAWN_INPUT_CATEGORY_LABEL} spend inputs`,
  });
  const walletUtxos = await lucid.wallet().getUtxos();
  const walletInputs = referenceInputs.reduce<readonly UTxO[]>(
    (candidates, utxo) => excludeUtxo(candidates, utxo),
    walletUtxos,
  );
  const feeInput = selectFeeInput(walletInputs);
  const outputDatum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: withdrawnInputStep03StateV1({
        input: withdrawnInput,
        withdrawalsRoot: state.blocks_withdrawals_root,
        withdrawalCount: state.blocks_withdrawal_count,
      }),
    },
    WithdrawnInputStep03Datum,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[2].spendingScriptAddress,
    datum: outputDatum,
    unit: threadToken.unit,
  });
  let layout:
    | { readonly inputIndex: bigint; readonly outputIndex: bigint }
    | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(
      ctx,
      threadUtxo,
      `${WITHDRAWN_INPUT_CATEGORY_LABEL} step 02`,
    );
    layout = {
      inputIndex: requireInputIndex(
        ctx,
        threadUtxo,
        `${WITHDRAWN_INPUT_CATEGORY_LABEL} step 02`,
      ),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        outputMatches,
        `${WITHDRAWN_INPUT_CATEGORY_LABEL} step 02 output`,
      ),
    };
    return Data.to(
      {
        Continue: [
          {
            input_index: layout.inputIndex,
            output_index: layout.outputIndex,
            spend_inputs_opening: opening,
            bad_input_index: BigInt(evidence.badInputIndex),
          },
        ],
      },
      WithdrawnInputStep02SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

  const tx = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer)
    .readFrom(referenceInputs)
    .pay.ToContract(
      contracts.steps[2].spendingScriptAddress,
      { kind: "inline", value: outputDatum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [threadToken.unit]: 1n,
      },
    )
    .addSignerKey(signer.paymentKeyHash);
  const unsigned = await tx.complete({
    localUPLCEval: true,
    presetWalletInputs: walletInputs as UTxO[],
  });
  if (layout === undefined) {
    throw withdrawnInputSubmitError("step-02 layout was not resolved.");
  }
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  if (awaitConfirmation) {
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }
  return {
    txHash,
    nextThreadOutRef: `${txHash}#${layout.outputIndex.toString()}`,
    thirdStepAddress: contracts.steps[2].spendingScriptAddress,
    withdrawnInput,
    computationThreadUnit: threadToken.unit,
    carriageTier: planned.plan.tier,
  };
};
