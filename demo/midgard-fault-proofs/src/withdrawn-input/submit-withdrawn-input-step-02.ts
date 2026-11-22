import {
  type MidgardTxInput,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk/core";
import {
  encodeMidgardTxInputCanonical,
  type FieldOpening,
  MIDGARD_FIELD_INDEX,
  WithdrawnInputStep02Datum,
  WithdrawnInputStep02SpendRedeemer,
  WithdrawnInputStep03Datum,
  withdrawnInputStep03State,
} from "@al-ft/midgard-sdk/fraud-proof";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  type FaultProofTurn,
  isPrerequisiteTurn,
} from "../fault-proof-turn.js";
import { planFaultProofFieldOpening } from "../field-opening.plan-fault-proof-field-opening.js";
import {
  faultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "../field-opening.publish-fault-proof-field-carriage.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { completeStep, finishStep, fundStep } from "../step-completion.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { ProofMaterialsAccess } from "../workflow/proof-materials-view.js";
import {
  type LocallyEvaluatedTransaction,
  type OutsideWorkflowSubmission,
  workflowReferenceScript,
} from "../workflow/transaction-boundary.js";
import {
  WITHDRAWN_INPUT_CATEGORY_LABEL,
  type WithdrawnInputContracts,
} from "./contracts.js";
import {
  requireWithdrawnInputReferenceScript,
  requireWithdrawnInputStepState,
  requireWithdrawnInputThreadUtxo,
  withdrawnInputSubmitError,
} from "./submit-common.js";

export type WithdrawnInputSpendInputsEvidence = {
  readonly inputs: readonly MidgardTxInput[];
  readonly badInputIndex: number;
  readonly nativeTxCompactCbor: string;
};

export type WithdrawnInputStep02Build = {
  readonly transaction: LocallyEvaluatedTransaction;
  readonly nextThreadOutRef: string;
  readonly thirdStepAddress: string;
  readonly withdrawnInput: MidgardTxInput;
  readonly computationThreadUnit: string;
  readonly carriageTier: string;
};

export type SubmitWithdrawnInputStep02Result =
  OutsideWorkflowSubmission<WithdrawnInputStep02Build>;

export const buildWithdrawnInputStep02 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  evidence,
  referenceScriptUtxo,
  publishCarriage = false,
  publishedCarriageUtxos,
  certificateUtxo,
  publishMissingCarriage = true,
  materials,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: WithdrawnInputContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: WithdrawnInputSpendInputsEvidence;
  readonly referenceScriptUtxo: UTxO;
  readonly publishCarriage?: boolean;
  /** Pre-authenticated §8 field-carriage UTxOs for production workflows. */
  readonly publishedCarriageUtxos?: readonly UTxO[];
  /** Pre-authenticated §8.6 certificate when the opening selects tier 3. */
  readonly certificateUtxo?: UTxO;
  /** Diagnostic/emulator fallback only. Production workflows set false. */
  readonly publishMissingCarriage?: boolean;
  /** The workflow's view of its proof-materials address, on a workflow. */
  readonly materials?: ProofMaterialsAccess;
}): Promise<FaultProofTurn<WithdrawnInputStep02Build>> => {
  const { threadUtxo, threadToken } = await requireWithdrawnInputThreadUtxo({
    lucid,
    contracts,
    categoryId,
    stepIndex: 1,
    threadOutRef,
  });
  const state = requireWithdrawnInputStepState({
    threadUtxo,
    signer,
    schema: WithdrawnInputStep02Datum,
    stepIndex: 1,
  });
  const stepReference = requireWithdrawnInputReferenceScript({
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
  const planned = planFaultProofFieldOpening({
    anchorSourceKind: 0n,
    fieldIndex: MIDGARD_FIELD_INDEX.spendInputs,
    anchorTxId: state.bad_tx_id,
    nativeTxCompactCbor: evidence.nativeTxCompactCbor,
    itemCbors: evidence.inputs.map(encodeMidgardTxInputCanonical),
    owner: signer.paymentKeyHash,
    publish: publishCarriage,
    label: `${WITHDRAWN_INPUT_CATEGORY_LABEL} spend inputs`,
  });
  const carriageUtxos =
    publishedCarriageUtxos ??
    (planned.plan.publications.length === 0
      ? []
      : publishMissingCarriage
        ? await publishFaultProofFieldCarriage({
            materials,
            lucid,
            signer,
            planned,
            publisherAddress: signer.proofMaterialsAddress,
            label: `${WITHDRAWN_INPUT_CATEGORY_LABEL} spend inputs`,
          })
        : (() => {
            throw withdrawnInputSubmitError(
              "step-02 requires authenticated pre-published field carriage",
            );
          })());
  if (isPrerequisiteTurn(carriageUtxos)) return carriageUtxos;
  if (planned.plan.tier === "Certified" && certificateUtxo === undefined) {
    throw withdrawnInputSubmitError(
      "step-02 requires its authenticated field-preimage certificate",
    );
  }
  const referenceInputs = [
    ...carriageUtxos,
    ...(certificateUtxo === undefined ? [] : [certificateUtxo]),
    stepReference,
  ];
  const funding = await fundStep(lucid, {
    signer,
    excluding: referenceInputs,
  });
  const outputDatum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: withdrawnInputStep03State({
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
    const opening: FieldOpening = faultProofFieldOpening({
      planned,
      referenceInputs: ctx.referenceInputs,
      certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
      label: `${WITHDRAWN_INPUT_CATEGORY_LABEL} spend inputs`,
    });
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

  const tx = funding.tx
    .collectFrom([threadUtxo], redeemer)
    .readFrom(referenceInputs)
    .pay.ToContract(
      contracts.steps[2].spendingScriptAddress,
      { kind: "inline", value: outputDatum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [threadToken.unit]: 1n,
      },
    );
  const signed = await completeStep(tx, funding);
  if (layout === undefined) {
    throw withdrawnInputSubmitError("step-02 layout was not resolved.");
  }
  const transaction = finishStep(signed, {
    referenceScripts: [
      workflowReferenceScript({
        role: "V1 fraud-proof withdrawn-input step-02",
        utxo: referenceScriptUtxo,
        expectedScript: contracts.steps[1].spendingScript,
      }),
    ],
  });
  return {
    transaction,
    nextThreadOutRef: `${transaction.txHash}#${layout.outputIndex.toString()}`,
    thirdStepAddress: contracts.steps[2].spendingScriptAddress,
    withdrawnInput,
    computationThreadUnit: threadToken.unit,
    carriageTier: planned.plan.tier,
  };
};
