import {
  FraudProofComputationThreadRedeemer,
  FraudProofTokenDatum,
  FraudProofTokenMintRedeemer,
  requireInputIndex,
  requireMintRedeemerIndex,
  requireOwnMintPurpose,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type RedeemerContext,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  type LinearFaultStepContract,
  linearFaultStepLabel,
  requireLinearFaultReferenceScript,
} from "./linear-fault-family.js";
import {
  completeStep,
  fundStep,
  type StepSigner,
  submitStep,
} from "./step-completion.js";
import { outputWithDatumAndUnitPredicate } from "./tx-layout.js";
import {
  type FaultProofWitnessReferenceScripts,
  witnessMintingPolicyCarriage,
} from "./witness-reference-scripts.js";
import {
  type FraudProofPreSubmitBoundary,
  workflowReferenceScript,
} from "./workflow/transaction-boundary.js";

export const submitLinearFaultFinalize = async ({
  lucid,
  family,
  stepIndex,
  step,
  computationThread,
  fraudProof,
  signer,
  threadUtxo,
  threadToken,
  spendRedeemerSchema,
  buildFamilyArgs,
  referenceScriptUtxo,
  carriageUtxos = [],
  extraReferenceInputs = [],
  witnessReferenceScripts,
  preSubmitBoundary,
  awaitConfirmation,
}: {
  readonly lucid: LucidEvolution;
  readonly family: string;
  readonly stepIndex: number;
  readonly step: LinearFaultStepContract;
  readonly computationThread: {
    readonly policyId: string;
    readonly mintingScript: NonNullable<UTxO["scriptRef"]>;
  };
  readonly fraudProof: {
    readonly policyId: string;
    readonly mintingScript: NonNullable<UTxO["scriptRef"]>;
    readonly spendingScriptAddress: string;
  };
  readonly signer: StepSigner;
  readonly threadUtxo: UTxO;
  readonly threadToken: {
    readonly unit: string;
    readonly assetName: string;
    readonly fraudulentHeaderHash: string;
  };
  readonly spendRedeemerSchema: Parameters<typeof Data.to>[1];
  /**
   * The family arguments, given the resolved layout and the builder context
   * any further positional index must be read from.
   */
  readonly buildFamilyArgs: (
    layout: {
      readonly inputIndex: bigint;
      readonly outputIndex: bigint;
      readonly fraudProofMintRedeemerIndex: bigint;
    },
    ctx: RedeemerContext,
  ) => Readonly<Record<string, unknown>>;
  readonly referenceScriptUtxo: UTxO;
  readonly carriageUtxos?: readonly UTxO[];
  readonly extraReferenceInputs?: readonly UTxO[];
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScripts;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation: boolean;
}) => {
  const label = linearFaultStepLabel(family, stepIndex);
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: step.spendingScriptHash,
    family,
    stepIndex,
  });
  const threadBurn = witnessMintingPolicyCarriage({
    script: computationThread.mintingScript,
    referenceUtxo: witnessReferenceScripts?.computationThreadMint,
    label: `${label} thread burn`,
  });
  const proofMint = witnessMintingPolicyCarriage({
    script: fraudProof.mintingScript,
    referenceUtxo: witnessReferenceScripts?.fraudProofMint,
    label: `${label} proof mint`,
  });
  const referenceInputs = [
    ...carriageUtxos,
    stepReference,
    ...extraReferenceInputs,
    ...threadBurn.referenceInputs,
    ...proofMint.referenceInputs,
  ];
  const fraudProofUnit = toUnit(fraudProof.policyId, threadToken.assetName);
  const fraudProofDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash },
    FraudProofTokenDatum,
  );
  const outputMatches = outputWithDatumAndUnitPredicate({
    address: fraudProof.spendingScriptAddress,
    datum: fraudProofDatum,
    unit: fraudProofUnit,
  });
  let spendLayout:
    | {
        readonly inputIndex: bigint;
        readonly outputIndex: bigint;
        readonly fraudProofMintRedeemerIndex: bigint;
      }
    | undefined;
  let threadMintRedeemerIndex: bigint | undefined;
  const spendRedeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, label);
    spendLayout = {
      inputIndex: requireInputIndex(ctx, threadUtxo, label),
      outputIndex: requireUniqueOutputIndex(ctx.outputs, outputMatches, label),
      fraudProofMintRedeemerIndex: requireMintRedeemerIndex(
        ctx,
        fraudProof.policyId,
        `${label} proof mint`,
      ),
    };
    return Data.to(
      { Continue: [buildFamilyArgs(spendLayout, ctx)] },
      spendRedeemerSchema,
    );
  }) satisfies BuildTxWithRedeemer;
  const threadBurnRedeemer = ((ctx) => {
    requireOwnMintPurpose(ctx, computationThread.policyId, label);
    return Data.to(
      { Success: { burning_token_asset_name: threadToken.assetName } },
      FraudProofComputationThreadRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const proofMintRedeemer = ((ctx) => {
    requireOwnMintPurpose(ctx, fraudProof.policyId, label);
    threadMintRedeemerIndex = requireMintRedeemerIndex(
      ctx,
      computationThread.policyId,
      label,
    );
    return Data.to(
      {
        computation_thread_token_asset_name: threadToken.assetName,
        computation_thread_mint_redeemer_index: threadMintRedeemerIndex,
      },
      FraudProofTokenMintRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const funding = await fundStep(lucid, { signer, excluding: carriageUtxos });
  const signed = await completeStep(
    proofMint.attach(
      threadBurn.attach(
        funding.tx
          .collectFrom([threadUtxo], spendRedeemer)
          .readFrom(referenceInputs)
          .mintAssets({ [threadToken.unit]: -1n }, threadBurnRedeemer)
          .mintAssets({ [fraudProofUnit]: 1n }, proofMintRedeemer)
          .pay.ToContract(
            fraudProof.spendingScriptAddress,
            { kind: "inline", value: fraudProofDatum },
            {
              lovelace: threadUtxo.assets.lovelace ?? 0n,
              [fraudProofUnit]: 1n,
            },
          ),
      ),
    ),
    funding,
  );
  if (spendLayout === undefined || threadMintRedeemerIndex === undefined) {
    throw new Error(`${label}: unresolved final layout`);
  }
  const txHash = await submitStep(lucid, signed, {
    referenceScripts: [
      workflowReferenceScript({
        role: label,
        utxo: stepReference,
        expectedScript: step.spendingScript,
      }),
      workflowReferenceScript({
        role: `${label}-thread-burn`,
        utxo: witnessReferenceScripts?.computationThreadMint,
        expectedScript: computationThread.mintingScript,
      }),
      workflowReferenceScript({
        role: `${label}-proof-mint`,
        utxo: witnessReferenceScripts?.fraudProofMint,
        expectedScript: fraudProof.mintingScript,
      }),
    ],
    boundary: preSubmitBoundary,
    awaitConfirmation,
  });
  return {
    txHash,
    fraudProofOutRef: `${txHash}#${spendLayout.outputIndex.toString()}`,
    fraudulentHeaderHash: threadToken.fraudulentHeaderHash,
    computationThreadUnit: threadToken.unit,
    fraudProofUnit,
    fraudProofAssetName: threadToken.assetName,
    inputIndex: Number(spendLayout.inputIndex),
    outputIndex: Number(spendLayout.outputIndex),
    fraudProofMintRedeemerIndex: Number(
      spendLayout.fraudProofMintRedeemerIndex,
    ),
    computationThreadMintRedeemerIndex: Number(threadMintRedeemerIndex),
  };
};
