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
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  type LinearFaultStepContractV1,
  linearFaultStepLabelV1,
  requireLinearFaultReferenceScriptV1,
} from "./linear-fault-family-v1.js";
import { DEFAULT_CONFIRMATION_POLL_MS } from "./runtime.js";
import { excludeUtxo } from "./spend-input-witness.js";
import { selectFeeInput } from "./submit-step-01.js";
import { outputWithDatumAndUnitPredicate } from "./tx-layout.js";
import {
  type FaultProofWitnessReferenceScriptsV1,
  witnessMintingPolicyCarriageV1,
} from "./witness-reference-scripts-v1.js";
import {
  type FraudProofPreSubmitBoundaryV1,
  reachFraudProofPreSubmitBoundaryV1,
  workflowReferenceScriptV1,
} from "./workflow/transaction-boundary-v1.js";

export const submitLinearFaultFinalizeV1 = async ({
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
  readonly step: LinearFaultStepContractV1;
  readonly computationThread: {
    readonly policyId: string;
    readonly mintingScript: NonNullable<UTxO["scriptRef"]>;
  };
  readonly fraudProof: {
    readonly policyId: string;
    readonly mintingScript: NonNullable<UTxO["scriptRef"]>;
    readonly spendingScriptAddress: string;
  };
  readonly signer: {
    readonly paymentKeyHash: string;
    readonly selectWallet: (lucid: LucidEvolution) => void;
  };
  readonly threadUtxo: UTxO;
  readonly threadToken: {
    readonly unit: string;
    readonly assetName: string;
    readonly fraudulentHeaderHash: string;
  };
  readonly spendRedeemerSchema: Parameters<typeof Data.to>[1];
  readonly buildFamilyArgs: (layout: {
    readonly inputIndex: bigint;
    readonly outputIndex: bigint;
    readonly fraudProofMintRedeemerIndex: bigint;
  }) => Readonly<Record<string, unknown>>;
  readonly referenceScriptUtxo: UTxO;
  readonly carriageUtxos?: readonly UTxO[];
  readonly extraReferenceInputs?: readonly UTxO[];
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScriptsV1;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation: boolean;
}) => {
  const label = linearFaultStepLabelV1(family, stepIndex);
  signer.selectWallet(lucid);
  const stepReference = requireLinearFaultReferenceScriptV1({
    utxo: referenceScriptUtxo,
    expectedScriptHash: step.spendingScriptHash,
    family,
    stepIndex,
  });
  const threadBurn = witnessMintingPolicyCarriageV1({
    script: computationThread.mintingScript,
    referenceUtxo: witnessReferenceScripts?.computationThreadMint,
    label: `${label} thread burn`,
  });
  const proofMint = witnessMintingPolicyCarriageV1({
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
  const walletUtxos = await lucid.wallet().getUtxos();
  const nonCarriageWalletUtxos = carriageUtxos.reduce<readonly UTxO[]>(
    (utxos, carriage) => excludeUtxo(utxos, carriage),
    walletUtxos,
  );
  const usableWalletUtxos = nonCarriageWalletUtxos;
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
      { Continue: [buildFamilyArgs(spendLayout)] },
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
  const unsigned = await proofMint
    .attach(
      threadBurn.attach(
        lucid
          .newTx()
          .collectFrom([selectFeeInput(usableWalletUtxos)])
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
          )
          .addSignerKey(signer.paymentKeyHash),
      ),
    )
    .complete({
      localUPLCEval: true,
      ...(carriageUtxos.length === 0
        ? {}
        : { presetWalletInputs: usableWalletUtxos as UTxO[] }),
    });
  if (spendLayout === undefined || threadMintRedeemerIndex === undefined) {
    throw new Error(`${label}: unresolved final layout`);
  }
  const signed = await unsigned.sign.withWallet().complete();
  const expectedTxHash = await reachFraudProofPreSubmitBoundaryV1({
    signed,
    referenceScripts: [
      workflowReferenceScriptV1({
        role: label,
        utxo: stepReference,
        expectedScript: step.spendingScript,
      }),
      workflowReferenceScriptV1({
        role: `${label}-thread-burn`,
        utxo: witnessReferenceScripts?.computationThreadMint,
        expectedScript: computationThread.mintingScript,
      }),
      workflowReferenceScriptV1({
        role: `${label}-proof-mint`,
        utxo: witnessReferenceScripts?.fraudProofMint,
        expectedScript: fraudProof.mintingScript,
      }),
    ],
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expectedTxHash) throw new Error(`${label}: hash mismatch`);
  if (awaitConfirmation)
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
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
