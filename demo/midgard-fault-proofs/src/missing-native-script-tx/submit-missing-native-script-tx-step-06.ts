import {
  type FieldOpeningV1,
  FraudProofComputationThreadRedeemer,
  FraudProofTokenDatum,
  FraudProofTokenMintRedeemer,
  MIDGARD_FIELD_INDEX_V1,
  MISSING_NATIVE_SCRIPT_TX_DIRECT_WITNESS_LIMIT_V1,
  missingNativeScriptIsAbsentV1,
  MissingNativeScriptTxStep06Datum,
  MissingNativeScriptTxStep06SpendRedeemer,
  type MissingNativeScriptTxStep06State,
  type NativeTxWitnessSetCompact,
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
import { outputWithDatumAndUnitPredicate } from "../tx-layout.js";
import {
  type FaultProofWitnessReferenceScriptsV1,
  witnessMintingPolicyCarriageV1,
} from "../witness-reference-scripts-v1.js";
import {
  type FraudProofPreSubmitBoundaryV1,
  reachFraudProofPreSubmitBoundaryV1,
  workflowReferenceScriptsUsedByTransactionV1,
} from "../workflow/transaction-boundary-v1.js";
import type { MissingNativeScriptTxContractsV1 } from "./contracts-v1.js";
import {
  missingNativeScriptTxStepLabelV1,
  missingNativeScriptTxSubmitError,
  requireMissingNativeScriptTxReferenceScriptV1,
  requireMissingNativeScriptTxStepStateV1,
  requireMissingNativeScriptTxThreadUtxoV1,
} from "./submit-common-v1.js";

const STEP_LABEL = missingNativeScriptTxStepLabelV1(5);

export type SubmitMissingNativeScriptTxStep06Result = {
  readonly txHash: string;
  readonly fraudProofOutRef: string;
  readonly fraudulentHeaderHash: string;
  readonly computationThreadUnit: string;
  readonly fraudProofUnit: string;
  readonly fraudProofAssetName: string;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly fraudProofMintRedeemerIndex: number;
  readonly computationThreadMintRedeemerIndex: number;
  readonly awaitedConfirmation: boolean;
};

export const submitMissingNativeScriptTxStep06 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  nativeTxCompactCbor,
  witnessSet,
  scriptTxWitsItems,
  publishCarriage = false,
  publishedCarriageUtxos,
  certificateUtxo,
  referenceScriptUtxo,
  witnessReferenceScripts,
  publicationPreSubmitBoundary,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: MissingNativeScriptTxContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly nativeTxCompactCbor: string;
  readonly witnessSet: NativeTxWitnessSetCompact;
  /** Complete canonical per-item encodings from field 6. */
  readonly scriptTxWitsItems: readonly Uint8Array[];
  readonly publishCarriage?: boolean;
  /** Pre-observed publications supplied by a journaled production action. */
  readonly publishedCarriageUtxos?: readonly UTxO[];
  /** Pre-observed tier-3 certificate supplied by its journaled mint action. */
  readonly certificateUtxo?: UTxO;
  readonly referenceScriptUtxo: UTxO;
  /** Required published witness reference scripts for this transaction. */
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScriptsV1;
  /** Durable subaction seam for each prerequisite field-carriage publication. */
  readonly publicationPreSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitMissingNativeScriptTxStep06Result> => {
  const { threadUtxo, threadToken } =
    await requireMissingNativeScriptTxThreadUtxoV1({
      lucid,
      contracts,
      categoryId,
      stepIndex: 5,
      threadOutRef,
    });
  const state: MissingNativeScriptTxStep06State =
    requireMissingNativeScriptTxStepStateV1({
      threadUtxo,
      signer,
      schema: MissingNativeScriptTxStep06Datum,
      stepIndex: 5,
    });
  if (state.phase !== "Ready") {
    throw missingNativeScriptTxSubmitError(
      "step-06 direct finalization requires the Ready phase.",
    );
  }
  if (
    scriptTxWitsItems.length > MISSING_NATIVE_SCRIPT_TX_DIRECT_WITNESS_LIMIT_V1
  ) {
    throw missingNativeScriptTxSubmitError(
      `field-6 carries ${scriptTxWitsItems.length.toString()} witnesses; direct finalization is bounded at ${MISSING_NATIVE_SCRIPT_TX_DIRECT_WITNESS_LIMIT_V1.toString()} and the staged 06→07→08 driver is required.`,
    );
  }
  if (
    !missingNativeScriptIsAbsentV1({
      scriptTxWitsItems,
      expectedMissingScriptHash: state.expected_missing_script_hash,
    })
  ) {
    throw missingNativeScriptTxSubmitError(
      "the accused native script is present in the authenticated field-6 preimage.",
    );
  }
  const planned = planFaultProofFieldOpeningV1({
    fieldIndex: MIDGARD_FIELD_INDEX_V1.scriptWitnesses,
    anchorTxId: state.bad_tx_id,
    nativeTxCompactCbor,
    itemCbors: scriptTxWitsItems,
    owner: signer.paymentKeyHash,
    publish: publishCarriage,
    witnessSet,
    anchorWitnessSetHash: state.bad_tx_witness_set_hash,
    label: `${STEP_LABEL} script witnesses`,
  });
  if (planned.plan.tier === "Certified") {
    throw missingNativeScriptTxSubmitError(
      "field-6 preimage requires tier-3 Certified carriage, but this count-consuming family is limited to tiers 1-2 (§8.3 erratum E2 limit 2).",
    );
  }
  signer.selectWallet(lucid);
  const carriageUtxos =
    publishedCarriageUtxos ??
    (await publishFaultProofFieldCarriageV1({
      lucid,
      signer,
      planned,
      publisherAddress: signer.address,
      label: `${STEP_LABEL} script witnesses`,
      preSubmitBoundary: publicationPreSubmitBoundary,
    }));
  const stepReference = requireMissingNativeScriptTxReferenceScriptV1({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[5].spendingScriptHash,
    stepIndex: 5,
  });
  const computationThreadBurnCarriage = witnessMintingPolicyCarriageV1({
    script: contracts.computationThread.mintingScript,
    referenceUtxo: witnessReferenceScripts?.computationThreadMint,
    label: `${STEP_LABEL} computation-thread burn`,
  });
  const fraudProofMintCarriage = witnessMintingPolicyCarriageV1({
    script: contracts.fraudProof.mintingScript,
    referenceUtxo: witnessReferenceScripts?.fraudProofMint,
    label: `${STEP_LABEL} fraud-proof mint`,
  });
  // The complete final reference-input set MUST stand before the field
  // opening derives its §8.7 indices (bug fc635c8f).
  const referenceInputs = [
    ...carriageUtxos,
    ...(certificateUtxo === undefined ? [] : [certificateUtxo]),
    stepReference,
    ...computationThreadBurnCarriage.referenceInputs,
    ...fraudProofMintCarriage.referenceInputs,
  ];
  const opening: FieldOpeningV1 = faultProofFieldOpeningV1({
    planned,
    referenceInputs,
    certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
    label: `${STEP_LABEL} script witnesses`,
  });
  const walletUtxos = await lucid.wallet().getUtxos();
  const usableWalletUtxos = [...carriageUtxos].reduce<readonly UTxO[]>(
    (utxos, carriage) => excludeUtxo(utxos, carriage),
    walletUtxos,
  );
  const feeInput = selectFeeInput(usableWalletUtxos);
  const fraudProofUnit = toUnit(
    contracts.fraudProof.policyId,
    threadToken.assetName,
  );
  const fraudProofDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash },
    FraudProofTokenDatum,
  );
  const outputMatches = outputWithDatumAndUnitPredicate({
    address: contracts.fraudProof.spendingScriptAddress,
    datum: fraudProofDatum,
    unit: fraudProofUnit,
  });
  let spendLayout:
    | {
        inputIndex: bigint;
        outputIndex: bigint;
        fraudProofMintRedeemerIndex: bigint;
      }
    | undefined;
  let computationThreadMintRedeemerIndex: bigint | undefined;
  const spendRedeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, STEP_LABEL);
    const resolved = {
      inputIndex: requireInputIndex(ctx, threadUtxo, STEP_LABEL),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        outputMatches,
        `${STEP_LABEL} fraud proof`,
      ),
      fraudProofMintRedeemerIndex: requireMintRedeemerIndex(
        ctx,
        contracts.fraudProof.policyId,
        `${STEP_LABEL} fraud-proof mint`,
      ),
    };
    spendLayout = resolved;
    return Data.to(
      {
        Continue: [
          {
            DirectFinalize: {
              input_index: resolved.inputIndex,
              output_index: resolved.outputIndex,
              fraud_proof_mint_redeemer_index:
                resolved.fraudProofMintRedeemerIndex,
              script_tx_wits_opening: opening,
            },
          },
        ],
      },
      MissingNativeScriptTxStep06SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const threadBurnRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      contracts.computationThread.policyId,
      `${STEP_LABEL} computation-thread burn`,
    );
    return Data.to(
      { Success: { burning_token_asset_name: threadToken.assetName } },
      FraudProofComputationThreadRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const fraudProofMintRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      contracts.fraudProof.policyId,
      `${STEP_LABEL} fraud-proof mint`,
    );
    computationThreadMintRedeemerIndex = requireMintRedeemerIndex(
      ctx,
      contracts.computationThread.policyId,
      `${STEP_LABEL} computation-thread burn`,
    );
    return Data.to(
      {
        computation_thread_token_asset_name: threadToken.assetName,
        computation_thread_mint_redeemer_index:
          computationThreadMintRedeemerIndex,
      },
      FraudProofTokenMintRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const withInputs = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], spendRedeemer)
    .mintAssets({ [threadToken.unit]: -1n }, threadBurnRedeemer)
    .mintAssets({ [fraudProofUnit]: 1n }, fraudProofMintRedeemer);
  const paid = withInputs
    .readFrom(referenceInputs)
    .pay.ToContract(
      contracts.fraudProof.spendingScriptAddress,
      { kind: "inline", value: fraudProofDatum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [fraudProofUnit]: 1n,
      },
    )
    .addSignerKey(signer.paymentKeyHash);
  const completed = fraudProofMintCarriage.attach(
    computationThreadBurnCarriage.attach(paid),
  );
  const unsigned = await completed.complete({
    localUPLCEval: true,
    ...(carriageUtxos.length === 0
      ? {}
      : { presetWalletInputs: usableWalletUtxos as UTxO[] }),
  });
  if (
    spendLayout === undefined ||
    computationThreadMintRedeemerIndex === undefined
  ) {
    throw missingNativeScriptTxSubmitError(
      "BuildTxWithRedeemer did not resolve step-06 layout.",
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  const expectedTxHash = await reachFraudProofPreSubmitBoundaryV1({
    signed,
    referenceScripts: workflowReferenceScriptsUsedByTransactionV1({
      signed,
      candidates: [
        {
          role: "V1 fraud-proof missing-native-script-tx step-06",
          utxo: referenceScriptUtxo,
          expectedScript: contracts.steps[5].spendingScript,
        },
        {
          role: "V1 fraud-proof computation-thread minting",
          utxo: witnessReferenceScripts?.computationThreadMint,
          expectedScript: contracts.computationThread.mintingScript,
        },
        {
          role: "V1 fraud-proof token minting",
          utxo: witnessReferenceScripts?.fraudProofMint,
          expectedScript: contracts.fraudProof.mintingScript,
        },
      ],
    }),
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expectedTxHash) {
    throw missingNativeScriptTxSubmitError(
      `step-06 provider returned ${txHash}, expected ${expectedTxHash}.`,
    );
  }
  if (awaitConfirmation) {
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }
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
    computationThreadMintRedeemerIndex: Number(
      computationThreadMintRedeemerIndex,
    ),
    awaitedConfirmation: awaitConfirmation,
  };
};
