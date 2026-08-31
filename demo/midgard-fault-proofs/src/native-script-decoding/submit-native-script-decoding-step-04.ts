/**
 * `native-script-decoding` step-04 submitter (offchain plan §4.2): the
 * proven verdict finalizes — the computation-thread NFT burns and the
 * permanent fraud-proof token mints to the fraud-proof address under the
 * generic finalization validations.
 *
 * The validator's per-direction shape gates are re-checked locally first:
 * direction A needs a refusal class in {0, 1, 2}; direction B needs a
 * forced source, the class-0 contradiction marker, and an accused class in
 * the family's domain.
 */
import type { NativeScriptDecodingScanThreadStateV1 } from "@al-ft/midgard-sdk";
import {
  FraudProofComputationThreadRedeemer,
  FraudProofTokenDatum,
  FraudProofTokenMintRedeemer,
  NATIVE_SCRIPT_DECODING_DIRECTION_WRONGFUL_ACCEPTANCE_V1,
  NATIVE_SCRIPT_DECODING_DIRECTION_WRONGFUL_REJECTION_V1,
  NATIVE_SCRIPT_DECODING_REFUSAL_CLASS_DEPTH_LIMIT_V1,
  NATIVE_SCRIPT_DECODING_REFUSAL_CLASS_MALFORMED_V1,
  NATIVE_SCRIPT_DECODING_REFUSAL_CLASS_NODE_LIMIT_V1,
  NATIVE_SCRIPT_DECODING_SOURCE_KIND_FORCED_V1,
  NativeScriptDecodingStep04Datum,
  NativeScriptDecodingStep04SpendRedeemer,
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
  type PreparedClaimRegistryMutationV1,
  prepareFamilyClaimRegistryMutationV1,
  requirePreparedClaimRegistryMutationV1,
} from "../claim-registry-transaction-v1.js";
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
import type { NativeScriptDecodingContractsV1 } from "./contracts-v1.js";
import {
  nativeScriptDecodingStepLabelV1,
  nativeScriptDecodingSubmitError,
  requireNativeScriptDecodingReferenceScriptV1,
  requireNativeScriptDecodingStepStateV1,
  requireNativeScriptDecodingThreadUtxoV1,
} from "./submit-common-v1.js";

const STEP_LABEL = nativeScriptDecodingStepLabelV1(5);

const IN_DOMAIN_CLASSES: readonly bigint[] = [
  NATIVE_SCRIPT_DECODING_REFUSAL_CLASS_MALFORMED_V1,
  NATIVE_SCRIPT_DECODING_REFUSAL_CLASS_NODE_LIMIT_V1,
  NATIVE_SCRIPT_DECODING_REFUSAL_CLASS_DEPTH_LIMIT_V1,
];

/** Twin of the validator's per-direction closed-verdict shape gate. */
export const assertNativeScriptDecodingStep04FinalizableV1 = (
  state: NativeScriptDecodingScanThreadStateV1,
): void => {
  if (
    state.direction === NATIVE_SCRIPT_DECODING_DIRECTION_WRONGFUL_REJECTION_V1
  ) {
    if (state.source_kind !== NATIVE_SCRIPT_DECODING_SOURCE_KIND_FORCED_V1) {
      throw nativeScriptDecodingSubmitError(
        "direction B finalizes forced threads only: nothing but a forced leaf carries an explicit rejection to dispute.",
      );
    }
    if (
      state.refusal_class !== NATIVE_SCRIPT_DECODING_REFUSAL_CLASS_MALFORMED_V1
    ) {
      throw nativeScriptDecodingSubmitError(
        `direction B closes with the class-0 contradiction marker; the thread carries class ${state.refusal_class.toString()}.`,
      );
    }
    if (!IN_DOMAIN_CLASSES.includes(state.scan_reason_class)) {
      throw nativeScriptDecodingSubmitError(
        `the accused class ${state.scan_reason_class.toString()} is outside the family's {0, 1, 2} domain.`,
      );
    }
    return;
  }
  if (
    state.direction !== NATIVE_SCRIPT_DECODING_DIRECTION_WRONGFUL_ACCEPTANCE_V1
  ) {
    throw nativeScriptDecodingSubmitError(
      `thread state carries direction ${state.direction.toString()}, outside {0, 1}.`,
    );
  }
  if (!IN_DOMAIN_CLASSES.includes(state.refusal_class)) {
    throw nativeScriptDecodingSubmitError(
      `direction A finalizes a refusal class in {0, 1, 2}; the thread carries ${state.refusal_class.toString()}.`,
    );
  }
};

export type SubmitNativeScriptDecodingStep04Result = {
  readonly txHash: string;
  readonly walletSource: string;
  readonly proverAddress: string;
  readonly fraudProver: string;
  readonly threadOutRef: string;
  readonly fraudProofOutRef: string;
  readonly fraudulentHeaderHash: string;
  readonly computationThreadUnit: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofAssetName: string;
  readonly fraudProofUnit: string;
  readonly fraudProofAddress: string;
  /** The closed verdict the token finalized. */
  readonly scanState: NativeScriptDecodingScanThreadStateV1;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly computationThreadMintRedeemerIndex: number;
  readonly fraudProofMintRedeemerIndex: number;
  readonly awaitedConfirmation: boolean;
};

type Step04SpendLayout = {
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
  readonly fraudProofMintRedeemerIndex: bigint;
};

export const submitNativeScriptDecodingStep04 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  referenceScriptUtxo,
  witnessReferenceScripts,
  claimRegistryMutation,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: NativeScriptDecodingContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  /** Q3: the mandatory published step-04 reference script. */
  readonly referenceScriptUtxo: UTxO;
  /** Required published witness reference scripts for this transaction. */
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScriptsV1;
  readonly claimRegistryMutation?: PreparedClaimRegistryMutationV1;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitNativeScriptDecodingStep04Result> => {
  const { threadUtxo, threadToken } =
    await requireNativeScriptDecodingThreadUtxoV1({
      lucid,
      contracts,
      categoryId,
      stepIndex: 5,
      threadOutRef,
    });
  const state: NativeScriptDecodingScanThreadStateV1 =
    requireNativeScriptDecodingStepStateV1({
      threadUtxo,
      signer,
      schema: NativeScriptDecodingStep04Datum,
      stepIndex: 5,
    });
  assertNativeScriptDecodingStep04FinalizableV1(state);

  signer.selectWallet(lucid);
  const resolvedClaimRegistryMutation = requirePreparedClaimRegistryMutationV1({
    mutation:
      claimRegistryMutation ??
      (await prepareFamilyClaimRegistryMutationV1({
        lucid,
        claimRegistry: contracts.claimRegistry,
        claimRegistryReferenceUtxo: witnessReferenceScripts?.claimRegistrySpend,
        hubOraclePolicyId: contracts.hubOraclePolicyId,
        computationThreadPolicyId: contracts.computationThread.policyId,
        claimId: threadToken.assetName,
        kind: "close",
      })),
    kind: "close",
    claimId: threadToken.assetName,
    label: STEP_LABEL,
  });
  const feeInput = selectFeeInput(
    resolvedClaimRegistryMutation.referenceInputs.reduce<readonly UTxO[]>(
      (utxos, reference) => excludeUtxo(utxos, reference),
      await lucid.wallet().getUtxos(),
    ),
  );
  const fraudProofUnit = toUnit(
    contracts.fraudProof.policyId,
    threadToken.assetName,
  );
  const fraudProofDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash },
    FraudProofTokenDatum,
  );
  const fraudProofOutputMatches = outputWithDatumAndUnitPredicate({
    address: contracts.fraudProof.spendingScriptAddress,
    datum: fraudProofDatum,
    unit: fraudProofUnit,
  });
  let spendLayout: Step04SpendLayout | undefined;
  let computationThreadMintRedeemerIndex: bigint | undefined;

  const spendRedeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, STEP_LABEL);
    const layout: Step04SpendLayout = {
      inputIndex: requireInputIndex(ctx, threadUtxo, STEP_LABEL),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        fraudProofOutputMatches,
        `${STEP_LABEL} fraud-proof`,
      ),
      fraudProofMintRedeemerIndex: requireMintRedeemerIndex(
        ctx,
        contracts.fraudProof.policyId,
        `${STEP_LABEL} fraud-proof`,
      ),
    };
    spendLayout = layout;
    return Data.to(
      {
        Continue: [
          {
            input_index: layout.inputIndex,
            output_index: layout.outputIndex,
            fraud_proof_mint_redeemer_index: layout.fraudProofMintRedeemerIndex,
          },
        ],
      },
      NativeScriptDecodingStep04SpendRedeemer,
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
  const referenceInputs = [
    requireNativeScriptDecodingReferenceScriptV1({
      utxo: referenceScriptUtxo,
      expectedScriptHash: contracts.steps[5].spendingScriptHash,
      stepIndex: 5,
    }),
    ...computationThreadBurnCarriage.referenceInputs,
    ...fraudProofMintCarriage.referenceInputs,
  ];
  const base = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], spendRedeemer)
    .mintAssets({ [threadToken.unit]: -1n }, threadBurnRedeemer)
    .mintAssets({ [fraudProofUnit]: 1n }, fraudProofMintRedeemer)
    .pay.ToContract(
      contracts.fraudProof.spendingScriptAddress,
      { kind: "inline", value: fraudProofDatum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [fraudProofUnit]: 1n,
      },
    )
    .addSignerKey(signer.paymentKeyHash)
    .readFrom(referenceInputs);
  const tx = fraudProofMintCarriage.attach(
    computationThreadBurnCarriage.attach(
      resolvedClaimRegistryMutation.apply(base),
    ),
  );

  const unsigned = await tx.complete({ localUPLCEval: true });
  if (
    spendLayout === undefined ||
    computationThreadMintRedeemerIndex === undefined
  ) {
    throw nativeScriptDecodingSubmitError(
      "BuildTxWithRedeemer did not resolve the step-04 layout.",
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  const expectedTxHash = await reachFraudProofPreSubmitBoundaryV1({
    signed,
    referenceScripts: workflowReferenceScriptsUsedByTransactionV1({
      signed,
      candidates: [
        {
          role: "V1 fraud-proof native-script-decoding step-04",
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
        {
          role: "claim-registry spending",
          utxo: resolvedClaimRegistryMutation.referenceScriptUtxo,
          expectedScript: resolvedClaimRegistryMutation.registryScript,
        },
      ],
    }),
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expectedTxHash) {
    throw nativeScriptDecodingSubmitError(
      `step-04 provider returned ${txHash}, expected ${expectedTxHash}.`,
    );
  }
  if (awaitConfirmation) {
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }

  return {
    txHash,
    walletSource: signer.source,
    proverAddress: signer.address,
    fraudProver: signer.paymentKeyHash,
    threadOutRef,
    fraudProofOutRef: `${txHash}#${spendLayout.outputIndex.toString()}`,
    fraudulentHeaderHash: threadToken.fraudulentHeaderHash,
    computationThreadUnit: threadToken.unit,
    fraudProofPolicyId: contracts.fraudProof.policyId,
    fraudProofAssetName: threadToken.assetName,
    fraudProofUnit,
    fraudProofAddress: contracts.fraudProof.spendingScriptAddress,
    scanState: state,
    inputIndex: Number(spendLayout.inputIndex),
    outputIndex: Number(spendLayout.outputIndex),
    computationThreadMintRedeemerIndex: Number(
      computationThreadMintRedeemerIndex,
    ),
    fraudProofMintRedeemerIndex: Number(
      spendLayout.fraudProofMintRedeemerIndex,
    ),
    awaitedConfirmation: awaitConfirmation,
  };
};
