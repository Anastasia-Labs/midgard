import {
  FraudProofComputationThreadRedeemer,
  FraudProofTokenDatum,
  FraudProofTokenMintRedeemer,
  hashBlockHeader,
  HUB_ORACLE_ASSET_NAME,
  requireInputIndex,
  requireMintRedeemerIndex,
  requireOwnMintPurpose,
  requireOwnSpendPurpose,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
  type TransitionFaultProof,
  TransitionTraceProofSpendRedeemer,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  credentialToAddress,
  Data,
  type LucidEvolution,
  type Network,
  scriptHashToCredential,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  DEFAULT_CONFIRMATION_POLL_MS,
  fetchUtxoByOutRef,
  makeLucidForSubmit,
  outRefLabel,
  parseOutRef,
  readJsonFile,
  requireSingletonUtxo,
  type ResolvedProverSigner,
  resolveProverSigner,
  resolveTransitionTraceDeploymentContracts,
  type SubmitProviderConfig,
} from "../runtime.js";
import {
  requireComputationThreadToken,
  requireInitialStepDatum,
  selectFeeInput,
} from "../submit-step-01.js";
import { outputWithDatumAndUnitPredicate } from "../tx-layout.js";
import { transitionTraceError } from "./errors.js";

export type SubmitTransitionTraceProofConfig = {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly proof: TransitionFaultProof;
  readonly additionalReferenceInputs?: readonly UTxO[];
  readonly awaitConfirmation?: boolean;
};

export type SubmitTransitionTraceProofFromFilesConfig = SubmitProviderConfig & {
  readonly blueprintPath: string;
  readonly deploymentInfoPath: string;
  readonly walletSeedPhrase?: string;
  readonly walletSeedPhraseEnv?: string;
  readonly walletPrivateKey?: string;
  readonly walletPrivateKeyEnv?: string;
  readonly threadOutRef: string;
  readonly proof: TransitionFaultProof;
  readonly awaitConfirmation?: boolean;
};

export type SubmitTransitionTraceProofResult = {
  readonly txHash: string;
  readonly walletSource: string;
  readonly proverAddress: string;
  readonly fraudProver: string;
  readonly threadOutRef: string;
  readonly fraudProofOutRef: string;
  readonly fraudulentHeaderHash: string;
  readonly computationThreadPolicyId: string;
  readonly computationThreadAssetName: string;
  readonly computationThreadUnit: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofAssetName: string;
  readonly fraudProofUnit: string;
  readonly fraudProofAddress: string;
  readonly transitionTraceProofAddress: string;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly hubOracleRefInputIndex: number;
  readonly computationThreadMintRedeemerIndex: number;
  readonly fraudProofMintRedeemerIndex: number;
  readonly awaitedConfirmation: boolean;
};

type TransitionTraceProofSpendLayout = {
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
  readonly hubOracleRefInputIndex: bigint;
  readonly fraudProofMintRedeemerIndex: bigint;
};

type TransitionTraceProofResolvedLayout = TransitionTraceProofSpendLayout & {
  readonly computationThreadMintRedeemerIndex: bigint;
};

const fraudProofOutputPredicate = ({
  fraudProofAddress,
  fraudProofUnit,
  fraudProofDatum,
}: {
  readonly fraudProofAddress: string;
  readonly fraudProofUnit: string;
  readonly fraudProofDatum: string;
}) =>
  outputWithDatumAndUnitPredicate({
    address: fraudProofAddress,
    datum: fraudProofDatum,
    unit: fraudProofUnit,
  });

const makeTransitionTraceProofSpendRedeemer = ({
  threadUtxo,
  hubOracleUtxo,
  fraudProofAddress,
  fraudProofPolicyId,
  fraudProofUnit,
  fraudProofDatum,
  proof,
  onLayout,
}: {
  readonly threadUtxo: UTxO;
  readonly hubOracleUtxo: UTxO;
  readonly fraudProofAddress: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofUnit: string;
  readonly fraudProofDatum: string;
  readonly proof: TransitionFaultProof;
  readonly onLayout: (layout: TransitionTraceProofSpendLayout) => void;
}): BuildTxWithRedeemer =>
  ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "transition-trace proof");
    const layout: TransitionTraceProofSpendLayout = {
      inputIndex: requireInputIndex(ctx, threadUtxo, "transition-trace proof"),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        fraudProofOutputPredicate({
          fraudProofAddress,
          fraudProofUnit,
          fraudProofDatum,
        }),
        "transition-trace fraud-proof output",
      ),
      hubOracleRefInputIndex: requireReferenceInputIndex(
        ctx,
        hubOracleUtxo,
        "transition-trace proof hub oracle",
      ),
      fraudProofMintRedeemerIndex: requireMintRedeemerIndex(
        ctx,
        fraudProofPolicyId,
        "transition-trace fraud-proof mint",
      ),
    };
    onLayout(layout);
    return Data.to(
      {
        Continue: [
          {
            input_index: layout.inputIndex,
            output_index: layout.outputIndex,
            hub_ref_input_index: layout.hubOracleRefInputIndex,
            fraud_proof_mint_redeemer_index: layout.fraudProofMintRedeemerIndex,
            proof,
          },
        ],
      },
      TransitionTraceProofSpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

const makeFraudProofMintRedeemer = ({
  fraudProofPolicyId,
  computationThreadPolicyId,
  computationThreadAssetName,
  onComputationThreadMintRedeemerIndex,
}: {
  readonly fraudProofPolicyId: string;
  readonly computationThreadPolicyId: string;
  readonly computationThreadAssetName: string;
  readonly onComputationThreadMintRedeemerIndex: (index: bigint) => void;
}): BuildTxWithRedeemer =>
  ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      fraudProofPolicyId,
      "transition-trace fraud-proof mint",
    );
    const computationThreadMintRedeemerIndex = requireMintRedeemerIndex(
      ctx,
      computationThreadPolicyId,
      "transition-trace computation-thread burn",
    );
    onComputationThreadMintRedeemerIndex(computationThreadMintRedeemerIndex);
    return Data.to(
      {
        computation_thread_token_asset_name: computationThreadAssetName,
        computation_thread_mint_redeemer_index:
          computationThreadMintRedeemerIndex,
      },
      FraudProofTokenMintRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

const makeComputationThreadSuccessRedeemer = ({
  computationThreadPolicyId,
  computationThreadAssetName,
}: {
  readonly computationThreadPolicyId: string;
  readonly computationThreadAssetName: string;
}): BuildTxWithRedeemer =>
  ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      computationThreadPolicyId,
      "transition-trace computation-thread burn",
    );
    return Data.to(
      {
        Success: { burning_token_asset_name: computationThreadAssetName },
      },
      FraudProofComputationThreadRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

export const submitTransitionTraceProof = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  proof,
  additionalReferenceInputs = [],
  awaitConfirmation = true,
}: SubmitTransitionTraceProofConfig): Promise<SubmitTransitionTraceProofResult> => {
  const { transitionTraceCategory, hubOraclePolicyId, contracts } =
    await resolveTransitionTraceDeploymentContracts({
      blueprint,
      deploymentInfo,
      network,
      requireFraudProofSpend: true,
    });
  const computedHeaderHash = await Effect.runPromise(
    hashBlockHeader(proof.header),
  );
  if (computedHeaderHash !== proof.challenged_header_hash) {
    throw transitionTraceError(
      "submissionRejected",
      `Transition fault proof header hashes to ${computedHeaderHash}, but proof.challenged_header_hash is ${proof.challenged_header_hash}.`,
    );
  }

  const [threadUtxo, hubOracleUtxo] = await Promise.all([
    fetchUtxoByOutRef({
      lucid,
      outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
      label: "transition-trace computation-thread UTxO",
    }),
    requireSingletonUtxo({
      lucid,
      address: credentialToAddress(
        network,
        scriptHashToCredential(hubOraclePolicyId),
      ),
      unit: toUnit(hubOraclePolicyId, HUB_ORACLE_ASSET_NAME),
      label: "hub oracle",
    }),
  ]);
  if (
    threadUtxo.address !==
    contracts.transitionTrace.firstStep.spendingScriptAddress
  ) {
    throw transitionTraceError(
      "submissionRejected",
      `Thread UTxO ${outRefLabel(
        threadUtxo,
      )} is not locked at the transition-trace proof validator.`,
    );
  }
  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: transitionTraceCategory.categoryId,
    categoryLabel: "transition-trace",
  });
  requireInitialStepDatum({ threadUtxo, signer });
  if (threadToken.fraudulentHeaderHash !== proof.challenged_header_hash) {
    throw transitionTraceError(
      "submissionRejected",
      `Transition proof challenges header ${proof.challenged_header_hash}, but thread token challenges ${threadToken.fraudulentHeaderHash}.`,
    );
  }

  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const fraudProofUnit = toUnit(
    contracts.fraudProof.policyId,
    threadToken.assetName,
  );
  const fraudProofDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash },
    FraudProofTokenDatum,
  );
  const fraudProofAssets = {
    lovelace: threadUtxo.assets.lovelace ?? 0n,
    [fraudProofUnit]: 1n,
  };
  let spendLayout: TransitionTraceProofSpendLayout | undefined;
  let computationThreadMintRedeemerIndex: bigint | undefined;

  const tx = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom(
      [threadUtxo],
      makeTransitionTraceProofSpendRedeemer({
        threadUtxo,
        hubOracleUtxo,
        fraudProofAddress: contracts.fraudProof.spendingScriptAddress,
        fraudProofPolicyId: contracts.fraudProof.policyId,
        fraudProofUnit,
        fraudProofDatum,
        proof,
        onLayout: (layout) => {
          spendLayout = layout;
        },
      }),
    )
    .readFrom([hubOracleUtxo, ...additionalReferenceInputs])
    .mintAssets(
      { [threadToken.unit]: -1n },
      makeComputationThreadSuccessRedeemer({
        computationThreadPolicyId: contracts.computationThread.policyId,
        computationThreadAssetName: threadToken.assetName,
      }),
    )
    .mintAssets(
      { [fraudProofUnit]: 1n },
      makeFraudProofMintRedeemer({
        fraudProofPolicyId: contracts.fraudProof.policyId,
        computationThreadPolicyId: contracts.computationThread.policyId,
        computationThreadAssetName: threadToken.assetName,
        onComputationThreadMintRedeemerIndex: (index) => {
          computationThreadMintRedeemerIndex = index;
        },
      }),
    )
    .pay.ToContract(
      contracts.fraudProof.spendingScriptAddress,
      { kind: "inline", value: fraudProofDatum },
      fraudProofAssets,
    )
    .addSignerKey(signer.paymentKeyHash)
    .attach.SpendingValidator(
      contracts.transitionTrace.firstStep.spendingScript,
    )
    .attach.MintingPolicy(contracts.computationThread.mintingScript)
    .attach.MintingPolicy(contracts.fraudProof.mintingScript);

  const unsigned = await tx.complete({ localUPLCEval: true });
  if (
    spendLayout === undefined ||
    computationThreadMintRedeemerIndex === undefined
  ) {
    throw transitionTraceError(
      "submissionRejected",
      "BuildTxWithRedeemer did not resolve transition-trace proof layout.",
    );
  }
  const resolvedLayout: TransitionTraceProofResolvedLayout = {
    ...spendLayout,
    computationThreadMintRedeemerIndex,
  };
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  if (awaitConfirmation) {
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }
  return {
    txHash,
    walletSource: signer.source,
    proverAddress: signer.address,
    fraudProver: signer.paymentKeyHash,
    threadOutRef,
    fraudProofOutRef: `${txHash}#${resolvedLayout.outputIndex.toString()}`,
    fraudulentHeaderHash: proof.challenged_header_hash,
    computationThreadPolicyId: contracts.computationThread.policyId,
    computationThreadAssetName: threadToken.assetName,
    computationThreadUnit: threadToken.unit,
    fraudProofPolicyId: contracts.fraudProof.policyId,
    fraudProofAssetName: threadToken.assetName,
    fraudProofUnit,
    fraudProofAddress: contracts.fraudProof.spendingScriptAddress,
    transitionTraceProofAddress:
      contracts.transitionTrace.firstStep.spendingScriptAddress,
    inputIndex: Number(resolvedLayout.inputIndex),
    outputIndex: Number(resolvedLayout.outputIndex),
    hubOracleRefInputIndex: Number(resolvedLayout.hubOracleRefInputIndex),
    computationThreadMintRedeemerIndex: Number(
      resolvedLayout.computationThreadMintRedeemerIndex,
    ),
    fraudProofMintRedeemerIndex: Number(
      resolvedLayout.fraudProofMintRedeemerIndex,
    ),
    awaitedConfirmation: awaitConfirmation,
  };
};

export const submitTransitionTraceProofFromFiles = async (
  config: SubmitTransitionTraceProofFromFilesConfig,
): Promise<SubmitTransitionTraceProofResult> => {
  const [blueprint, deploymentInfo, lucid] = await Promise.all([
    readJsonFile(config.blueprintPath),
    readJsonFile(config.deploymentInfoPath),
    makeLucidForSubmit(config),
  ]);
  return await submitTransitionTraceProof({
    lucid,
    blueprint,
    deploymentInfo,
    network: config.network,
    signer: resolveProverSigner(config),
    threadOutRef: config.threadOutRef,
    proof: config.proof,
    awaitConfirmation: config.awaitConfirmation,
  });
};
