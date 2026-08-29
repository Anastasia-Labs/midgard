import {
  FraudProofComputationThreadRedeemer,
  FraudProofTokenDatum,
  FraudProofTokenMintRedeemer,
  hashBlockHeaderV1,
  HUB_ORACLE_ASSET_NAME,
  requireInputIndex,
  requireMintRedeemerIndex,
  requireOwnMintPurpose,
  requireOwnSpendPurpose,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
  type TransitionFaultProof,
  TransitionTraceFinalSpendRedeemer,
  TransitionTraceRouteSpendRedeemer,
  TransitionTraceStepDatum,
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
  requireDeploymentReferenceScript,
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
import {
  type FaultProofWitnessReferenceScriptsV1,
  witnessMintingPolicyCarriageV1,
} from "../witness-reference-scripts-v1.js";
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
  /** Published shared minting witnesses; each absent entry inline-attaches. */
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScriptsV1;
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
  readonly routeTxHash: string;
  readonly routeOutRef: string;
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

type TransitionTraceRouteSpendLayout = {
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
};

type TransitionTraceFinalSpendLayout = {
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
  readonly hubOracleRefInputIndex: bigint;
  readonly fraudProofMintRedeemerIndex: bigint;
};

type TransitionTraceFinalResolvedLayout = TransitionTraceFinalSpendLayout & {
  readonly computationThreadMintRedeemerIndex: bigint;
};

const TRANSITION_TRACE_FINAL_REFERENCE_SCRIPT_ENTRIES = [
  "fraudProofTransitionTraceControl",
  "fraudProofTransitionTraceSource",
  "fraudProofTransitionTraceWithdrawal",
  "fraudProofTransitionTraceForced",
  "fraudProofTransitionTraceAcceptedTransaction",
  "fraudProofTransitionTraceDeposit",
  "fraudProofTransitionTraceL1Event",
  "fraudProofTransitionTraceDuplicate",
] as const;

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

const makeTransitionTraceRouteSpendRedeemer = ({
  threadUtxo,
  routeAddress,
  routeDatum,
  computationThreadUnit,
  proof,
  onLayout,
}: {
  readonly threadUtxo: UTxO;
  readonly routeAddress: string;
  readonly routeDatum: string;
  readonly computationThreadUnit: string;
  readonly proof: TransitionFaultProof;
  readonly onLayout: (layout: TransitionTraceRouteSpendLayout) => void;
}): BuildTxWithRedeemer =>
  ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "transition-trace route");
    const layout: TransitionTraceRouteSpendLayout = {
      inputIndex: requireInputIndex(ctx, threadUtxo, "transition-trace route"),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        outputWithDatumAndUnitPredicate({
          address: routeAddress,
          datum: routeDatum,
          unit: computationThreadUnit,
        }),
        "transition-trace routed computation-thread output",
      ),
    };
    onLayout(layout);
    return Data.to(
      {
        Continue: [
          {
            input_index: layout.inputIndex,
            output_index: layout.outputIndex,
            proof,
          },
        ],
      },
      TransitionTraceRouteSpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

const makeTransitionTraceFinalSpendRedeemer = ({
  threadUtxo,
  hubOracleUtxo,
  fraudProofAddress,
  fraudProofPolicyId,
  fraudProofUnit,
  fraudProofDatum,
  onLayout,
}: {
  readonly threadUtxo: UTxO;
  readonly hubOracleUtxo: UTxO;
  readonly fraudProofAddress: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofUnit: string;
  readonly fraudProofDatum: string;
  readonly onLayout: (layout: TransitionTraceFinalSpendLayout) => void;
}): BuildTxWithRedeemer =>
  ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "transition-trace final proof");
    const layout: TransitionTraceFinalSpendLayout = {
      inputIndex: requireInputIndex(
        ctx,
        threadUtxo,
        "transition-trace final proof",
      ),
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
          },
        ],
      },
      TransitionTraceFinalSpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

const unreachableTransitionVariant = (value: never): never => {
  const variant =
    typeof value === "object" && value !== null
      ? Object.keys(value).join(",")
      : String(value);
  throw transitionTraceError(
    "submissionRejected",
    `Unsupported transition-trace proof variant: ${variant}`,
  );
};

export const transitionTraceFinalIndex = (
  proof: Pick<TransitionFaultProof, "fault">,
): number => {
  const { fault } = proof;
  if (
    "TraceBoundaryFault" in fault ||
    "TraceLinkFault" in fault ||
    "EventToStepMismatch" in fault ||
    "CountFault" in fault
  ) {
    return 0;
  }
  if ("SourceMembershipMismatch" in fault) {
    return 1;
  }
  if ("InvalidOneStepTransition" in fault) {
    const { witness } = fault.InvalidOneStepTransition;
    if (
      "ValidWithdrawalTransition" in witness ||
      "InvalidWithdrawalNoOpTransition" in witness
    ) {
      return 2;
    }
    if ("InvalidForcedTransactionNoOpTransition" in witness) {
      return 3;
    }
    if ("L2TransactionTransition" in witness) {
      return 4;
    }
    if ("ValidDepositTransition" in witness) {
      return 5;
    }
    return unreachableTransitionVariant(witness);
  }
  if ("AcceptedTransactionTransitionMismatch" in fault) {
    return 4;
  }
  if ("OmittedDueL1Event" in fault || "OutOfWindowSourceEvent" in fault) {
    return 6;
  }
  if ("DuplicateTraceEvent" in fault) {
    return 7;
  }
  return unreachableTransitionVariant(fault);
};

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
  witnessReferenceScripts,
  awaitConfirmation = true,
}: SubmitTransitionTraceProofConfig): Promise<SubmitTransitionTraceProofResult> => {
  const {
    deploymentInfo: parsedDeploymentInfo,
    transitionTraceCategory,
    hubOraclePolicyId,
    contracts,
  } = await resolveTransitionTraceDeploymentContracts({
    blueprint,
    deploymentInfo,
    network,
    requireFraudProofSpend: true,
  });
  const computedHeaderHash = await Effect.runPromise(
    hashBlockHeaderV1(proof.header),
  );
  if (computedHeaderHash !== proof.challenged_header_hash) {
    throw transitionTraceError(
      "submissionRejected",
      `Transition fault proof header hashes to ${computedHeaderHash}, but proof.challenged_header_hash is ${proof.challenged_header_hash}.`,
    );
  }

  const finalIndex = transitionTraceFinalIndex(proof);
  const [
    threadUtxo,
    hubOracleUtxo,
    routeReferenceScript,
    finalReferenceScript,
  ] = await Promise.all([
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
    requireDeploymentReferenceScript({
      lucid,
      deploymentInfo: parsedDeploymentInfo,
      name: "fraudProofTransitionTrace",
    }),
    requireDeploymentReferenceScript({
      lucid,
      deploymentInfo: parsedDeploymentInfo,
      name: TRANSITION_TRACE_FINAL_REFERENCE_SCRIPT_ENTRIES[finalIndex]!,
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
  const finalValidator = contracts.transitionTrace.finals[finalIndex]!;
  const routeDatum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: proof,
    },
    TransitionTraceStepDatum,
  );
  const routeAssets = {
    lovelace: threadUtxo.assets.lovelace ?? 0n,
    [threadToken.unit]: 1n,
  };
  let routeLayout: TransitionTraceRouteSpendLayout | undefined;
  const routeFeeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const routeTx = lucid
    .newTx()
    .collectFrom([routeFeeInput])
    .collectFrom(
      [threadUtxo],
      makeTransitionTraceRouteSpendRedeemer({
        threadUtxo,
        routeAddress: finalValidator.spendingScriptAddress,
        routeDatum,
        computationThreadUnit: threadToken.unit,
        proof,
        onLayout: (layout) => {
          routeLayout = layout;
        },
      }),
    )
    .readFrom([routeReferenceScript])
    .pay.ToContract(
      finalValidator.spendingScriptAddress,
      { kind: "inline", value: routeDatum },
      routeAssets,
    )
    .addSignerKey(signer.paymentKeyHash);
  const unsignedRoute = await routeTx.complete({ localUPLCEval: true });
  if (routeLayout === undefined) {
    throw transitionTraceError(
      "submissionRejected",
      "BuildTxWithRedeemer did not resolve transition-trace route layout.",
    );
  }
  const signedRoute = await unsignedRoute.sign.withWallet().complete();
  const routeTxHash = await signedRoute.submit();
  const routeOutRef = `${routeTxHash}#${routeLayout.outputIndex.toString()}`;
  // The final transaction must consume the exact authenticated router output.
  // Awaiting this internal hop also prevents providers from selecting a stale
  // initial thread UTxO.
  await lucid.awaitTx(routeTxHash, DEFAULT_CONFIRMATION_POLL_MS);
  const routedThreadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(routeOutRef, "transition-trace route out-ref"),
    label: "routed transition-trace computation-thread UTxO",
  });
  if (routedThreadUtxo.address !== finalValidator.spendingScriptAddress) {
    throw transitionTraceError(
      "submissionRejected",
      `Router output ${routeOutRef} is not locked at the selected transition-trace final validator.`,
    );
  }

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
    lovelace: routedThreadUtxo.assets.lovelace ?? 0n,
    [fraudProofUnit]: 1n,
  };
  let spendLayout: TransitionTraceFinalSpendLayout | undefined;
  let computationThreadMintRedeemerIndex: bigint | undefined;
  const computationThreadMintCarriage = witnessMintingPolicyCarriageV1({
    script: contracts.computationThread.mintingScript,
    referenceUtxo: witnessReferenceScripts?.computationThreadMint,
    label: "transition-trace computation-thread mint",
  });
  const fraudProofMintCarriage = witnessMintingPolicyCarriageV1({
    script: contracts.fraudProof.mintingScript,
    referenceUtxo: witnessReferenceScripts?.fraudProofMint,
    label: "transition-trace fraud-proof mint",
  });
  const referenceInputs = [
    hubOracleUtxo,
    finalReferenceScript,
    ...additionalReferenceInputs,
    ...computationThreadMintCarriage.referenceInputs,
    ...fraudProofMintCarriage.referenceInputs,
  ];

  const base = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom(
      [routedThreadUtxo],
      makeTransitionTraceFinalSpendRedeemer({
        threadUtxo: routedThreadUtxo,
        hubOracleUtxo,
        fraudProofAddress: contracts.fraudProof.spendingScriptAddress,
        fraudProofPolicyId: contracts.fraudProof.policyId,
        fraudProofUnit,
        fraudProofDatum,
        onLayout: (layout) => {
          spendLayout = layout;
        },
      }),
    )
    .readFrom(referenceInputs)
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
    .addSignerKey(signer.paymentKeyHash);
  const tx = fraudProofMintCarriage.attach(
    computationThreadMintCarriage.attach(base),
  );

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
  const resolvedLayout: TransitionTraceFinalResolvedLayout = {
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
    routeTxHash,
    routeOutRef,
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
    transitionTraceProofAddress: finalValidator.spendingScriptAddress,
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
