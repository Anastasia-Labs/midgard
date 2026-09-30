import {
  FraudProofTokenDatum,
  hashBlockHeader,
  HUB_ORACLE_ASSET_NAME,
  HubOracleDatum,
} from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  Data,
  scriptHashToCredential,
  toUnit,
  validatorToRewardAddress,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  DEFAULT_CONFIRMATION_POLL_MS,
  fetchUtxoByOutRef,
  outRefLabel,
  parseOutRef,
  requireDeploymentReferenceScript,
  requireSingletonUtxo,
  resolveTransitionTraceDeploymentContracts,
} from "../runtime.js";
import {
  requireComputationThreadToken,
  requireInitialStepDatum,
  selectFeeInput,
} from "../step-support.js";
import { witnessMintingPolicyCarriage } from "../witness-reference-scripts.js";
import { transitionTraceError } from "./errors.js";
import {
  resolveTransitionTraceProofCarriage,
  transitionTraceTimedProofNeedsChunks,
} from "./proof-carriage.js";
import { readTransitionProof } from "./proof-material.js";
import {
  makeComputationThreadSuccessRedeemer,
  makeFraudProofMintRedeemer,
  makeTransitionTraceFinalSpendRedeemer,
  transitionTraceFinalIndex,
} from "./submit.make-transition-trace-final-spend-redeemer.js";
import {
  makeTransitionTraceRouteSpendRedeemer,
  type SubmitTransitionTraceProofConfig,
  type SubmitTransitionTraceProofResult,
  TRANSITION_TRACE_FINAL_REFERENCE_SCRIPT_ENTRIES,
  type TransitionTraceFinalResolvedLayout,
  type TransitionTraceFinalSpendLayout,
  type TransitionTraceRouteSpendLayout,
} from "./submit.make-transition-trace-route-spend-redeemer.js";
import { submitTransitionTraceFinal } from "./submit.submit-transition-trace-final.js";
import { transitionTraceRouteDatum } from "./submit.transition-trace-route-datum.js";
import { transitionTraceYieldData } from "./yield-data.js";
import { TRANSITION_TRACE_YIELD_REFERENCES } from "./yield-references.js";

export const submitTransitionTraceProof = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  proof: proofInput,
  additionalReferenceInputs = [],
  depositOpening,
  witnessReferenceScripts,
  awaitConfirmation = true,
}: SubmitTransitionTraceProofConfig): Promise<SubmitTransitionTraceProofResult> => {
  const proof = readTransitionProof(proofInput);
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
    hashBlockHeader(proof.header),
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
  const proofReferences =
    finalIndex === 4 ||
    finalIndex === 5 ||
    transitionTraceTimedProofNeedsChunks(proofInput)
      ? await resolveTransitionTraceProofCarriage({
          lucid,
          proof: proofInput,
          publish: true,
        })
      : [];
  const routeDatum = transitionTraceRouteDatum(
    proofInput,
    signer.paymentKeyHash,
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
        proofReferences,
        proof: proofInput,
        onLayout: (layout) => {
          routeLayout = layout;
        },
      }),
    )
    .readFrom([routeReferenceScript, ...proofReferences])
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

  if (finalIndex === 4 || finalIndex === 5 || finalIndex === 6) {
    const result = await submitTransitionTraceFinal({
      lucid,
      blueprint,
      deploymentInfo,
      network,
      signer,
      threadOutRef: routeOutRef,
      proof: proofInput,
      depositOpening,
      additionalReferenceInputs,
      witnessReferenceScripts,
      awaitConfirmation,
    });
    return {
      ...result,
      routeTxHash,
      routeOutRef,
      walletSource: signer.source,
      proverAddress: signer.address,
      fraudProver: signer.paymentKeyHash,
      threadOutRef,
      computationThreadPolicyId: contracts.computationThread.policyId,
      computationThreadAssetName: threadToken.assetName,
      fraudProofPolicyId: contracts.fraudProof.policyId,
      fraudProofAssetName: threadToken.assetName,
      fraudProofAddress: contracts.fraudProof.spendingScriptAddress,
      transitionTraceProofAddress: finalValidator.spendingScriptAddress,
    };
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
  if (hubOracleUtxo.datum == null)
    throw new Error("transition-trace hub oracle omitted datum");
  const semanticYields = await Promise.all(
    transitionTraceYieldData({
      proof: proofInput,
      network,
      depositOpening,
      depositPolicyId: Data.from(hubOracleUtxo.datum, HubOracleDatum).deposit,
    }).map(async (item) => ({
      ...item,
      reference: await requireDeploymentReferenceScript({
        lucid,
        deploymentInfo: parsedDeploymentInfo,
        name: TRANSITION_TRACE_YIELD_REFERENCES[item.key].entry,
      }),
      validator: contracts.transitionTrace.yields[item.key],
    })),
  );
  let spendLayout: TransitionTraceFinalSpendLayout | undefined;
  let computationThreadMintRedeemerIndex: bigint | undefined;
  const computationThreadMintCarriage = witnessMintingPolicyCarriage({
    script: contracts.computationThread.mintingScript,
    referenceUtxo: witnessReferenceScripts?.computationThreadMint,
    label: "transition-trace computation-thread mint",
  });
  const fraudProofMintCarriage = witnessMintingPolicyCarriage({
    script: contracts.fraudProof.mintingScript,
    referenceUtxo: witnessReferenceScripts?.fraudProofMint,
    label: "transition-trace fraud-proof mint",
  });
  const referenceInputs = [
    hubOracleUtxo,
    finalReferenceScript,
    ...additionalReferenceInputs,
    ...semanticYields.map((item) => item.reference),
    ...proofReferences,
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
        yieldReferences: semanticYields.map((item) => item.reference),
        proofReferences,
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
  for (const item of semanticYields)
    base.withdraw(
      validatorToRewardAddress(network, item.validator.withdrawalScript),
      0n,
      item.redeemer,
    );
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
