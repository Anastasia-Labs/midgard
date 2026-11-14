import { hashBlockHeader } from "@al-ft/midgard-sdk";
import { Effect } from "effect";

import {
  fetchUtxoByOutRef,
  makeLucidForSubmit,
  parseOutRef,
  readJsonFile,
  requireDeploymentReferenceScript,
  resolveProverSigner,
  resolveTransitionTraceDeploymentContracts,
} from "../runtime.js";
import { completeStep, fundStep, submitStep } from "../step-completion.js";
import {
  requireComputationThreadToken,
  requireInitialStepDatum,
} from "../step-support.js";
import {
  type FraudProofPreSubmitBoundary,
  workflowReferenceScriptsUsedByTransaction,
} from "../workflow/transaction-boundary.js";
import { transitionTraceError } from "./errors.js";
import {
  resolveTransitionTraceProofCarriage,
  transitionTraceTimedProofNeedsChunks,
} from "./proof-carriage.js";
import { readTransitionProof } from "./proof-material.js";
import { transitionTraceFinalIndex } from "./submit.make-transition-trace-final-spend-redeemer.js";
import {
  makeTransitionTraceRouteSpendRedeemer,
  type SubmitTransitionTraceProofConfig,
  type SubmitTransitionTraceProofFromFilesConfig,
  type SubmitTransitionTraceProofResult,
  type SubmitTransitionTraceRouteResult,
  type TransitionTraceRouteSpendLayout,
} from "./submit.make-transition-trace-route-spend-redeemer.js";
import { submitTransitionTraceProof } from "./submit.submit-transition-trace-proof.js";
import { transitionTraceRouteDatum } from "./submit.transition-trace-route-datum.js";

/**
 * Journal-visible router transaction. It never submits before the signed body
 * has crossed the shared local-evaluation boundary.
 */
export const submitTransitionTraceRoute = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  proof: proofInput,
  preSubmitBoundary,
  awaitConfirmation = true,
}: Omit<
  SubmitTransitionTraceProofConfig,
  "additionalReferenceInputs" | "witnessReferenceScripts"
> & {
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
}): Promise<SubmitTransitionTraceRouteResult> => {
  const proof = readTransitionProof(proofInput);
  const {
    deploymentInfo: parsedDeploymentInfo,
    transitionTraceCategory,
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
      "Transition-trace router proof changed its challenged header.",
    );
  }
  const finalIndex = transitionTraceFinalIndex(proof);
  const [threadUtxo, routeReferenceScript] = await Promise.all([
    fetchUtxoByOutRef({
      lucid,
      outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
      label: "transition-trace computation-thread UTxO",
    }),
    requireDeploymentReferenceScript({
      lucid,
      deploymentInfo: parsedDeploymentInfo,
      name: "fraudProofTransitionTrace",
    }),
  ]);
  if (
    threadUtxo.address !==
    contracts.transitionTrace.firstStep.spendingScriptAddress
  ) {
    throw transitionTraceError(
      "submissionRejected",
      "Transition-trace router input is not locked at the manifest-bound router.",
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
      "Transition-trace router proof targets another computation thread.",
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
          publish: preSubmitBoundary === undefined,
        })
      : [];
  const routeDatum = transitionTraceRouteDatum(
    proofInput,
    signer.paymentKeyHash,
  );
  let layout: TransitionTraceRouteSpendLayout | undefined;
  const funding = await fundStep(lucid, { signer });
  const routeTx = funding.tx
    .collectFrom(
      [threadUtxo],
      makeTransitionTraceRouteSpendRedeemer({
        threadUtxo,
        routeAddress: finalValidator.spendingScriptAddress,
        routeDatum,
        computationThreadUnit: threadToken.unit,
        proofReferences,
        proof: proofInput,
        onLayout: (resolved) => {
          layout = resolved;
        },
      }),
    )
    .readFrom([routeReferenceScript, ...proofReferences])
    .pay.ToContract(
      finalValidator.spendingScriptAddress,
      { kind: "inline", value: routeDatum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [threadToken.unit]: 1n,
      },
    );
  const signed = await completeStep(routeTx, funding);
  if (layout === undefined) {
    throw transitionTraceError(
      "submissionRejected",
      "Transition-trace router layout was not resolved.",
    );
  }
  const txHash = await submitStep(lucid, signed, {
    label: "transition-trace route",
    referenceScripts: workflowReferenceScriptsUsedByTransaction({
      signed,
      candidates: [
        {
          role: "V1 fraud-proof transition-trace router",
          utxo: routeReferenceScript,
          expectedScript: contracts.transitionTrace.firstStep.spendingScript,
        },
      ],
    }),
    boundary: preSubmitBoundary,
    awaitConfirmation,
  });
  return Object.freeze({
    txHash,
    routeOutRef: `${txHash}#${layout.outputIndex.toString()}`,
    fraudulentHeaderHash: proof.challenged_header_hash,
    finalIndex,
    awaitedConfirmation: awaitConfirmation,
  });
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
