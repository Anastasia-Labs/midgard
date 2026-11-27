import { hashBlockHeader } from "@al-ft/midgard-sdk/core";
import { Effect } from "effect";

import {
  type FaultProofTurn,
  isPrerequisiteTurn,
} from "../fault-proof-turn.js";
import {
  fetchUtxoByOutRef,
  makeLucidForSubmit,
  parseOutRef,
  readJsonFile,
  requireDeploymentReferenceScript,
  resolveProverSigner,
  resolveTransitionTraceDeploymentContracts,
} from "../runtime.js";
import { completeStep, fundStep } from "../step-completion.js";
import { finishStep } from "../step-completion.js";
import {
  requireComputationThreadToken,
  requireInitialStepDatum,
} from "../step-support.js";
import { submitFaultProofTurns } from "../submit-fault-proof-turns.js";
import { transitionTraceError } from "./errors.js";
import {
  resolveTransitionTraceProofCarriage,
  transitionTraceTimedProofNeedsChunks,
} from "./proof-carriage.js";
import { readTransitionProof } from "./proof-material.js";
import { transitionTraceFinalIndex } from "./submit.make-transition-trace-final-spend-redeemer.js";
import type { TransitionTraceRouteBuild } from "./submit.make-transition-trace-route-spend-redeemer.js";
import {
  makeTransitionTraceRouteSpendRedeemer,
  type SubmitTransitionTraceProofConfig,
  type SubmitTransitionTraceProofFromFilesConfig,
  type SubmitTransitionTraceProofResult,
  type TransitionTraceRouteSpendLayout,
} from "./submit.make-transition-trace-route-spend-redeemer.js";
import { buildTransitionTraceProof } from "./submit.submit-transition-trace-proof.js";
import { transitionTraceRouteDatum } from "./submit.transition-trace-route-datum.js";

/**
 * The router transaction a workflow submits as its own step. The
 * proof carriage it reads must already be live unless `publishCarriage` is
 * set, when its missing publication is returned as the next turn instead.
 */
export const buildTransitionTraceRoute = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  proof: proofInput,
  publishCarriage = false,
  materials,
}: Omit<
  SubmitTransitionTraceProofConfig,
  "additionalReferenceInputs" | "witnessReferenceScripts"
>): Promise<FaultProofTurn<TransitionTraceRouteBuild>> => {
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
          address: signer.proofMaterialsAddress,
          proof: proofInput,
          materials,
          publish: publishCarriage,
        })
      : [];
  if (isPrerequisiteTurn(proofReferences)) return proofReferences;
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
  const transaction = finishStep(signed, {
    referenceScriptCandidates: [
      {
        role: "V1 fraud-proof transition-trace router",
        utxo: routeReferenceScript,
        expectedScript: contracts.transitionTrace.firstStep.spendingScript,
      },
    ],
  });
  return Object.freeze({
    transaction,
    routeOutRef: `${transaction.txHash}#${layout.outputIndex.toString()}`,
    fraudulentHeaderHash: proof.challenged_header_hash,
    finalIndex,
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
  return await submitFaultProofTurns(
    buildTransitionTraceProof,
    {
      lucid,
      blueprint,
      deploymentInfo,
      network: config.network,
      signer: resolveProverSigner(config),
      threadOutRef: config.threadOutRef,
      proof: config.proof,
      publishCarriage: true,
    },
    {
      label: "transition-trace proof",
      awaitConfirmation: config.awaitConfirmation ?? true,
    },
  );
};
