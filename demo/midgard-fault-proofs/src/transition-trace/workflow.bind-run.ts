import { decodeMidgardTxOutput } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import { fetchUtxoByOutRef, parseOutRef } from "../runtime.js";
import { submitInit } from "../submit-init.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import {
  CURSOR_FAMILY_TRANSACTION_PORT,
  type CursorFamilyTransactionPort,
} from "../workflow/cursor-family-adapter.js";
import {
  captureCursorRemoval,
  cursorFamilyActionInput,
  cursorStringField,
} from "../workflow/cursor-family-runtime.js";
import { requireHistoricalNativeScriptHistoryAuthority } from "../workflow/historical-native-script-corpus.js";
import type { JournalJsonObject } from "../workflow/journal.js";
import { type FraudProofWorkflowAction } from "../workflow/orchestrator.js";
import {
  createChunkedRawDatumPreimageRequirement,
  createStructuredDataPreimageRequirement,
} from "../workflow/raw-datum-preimage-prerequisite.js";
import { structuredDataPublicationPlan } from "../workflow/structured-data-preimage.js";
import { captureLocallyEvaluatedTransaction } from "../workflow/transaction-boundary.js";
import {
  transitionTraceProofChunks,
  transitionTraceTimedProofNeedsChunks,
} from "./proof-carriage.js";
import {
  submitTransitionTraceFinal,
  submitTransitionTraceRoute,
  TRANSITION_TRACE_FINAL_REFERENCE_SCRIPT_ENTRIES,
  transitionTraceFinalIndex,
} from "./submit.js";
import {
  type Prepared,
  type TransitionContext,
} from "./workflow.manifest-bound-transition-trace-workflow-config.js";
import { requireTransitionTraceWorkflowArtifact } from "./workflow-artifact.js";
import { transitionDepositCheckpointRequiresQueueLease } from "./workflow-checkpoint.js";
import { transitionTraceYieldData } from "./yield-data.js";

export const boundRuns = new WeakMap<
  TransitionContext,
  ReturnType<typeof bindRun>
>();

const bindRun = (context: TransitionContext) => {
  const { binding } = context;
  const { config, cell } = context.runtime;
  requireHistoricalNativeScriptHistoryAuthority({
    deploymentFingerprint: binding.deploymentFingerprint,
    checkpointStore: config.historicalNativeScriptCheckpointStore,
    historySource: config.historicalNativeScriptHistorySource,
  });
  const references = Object.freeze({
    ...context.auxiliaryReferences,
    ...context.references.witnesses,
    ...Object.fromEntries(
      [
        "fraudProofTransitionTrace",
        ...TRANSITION_TRACE_FINAL_REFERENCE_SCRIPT_ENTRIES,
      ].map((name, index) => [name, context.references.steps[index]!]),
    ),
  });
  const witnesses: FaultProofWitnessReferenceScripts =
    context.references.witnesses;
  const prerequisite = context.fieldCarriagePrerequisites[0]!;
  const admit = (artifact: JournalJsonObject): Prepared => {
    if (typeof artifact.detectionId !== "string")
      throw new Error("Transition artifact has no detection identity");
    const fresh = cell.prepared?.get(artifact.detectionId);
    if (fresh === undefined)
      throw new Error(
        "Transition artifact has not been freshly derived from retained history",
      );
    requireTransitionTraceWorkflowArtifact(artifact, fresh.artifact);
    return fresh;
  };
  const refineAction = async ({
    action,
    artifact,
  }: {
    action: FraudProofWorkflowAction;
    artifact: JournalJsonObject;
  }): Promise<JournalJsonObject> => {
    if (action.input.stage === "step_08") {
      const prepared = admit(artifact);
      if (transitionTraceFinalIndex(prepared.proof) !== 6)
        throw new Error(
          "Timed transition mutation action selected another proof route",
        );
      return { requiresMutationLease: true };
    }
    if (action.input.stage !== "step_07") return {};
    const prepared = admit(artifact);
    const input = cursorFamilyActionInput({
      category: "transitionTrace",
      action,
    });
    const thread = await fetchUtxoByOutRef({
      label: "transition deposit mutation checkpoint",
      lucid: config.lucid,
      outRef: parseOutRef(
        cursorStringField(input, "threadOutRef"),
        "transition deposit checkpoint",
      ),
    });
    const requiresMutationLease = transitionDepositCheckpointRequiresQueueLease(
      {
        thread,
        address: binding.definition.computationThread.steps[6]!.address,
        unit:
          binding.definition.computationThread.policyId +
          binding.definition.categoryId +
          binding.definition.headerHash,
        prover: config.signer.paymentKeyHash,
        proofHash: transitionTraceProofChunks(prepared.proof).hash,
      },
    );
    return { requiresMutationLease };
  };
  const requirementForAction = async ({
    action,
    artifact,
  }: {
    action: FraudProofWorkflowAction;
    artifact: JournalJsonObject;
  }) => {
    const prepared = admit(artifact);
    const input = cursorFamilyActionInput({
      category: "transitionTrace",
      action,
    });
    const finalIndex = transitionTraceFinalIndex(prepared.proof);
    if (
      input.stage === "step_01" &&
      (finalIndex === 4 ||
        finalIndex === 5 ||
        transitionTraceTimedProofNeedsChunks(prepared.proof))
    )
      return createChunkedRawDatumPreimageRequirement({
        preimage: Buffer.from(
          transitionTraceProofChunks(prepared.proof).chunks.join(""),
          "hex",
        ),
      });
    if (input.stage !== "step_06" && input.stage !== "step_07") return null;
    const thread = await fetchUtxoByOutRef({
      label: "transition workflow checkpoint",
      lucid: config.lucid,
      outRef: parseOutRef(
        cursorStringField(input, "threadOutRef"),
        "transition workflow checkpoint",
      ),
    });
    if (thread.datum == null)
      throw new Error("Transition checkpoint omitted its datum");
    const datum = Data.from(
      thread.datum,
      SDK.TransitionTraceProofCommitmentDatum,
    );
    const state = datum.data;
    if (
      state === null ||
      datum.fraud_prover !== config.signer.paymentKeyHash ||
      state.proof_commitment.hash !==
        transitionTraceProofChunks(prepared.proof).hash
    )
      throw new Error(
        "Transition prerequisite checkpoint differs from admitted proof",
      );
    const outputs = transitionTraceYieldData({
      proof: prepared.proof,
      network: binding.network,
      depositPolicyId: prepared.depositPolicyId,
      depositOpening: prepared.depositOpening,
    }).find((item) => item.outputCbors !== undefined)?.outputCbors;
    const output = outputs?.[Number(state.output_index)];
    if (output === undefined) return null;
    if (state.kind === 0n && state.phase === 8n) {
      const datum = decodeMidgardTxOutput(Buffer.from(output, "hex")).datum;
      if (datum?.kind !== "inline") return null;
      const bytes = Buffer.from(datum.cbor).toString("hex");
      return structuredDataPublicationPlan(bytes).publicationDatums.length === 0
        ? null
        : createStructuredDataPreimageRequirement({ preimageHex: bytes });
    }
    return createChunkedRawDatumPreimageRequirement({
      preimage: Buffer.from(output, "hex"),
    });
  };
  const transactions: CursorFamilyTransactionPort<"transitionTrace"> = {
    portVersion: CURSOR_FAMILY_TRANSACTION_PORT,
    category: "transitionTrace",
    prepare: async ({ evidence, classification }) => {
      const prepared = cell.prepared?.get(classification.selected.detectionId);
      if (
        cell.evidence !== evidence ||
        prepared === undefined ||
        classification.headerHash !== evidence.headerHash
      )
        throw new Error(
          "Transition classification differs from the freshly admitted replay",
        );
      return prepared.artifact;
    },
    capture: async ({ action, artifact }) => {
      const prepared = admit(artifact);
      const input = cursorFamilyActionInput({
        category: "transitionTrace",
        action,
      });
      if (input.stage === "remove")
        return captureCursorRemoval({
          category: "transitionTrace",
          lucid: config.lucid,
          blueprint: binding.blueprint,
          deploymentInfo: binding.deploymentInfo,
          network: binding.network,
          signer: config.signer,
          headerHash: binding.definition.headerHash,
          input,
          stateQueueMutationLeaseCoordinator:
            config.stateQueueMutationLeaseCoordinator,
          fraudProverRewardLovelace: BigInt(
            binding.releaseEconomics.policy.fraudProverRewardLovelace,
          ),
        });
      if ((await requirementForAction({ action, artifact })) !== null)
        await prerequisite.resolveAuthenticated({
          headerHash: binding.definition.headerHash,
          action,
          artifact,
        });
      const fields = await refineAction({ action, artifact });
      if (fields.requiresMutationLease !== input.requiresMutationLease)
        throw new Error(
          "Transition checkpoint queue mutation changed before capture",
        );
      const mutationLease =
        fields.requiresMutationLease === true
          ? await config.stateQueueMutationLeaseCoordinator.acquire()
          : undefined;
      try {
        await mutationLease?.renew();
        const transaction = await captureLocallyEvaluatedTransaction(
          async (boundary) => {
            const preSubmitBoundary: typeof boundary = async (built) => {
              await mutationLease?.renew();
              await boundary(built);
            };
            const common = {
              lucid: config.lucid,
              blueprint: binding.blueprint,
              deploymentInfo: binding.deploymentInfo,
              network: binding.network,
              signer: config.signer,
              witnessReferenceScripts: witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            };
            if (input.stage === "init") {
              await submitInit({
                ...common,
                fraudCategory: "transitionTrace",
                fraudulentBlockOutRef: cursorStringField(
                  input,
                  "stateQueueBlockOutRef",
                ),
                fraudulentHeaderHash: binding.definition.headerHash,
              });
              return;
            }
            const threadOutRef = cursorStringField(input, "threadOutRef");
            if (input.stage === "step_01") {
              await submitTransitionTraceRoute({
                ...common,
                threadOutRef,
                proof: prepared.proof,
              });
              return;
            }
            const expected = `step_${String(transitionTraceFinalIndex(prepared.proof) + 2).padStart(2, "0")}`;
            if (input.stage !== expected)
              throw new Error(
                "Transition workflow cursor selected a different final",
              );
            await submitTransitionTraceFinal({
              ...common,
              threadOutRef,
              proof: prepared.proof,
              additionalReferenceInputs: prepared.references,
              depositOpening: prepared.depositOpening,
            });
          },
        );
        return {
          transaction,
          ...(mutationLease === undefined ? {} : { mutationLease }),
        };
      } catch (error) {
        await mutationLease?.fail(
          `Terminal transition proof capture failed: ${String(error)}`,
        );
        throw error;
      }
    },
  };

  return {
    transactions,
    requirementForAction,
    references,
    witnesses,
    refineAction,
  };
};

export const runFor = (context: TransitionContext) => {
  const existing = boundRuns.get(context);
  if (existing !== undefined) return existing;
  const run = bindRun(context);
  boundRuns.set(context, run);
  return run;
};
