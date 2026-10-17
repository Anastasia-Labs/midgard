import {
  DaHashPreimageStep02Datum,
  FraudProofComputationThreadStepDatum,
} from "@al-ft/midgard-sdk";

import {
  type StateQueueMutationLease,
  type StateQueueMutationLeaseCoordinator,
} from "../remove-fraudulent-block.js";
import { parseSubmitDaHashPreimageTxInclusion } from "../submit-da-hash-preimage-step-01.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import { DA_HASH_PREIMAGE_COMPLETE_CANONICAL_REPLAY } from "./complete-replay.js";
import {
  admitDaHashPreimageArtifact,
  type BoundDaHashPreimageTransactionsConfig,
  type DaHashPreimageBuilderSet,
  productionBuilders,
  requiredAction,
  stringField,
  WITNESS_ROLES,
} from "./da-hash-preimage.artifact-input.js";
import {
  defineLinearFamily,
  type ManifestBoundLinearFamilyWorkflow,
  type ManifestBoundLinearFamilyWorkflowConfig,
} from "./family-definition.js";
import { observeFraudProofWorkflowHeader } from "./family-l1-observation.js";
import {
  type FraudProofWorkflowJournalStore,
  type JournalJsonObject,
} from "./journal.js";
import {
  LINEAR_FAMILY_TRANSACTION_PORT,
  type LinearFamilyCapturedAction,
  type LinearFamilyTransactionPort,
} from "./linear-family-adapter.js";
import { assembleManifestBoundFamilyWorkflow } from "./manifest-bound-family-assembly.js";
import {
  createFraudProofWorkflowRegistry,
  type FraudProofWorkflowAction,
  type FraudProofWorkflowRunResult,
  runDaHashPreimageWorkflowFromRetainedDa,
} from "./orchestrator.js";
import {
  captureLocallyEvaluatedTransaction,
  workflowTransactionInputOutRefs,
  workflowTransactionReferenceInputOutRefs,
} from "./transaction-boundary.js";

const createBoundDaHashPreimageTransactionPort = ({
  config,
  builders,
}: {
  readonly config: BoundDaHashPreimageTransactionsConfig;
  readonly builders: DaHashPreimageBuilderSet;
}): LinearFamilyTransactionPort<"daHashPreimage"> => {
  const capture = async ({
    action,
    artifact,
  }: {
    readonly action: FraudProofWorkflowAction;
    readonly artifact: JournalJsonObject;
  }): Promise<LinearFamilyCapturedAction> => {
    const plan = await admitDaHashPreimageArtifact(artifact);
    if (plan.headerHash !== config.headerHash) {
      throw new Error(
        "da-hash-preimage artifact targets a different manifest-bound header",
      );
    }
    const input = requiredAction(action);
    if (input.stage === "init") {
      const transaction = await captureLocallyEvaluatedTransaction(
        async (preSubmitBoundary) => {
          await builders.init({
            lucid: config.lucid,
            blueprint: config.blueprint,
            deploymentInfo: config.deploymentInfo,
            network: config.network,
            signer: config.signer,
            fraudCategory: "daHashPreimage",
            fraudulentBlockOutRef: stringField(input, "stateQueueBlockOutRef"),
            fraudulentHeaderHash: config.headerHash,
            witnessReferenceScripts: config.referenceScripts.witnesses,
            preSubmitBoundary,
            awaitConfirmation: false,
          });
        },
      );
      return Object.freeze({ transaction });
    }
    if (input.stage === "step_01") {
      const transaction = await captureLocallyEvaluatedTransaction(
        async (preSubmitBoundary) => {
          await builders.step01({
            lucid: config.lucid,
            blueprint: config.blueprint,
            deploymentInfo: config.deploymentInfo,
            network: config.network,
            signer: config.signer,
            threadOutRef: stringField(input, "threadOutRef"),
            stateQueueBlockOutRef: stringField(input, "stateQueueBlockOutRef"),
            txInclusion: parseSubmitDaHashPreimageTxInclusion(plan.txInclusion),
            referenceScriptUtxo: config.referenceScripts.steps[0],
            witnessReferenceScripts: config.referenceScripts.witnesses,
            preSubmitBoundary,
            awaitConfirmation: false,
          });
        },
      );
      return Object.freeze({ transaction });
    }
    if (input.stage === "step_02") {
      const transaction = await captureLocallyEvaluatedTransaction(
        async (preSubmitBoundary) => {
          await builders.step02({
            lucid: config.lucid,
            blueprint: config.blueprint,
            deploymentInfo: config.deploymentInfo,
            network: config.network,
            signer: config.signer,
            threadOutRef: stringField(input, "threadOutRef"),
            referenceScriptUtxo: config.referenceScripts.steps[1],
            witnessReferenceScripts: config.referenceScripts.witnesses,
            preSubmitBoundary,
            awaitConfirmation: false,
          });
        },
      );
      return Object.freeze({ transaction });
    }
    if (input.stage === "remove") {
      let mutationLease: StateQueueMutationLease | undefined;
      const retainingCoordinator: StateQueueMutationLeaseCoordinator = {
        acquire: async () => {
          const acquired =
            await config.stateQueueMutationLeaseCoordinator.acquire();
          mutationLease = acquired;
          return acquired;
        },
      };
      const nextRemovalOutRef = stringField(input, "nextRemovalOutRef");
      const fraudProofOutRef = stringField(input, "fraudProofOutRef");
      const transaction = await captureLocallyEvaluatedTransaction(
        async (boundary) => {
          await builders.remove({
            lucid: config.lucid,
            blueprint: config.blueprint,
            deploymentInfo: config.deploymentInfo,
            network: config.network,
            signer: config.signer,
            fraudCategory: "daHashPreimage",
            fraudulentHeaderHash: config.headerHash,
            requireReferenceScripts: true,
            stateQueueMutationLeaseCoordinator: retainingCoordinator,
            fraudProverRewardLovelace: config.fraudProverRewardLovelace,
            preSubmitBoundary: async (transaction) => {
              if (
                !workflowTransactionInputOutRefs(transaction.signed).includes(
                  nextRemovalOutRef,
                )
              ) {
                throw new Error(
                  "da-hash-preimage removal does not consume the authenticated next queue input",
                );
              }
              if (
                !workflowTransactionReferenceInputOutRefs(
                  transaction.signed,
                ).includes(fraudProofOutRef)
              ) {
                throw new Error(
                  "da-hash-preimage removal does not reference the authenticated retained proof token",
                );
              }
              await boundary(transaction);
            },
          });
        },
      );
      return Object.freeze({
        transaction,
        ...(mutationLease === undefined ? {} : { mutationLease }),
      });
    }
    throw new Error(
      `da-hash-preimage workflow action has unsupported stage ${String(input.stage)}`,
    );
  };
  return Object.freeze({
    portVersion: LINEAR_FAMILY_TRANSACTION_PORT,
    category: "daHashPreimage",
    prepare: async () => {
      throw new Error(
        "da-hash-preimage requires the authenticated raw-source-leaf evidence route",
      );
    },
    capture,
  });
};

export type ManifestBoundDaHashPreimageWorkflowConfig =
  ManifestBoundLinearFamilyWorkflowConfig<
    "daHashPreimage",
    (typeof WITNESS_ROLES)[number],
    false
  >;

export type ManifestBoundDaHashPreimageWorkflow =
  ManifestBoundLinearFamilyWorkflow<"daHashPreimage", false>;

/**
 * Q44 manifest-bound definition. It is deliberately not an admitted runner
 * yet: the generic canonical classifier cannot route a raw source-leaf defect,
 * so the workflow launches through the dedicated evidence route below rather
 * than the generic retained-DA runner. Readiness remains missing until that
 * route enters the shared durable workflow loop without manufacturing
 * canonical evidence.
 */
export const DA_HASH_PREIMAGE_FAMILY_DEFINITION = defineLinearFamily({
  category: "daHashPreimage",
  stepDatumSchemas: [
    FraudProofComputationThreadStepDatum,
    DaHashPreimageStep02Datum,
  ],
  witnessRoles: WITNESS_ROLES,
  fieldPreimageCertificate: false,
  replayer: () => DA_HASH_PREIMAGE_COMPLETE_CANONICAL_REPLAY,
  adapter: {
    kind: "linear",
    transactionPort: (context) =>
      createBoundDaHashPreimageTransactionPort({
        config: {
          lucid: context.lucid,
          blueprint: context.binding.blueprint,
          deploymentInfo: context.binding.deploymentInfo,
          network: context.binding.network,
          signer: context.signer,
          headerHash: context.binding.definition.headerHash,
          referenceScripts: context.references,
          stateQueueMutationLeaseCoordinator:
            context.stateQueueMutationLeaseCoordinator,
          fraudProverRewardLovelace: BigInt(
            context.binding.releaseEconomics.policy.fraudProverRewardLovelace,
          ),
        },
        builders: productionBuilders,
      }),
  },
});

export const createManifestBoundDaHashPreimageWorkflow = (
  config: ManifestBoundDaHashPreimageWorkflowConfig,
): Promise<ManifestBoundDaHashPreimageWorkflow> =>
  assembleManifestBoundFamilyWorkflow(
    DA_HASH_PREIMAGE_FAMILY_DEFINITION,
    config,
  );

/**
 * Exact public-DA Q44 route into the shared durable lifecycle. It does not
 * alias the generic run-or-resume: the raw source-leaf defect is routed by
 * the authenticated evidence fetch, not by a canonical replayer, so the
 * dedicated orchestrator entry stays the launch path.
 */
export const runOrResumeManifestBoundDaHashPreimageWorkflow = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundDaHashPreimageWorkflow;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStore;
}): Promise<FraudProofWorkflowRunResult> => {
  const observation = await observeFraudProofWorkflowHeader(workflow.l1, {
    headerHash: workflow.binding.definition.headerHash,
  });
  return await runDaHashPreimageWorkflowFromRetainedDa({
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    observation,
    sources,
    registry: createFraudProofWorkflowRegistry({
      adapters: [workflow.adapter],
      launchScope: ["daHashPreimage"],
    }),
    journal,
    terminalVerifier: workflow.terminalVerifier,
    releaseFinalityAuthority: workflow.releaseFinalityAuthority,
  });
};

/** Narrow builder-injection seam for focused tests; never admit this as ready. */
export const unsafeCreateDaHashPreimageTransactionPortForTest = (input: {
  readonly config: BoundDaHashPreimageTransactionsConfig;
  readonly builders: DaHashPreimageBuilderSet;
}): LinearFamilyTransactionPort<"daHashPreimage"> =>
  createBoundDaHashPreimageTransactionPort(input);
