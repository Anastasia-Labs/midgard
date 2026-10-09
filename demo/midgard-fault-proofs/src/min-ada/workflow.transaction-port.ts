import { type LucidEvolution } from "@lucid-evolution/lucid";

import { resolvePublishedProofChunks } from "../publish-proof-chunks.js";
import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import { type ResolvedProverSigner } from "../runtime.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import {
  type CompleteCanonicalReplayContext,
  completeCanonicalReplayPredecessorEvidence,
} from "../workflow/complete-replay.js";
import {
  CURSOR_FAMILY_TRANSACTION_PORT,
  type CursorFamilyTransactionPort,
} from "../workflow/cursor-family-adapter.js";
import {
  captureCursorRemoval,
  cursorFamilyActionInput,
  cursorStringField,
} from "../workflow/cursor-family-runtime.js";
import { type FraudProofWorkflowDeploymentBinding } from "../workflow/deployment-manifest-binding.js";
import { type FamilyAssemblyContext } from "../workflow/family-definition.js";
import { type FraudProofFamilyL1ObservationPort } from "../workflow/family-l1-observation.js";
import type { FraudProofL1Source } from "../workflow/l1-source.js";
import {
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowTerminalVerifier,
} from "../workflow/orchestrator.js";
import type { FraudProofReleaseFinalityAuthority } from "../workflow/release-finality-policy.js";
import { captureLocallyEvaluatedTransaction } from "../workflow/transaction-boundary.js";
import { submitMinAdaInit } from "./submit-init.js";
import {
  submitMinAdaTxStep01,
  submitMinAdaUtxoStep01,
} from "./submit-step-01.js";
import { submitMinAdaStep01Forced } from "./submit-step-01-forced.js";
import {
  submitMinAdaTxStep02,
  submitMinAdaUtxoStep02,
} from "./submit-step-02.js";
import { submitMinAdaUtxoStep03 } from "./submit-step-03.js";
import { submitMinAdaUtxoStep04 } from "./submit-step-04.js";
import { submitMinAdaStep05 } from "./submit-step-05.js";
import {
  type BoundConfig,
  isForced,
  isTx,
  type MinAdaWorkflowReferenceScripts,
  resolveField,
} from "./workflow.resolve-field.js";
import {
  admitMinAdaWorkflowArtifact as admitMinAdaArtifact,
  prepareMinAdaWorkflowArtifact as prepareMinAdaArtifact,
} from "./workflow-artifact.js";

export const transactionPort = (
  config: BoundConfig,
): CursorFamilyTransactionPort<"minAda"> => ({
  portVersion: CURSOR_FAMILY_TRANSACTION_PORT,
  category: "minAda",
  prepare: async ({ evidence, classification }) =>
    await prepareMinAdaArtifact({
      evidence,
      predecessor: completeCanonicalReplayPredecessorEvidence({
        evidence,
        context: config.replayContext,
      }),
      classification,
    }),
  capture: async ({ action, artifact }) => {
    const admitted = await admitMinAdaArtifact(artifact);
    if (admitted.artifact.headerHash !== config.binding.definition.headerHash) {
      throw new Error("min-ada artifact changed the bound header");
    }
    const input = cursorFamilyActionInput({
      category: "minAda",
      action,
    });
    const categoryId = config.binding.resolvedContracts.category.categoryId;
    const threadOutRef = () => cursorStringField(input, "threadOutRef");
    if (input.stage === "init") {
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitMinAdaInit({
              lucid: config.lucid,
              blueprint: config.binding.blueprint,
              deploymentInfo: config.binding.deploymentInfo,
              network: config.binding.network,
              signer: config.signer,
              fraudulentBlockOutRef: cursorStringField(
                input,
                "stateQueueBlockOutRef",
              ),
              fraudulentHeaderHash: admitted.artifact.headerHash,
              witnessReferenceScripts: config.references.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "step_01") {
      const chunks =
        isTx(admitted) && !isForced(admitted)
          ? await resolvePublishedProofChunks({
              lucid: config.lucid,
              address: config.signer.address,
              proofCbor: admitted.prepared.txInclusion.txMembershipProofCbor,
            })
          : [];
      if (chunks === undefined) {
        throw new Error("min-ada transaction proof disappeared");
      }
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            const shared = {
              lucid: config.lucid,
              contracts: config.contracts,
              categoryId,
              network: config.binding.network,
              signer: config.signer,
              threadOutRef: threadOutRef(),
              stateQueueBlockOutRef: cursorStringField(
                input,
                "stateQueueBlockOutRef",
              ),
              prepared: admitted.prepared,
              referenceScriptUtxo: config.references.steps[0],
              preSubmitBoundary,
              awaitConfirmation: false,
            } as const;
            if (isForced(admitted)) {
              await submitMinAdaStep01Forced({
                ...shared,
                state: admitted.prepared.state,
                forcedSource: admitted.forcedSource,
              });
            } else if (isTx(admitted)) {
              await submitMinAdaTxStep01({
                ...shared,
                blueprint: config.binding.blueprint,
                prepared: admitted.prepared,
                publishedProofChunks: chunks,
                witnessReferenceScripts: config.references.witnesses,
              });
            } else {
              await submitMinAdaUtxoStep01({
                ...shared,
                prepared: admitted.prepared,
              });
            }
          },
        ),
      });
    }
    if (input.stage === "step_02") {
      if (isTx(admitted)) {
        const carriage = await resolveField({ config, admitted });
        return Object.freeze({
          transaction: await captureLocallyEvaluatedTransaction(
            async (preSubmitBoundary) => {
              await submitMinAdaTxStep02({
                lucid: config.lucid,
                contracts: config.contracts,
                categoryId,
                signer: config.signer,
                threadOutRef: threadOutRef(),
                prepared: admitted.prepared,
                publishedCarriageUtxos: carriage.publications,
                ...(carriage.certificate === undefined
                  ? {}
                  : { certificateUtxo: carriage.certificate }),
                referenceScriptUtxo: config.references.steps[1],
                yieldReferenceScriptUtxo: config.references.yields.tx,
                preSubmitBoundary,
                awaitConfirmation: false,
              });
            },
          ),
        });
      }
      const chunks = await resolvePublishedProofChunks({
        lucid: config.lucid,
        address: config.signer.address,
        proofCbor: admitted.prepared.postMembershipProofCbor,
      });
      if (chunks === undefined) {
        throw new Error("min-ada post-membership proof disappeared");
      }
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitMinAdaUtxoStep02({
              lucid: config.lucid,
              blueprint: config.binding.blueprint,
              network: config.binding.network,
              contracts: config.contracts,
              categoryId,
              signer: config.signer,
              threadOutRef: threadOutRef(),
              prepared: admitted.prepared,
              publishedProofChunks: chunks,
              referenceScriptUtxo: config.references.steps[1],
              yieldReferenceScriptUtxo: config.references.yields.utxo,
              witnessReferenceScripts: config.references.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "step_03") {
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitMinAdaUtxoStep03({
              lucid: config.lucid,
              contracts: config.contracts,
              categoryId,
              signer: config.signer,
              threadOutRef: threadOutRef(),
              ...(isTx(admitted)
                ? { outputItemCbors: admitted.prepared.outputItemCbors }
                : {}),
              coinsPerUtxoByte: BigInt(
                config.binding.cardanoProtocolParameters.coinsPerUtxoByte,
              ),
              referenceScriptUtxo: config.references.steps[2],
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "step_04" && !isTx(admitted)) {
      const chunks = await resolvePublishedProofChunks({
        lucid: config.lucid,
        address: config.signer.address,
        proofCbor: admitted.prepared.predecessorNonMembershipProofCbor,
      });
      if (chunks === undefined) {
        throw new Error("min-ada predecessor proof disappeared");
      }
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitMinAdaUtxoStep04({
              lucid: config.lucid,
              blueprint: config.binding.blueprint,
              network: config.binding.network,
              contracts: config.contracts,
              categoryId,
              signer: config.signer,
              threadOutRef: threadOutRef(),
              predecessorNonMembershipProofCbor:
                admitted.prepared.predecessorNonMembershipProofCbor,
              publishedProofChunks: chunks,
              referenceScriptUtxo: config.references.steps[3],
              witnessReferenceScripts: config.references.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "step_05") {
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitMinAdaStep05({
              lucid: config.lucid,
              contracts: config.contracts,
              categoryId,
              signer: config.signer,
              threadOutRef: threadOutRef(),
              referenceScriptUtxo: config.references.steps[4],
              witnessReferenceScripts: config.references.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "remove") {
      return await captureCursorRemoval({
        category: "minAda",
        lucid: config.lucid,
        blueprint: config.binding.blueprint,
        deploymentInfo: config.binding.deploymentInfo,
        network: config.binding.network,
        signer: config.signer,
        headerHash: admitted.artifact.headerHash,
        input,
        stateQueueMutationLeaseCoordinator:
          config.stateQueueMutationLeaseCoordinator,
        fraudProverRewardLovelace: BigInt(
          config.binding.releaseEconomics.policy.fraudProverRewardLovelace,
        ),
      });
    }
    throw new Error(
      `min-ada ${admitted.artifact.kind} cannot execute ${input.stage}`,
    );
  },
});

export type ManifestBoundMinAdaWorkflowConfig = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  referenceScripts: MinAdaWorkflowReferenceScripts;
  l1Source: FraudProofL1Source;
  /** The classifier-admitted context carrying the authenticated predecessor. */
  replayContext?: CompleteCanonicalReplayContext;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

export type ManifestBoundMinAdaWorkflow = Readonly<{
  binding: FraudProofWorkflowDeploymentBinding<"minAda">;
  l1: FraudProofFamilyL1ObservationPort<"minAda">;
  transactions: CursorFamilyTransactionPort<"minAda">;
  adapter: FraudProofFamilyWorkflowAdapter;
  terminalVerifier: FraudProofWorkflowTerminalVerifier;
  releaseFinalityAuthority: FraudProofReleaseFinalityAuthority;
  replayContext?: CompleteCanonicalReplayContext;
}>;

export type MinAdaRuntime = Pick<
  ManifestBoundMinAdaWorkflowConfig,
  "replayContext"
>;

export type AssemblyContext = FamilyAssemblyContext<
  "minAda",
  keyof FaultProofWitnessReferenceScripts,
  true,
  5,
  MinAdaRuntime
>;

export const boundConfigs = new WeakMap<AssemblyContext, BoundConfig>();
