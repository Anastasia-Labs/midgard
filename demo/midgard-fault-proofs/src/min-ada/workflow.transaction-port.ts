import { type LucidEvolution } from "@lucid-evolution/lucid";

import { resolvePublishedProofChunks } from "../publish-proof-chunks.js";
import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import { type ResolvedProverSigner } from "../runtime.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import {
  CURSOR_FAMILY_TRANSACTION_PORT,
  type CursorFamilyTransactionPort,
} from "../workflow/cursor-family-adapter.admit-reference-scripts.js";
import {
  captureCursorRemoval,
  cursorFamilyActionInput,
  cursorStringField,
} from "../workflow/cursor-family-runtime.js";
import {
  type FamilyAssemblyContext,
  type ManifestBoundFamilyWorkflow,
} from "../workflow/family-definition.js";
import { workflowFieldCarriageOwnOutputs } from "../workflow/field-carriage-prerequisite.own-carriage-claims.js";
import type { FraudProofL1Source } from "../workflow/l1-source.js";
import { proofMaterialsAccess } from "../workflow/proof-materials-view.js";
import { workflowStepTransaction } from "../workflow/step-transaction.js";
import { buildMinAdaInit } from "./submit-init.js";
import { buildMinAdaTxStep01 } from "./submit-step-01.js";
import { buildMinAdaStep01Forced } from "./submit-step-01-forced.js";
import { buildMinAdaTxStep02 } from "./submit-step-02.submit-min-ada-tx-step02.js";
import { buildMinAdaStep03 } from "./submit-step-03.js";
import { buildMinAdaStep04 } from "./submit-step-04.js";
import {
  type BoundConfig,
  isForced,
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
    await prepareMinAdaArtifact({ evidence, classification }),
  capture: async ({ action, artifact, holder }) => {
    const ownOutputs = workflowFieldCarriageOwnOutputs({
      category: "minAda",
      materials: proofMaterialsAccess({
        signer: config.signer,
        lucid: config.lucid,
        holder,
      }),
    });
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
        transaction: (
          await workflowStepTransaction(
            await buildMinAdaInit({
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
            }),
            "min-ada buildMinAdaInit",
          )
        ).transaction,
      });
    }
    if (input.stage === "step_01") {
      const chunks = !isForced(admitted)
        ? await resolvePublishedProofChunks({
            lucid: config.lucid,
            address: config.signer.proofMaterialsAddress,
            proofCbor: admitted.prepared.txInclusion.txMembershipProofCbor,
            view: config.signer.proofMaterials,
            holder,
          })
        : [];
      if (chunks === undefined) {
        throw new Error("min-ada transaction proof disappeared");
      }
      return Object.freeze({
        transaction: (
          await workflowStepTransaction(
            await (async () => {
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
              } as const;
              if (isForced(admitted)) {
                return await buildMinAdaStep01Forced({
                  ...shared,
                  state: admitted.prepared.state,
                  forcedSource: admitted.forcedSource,
                });
              } else {
                return await buildMinAdaTxStep01({
                  ...shared,
                  blueprint: config.binding.blueprint,
                  prepared: admitted.prepared,
                  publishedProofChunks: chunks,
                  witnessReferenceScripts: config.references.witnesses,
                });
              }
            })(),
            "min-ada step_01",
          )
        ).transaction,
      });
    }
    if (input.stage === "step_02") {
      const carriage = await resolveField({ ownOutputs, config, admitted });
      return Object.freeze({
        transaction: (
          await workflowStepTransaction(
            await buildMinAdaTxStep02({
              ownOutputs,
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
            }),
            "min-ada buildMinAdaTxStep02",
          )
        ).transaction,
      });
    }
    if (input.stage === "step_03") {
      return Object.freeze({
        transaction: (
          await workflowStepTransaction<
            Awaited<ReturnType<typeof buildMinAdaStep03>>
          >(
            await buildMinAdaStep03({
              lucid: config.lucid,
              contracts: config.contracts,
              categoryId,
              signer: config.signer,
              threadOutRef: threadOutRef(),
              outputItemCbors: admitted.prepared.outputItemCbors,
              referenceScriptUtxo: config.references.steps[2],
            }),
            "min-ada buildMinAdaStep03",
          )
        ).transaction,
      });
    }
    if (input.stage === "step_04") {
      return Object.freeze({
        transaction: (
          await workflowStepTransaction(
            await buildMinAdaStep04({
              lucid: config.lucid,
              contracts: config.contracts,
              categoryId,
              signer: config.signer,
              threadOutRef: threadOutRef(),
              referenceScriptUtxo: config.references.steps[3],
              witnessReferenceScripts: config.references.witnesses,
            }),
            "min-ada buildMinAdaStep04",
          )
        ).transaction,
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
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

export type ManifestBoundMinAdaWorkflow = ManifestBoundFamilyWorkflow<
  "minAda",
  true,
  4
>;

export type AssemblyContext = FamilyAssemblyContext<
  "minAda",
  keyof FaultProofWitnessReferenceScripts,
  true,
  4
>;

export const boundConfigs = new WeakMap<AssemblyContext, BoundConfig>();
