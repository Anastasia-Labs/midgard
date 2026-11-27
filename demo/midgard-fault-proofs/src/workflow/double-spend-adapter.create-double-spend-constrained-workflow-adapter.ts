import { FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_HEX } from "@al-ft/midgard-sdk/core";
import type { UTxO } from "@lucid-evolution/lucid";

import { buildStep01 } from "../double-spend/submit-step-01.js";
import { buildStep02 } from "../double-spend/submit-step-02.js";
import { buildStep03 } from "../double-spend/submit-step-03.js";
import { buildStep04 } from "../double-spend/submit-step-04.submit-step04.js";
import { prepareDoubleSpendFromCanonicalEvidence } from "../evidence/prepare-from-evidence.js";
import {
  certifyFaultProofFieldCarriage,
  fieldPreimageCertificateAddress,
  resolveFaultProofFieldPreimageCertificate,
} from "../field-opening.certify-fault-proof-field-carriage.js";
import { resolveFaultProofFieldCarriagePublications } from "../field-opening.publish-fault-proof-field-carriage.js";
import { buildProofChunkPublication } from "../publish-proof-chunks.js";
import { buildRemoveFraudulentBlock } from "../remove-fraudulent-block.build-remove-fraudulent-block.js";
import type { StateQueueMutationLease } from "../remove-fraudulent-block.js";
import { parseSubmitStep01TxInclusion } from "../step-support.js";
import { buildInit } from "../submit-init.js";
import { WorkflowActionChangedError } from "./action-changed.js";
import { firstMissingFieldPublication } from "./bespoke-adapter-field-carriage.js";
import { admitSnapshot } from "./double-spend-adapter.create-double-spend-raw-l1-observation-port.js";
import {
  createDoubleSpendFieldOpeningPlan,
  createDoubleSpendOwnMaterials,
} from "./double-spend-adapter.field-opening-plan.js";
import {
  action,
  artifactFrom,
  type DoubleSpendArtifact,
  type DoubleSpendConstrainedWorkflowAdapterConfig,
  journalValue,
  preflightOf,
  requireJournalString,
} from "./double-spend-adapter.preflight-of.js";
import type {
  FraudProofWorkflowTerminal,
  WorkflowJsonObject,
} from "./identity.js";
import {
  FRAUD_PROOF_WORKFLOW_ADAPTER,
  FRAUD_PROOF_WORKFLOW_SAFETY,
} from "./orchestrator.fraud-proof-family-workflow-adapter.js";
import type {
  FraudProofFamilyWorkflowAdapter,
  FraudProofWorkflowAction,
} from "./orchestrator.js";
import { workflowProofMaterialsHolder } from "./proof-materials-view.holder.js";
import type { ProofMaterialsHolder } from "./proof-materials-view.js";
import { admitRemovalTransaction } from "./removal-admission.js";
import { workflowStepTransaction } from "./step-transaction.js";
import {
  type LocallyEvaluatedTransaction,
  submitCapturedTransaction,
  workflowTransactionInputOutRefs,
  workflowTransactionReferenceInputOutRefs,
} from "./transaction-boundary.js";

/**
 * Constrained adapter over the existing double-spend init/step/removal
 * builders. Its transaction boundary is real, but it is not production
 * registered until tier-3 carriage and raw L1 authentication are closed.
 */
export const createDoubleSpendConstrainedWorkflowAdapter = (
  config: DoubleSpendConstrainedWorkflowAdapterConfig,
): FraudProofFamilyWorkflowAdapter => {
  const preparedByAction = new Map<
    string,
    {
      readonly transaction: LocallyEvaluatedTransaction;
      readonly mutationLease?: StateQueueMutationLease;
    }
  >();
  const snapshot = async (headerHash: string) => {
    const observed = await config.l1.observe({ headerHash });
    return admitSnapshot({ headerHash, ...observed });
  };

  const fieldPlan = createDoubleSpendFieldOpeningPlan(config);
  const { proofChunks, materials: materialsFor } =
    createDoubleSpendOwnMaterials(config);

  const authenticatePublications = async ({
    headerHash,
    publications,
    certificate,
  }: {
    readonly headerHash: string;
    readonly publications: readonly UTxO[];
    readonly certificate?: UTxO;
  }): Promise<void> => {
    const observer = config.l1.publications;
    if (observer === undefined) {
      throw new Error(
        "production double-spend field inputs require authenticated publication observation",
      );
    }
    for (const publication of publications) {
      if (publication.datum == null) {
        throw new Error("double-spend publication omitted its inline datum");
      }
      const observed = await observer.observeExact({
        headerHash,
        kind: "field_publication",
        address: config.signer.proofMaterialsAddress,
        expectedOutRef: `${publication.txHash}#${publication.outputIndex.toString()}`,
        expectedDatumCbor: publication.datum,
      });
      if (observed.kind !== "confirmed") {
        throw new Error(
          `double-spend publication ${publication.txHash}#${publication.outputIndex.toString()} is not release-final`,
        );
      }
    }
    if (certificate !== undefined) {
      if (certificate.datum == null) {
        throw new Error("double-spend certificate omitted its inline datum");
      }
      const observed = await observer.observeExact({
        headerHash,
        kind: "field_certificate",
        address: fieldPreimageCertificateAddress({
          network: config.network,
          certificatePolicyId: config.fieldPreimageCertificate.policyId,
        }),
        expectedOutRef: `${certificate.txHash}#${certificate.outputIndex.toString()}`,
        expectedDatumCbor: certificate.datum,
        expectedUnit: `${config.fieldPreimageCertificate.policyId}${FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_HEX}`,
      });
      if (observed.kind !== "confirmed") {
        throw new Error(
          `double-spend certificate ${certificate.txHash}#${certificate.outputIndex.toString()} is not release-final`,
        );
      }
    }
  };

  const nextAction = async ({
    artifact,
    holder,
  }: {
    readonly artifact: DoubleSpendArtifact;
    readonly holder: ProofMaterialsHolder;
  }): Promise<
    | {
        readonly kind: "completed";
        readonly terminal: FraudProofWorkflowTerminal;
      }
    | { readonly kind: "conflict"; readonly reason: string }
    | { readonly kind: "action"; readonly action: FraudProofWorkflowAction }
  > => {
    const stage = await snapshot(artifact.headerHash);
    if (stage.kind === "removed") {
      return { kind: "completed", terminal: stage.terminal };
    }
    if (stage.kind === "not_started") {
      return {
        kind: "action",
        action: action("init", {
          stage: "init",
          stateQueueBlockOutRef: stage.stateQueueBlockOutRef,
        }),
      };
    }
    if (stage.kind === "step_01" || stage.kind === "step_02") {
      const tx = stage.kind === "step_01" ? artifact.tx1 : artifact.tx2;
      const publicationId = `${stage.kind}:publish-proof`;
      const chunks = await proofChunks(
        holder,
        requireJournalString(
          tx.inclusion.txMembershipProofCbor,
          "tx.inclusion.txMembershipProofCbor",
        ),
      );
      if (chunks === undefined) {
        return {
          kind: "action",
          action: action(publicationId, {
            stage: "publish-proof",
            proofFor: stage.kind,
            proofCbor: requireJournalString(
              tx.inclusion.txMembershipProofCbor,
              "tx.inclusion.txMembershipProofCbor",
            ),
          }),
        };
      }
      return {
        kind: "action",
        action: action(stage.kind, {
          stage: stage.kind,
          threadOutRef: stage.threadOutRef,
          stateQueueBlockOutRef: stage.stateQueueBlockOutRef,
        }),
      };
    }
    if (stage.kind === "step_03" || stage.kind === "step_04") {
      const plan = fieldPlan(stage.kind, artifact);
      const materials = materialsFor(holder);
      const missing = await firstMissingFieldPublication({
        lucid: config.lucid,
        publisherAddress: config.signer.proofMaterialsAddress,
        planned: plan,
        materials,
      });
      if (missing !== undefined) {
        const base = `${stage.kind}:publish-field:${missing.digest}`;
        return {
          kind: "action",
          action: action(base, {
            stage: "publish-field",
            proofFor: stage.kind,
            threadOutRef: stage.threadOutRef,
            publicationDatumCbor: missing.datumCbor,
          }),
        };
      }
      if (plan.plan.tier === "Certified") {
        const certificate = await resolveFaultProofFieldPreimageCertificate({
          lucid: config.lucid,
          network: config.network,
          planned: plan,
          certificatePolicyId: config.fieldPreimageCertificate.policyId,
        });
        if (certificate === undefined) {
          const publications = await resolveFaultProofFieldCarriagePublications(
            {
              materials,
              lucid: config.lucid,
              publisherAddress: config.signer.proofMaterialsAddress,
              planned: plan,
            },
          );
          if (publications === undefined) {
            throw new Error(
              "tier-3 field publications disappeared before certification",
            );
          }
          const base = `${stage.kind}:certify-field:${plan.commitment}`;
          return {
            kind: "action",
            action: action(base, {
              stage: "certify-field",
              proofFor: stage.kind,
              fieldCommitment: plan.commitment,
              chunkOutRefs: publications.map(
                (utxo) => `${utxo.txHash}#${utxo.outputIndex.toString()}`,
              ),
            }),
          };
        }
        return {
          kind: "action",
          action: action(stage.kind, {
            stage: stage.kind,
            threadOutRef: stage.threadOutRef,
            certificateOutRef: `${certificate.txHash}#${certificate.outputIndex.toString()}`,
          }),
        };
      }
      return {
        kind: "action",
        action: action(stage.kind, {
          stage: stage.kind,
          threadOutRef: stage.threadOutRef,
        }),
      };
    }
    if (stage.kind !== "proof_token") {
      throw new Error(
        `unsupported authenticated double-spend stage: ${String(stage.kind)}`,
      );
    }
    return {
      kind: "action",
      action: action(`remove:${stage.nextRemovalOutRef}`, {
        stage: "remove",
        fraudProofOutRef: stage.fraudProofOutRef,
        nextRemovalOutRef: stage.nextRemovalOutRef,
        requiresMutationLease:
          stage.nextRemovalOutRef !== stage.stateQueueBlockOutRef,
      }),
    };
  };

  const capture = async ({
    action: requested,
    artifact,
    holder,
  }: {
    readonly action: FraudProofWorkflowAction;
    readonly artifact: DoubleSpendArtifact;
    readonly holder: ProofMaterialsHolder;
  }): Promise<LocallyEvaluatedTransaction> => {
    const stage = requireJournalString(requested.input.stage, "action.stage");
    if (stage === "publish-proof") {
      return (
        await buildProofChunkPublication({
          lucid: config.lucid,
          signer: config.signer,
          proofCbor: requireJournalString(
            requested.input.proofCbor,
            "action.proofCbor",
          ),
        })
      ).transaction;
    }
    if (stage === "init") {
      return (
        await workflowStepTransaction(
          await buildInit({
            lucid: config.lucid,
            blueprint: config.blueprint,
            deploymentInfo: config.deploymentInfo,
            network: config.network,
            signer: config.signer,
            fraudCategory: "doubleSpend",
            fraudulentBlockOutRef: requireJournalString(
              requested.input.stateQueueBlockOutRef,
              "action.stateQueueBlockOutRef",
            ),
            fraudulentHeaderHash: artifact.headerHash,
            witnessReferenceScripts: config.referenceScripts.witnesses,
          }),
          "double-spend-adapter buildInit",
        )
      ).transaction;
    }
    if (stage === "step_01" || stage === "step_02") {
      const tx = stage === "step_01" ? artifact.tx1 : artifact.tx2;
      const chunks = await proofChunks(
        holder,
        requireJournalString(
          tx.inclusion.txMembershipProofCbor,
          "tx.inclusion.txMembershipProofCbor",
        ),
      );
      if (chunks === undefined) {
        throw new WorkflowActionChangedError(`${stage} chunks gone from L1`);
      }
      await authenticatePublications({
        headerHash: artifact.headerHash,
        publications: chunks.map((chunk) => chunk.utxo),
      });
      const common = {
        lucid: config.lucid,
        blueprint: config.blueprint,
        deploymentInfo: config.deploymentInfo,
        network: config.network,
        signer: config.signer,
        threadOutRef: requireJournalString(
          requested.input.threadOutRef,
          "action.threadOutRef",
        ),
        stateQueueBlockOutRef: requireJournalString(
          requested.input.stateQueueBlockOutRef,
          "action.stateQueueBlockOutRef",
        ),
        txInclusion: parseSubmitStep01TxInclusion(tx.inclusion),
        publishedProofChunks: chunks,
        witnessReferenceScripts: config.referenceScripts.witnesses,
      } as const;
      return (
        await workflowStepTransaction(
          await (async () => {
            if (stage === "step_01") {
              return await buildStep01({
                ...common,
                referenceScriptUtxo: config.referenceScripts.steps[0],
              });
            } else {
              return await buildStep02({
                ...common,
                referenceScriptUtxo: config.referenceScripts.steps[1],
              });
            }
          })(),
          `double-spend ${stage}`,
        )
      ).transaction;
    }
    if (stage === "certify-field") {
      const proofStage = requireJournalString(
        requested.input.proofFor,
        "action.proofFor",
      );
      if (proofStage !== "step_03" && proofStage !== "step_04") {
        throw new Error("field certification names an unknown proof stage");
      }
      const tx = proofStage === "step_03" ? artifact.tx1 : artifact.tx2;
      const plan = fieldPlan(proofStage, artifact);
      if (
        plan.plan.tier !== "Certified" ||
        requested.input.fieldCommitment !== plan.commitment
      ) {
        throw new Error(
          "field certification action does not match the tier-3 plan",
        );
      }
      const materials = materialsFor(holder);
      const publications = await resolveFaultProofFieldCarriagePublications({
        lucid: config.lucid,
        publisherAddress: config.signer.proofMaterialsAddress,
        planned: plan,
        materials,
      });
      if (publications === undefined) {
        // Observation scheduled this action only once every chunk was on L1,
        // so a chunk gone now (a rollback or a spend) changes the next action.
        throw new WorkflowActionChangedError(
          "tier-3 field publications are no longer observable on L1",
        );
      }
      await authenticatePublications({
        headerHash: artifact.headerHash,
        publications,
      });
      const observedOutRefs = publications.map(
        (utxo) => `${utxo.txHash}#${utxo.outputIndex.toString()}`,
      );
      if (
        JSON.stringify(requested.input.chunkOutRefs) !==
        JSON.stringify(observedOutRefs)
      ) {
        throw new WorkflowActionChangedError(
          "field certification action does not match the observed chunk UTxOs",
        );
      }
      return (
        await certifyFaultProofFieldCarriage({
          lucid: config.lucid,
          network: config.network,
          signer: config.signer,
          planned: plan,
          certificatePolicyId: config.fieldPreimageCertificate.policyId,
          certificateMintingScript:
            config.fieldPreimageCertificate.mintingScript,
          certificateReferenceScriptUtxo:
            config.fieldPreimageCertificate.referenceScriptUtxo,
          chunkUtxos: publications,
          compactCbor: tx.nativeTxCompactCbor,
        })
      ).transaction;
    }
    if (
      stage === "step_03" ||
      stage === "step_04" ||
      stage === "publish-field"
    ) {
      const proofStage =
        stage === "publish-field"
          ? requireJournalString(requested.input.proofFor, "action.proofFor")
          : stage;
      if (proofStage !== "step_03" && proofStage !== "step_04") {
        throw new Error("field action names an unknown proof stage");
      }
      const tx = proofStage === "step_03" ? artifact.tx1 : artifact.tx2;
      const plan = fieldPlan(proofStage, artifact);
      const materials = materialsFor(holder);
      if (stage === "publish-field") {
        const expectedMissing = await firstMissingFieldPublication({
          lucid: config.lucid,
          publisherAddress: config.signer.proofMaterialsAddress,
          planned: plan,
          materials,
        });
        if (
          expectedMissing === undefined ||
          requested.input.publicationDatumCbor !== expectedMissing.datumCbor
        ) {
          throw new WorkflowActionChangedError(
            "field publication action does not match the next missing plan chunk",
          );
        }
      }
      const publications =
        stage === "publish-field"
          ? undefined
          : await resolveFaultProofFieldCarriagePublications({
              materials,
              lucid: config.lucid,
              publisherAddress: config.signer.proofMaterialsAddress,
              planned: plan,
            });
      if (stage !== "publish-field" && publications === undefined) {
        throw new WorkflowActionChangedError("field chunks gone from L1");
      }
      const certificate =
        stage !== "publish-field" && plan.plan.tier === "Certified"
          ? await resolveFaultProofFieldPreimageCertificate({
              lucid: config.lucid,
              network: config.network,
              planned: plan,
              certificatePolicyId: config.fieldPreimageCertificate.policyId,
            })
          : undefined;
      if (
        stage !== "publish-field" &&
        plan.plan.tier === "Certified" &&
        (certificate === undefined ||
          requested.input.certificateOutRef !==
            `${certificate.txHash}#${certificate.outputIndex.toString()}`)
      ) {
        throw new WorkflowActionChangedError(
          "tier-3 proof step does not bind the observed field certificate",
        );
      }
      if (stage !== "publish-field") {
        await authenticatePublications({
          headerHash: artifact.headerHash,
          publications: publications ?? [],
          ...(certificate === undefined ? {} : { certificate }),
        });
      }
      return (
        await workflowStepTransaction(
          await (async () => {
            if (proofStage === "step_03") {
              return await buildStep03({
                materials,
                lucid: config.lucid,
                blueprint: config.blueprint,
                deploymentInfo: config.deploymentInfo,
                network: config.network,
                signer: config.signer,
                threadOutRef: requireJournalString(
                  requested.input.threadOutRef,
                  "action.threadOutRef",
                ),
                tx1SpendInputCbors: tx.spendInputCbors,
                nativeTxCompactCbor: tx.nativeTxCompactCbor,
                doubleSpentInputIndex: BigInt(tx.doubleSpentInputIndex),
                ...(publications === undefined
                  ? {}
                  : { publishedCarriageUtxos: publications }),
                ...(certificate === undefined
                  ? {}
                  : { certificateUtxo: certificate }),
                ...(plan.plan.tier === "Certified"
                  ? {
                      certificatePolicyId:
                        config.fieldPreimageCertificate.policyId,
                    }
                  : {}),
                referenceScriptUtxo: config.referenceScripts.steps[2],
              });
            } else {
              return await buildStep04({
                materials,
                lucid: config.lucid,
                blueprint: config.blueprint,
                deploymentInfo: config.deploymentInfo,
                network: config.network,
                signer: config.signer,
                threadOutRef: requireJournalString(
                  requested.input.threadOutRef,
                  "action.threadOutRef",
                ),
                tx2SpendInputCbors: tx.spendInputCbors,
                nativeTxCompactCbor: tx.nativeTxCompactCbor,
                doubleSpentInputIndex: BigInt(tx.doubleSpentInputIndex),
                ...(publications === undefined
                  ? {}
                  : { publishedCarriageUtxos: publications }),
                ...(certificate === undefined
                  ? {}
                  : { certificateUtxo: certificate }),
                ...(plan.plan.tier === "Certified"
                  ? {
                      certificatePolicyId:
                        config.fieldPreimageCertificate.policyId,
                    }
                  : {}),
                referenceScriptUtxo: config.referenceScripts.steps[3],
                witnessReferenceScripts: config.referenceScripts.witnesses,
              });
            }
          })(),
          `double-spend ${proofStage}`,
        )
      ).transaction;
    }
    if (stage === "remove") {
      const { transaction, mutationLease } = await admitRemovalTransaction(
        await buildRemoveFraudulentBlock({
          lucid: config.lucid,
          blueprint: config.blueprint,
          deploymentInfo: config.deploymentInfo,
          network: config.network,
          signer: config.signer,
          fraudCategory: "doubleSpend",
          fraudulentHeaderHash: artifact.headerHash,
          fraudProofOutRef: requireJournalString(
            requested.input.fraudProofOutRef,
            "action.fraudProofOutRef",
          ),
          requireReferenceScripts: true,
          stateQueueMutationLeaseCoordinator:
            config.stateQueueMutationLeaseCoordinator,
          ...(config.fraudProverRewardLovelace === undefined
            ? {}
            : {
                fraudProverRewardLovelace: config.fraudProverRewardLovelace,
              }),
        }),
        async (transaction) => {
          if (
            !workflowTransactionInputOutRefs(transaction.signed).includes(
              requireJournalString(
                requested.input.nextRemovalOutRef,
                "action.nextRemovalOutRef",
              ),
            )
          ) {
            throw new Error(
              "removal transaction does not consume the authenticated next state-queue outRef",
            );
          }
          if (
            !workflowTransactionReferenceInputOutRefs(
              transaction.signed,
            ).includes(
              requireJournalString(
                requested.input.fraudProofOutRef,
                "action.fraudProofOutRef",
              ),
            )
          ) {
            throw new Error(
              "removal transaction does not reference the authenticated permanent proof token",
            );
          }
        },
      );
      preparedByAction.set(requested.actionId, {
        transaction,
        ...(mutationLease === undefined ? {} : { mutationLease }),
      });
      if (
        (requested.input.requiresMutationLease === true) !==
        (mutationLease !== undefined)
      ) {
        await mutationLease?.fail(
          "authenticated removal topology disagreed with lease requirement",
        );
        preparedByAction.delete(requested.actionId);
        throw new Error(
          "authenticated removal topology disagreed with mutation-lease acquisition",
        );
      }
      return transaction;
    }
    throw new Error(`unknown double-spend workflow action stage: ${stage}`);
  };

  return {
    adapterVersion: FRAUD_PROOF_WORKFLOW_ADAPTER,
    category: "doubleSpend",
    safety: FRAUD_PROOF_WORKFLOW_SAFETY,
    prepare: async ({ evidence }) => {
      const prepared = await prepareDoubleSpendFromCanonicalEvidence({
        evidence,
      });
      return journalValue({
        headerHash: prepared.headerHash,
        tx1: {
          inclusion: prepared.tx1.txInclusion,
          nativeTxId: prepared.tx1.nodeTxId,
          nativeTxCompactCbor: prepared.tx1.nativeTxCompactCbor,
          spendInputCbors: prepared.tx1.spendInputCbors,
          doubleSpentInputIndex: prepared.tx1.doubleSpentInputIndex,
        },
        tx2: {
          inclusion: prepared.tx2.txInclusion,
          nativeTxId: prepared.tx2.nodeTxId,
          nativeTxCompactCbor: prepared.tx2.nativeTxCompactCbor,
          spendInputCbors: prepared.tx2.spendInputCbors,
          doubleSpentInputIndex: prepared.tx2.doubleSpentInputIndex,
        },
      }) as WorkflowJsonObject;
    },
    observe: async ({ artifact, workflowId, identity }) => {
      const next = await nextAction({
        artifact: artifactFrom(artifact),
        holder: workflowProofMaterialsHolder({ workflowId, identity }),
      });
      return next.kind === "completed"
        ? { kind: "completed", terminal: next.terminal }
        : next.kind === "conflict"
          ? { kind: "conflict", reason: next.reason }
          : { kind: "action_required", action: next.action };
    },
    preflight: async ({
      action: requested,
      artifact,
      workflowId,
      identity,
    }) => {
      const transaction = await capture({
        action: requested,
        artifact: artifactFrom(artifact),
        holder: workflowProofMaterialsHolder({ workflowId, identity }),
      });
      if (!preparedByAction.has(requested.actionId)) {
        preparedByAction.set(requested.actionId, { transaction });
      }
      return preflightOf(requested.actionId, transaction);
    },
    submit: async ({ action: requested, preflight }) => {
      const prepared = preparedByAction.get(requested.actionId);
      if (prepared === undefined) {
        throw new Error(
          `locally evaluated transaction for ${requested.actionId} is not available in this process`,
        );
      }
      if (prepared.transaction.txHash !== preflight.txHash) {
        throw new Error("cached transaction does not match the preflight hash");
      }
      preparedByAction.delete(requested.actionId);
      try {
        return {
          kind: "submitted",
          txHash: await submitCapturedTransaction(prepared.transaction),
        };
      } finally {
        // The runner follows the submitted transaction on chain, so the
        // lease ends with its submission.
        await prepared.mutationLease?.release();
      }
    },
  };
};
