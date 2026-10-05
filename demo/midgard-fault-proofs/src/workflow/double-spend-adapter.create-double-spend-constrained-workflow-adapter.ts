import {
  deriveFieldPreimageCertification,
  FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_HEX,
} from "@al-ft/midgard-sdk";
import type { UTxO } from "@lucid-evolution/lucid";

import { submitStep01 } from "../double-spend/submit-step-01.js";
import { submitStep02 } from "../double-spend/submit-step-02.js";
import { submitStep03 } from "../double-spend/submit-step-03.js";
import { submitStep04 } from "../double-spend/submit-step-04.js";
import { prepareDoubleSpendFromCanonicalEvidence } from "../evidence/prepare-from-evidence.js";
import {
  certifyFaultProofFieldCarriage,
  fieldPreimageCertificateAddress,
  findMissingFaultProofFieldPublication,
  resolveFaultProofFieldCarriagePublications,
  resolveFaultProofFieldPreimageCertificate,
} from "../field-opening.js";
import {
  publishProofChunks,
  resolvePublishedProofChunks,
} from "../publish-proof-chunks.js";
import {
  type StateQueueMutationLease,
  type StateQueueMutationLeaseCoordinator,
  submitRemoveFraudulentBlock,
} from "../remove-fraudulent-block.js";
import { parseSubmitStep01TxInclusion } from "../step-support.js";
import { submitInit } from "../submit-init.js";
import { admitSnapshot } from "./double-spend-adapter.create-double-spend-raw-l1-observation-port.js";
import { createDoubleSpendFieldOpeningPlan } from "./double-spend-adapter.field-opening-plan.js";
import {
  action,
  artifactFrom,
  confirmed,
  contentActionId,
  type DoubleSpendArtifact,
  type DoubleSpendConstrainedWorkflowAdapterConfig,
  journalValue,
  mutationLeaseRecovery,
  parseMutationLeaseRecovery,
  preflightOf,
  requireJournalString,
} from "./double-spend-adapter.preflight-of.js";
import type {
  FraudProofWorkflowJournalEntry,
  FraudProofWorkflowTerminal,
  JournalJsonObject,
} from "./journal.js";
import type {
  FraudProofFamilyWorkflowAdapter,
  FraudProofWorkflowAction,
  FraudProofWorkflowReconcileResult,
} from "./orchestrator.js";
import {
  FRAUD_PROOF_WORKFLOW_ADAPTER,
  FRAUD_PROOF_WORKFLOW_SAFETY,
} from "./orchestrator.js";
import { reconcileSignedWorkflowTransaction } from "./signed-transaction-reconciliation.js";
import {
  captureLocallyEvaluatedTransaction,
  type LocallyEvaluatedTransaction,
  submitCapturedTransaction,
  workflowTransactionInputOutRefs,
  workflowTransactionReferenceInputOutRefs,
} from "./transaction-boundary.js";

/**
 * Constrained adapter over the existing double-spend init/step/removal
 * builders. Its transaction boundary is real, but it is not production
 * registered until tier-3 carriage, raw L1 authentication, and durable lease
 * recovery are closed.
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
  const mutationLeaseByTxHash = new Map<string, StateQueueMutationLease>();
  const snapshot = async (headerHash: string) => {
    const observed = await config.l1.observe({ headerHash });
    return admitSnapshot({ headerHash, ...observed });
  };

  const proofChunks = async (proofCbor: string) =>
    await resolvePublishedProofChunks({
      lucid: config.lucid,
      address: config.signer.address,
      proofCbor,
    });

  const fieldPlan = createDoubleSpendFieldOpeningPlan(config);

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
        address: config.signer.address,
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
    entries,
  }: {
    readonly artifact: DoubleSpendArtifact;
    readonly entries: readonly FraudProofWorkflowJournalEntry[];
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
        requireJournalString(
          tx.inclusion.txMembershipProofCbor,
          "tx.inclusion.txMembershipProofCbor",
        ),
      );
      if (chunks === undefined && !confirmed(entries, publicationId)) {
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
      const missing = await findMissingFaultProofFieldPublication({
        lucid: config.lucid,
        publisherAddress: config.signer.address,
        planned: plan,
      });
      if (missing !== undefined) {
        const base = `${stage.kind}:publish-field:${missing.digest}`;
        return {
          kind: "action",
          action: action(contentActionId({ base, entries }), {
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
              lucid: config.lucid,
              publisherAddress: config.signer.address,
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
            action: action(contentActionId({ base, entries }), {
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
  }: {
    readonly action: FraudProofWorkflowAction;
    readonly artifact: DoubleSpendArtifact;
  }): Promise<LocallyEvaluatedTransaction> => {
    const stage = requireJournalString(requested.input.stage, "action.stage");
    if (stage === "publish-proof") {
      return await captureLocallyEvaluatedTransaction(async (boundary) => {
        await publishProofChunks({
          lucid: config.lucid,
          network: config.network,
          signer: config.signer,
          proofCbor: requireJournalString(
            requested.input.proofCbor,
            "action.proofCbor",
          ),
          preSubmitBoundary: boundary,
        });
      });
    }
    if (stage === "init") {
      return await captureLocallyEvaluatedTransaction(async (boundary) => {
        await submitInit({
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
          preSubmitBoundary: boundary,
        });
      });
    }
    if (stage === "step_01" || stage === "step_02") {
      const tx = stage === "step_01" ? artifact.tx1 : artifact.tx2;
      const chunks = await proofChunks(
        requireJournalString(
          tx.inclusion.txMembershipProofCbor,
          "tx.inclusion.txMembershipProofCbor",
        ),
      );
      if (chunks === undefined) {
        throw new Error(`${stage} proof chunks are not observable on L1`);
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
      return await captureLocallyEvaluatedTransaction(async (boundary) => {
        if (stage === "step_01") {
          await submitStep01({
            ...common,
            referenceScriptUtxo: config.referenceScripts.steps[0],
            preSubmitBoundary: boundary,
          });
        } else {
          await submitStep02({
            ...common,
            referenceScriptUtxo: config.referenceScripts.steps[1],
            preSubmitBoundary: boundary,
          });
        }
      });
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
      const publications = await resolveFaultProofFieldCarriagePublications({
        lucid: config.lucid,
        publisherAddress: config.signer.address,
        planned: plan,
      });
      if (publications === undefined) {
        throw new Error("tier-3 field publications are not observable on L1");
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
        throw new Error(
          "field certification action does not match the observed chunk UTxOs",
        );
      }
      return await captureLocallyEvaluatedTransaction(async (boundary) => {
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
          preSubmitBoundary: boundary,
          awaitConfirmation: false,
        });
      });
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
      if (stage === "publish-field") {
        const expectedMissing = await findMissingFaultProofFieldPublication({
          lucid: config.lucid,
          publisherAddress: config.signer.address,
          planned: plan,
        });
        if (
          expectedMissing === undefined ||
          requested.input.publicationDatumCbor !== expectedMissing.datumCbor
        ) {
          throw new Error(
            "field publication action does not match the next missing plan chunk",
          );
        }
      }
      const publications =
        stage === "publish-field"
          ? undefined
          : await resolveFaultProofFieldCarriagePublications({
              lucid: config.lucid,
              publisherAddress: config.signer.address,
              planned: plan,
            });
      if (stage !== "publish-field" && publications === undefined) {
        throw new Error("field carriage publications are not observable on L1");
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
        throw new Error(
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
      return await captureLocallyEvaluatedTransaction(async (boundary) => {
        if (proofStage === "step_03") {
          await submitStep03({
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
                  certificatePolicyId: config.fieldPreimageCertificate.policyId,
                }
              : {}),
            referenceScriptUtxo: config.referenceScripts.steps[2],
            preSubmitBoundary: boundary,
          });
        } else {
          await submitStep04({
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
                  certificatePolicyId: config.fieldPreimageCertificate.policyId,
                }
              : {}),
            referenceScriptUtxo: config.referenceScripts.steps[3],
            witnessReferenceScripts: config.referenceScripts.witnesses,
            preSubmitBoundary: boundary,
          });
        }
      });
    }
    if (stage === "remove") {
      let mutationLease: StateQueueMutationLease | undefined;
      const retainingCoordinator: StateQueueMutationLeaseCoordinator = {
        acquire: async () => {
          const acquired =
            await config.stateQueueMutationLeaseCoordinator.acquire();
          mutationLease = acquired;
          return acquired;
        },
      };
      const transaction = await captureLocallyEvaluatedTransaction(
        async (boundary) => {
          await submitRemoveFraudulentBlock({
            lucid: config.lucid,
            blueprint: config.blueprint,
            deploymentInfo: config.deploymentInfo,
            network: config.network,
            signer: config.signer,
            fraudCategory: "doubleSpend",
            fraudulentHeaderHash: artifact.headerHash,
            requireReferenceScripts: true,
            stateQueueMutationLeaseCoordinator: retainingCoordinator,
            ...(config.fraudProverRewardLovelace === undefined
              ? {}
              : {
                  fraudProverRewardLovelace: config.fraudProverRewardLovelace,
                }),
            preSubmitBoundary: async (transaction) => {
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
              await boundary(transaction);
            },
          });
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
      }) as JournalJsonObject;
    },
    observe: async ({ artifact, entries }) => {
      const next = await nextAction({
        artifact: artifactFrom(artifact),
        entries,
      });
      return next.kind === "completed"
        ? { kind: "completed", terminal: next.terminal }
        : next.kind === "conflict"
          ? { kind: "conflict", reason: next.reason }
          : { kind: "action_required", action: next.action };
    },
    preflight: async ({ action: requested, artifact }) => {
      const transaction = await capture({
        action: requested,
        artifact: artifactFrom(artifact),
      });
      if (!preparedByAction.has(requested.actionId)) {
        preparedByAction.set(requested.actionId, { transaction });
      }
      const prepared = preparedByAction.get(requested.actionId)!;
      return preflightOf(
        requested.actionId,
        transaction,
        prepared.mutationLease === undefined
          ? undefined
          : mutationLeaseRecovery(prepared.mutationLease),
      );
    },
    submit: async ({ action: requested, preflight }) => {
      const prepared = preparedByAction.get(requested.actionId);
      if (prepared === undefined) {
        throw new Error(
          `locally evaluated transaction for ${requested.actionId} is not available in this process`,
        );
      }
      if (prepared.transaction.txHash !== preflight.txHash) {
        throw new Error(
          "cached transaction does not match durable intent hash",
        );
      }
      const recoveryIdentity = parseMutationLeaseRecovery(
        preflight.durableRecovery,
      );
      if (
        (prepared.mutationLease === undefined) !==
          (recoveryIdentity === undefined) ||
        (prepared.mutationLease !== undefined &&
          (prepared.mutationLease.token !== recoveryIdentity?.token ||
            prepared.mutationLease.source !== recoveryIdentity.source))
      ) {
        throw new Error(
          "cached mutation lease does not match durable recovery identity",
        );
      }
      preparedByAction.delete(requested.actionId);
      if (prepared.mutationLease !== undefined) {
        mutationLeaseByTxHash.set(preflight.txHash, prepared.mutationLease);
      }
      return {
        kind: "submitted",
        txHash: await submitCapturedTransaction(prepared.transaction),
      };
    },
    reconcile: async ({
      action: requested,
      artifact,
      txHash,
      durableRecovery,
      signedTransactionCborHex,
      retirementOnly,
      authorizeResubmission,
    }) => {
      if (retirementOnly && txHash !== undefined)
        return await reconcileSignedWorkflowTransaction({
          transactionHash: txHash,
          signedTransactionCborHex,
          observe: config.l1?.observeSignedTransaction,
        });
      if (txHash === undefined) {
        return { kind: "conflict", reason: "durable intent omitted tx hash" };
      }
      const requiresMutationLease =
        requested.input.stage === "remove" &&
        requested.input.requiresMutationLease === true;
      let mutationLease = mutationLeaseByTxHash.get(txHash);
      const recoveryIdentity = parseMutationLeaseRecovery(durableRecovery);
      if (requiresMutationLease && recoveryIdentity === undefined) {
        return {
          kind: "conflict",
          reason:
            "descendant removal intent omitted its durable mutation-lease identity",
        };
      }
      if (!requiresMutationLease && recoveryIdentity !== undefined) {
        return {
          kind: "conflict",
          reason:
            "non-descendant action carried an unexpected mutation-lease identity",
        };
      }
      if (mutationLease === undefined && recoveryIdentity !== undefined) {
        if (config.stateQueueMutationLeaseCoordinator.resume === undefined) {
          return {
            kind: "conflict",
            reason:
              "mutation-lease coordinator cannot resume the journaled fencing token",
          };
        }
        try {
          mutationLease =
            await config.stateQueueMutationLeaseCoordinator.resume(
              recoveryIdentity,
            );
        } catch (cause) {
          return {
            kind: "conflict",
            reason: `journaled mutation lease cannot be resumed: ${String(cause)}`,
          };
        }
        mutationLeaseByTxHash.set(txHash, mutationLease);
      }
      const unconfirmed =
        async (): Promise<FraudProofWorkflowReconcileResult> => {
          const result =
            signedTransactionCborHex === undefined ||
            config.l1?.observeSignedTransaction === undefined
              ? { kind: "pending" as const, txHash }
              : await reconcileSignedWorkflowTransaction({
                  transactionHash: txHash,
                  signedTransactionCborHex,
                  observe: config.l1.observeSignedTransaction,
                  rebroadcast: config.l1.rebroadcastSignedTransaction,
                  authorizeResubmission:
                    authorizeResubmission === undefined
                      ? undefined
                      : async (signed) => {
                          await mutationLease?.renew();
                          await authorizeResubmission(signed);
                        },
                });
          if (result.kind === "not_found") {
            await mutationLease?.release();
            mutationLeaseByTxHash.delete(txHash);
          } else if (result.kind === "conflict") {
            await mutationLease?.fail(result.reason);
            mutationLeaseByTxHash.delete(txHash);
          } else {
            await mutationLease?.renew();
          }
          return result;
        };
      const preparedArtifact = artifactFrom(artifact);
      const actionStage = requireJournalString(
        requested.input.stage,
        "action.stage",
      );
      if (
        actionStage === "publish-proof" ||
        actionStage === "publish-field" ||
        actionStage === "certify-field"
      ) {
        const publicationObserver = config.l1?.publications;
        if (publicationObserver === undefined) {
          return {
            kind: "conflict",
            reason:
              "double-spend publication reconciliation has no authenticated raw-L1 observer",
          };
        }
        if (actionStage === "publish-proof") {
          const chunks = await proofChunks(
            requireJournalString(requested.input.proofCbor, "action.proofCbor"),
          );
          if (
            chunks === undefined ||
            chunks.some((chunk) => chunk.utxo.txHash !== txHash)
          ) {
            return await unconfirmed();
          }
          for (const chunk of chunks) {
            const observed = await publicationObserver.observeExact({
              headerHash: preparedArtifact.headerHash,
              kind: "field_publication",
              address: config.signer.address,
              expectedOutRef: chunk.outRef,
              expectedDatumCbor: chunk.datumCbor,
            });
            if (observed.kind !== "confirmed") return await unconfirmed();
          }
        } else {
          const proofFor = requireJournalString(
            requested.input.proofFor,
            "action.proofFor",
          );
          if (proofFor !== "step_03" && proofFor !== "step_04") {
            return {
              kind: "conflict",
              reason:
                "double-spend publication action names an invalid proof step",
            };
          }
          const plan = fieldPlan(proofFor, preparedArtifact);
          if (actionStage === "publish-field") {
            const candidates = await config.lucid.utxosAt(
              config.signer.address,
            );
            const candidate = candidates.find(
              (utxo) =>
                utxo.txHash === txHash &&
                utxo.datum === requested.input.publicationDatumCbor,
            );
            if (candidate?.datum == null) return await unconfirmed();
            const observed = await publicationObserver.observeExact({
              headerHash: preparedArtifact.headerHash,
              kind: "field_publication",
              address: config.signer.address,
              expectedOutRef: `${candidate.txHash}#${candidate.outputIndex.toString()}`,
              expectedDatumCbor: candidate.datum,
            });
            if (observed.kind !== "confirmed") return await unconfirmed();
          } else {
            const certificate = await resolveFaultProofFieldPreimageCertificate(
              {
                lucid: config.lucid,
                network: config.network,
                planned: plan,
                certificatePolicyId: config.fieldPreimageCertificate.policyId,
              },
            );
            if (certificate === undefined || certificate.txHash !== txHash) {
              return await unconfirmed();
            }
            const certification = deriveFieldPreimageCertification(plan.plan);
            const observed = await publicationObserver.observeExact({
              headerHash: preparedArtifact.headerHash,
              kind: "field_certificate",
              address: fieldPreimageCertificateAddress({
                network: config.network,
                certificatePolicyId: config.fieldPreimageCertificate.policyId,
              }),
              expectedOutRef: `${certificate.txHash}#${certificate.outputIndex.toString()}`,
              expectedDatumCbor: certification.datumCbor,
              expectedUnit: `${config.fieldPreimageCertificate.policyId}${FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_HEX}`,
            });
            if (observed.kind !== "confirmed") return await unconfirmed();
          }
        }
        return { kind: "confirmed", txHash };
      }
      if (config.l1 === undefined) {
        const legacyStatus = await config.lucid.transactionStatus(txHash);
        await mutationLease?.renew();
        return legacyStatus.status === "pending"
          ? { kind: "pending", txHash }
          : { kind: "not_found" };
      }
      if (config.l1.transactionConfirmed === undefined) {
        return {
          kind: "conflict",
          reason:
            "double-spend reconciliation has no authenticated unit-history transaction observer",
        };
      }
      const [observedStage, intendedTransactionConfirmed] = await Promise.all([
        snapshot(preparedArtifact.headerHash),
        config.l1.transactionConfirmed({
          headerHash: preparedArtifact.headerHash,
          txHash,
        }),
      ]);
      const stageAdvanced =
        actionStage === "init"
          ? observedStage.kind !== "not_started"
          : actionStage === "step_01"
            ? observedStage.kind !== "step_01"
            : actionStage === "step_02"
              ? observedStage.kind !== "step_02"
              : actionStage === "step_03"
                ? observedStage.kind !== "step_03"
                : actionStage === "step_04"
                  ? observedStage.kind === "proof_token" ||
                    observedStage.kind === "removed"
                  : actionStage === "remove"
                    ? observedStage.kind === "removed" ||
                      (observedStage.kind === "proof_token" &&
                        observedStage.nextRemovalOutRef !==
                          requested.input.nextRemovalOutRef)
                    : false;
      if (
        !intendedTransactionConfirmed &&
        actionStage === "remove" &&
        observedStage.kind === "proof_token"
      ) {
        return await unconfirmed();
      }
      if (stageAdvanced && !intendedTransactionConfirmed) {
        await mutationLease?.fail(
          "chain advanced without the journaled transaction in authenticated unit history",
        );
        mutationLeaseByTxHash.delete(txHash);
        return {
          kind: "conflict",
          reason:
            "double-spend chain advanced without the journaled transaction in authenticated unit history",
        };
      }
      if (stageAdvanced && intendedTransactionConfirmed) {
        await mutationLease?.release();
        mutationLeaseByTxHash.delete(txHash);
        return { kind: "confirmed", txHash };
      }
      if (
        "threadOutRef" in observedStage &&
        requested.input.threadOutRef !== observedStage.threadOutRef
      ) {
        return {
          kind: "conflict",
          reason:
            "double-spend computation thread changed without the journaled transaction",
        };
      }
      return await unconfirmed();
    },
  };
};
