import { outRefLabel } from "@al-ft/midgard-core";
import { FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_HEX } from "@al-ft/midgard-sdk/core";
import { NetworkIdForcedScanDatum } from "@al-ft/midgard-sdk/fraud-proof";
import { Data, toUnit, type UTxO } from "@lucid-evolution/lucid";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { isPrerequisiteTurn } from "../fault-proof-turn.js";
import {
  certifyFaultProofFieldCarriage,
  fieldPreimageCertificateAddress,
  resolveFaultProofFieldPreimageCertificate,
} from "../field-opening.certify-fault-proof-field-carriage.js";
import type { FaultProofFieldOpeningPlan } from "../field-opening.plan-fault-proof-field-opening.js";
import {
  publishFaultProofFieldCarriage,
  resolveFaultProofFieldCarriagePublications,
} from "../field-opening.publish-fault-proof-field-carriage.js";
import { buildRemoveFraudulentBlock } from "../remove-fraudulent-block.build-remove-fraudulent-block.js";
import type { StateQueueMutationLease } from "../remove-fraudulent-block.js";
import { NETWORK_ID_FORCED_SCAN_DEPLOYMENT_ENTRY } from "../runtime.js";
import { WorkflowActionChangedError } from "../workflow/action-changed.js";
import {
  addressFieldCarriageOwnOutputs,
  firstUnclaimedFieldPublication,
} from "../workflow/bespoke-adapter-field-carriage.js";
import type { CanonicalBlockClassification } from "../workflow/classification.js";
import type {
  FraudProofWorkflowIdentity,
  JournalJsonObject,
} from "../workflow/journal.js";
import {
  FRAUD_PROOF_WORKFLOW_ADAPTER,
  FRAUD_PROOF_WORKFLOW_SAFETY,
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowObservation,
  type FraudProofWorkflowPreflight,
} from "../workflow/orchestrator.fraud-proof-family-workflow-adapter.js";
import { createOwnOutputJournal } from "../workflow/own-outputs.js";
import { workflowProofMaterialsHolder } from "../workflow/proof-materials-view.holder.js";
import { proofMaterialsAccess } from "../workflow/proof-materials-view.js";
import { admitRemovalTransaction } from "../workflow/removal-admission.js";
import { workflowStepTransaction } from "../workflow/step-transaction.js";
import {
  bindWorkflowPreflightTransaction,
  LOCAL_UPLC_EVALUATOR,
  type LocallyEvaluatedTransaction,
  requireReferenceOnlyScriptWitnesses,
  submitCapturedTransaction,
  workflowTransactionInputOutRefs,
  workflowTransactionReferenceInputOutRefs,
} from "../workflow/transaction-boundary.js";
import type { PreparedNetworkIdAcceptedForced } from "./accepted-forced.js";
import type { NetworkIdForcedScanStep } from "./forced-scan-plan.advance-grammar-by.js";
import {
  networkIdForcedScanStepForState,
  networkIdForcedScanStepOrdinal,
  planNetworkIdForcedScan,
} from "./forced-scan-plan.plan-network-id-forced-scan.js";
import {
  planNetworkIdOutputsOpening,
  type PreparedNetworkIdProof,
} from "./prepare.js";
import { buildNetworkIdAcceptedForcedBind } from "./submit-network-id-accepted-forced-bind.js";
import { buildNetworkIdForcedBind } from "./submit-network-id-forced-bind.js";
import { buildNetworkIdForcedScanAction } from "./submit-network-id-forced-scan.submit-network-id-forced-scan-action.js";
import { buildNetworkIdForcedStep01 } from "./submit-network-id-forced-step-01.js";
import { buildNetworkIdInit } from "./submit-network-id-init.js";
import { buildNetworkIdStep01 } from "./submit-network-id-step-01.js";
import { buildNetworkIdStep02 } from "./submit-network-id-step-02.submit-network-id-step02.js";
import {
  admitNetworkIdWorkflowArtifact,
  type AdmittedNetworkIdArtifact,
} from "./workflow-adapter.admit-network-id-forced-artifact.js";
import type { NetworkIdWorkflowAdapterConfig } from "./workflow-adapter.create-network-id-raw-l1-observation-port.js";
import { createNetworkIdForcedStepContract } from "./workflow-adapter.forced-step-contract.js";
import { createNetworkIdOwnThreadReads } from "./workflow-adapter.own-thread-reads.js";
import { prepareNetworkIdWorkflowArtifact } from "./workflow-adapter.prepare-artifact.js";
import {
  action,
  actionKind,
  CATEGORY,
  forcedScanKindFor,
  isForcedScanKind,
  requireString,
  type WorkflowContext,
} from "./workflow-adapter.prepared-from-artifact.js";
import type { PreparedNetworkIdWrongfulRejection } from "./wrongful-rejection.js";

/**
 * Concrete last-header adapter. It captures the actual signed transaction at
 * the post-local-evaluation/pre-network boundary, then submits exactly the
 * captured body.
 */
export const createNetworkIdWorkflowAdapter = (
  config: NetworkIdWorkflowAdapterConfig,
): FraudProofFamilyWorkflowAdapter => {
  const captured = new Map<
    string,
    {
      readonly transaction: LocallyEvaluatedTransaction;
      readonly mutationLease?: StateQueueMutationLease;
    }
  >();
  // The removal this process last submitted per header: the legacy
  // provider route reads its completion facts from it, since only the raw-L1
  // route derives a removal from the chain. A restart forgets it.
  const submittedRemovals = new Map<
    string,
    Readonly<{ txHash: string; removedStateQueueOutRef: string }>
  >();
  const category = CATEGORY;

  const { requireForcedStep, requireForcedStepReferenceScript } =
    createNetworkIdForcedStepContract(config);

  const { requireForcedScan, forcedScanThread, forcedThreadOutRef, live } =
    createNetworkIdOwnThreadReads(config);

  const requireForcedScanReferenceScript = (): UTxO => {
    const referenceScript = config.forcedScanReferenceScript;
    if (referenceScript === undefined) {
      throw new Error(
        `network-id forced direction requires the published ${NETWORK_ID_FORCED_SCAN_DEPLOYMENT_ENTRY} reference script`,
      );
    }
    return referenceScript;
  };

  /**
   * The §2.5 field-2 opening this direction's final step needs, or `undefined`
   * when the claim opens no field at all. The forced contradiction always opens
   * the complete output field, and publishes it: the door reads every output's
   * network id, so the bytes never ride the step redeemer.
   */
  const outputsOpeningPlanFor = (
    admitted: AdmittedNetworkIdArtifact,
  ): FaultProofFieldOpeningPlan | undefined =>
    admitted.direction === "forced"
      ? planNetworkIdOutputsOpening({
          prepared: admitted.prepared,
          owner: config.signer.paymentKeyHash,
          publish: true,
        })
      : admitted.prepared.faultClaim.kind === "output-network"
        ? planNetworkIdOutputsOpening({
            prepared: admitted.prepared,
            owner: config.signer.paymentKeyHash,
          })
        : undefined;

  /**
   * What the *final* step opens. The forced direction opens nothing there any
   * more: `forced_scan` certifies field 2 in batches and step 02 takes its
   * terminal state as established, so re-opening the field at finalization
   * would put back exactly the single-transaction budget the scan escapes.
   */
  const finalStepOutputsOpeningPlanFor = (
    admitted: AdmittedNetworkIdArtifact,
  ): FaultProofFieldOpeningPlan | undefined =>
    admitted.direction === "forced"
      ? undefined
      : outputsOpeningPlanFor(admitted);

  /** The carriage of `opening`: publications found by content at the
   * proof-materials address for the workflow of `context`, the certificate
   * by its token and datum at the certificate address. */
  const fieldCarriage = async (
    opening: FaultProofFieldOpeningPlan,
    context: Readonly<{
      workflowId: string;
      identity: FraudProofWorkflowIdentity;
    }>,
  ) =>
    await addressFieldCarriageOwnOutputs({
      materials: proofMaterialsAccess({
        signer: config.signer,
        lucid: config.lucid,
        holder: workflowProofMaterialsHolder(context),
      }),
      lucid: config.lucid,
      network: config.network,
      publisherAddress: config.signer.proofMaterialsAddress,
      planned: opening,
      certificatePolicyId: config.contracts.fieldPreimageCertificatePolicyId,
    });

  /** The planned §10 batch schedule for an admitted forced artifact. */
  const forcedScanPlanFor = (admitted: AdmittedNetworkIdArtifact) => {
    const opening = outputsOpeningPlanFor(admitted);
    if (opening === undefined) {
      throw new Error(
        "network-id forced scan requires the authenticated field-2 opening",
      );
    }
    return {
      opening,
      plan: planNetworkIdForcedScan({
        outputsCarriagePlan: opening,
        outputCount: opening.itemCount,
      }),
    };
  };

  /** The batch the live scan thread is waiting for, read from its datum. */
  const forcedScanNextStep = ({
    utxo,
    plan,
  }: {
    readonly utxo: UTxO;
    readonly plan: ReturnType<typeof forcedScanPlanFor>["plan"];
  }): NetworkIdForcedScanStep => {
    if (utxo.datum == null) {
      throw new Error(
        `network-id forced scan thread ${outRefLabel(utxo)} has no inline datum`,
      );
    }
    const datum = Data.from(utxo.datum, NetworkIdForcedScanDatum);
    if (datum.data === null) {
      throw new Error(
        "network-id forced scan thread carries an empty scan state",
      );
    }
    return networkIdForcedScanStepForState(plan, datum.data);
  };

  const authenticateFieldInputs = async ({
    prepared,
    publications,
    certificate,
  }: {
    readonly prepared:
      | PreparedNetworkIdProof
      | PreparedNetworkIdWrongfulRejection
      | PreparedNetworkIdAcceptedForced;
    readonly publications: readonly UTxO[];
    readonly certificate?: UTxO;
  }): Promise<void> => {
    if (config.rawL1 === undefined) return;
    const observer = config.rawL1.publications;
    if (observer === undefined) {
      throw new Error(
        "production network-id field inputs require authenticated publication observation",
      );
    }
    for (const publication of publications) {
      if (publication.datum == null) {
        throw new Error(
          "network-id field publication omitted its inline datum",
        );
      }
      const observed = await observer.observeExact({
        headerHash: prepared.headerHash,
        kind: "field_publication",
        address: config.signer.proofMaterialsAddress,
        expectedOutRef: outRefLabel(publication),
        expectedDatumCbor: publication.datum,
      });
      if (observed.kind !== "confirmed") {
        throw new Error(
          `network-id field publication ${outRefLabel(publication)} is not release-final`,
        );
      }
    }
    if (certificate !== undefined) {
      if (certificate.datum == null) {
        throw new Error(
          "network-id field certificate omitted its inline datum",
        );
      }
      const observed = await observer.observeExact({
        headerHash: prepared.headerHash,
        kind: "field_certificate",
        address: fieldPreimageCertificateAddress({
          network: config.network,
          certificatePolicyId:
            config.contracts.fieldPreimageCertificatePolicyId,
        }),
        expectedOutRef: outRefLabel(certificate),
        expectedDatumCbor: certificate.datum,
        expectedUnit: `${config.contracts.fieldPreimageCertificatePolicyId}${FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_HEX}`,
      });
      if (observed.kind !== "confirmed") {
        throw new Error(
          `network-id field certificate ${outRefLabel(certificate)} is not release-final`,
        );
      }
    }
  };

  const observe = async (
    context: WorkflowContext,
  ): Promise<FraudProofWorkflowObservation> => {
    const admitted = admitNetworkIdWorkflowArtifact(context.artifact);
    const prepared = admitted.prepared;
    const rawStage = await config.rawL1?.observe({
      headerHash: prepared.headerHash,
    });
    if (rawStage?.kind === "removed") {
      return { kind: "completed", terminal: rawStage.terminal };
    }
    if (rawStage?.kind === "pending")
      return { kind: "pending", reason: rawStage.reason };
    const state =
      rawStage === undefined ? await live(prepared.headerHash) : undefined;
    const proofUnit =
      state?.proofUnit ??
      toUnit(
        config.contracts.fraudProof.policyId,
        `${config.category.categoryId}${prepared.headerHash}`,
      );
    const stateQueueOutRef =
      rawStage === undefined
        ? state?.stateQueue === undefined
          ? undefined
          : outRefLabel(state.stateQueue)
        : rawStage.stateQueueBlockOutRef;
    const step01OutRef =
      rawStage?.kind === "step" && rawStage.step === 1
        ? rawStage.threadOutRef
        : state?.step01 === undefined
          ? undefined
          : outRefLabel(state.step01);
    const step02OutRef =
      rawStage?.kind === "step" && rawStage.step === 2
        ? rawStage.threadOutRef
        : state?.step02 === undefined
          ? undefined
          : outRefLabel(state.step02);
    const proofOutRef =
      rawStage?.kind === "proof_token"
        ? rawStage.fraudProofOutRef
        : state?.proof === undefined
          ? undefined
          : outRefLabel(state.proof);
    if (stateQueueOutRef === undefined) {
      if (proofOutRef === undefined) {
        return {
          kind: "conflict",
          reason:
            "fraudulent header disappeared without the permanent Q35 proof token",
        };
      }
      const removal = submittedRemovals.get(prepared.headerHash);
      if (removal === undefined) {
        return {
          kind: "conflict",
          reason:
            "fraudulent header disappeared and the provider route cannot derive its removal",
        };
      }
      const removalTxHash = removal.txHash;
      if (state?.proof === undefined) {
        return {
          kind: "conflict",
          reason: "raw L1 removal did not provide a derived terminal",
        };
      }
      if (config.terminalFacts === undefined) {
        return {
          kind: "conflict",
          reason:
            "legacy network-id observation has no authenticated terminal-facts authority",
        };
      }
      const facts = await config.terminalFacts({
        headerHash: prepared.headerHash,
        removalTxHash,
        proofTokenOutRef: proofOutRef,
      });
      return {
        kind: "completed",
        terminal: {
          schemaVersion: "midgard-fraud-proof-workflow-terminal-v1",
          category,
          headerHash: prepared.headerHash,
          proofToken: {
            unit: proofUnit,
            outRef: proofOutRef,
            createdByTxHash: state.proof.txHash,
            retainedAtFinalState: true,
          },
          correction: {
            removalTxHash,
            removedStateQueueOutRef: removal.removedStateQueueOutRef,
            fraudulentHeaderAbsent: true,
            referencedProofTokenOutRef: proofOutRef,
          },
          economics: facts.economics,
          observedAt: facts.observedAt,
        },
      };
    }
    if (proofOutRef !== undefined) {
      if (
        rawStage === undefined &&
        (config.removal.isCurrentHead === undefined ||
          !(await config.removal.isCurrentHead(prepared.headerHash)))
      ) {
        return {
          kind: "conflict",
          reason:
            "network-id adapter requires one journal action per descendant removal; target is not the current head",
        };
      }
      return {
        kind: "action_required",
        // The removal action names the outputs it spends: the next removal
        // and the fraud proof (`nextRemovalOutRef`, `fraudProofOutRef`).
        action: action("remove", {
          nextRemovalOutRef:
            rawStage?.kind === "proof_token"
              ? rawStage.nextRemovalOutRef
              : stateQueueOutRef,
          targetStateQueueBlockOutRef: stateQueueOutRef,
          fraudProofOutRef: proofOutRef,
          requiresMutationLease:
            rawStage?.kind === "proof_token" &&
            rawStage.nextRemovalOutRef !== rawStage.stateQueueBlockOutRef
              ? "true"
              : "false",
        }),
      };
    }
    if (step02OutRef !== undefined) {
      const opening = finalStepOutputsOpeningPlanFor(admitted);
      if (opening !== undefined) {
        const ownOutputs = await fieldCarriage(opening, context);
        const missing = firstUnclaimedFieldPublication({
          publisherAddress: config.signer.proofMaterialsAddress,
          planned: opening,
          ownOutputs,
        });
        if (missing !== undefined) {
          const base = `network-id:publish-field:${opening.commitment}:${missing.digest}`;
          return {
            kind: "action_required",
            action: action(
              "publish_field",
              {
                threadOutRef: step02OutRef,
                fieldCommitment: opening.commitment,
                publicationDatumCbor: missing.datumCbor,
              },
              base,
            ),
          };
        }
        if (opening.plan.tier === "Certified") {
          const certificate = await resolveFaultProofFieldPreimageCertificate({
            ownOutputs,
            lucid: config.lucid,
            network: config.network,
            planned: opening,
            certificatePolicyId:
              config.contracts.fieldPreimageCertificatePolicyId,
          });
          if (certificate === undefined) {
            const publications =
              await resolveFaultProofFieldCarriagePublications({
                ownOutputs,
                lucid: config.lucid,
                publisherAddress: config.signer.proofMaterialsAddress,
                planned: opening,
              });
            if (publications === undefined) {
              throw new Error(
                "network-id tier-3 publications disappeared before certification",
              );
            }
            const base = `network-id:certify-field:${opening.commitment}`;
            return {
              kind: "action_required",
              action: action(
                "certify_field",
                {
                  threadOutRef: step02OutRef,
                  fieldCommitment: opening.commitment,
                  chunkOutRefs: publications
                    .map((utxo) => outRefLabel(utxo))
                    .join(","),
                },
                base,
              ),
            };
          }
          return {
            kind: "action_required",
            action: action("step02", {
              threadOutRef: step02OutRef,
              certificateOutRef: outRefLabel(certificate),
            }),
          };
        }
      }
      return {
        kind: "action_required",
        action: action("step02", { threadOutRef: step02OutRef }),
      };
    }
    if (
      admitted.direction === "forced" &&
      config.contracts.forcedScan !== undefined
    ) {
      const scanUtxo = await forcedScanThread(prepared.headerHash);
      if (scanUtxo !== undefined) {
        const scanOutRef = outRefLabel(scanUtxo);
        const { opening, plan } = forcedScanPlanFor(admitted);
        const ownOutputs = await fieldCarriage(opening, context);
        const missing = firstUnclaimedFieldPublication({
          publisherAddress: config.signer.proofMaterialsAddress,
          planned: opening,
          ownOutputs,
        });
        if (missing !== undefined) {
          const base = `network-id:publish-field:${opening.commitment}:${missing.digest}`;
          return {
            kind: "action_required",
            action: action(
              "publish_field",
              {
                threadOutRef: scanOutRef,
                fieldCommitment: opening.commitment,
                publicationDatumCbor: missing.datumCbor,
              },
              base,
            ),
          };
        }
        let certificateOutRef: string | undefined;
        if (opening.plan.tier === "Certified") {
          const certificate = await resolveFaultProofFieldPreimageCertificate({
            ownOutputs,
            lucid: config.lucid,
            network: config.network,
            planned: opening,
            certificatePolicyId:
              config.contracts.fieldPreimageCertificatePolicyId,
          });
          if (certificate === undefined) {
            const publications =
              await resolveFaultProofFieldCarriagePublications({
                ownOutputs,
                lucid: config.lucid,
                publisherAddress: config.signer.proofMaterialsAddress,
                planned: opening,
              });
            if (publications === undefined) {
              throw new Error(
                "network-id tier-3 publications disappeared before certification",
              );
            }
            const base = `network-id:certify-field:${opening.commitment}`;
            return {
              kind: "action_required",
              action: action(
                "certify_field",
                {
                  threadOutRef: scanOutRef,
                  fieldCommitment: opening.commitment,
                  chunkOutRefs: publications
                    .map((utxo) => outRefLabel(utxo))
                    .join(","),
                },
                base,
              ),
            };
          }
          certificateOutRef = outRefLabel(certificate);
        }
        const step = forcedScanNextStep({ utxo: scanUtxo, plan });
        return {
          kind: "action_required",
          action: action(forcedScanKindFor(step), {
            threadOutRef: scanOutRef,
            scanAction: step.kind,
            scanOrdinal: networkIdForcedScanStepOrdinal(step).toString(),
            ...(certificateOutRef === undefined ? {} : { certificateOutRef }),
          }),
        };
      }
    }
    if (step01OutRef !== undefined) {
      return {
        kind: "action_required",
        action:
          admitted.direction !== "accepted"
            ? action("forced_step01", { threadOutRef: step01OutRef })
            : action("step01", {
                threadOutRef: step01OutRef,
                stateQueueBlockOutRef: stateQueueOutRef,
              }),
      };
    }
    if (admitted.direction !== "accepted") {
      const forcedOutRef = await forcedThreadOutRef(prepared.headerHash);
      if (forcedOutRef !== undefined) {
        return {
          kind: "action_required",
          action: action("forced_bind", { threadOutRef: forcedOutRef }),
        };
      }
    }
    return {
      kind: "action_required",
      action: action("init", {
        fraudulentBlockOutRef: stateQueueOutRef,
      }),
    };
  };

  return {
    adapterVersion: FRAUD_PROOF_WORKFLOW_ADAPTER,
    category,
    safety: FRAUD_PROOF_WORKFLOW_SAFETY,
    prepare: async ({
      evidence,
      classification,
    }: {
      readonly evidence: CanonicalBlockEvidence;
      readonly classification?: Extract<
        CanonicalBlockClassification,
        { readonly decision: "fault_detected" }
      >;
    }): Promise<JournalJsonObject> => {
      return await prepareNetworkIdWorkflowArtifact({
        evidence,
        classification,
        expectedNetworkId: config.contracts.expectedNetworkId,
      });
    },
    observe,
    preflight: async (context): Promise<FraudProofWorkflowPreflight> => {
      const admitted = admitNetworkIdWorkflowArtifact(context.artifact);
      const prepared = admitted.prepared;
      const kind = actionKind(context.action);
      let mutationLease: StateQueueMutationLease | undefined;
      const buildAction = async (): Promise<LocallyEvaluatedTransaction> => {
        if (kind === "init") {
          return (
            await workflowStepTransaction(
              await buildNetworkIdInit({
                lucid: config.lucid,
                blueprint: config.blueprint,
                network: config.network,
                contracts: config.contracts,
                category: config.category,
                catalogue: config.catalogue,
                signer: config.signer,
                fraudulentBlockOutRef: requireString(
                  context.action.input.fraudulentBlockOutRef,
                  "init block out-ref",
                ),
                witnessReferenceScripts: config.witnessReferenceScripts,
              }),
              "network-id init",
            )
          ).transaction;
        }
        if (kind === "step01") {
          if (admitted.direction !== "accepted") {
            throw new Error(
              "network-id forced artifact cannot enter the accepted step-01",
            );
          }
          return (
            await workflowStepTransaction(
              await buildNetworkIdStep01({
                lucid: config.lucid,
                blueprint: config.blueprint,
                contracts: config.contracts,
                categoryId: config.category.categoryId,
                network: config.network,
                signer: config.signer,
                threadOutRef: requireString(
                  context.action.input.threadOutRef,
                  "step-01 thread out-ref",
                ),
                stateQueueBlockOutRef: requireString(
                  context.action.input.stateQueueBlockOutRef,
                  "step-01 state-queue out-ref",
                ),
                prepared: admitted.prepared,
                referenceScriptUtxo: config.stepReferenceScripts[0],
                witnessReferenceScripts: config.witnessReferenceScripts,
              }),
              "network-id step01",
            )
          ).transaction;
        }
        if (kind === "forced_step01") {
          if (admitted.direction === "accepted") {
            throw new Error(
              "network-id accepted artifact cannot enter the forced step-01 handover",
            );
          }
          requireForcedStep();
          return (
            await workflowStepTransaction(
              await buildNetworkIdForcedStep01({
                lucid: config.lucid,
                contracts: config.contracts,
                categoryId: config.category.categoryId,
                signer: config.signer,
                threadOutRef: requireString(
                  context.action.input.threadOutRef,
                  "forced step-01 thread out-ref",
                ),
                prepared: admitted.prepared,
                referenceScriptUtxo: config.stepReferenceScripts[0],
              }),
              "network-id forced_step01",
            )
          ).transaction;
        }
        if (kind === "forced_bind") {
          if (admitted.direction === "accepted") {
            throw new Error(
              "network-id accepted artifact cannot enter the forced door",
            );
          }
          const args = {
            lucid: config.lucid,
            contracts: config.contracts,
            categoryId: config.category.categoryId,
            signer: config.signer,
            threadOutRef: requireString(
              context.action.input.threadOutRef,
              "forced-step thread out-ref",
            ),
            referenceScriptUtxo: requireForcedStepReferenceScript(),
          };
          return (
            await workflowStepTransaction(
              admitted.direction === "accepted_forced"
                ? await buildNetworkIdAcceptedForcedBind({
                    ...args,
                    prepared: admitted.prepared,
                  })
                : await buildNetworkIdForcedBind({
                    ...args,
                    prepared: admitted.prepared,
                  }),
              "network-id forced_bind",
            )
          ).transaction;
        }
        if (isForcedScanKind(kind)) {
          if (admitted.direction !== "forced") {
            throw new Error(
              "network-id accepted artifact cannot enter the forced outputs scan",
            );
          }
          requireForcedScan();
          const threadOutRef = requireString(
            context.action.input.threadOutRef,
            "forced-scan thread out-ref",
          );
          const { opening, plan } = forcedScanPlanFor(admitted);
          const ownOutputs = await fieldCarriage(opening, context);
          const scanUtxo = await forcedScanThread(prepared.headerHash);
          if (
            scanUtxo === undefined ||
            outRefLabel(scanUtxo) !== threadOutRef
          ) {
            throw new WorkflowActionChangedError(
              "network-id forced scan thread moved away from the expected batch out-ref",
            );
          }
          const step = forcedScanNextStep({ utxo: scanUtxo, plan });
          if (
            forcedScanKindFor(step) !== kind ||
            context.action.input.scanAction !== step.kind ||
            context.action.input.scanOrdinal !==
              networkIdForcedScanStepOrdinal(step).toString()
          ) {
            throw new WorkflowActionChangedError(
              "network-id forced scan action is no longer the batch the live thread state is waiting for",
            );
          }
          const publications = await resolveFaultProofFieldCarriagePublications(
            {
              ownOutputs,
              lucid: config.lucid,
              publisherAddress: config.signer.proofMaterialsAddress,
              planned: opening,
            },
          );
          if (publications === undefined) {
            throw new WorkflowActionChangedError(
              "network-id forced scan carriage is no longer observable on L1",
            );
          }
          const certificate =
            opening.plan.tier === "Certified"
              ? await resolveFaultProofFieldPreimageCertificate({
                  ownOutputs,
                  lucid: config.lucid,
                  network: config.network,
                  planned: opening,
                  certificatePolicyId:
                    config.contracts.fieldPreimageCertificatePolicyId,
                })
              : undefined;
          if (
            opening.plan.tier === "Certified" &&
            (certificate === undefined ||
              context.action.input.certificateOutRef !==
                outRefLabel(certificate))
          ) {
            throw new WorkflowActionChangedError(
              "network-id forced scan does not bind the observed field certificate",
            );
          }
          await authenticateFieldInputs({
            prepared,
            publications,
            ...(certificate === undefined ? {} : { certificate }),
          });
          return (
            await workflowStepTransaction(
              await buildNetworkIdForcedScanAction({
                ownOutputs,
                lucid: config.lucid,
                contracts: config.contracts,
                categoryId: config.category.categoryId,
                network: config.network,
                signer: config.signer,
                threadOutRef,
                prepared: admitted.prepared,
                outputsOpeningPlan: opening,
                scan: plan,
                step,
                referenceScriptUtxo: requireForcedScanReferenceScript(),
                carriageUtxos: publications,
                certificateUtxos:
                  certificate === undefined ? [] : [certificate],
              }),
              "network-id forced scan",
            )
          ).transaction;
        }
        if (kind === "publish_field" || kind === "certify_field") {
          const opening = outputsOpeningPlanFor(admitted);
          if (opening === undefined) {
            throw new Error(
              "transaction-network faults have no field-carriage action",
            );
          }
          if (context.action.input.fieldCommitment !== opening.commitment) {
            throw new Error(
              "network-id field action does not match the prepared opening",
            );
          }
          const ownOutputs = await fieldCarriage(opening, context);
          if (kind === "publish_field") {
            const missing = firstUnclaimedFieldPublication({
              publisherAddress: config.signer.proofMaterialsAddress,
              planned: opening,
              ownOutputs,
            });
            if (
              missing === undefined ||
              context.action.input.publicationDatumCbor !== missing.datumCbor
            ) {
              throw new WorkflowActionChangedError(
                "network-id publication action is not the next missing plan chunk",
              );
            }
            const publication = await publishFaultProofFieldCarriage({
              ownOutputs,
              lucid: config.lucid,
              signer: config.signer,
              planned: opening,
              publisherAddress: config.signer.proofMaterialsAddress,
              label: "network-id step-02 outputs",
            });
            if (!isPrerequisiteTurn(publication)) {
              throw new Error(
                "network-id publication action built no publication transaction",
              );
            }
            return publication.transaction;
          }
          if (opening.plan.tier !== "Certified") {
            throw new Error(
              "network-id certification action requires tier-3 carriage",
            );
          }
          const publications = await resolveFaultProofFieldCarriagePublications(
            {
              ownOutputs,
              lucid: config.lucid,
              publisherAddress: config.signer.proofMaterialsAddress,
              planned: opening,
            },
          );
          if (publications === undefined) {
            throw new WorkflowActionChangedError(
              "network-id tier-3 publications are no longer observable on L1",
            );
          }
          await authenticateFieldInputs({ prepared, publications });
          if (
            context.action.input.chunkOutRefs !==
            publications.map((utxo) => outRefLabel(utxo)).join(",")
          ) {
            throw new WorkflowActionChangedError(
              "network-id certification action changed the observed chunks",
            );
          }
          const certification = await certifyFaultProofFieldCarriage({
            ownOutputs,
            lucid: config.lucid,
            network: config.network,
            signer: config.signer,
            planned: opening,
            certificatePolicyId:
              config.contracts.fieldPreimageCertificatePolicyId,
            certificateMintingScript:
              config.contracts.fieldPreimageCertificateMintingScript,
            certificateReferenceScriptUtxo:
              config.fieldPreimageCertificateReferenceScript,
            chunkUtxos: publications,
            compactCbor: prepared.nativeTxCompactCbor,
          });
          return certification.transaction;
        }
        if (kind === "step02") {
          const opening = finalStepOutputsOpeningPlanFor(admitted);
          const ownOutputs =
            opening === undefined
              ? createOwnOutputJournal()
              : await fieldCarriage(opening, context);
          const publications =
            opening === undefined
              ? []
              : await resolveFaultProofFieldCarriagePublications({
                  ownOutputs,
                  lucid: config.lucid,
                  publisherAddress: config.signer.proofMaterialsAddress,
                  planned: opening,
                });
          if (publications === undefined) {
            throw new WorkflowActionChangedError(
              "network-id field publications are no longer observable on L1",
            );
          }
          const certificate =
            opening?.plan.tier === "Certified"
              ? await resolveFaultProofFieldPreimageCertificate({
                  ownOutputs,
                  lucid: config.lucid,
                  network: config.network,
                  planned: opening,
                  certificatePolicyId:
                    config.contracts.fieldPreimageCertificatePolicyId,
                })
              : undefined;
          if (
            opening?.plan.tier === "Certified" &&
            (certificate === undefined ||
              context.action.input.certificateOutRef !==
                outRefLabel(certificate))
          ) {
            throw new WorkflowActionChangedError(
              "network-id final step does not bind the observed field certificate",
            );
          }
          await authenticateFieldInputs({
            prepared,
            publications,
            ...(certificate === undefined ? {} : { certificate }),
          });
          return (
            await workflowStepTransaction(
              await buildNetworkIdStep02({
                ownOutputs,
                lucid: config.lucid,
                contracts: config.contracts,
                categoryId: config.category.categoryId,
                signer: config.signer,
                threadOutRef: requireString(
                  context.action.input.threadOutRef,
                  "step-02 thread out-ref",
                ),
                prepared,
                ...(opening === undefined
                  ? {}
                  : { outputsOpeningPlan: opening }),
                ...(certificate === undefined
                  ? {}
                  : { certificateUtxos: [certificate] }),
                referenceScriptUtxo: config.stepReferenceScripts[1],
                witnessReferenceScripts: config.witnessReferenceScripts,
                // A missing grammar certificate is certified and submitted
                // inline before the step is built (certifyFaultProofFieldGrammar).
                ...(config.fieldGrammar === undefined
                  ? {}
                  : { fieldGrammar: config.fieldGrammar }),
              }),
              "network-id step02",
            )
          ).transaction;
        }
        if (
          config.rawL1 !== undefined &&
          config.removal.stateQueueMutationLeaseCoordinator === undefined
        ) {
          throw new Error(
            "production network-id removal requires a state-queue mutation-lease coordinator",
          );
        }
        const built = await buildRemoveFraudulentBlock({
          lucid: config.lucid,
          blueprint: config.blueprint,
          deploymentInfo: config.removal.deploymentInfo,
          network: config.network,
          signer: config.signer,
          fraudCategory: config.removal.category,
          fraudulentHeaderHash: prepared.headerHash,
          fraudProofOutRef: requireString(
            context.action.input.fraudProofOutRef,
            "permanent proof-token out-ref",
          ),
          requireReferenceScripts:
            config.removal.requireReferenceScripts ?? true,
          ...(config.removal.stateQueueMutationLeaseCoordinator === undefined
            ? {}
            : {
                stateQueueMutationLeaseCoordinator:
                  config.removal.stateQueueMutationLeaseCoordinator,
              }),
          ...(config.removal.validFrom === undefined
            ? {}
            : { validFrom: config.removal.validFrom }),
          ...(config.removal.validTo === undefined
            ? {}
            : { validTo: config.removal.validTo }),
        });
        const removal = await admitRemovalTransaction(built, (transaction) => {
          if (
            !workflowTransactionInputOutRefs(transaction.signed).includes(
              requireString(
                context.action.input.nextRemovalOutRef,
                "next removal out-ref",
              ),
            )
          ) {
            throw new Error(
              "network-id removal does not consume the authenticated next state-queue outRef",
            );
          }
          if (
            !workflowTransactionReferenceInputOutRefs(
              transaction.signed,
            ).includes(
              requireString(
                context.action.input.fraudProofOutRef,
                "permanent proof-token out-ref",
              ),
            )
          ) {
            throw new Error(
              "network-id removal does not reference the authenticated permanent proof token",
            );
          }
        });
        mutationLease = removal.mutationLease;
        return removal.transaction;
      };
      const transaction = await buildAction();
      requireReferenceOnlyScriptWitnesses({
        transaction,
        label: "network-id production transaction",
      });
      const requiresMutationLease =
        context.action.input.requiresMutationLease === "true";
      if (
        kind === "remove" &&
        requiresMutationLease !== (mutationLease !== undefined)
      ) {
        await mutationLease?.fail(
          "authenticated network-id removal topology disagreed with lease requirement",
        );
        throw new Error(
          "authenticated network-id removal topology disagreed with mutation-lease acquisition",
        );
      }
      captured.set(`${context.workflowId}:${context.action.actionId}`, {
        transaction,
        ...(mutationLease === undefined ? {} : { mutationLease }),
      });
      // The runner refuses a preflight that carries no signed body.
      return bindWorkflowPreflightTransaction(
        {
          actionId: context.action.actionId,
          txHash: transaction.txHash,
          scriptExecution: "reference_scripts",
          localUplcEvaluation: {
            status: "passed",
            evaluator: LOCAL_UPLC_EVALUATOR,
          },
          referenceScripts: transaction.referenceScripts,
        },
        transaction.signed,
      );
    },
    submit: async (context) => {
      const key = `${context.workflowId}:${context.action.actionId}`;
      const prepared = captured.get(key);
      if (
        prepared === undefined ||
        prepared.transaction.txHash !== context.preflight.txHash
      ) {
        return {
          kind: "ambiguous",
          detail:
            "captured locally evaluated transaction is unavailable or differs from the preflight hash",
        };
      }
      if (actionKind(context.action) === "remove") {
        const admitted = admitNetworkIdWorkflowArtifact(context.artifact);
        submittedRemovals.set(admitted.prepared.headerHash, {
          txHash: prepared.transaction.txHash,
          removedStateQueueOutRef: requireString(
            context.action.input.nextRemovalOutRef,
            "removed state-queue out-ref",
          ),
        });
      }
      try {
        const txHash = await submitCapturedTransaction(prepared.transaction);
        captured.delete(key);
        return { kind: "submitted", txHash };
      } catch (cause) {
        return {
          kind: "ambiguous",
          txHash: prepared.transaction.txHash,
          detail: cause instanceof Error ? cause.message : String(cause),
        };
      } finally {
        // The runner follows the submitted transaction on chain, so the
        // lease ends with its submission.
        await prepared.mutationLease?.release();
      }
    },
  };
};
