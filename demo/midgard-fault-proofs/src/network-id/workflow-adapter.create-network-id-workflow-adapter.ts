import { outRefLabel } from "@al-ft/midgard-core";
import {
  deriveFieldPreimageCertification,
  FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_HEX,
  NetworkIdForcedScanDatum,
  STATE_QUEUE_NODE_ASSET_NAME_PREFIX,
} from "@al-ft/midgard-sdk";
import { Data, toUnit, type UTxO } from "@lucid-evolution/lucid";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import {
  certifyFaultProofFieldCarriage,
  type FaultProofFieldOpeningPlan,
  fieldPreimageCertificateAddress,
  findMissingFaultProofFieldPublication,
  publishFaultProofFieldCarriage,
  resolveFaultProofFieldCarriagePublications,
  resolveFaultProofFieldPreimageCertificate,
} from "../field-opening.js";
import type {
  StateQueueMutationLease,
  StateQueueMutationLeaseCoordinator,
} from "../remove-fraudulent-block.js";
import { submitRemoveFraudulentBlock } from "../remove-fraudulent-block.js";
import { NETWORK_ID_FORCED_SCAN_DEPLOYMENT_ENTRY } from "../runtime.js";
import { WorkflowActionChangedError } from "../workflow/action-changed.js";
import type { CanonicalBlockClassification } from "../workflow/classification.js";
import type { JournalJsonObject } from "../workflow/journal.js";
import {
  FRAUD_PROOF_WORKFLOW_ADAPTER,
  FRAUD_PROOF_WORKFLOW_SAFETY,
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowObservation,
  type FraudProofWorkflowPreflight,
  type FraudProofWorkflowReconcileResult,
} from "../workflow/orchestrator.js";
import { reconcileSignedWorkflowTransaction } from "../workflow/signed-transaction-reconciliation.js";
import {
  bindWorkflowPreflightTransaction,
  captureLocallyEvaluatedTransaction,
  type FraudProofPreSubmitBoundary,
  LOCAL_UPLC_EVALUATOR,
  type LocallyEvaluatedTransaction,
  requireReferenceOnlyScriptWitnesses,
  submitCapturedTransaction,
  workflowTransactionInputOutRefs,
  workflowTransactionReferenceInputOutRefs,
} from "../workflow/transaction-boundary.js";
import {
  type NetworkIdForcedScanStep,
  networkIdForcedScanStepForState,
  networkIdForcedScanStepOrdinal,
  planNetworkIdForcedScan,
} from "./forced-scan-plan.js";
import {
  planNetworkIdOutputsOpening,
  type PreparedNetworkIdProof,
  prepareNetworkIdFromCanonicalEvidence,
} from "./prepare.js";
import { submitNetworkIdForcedBind } from "./submit-network-id-forced-bind.js";
import { submitNetworkIdForcedScanAction } from "./submit-network-id-forced-scan.js";
import { submitNetworkIdForcedStep01 } from "./submit-network-id-forced-step-01.js";
import { submitNetworkIdInit } from "./submit-network-id-init.js";
import { submitNetworkIdStep01 } from "./submit-network-id-step-01.js";
import { submitNetworkIdStep02 } from "./submit-network-id-step-02.js";
import {
  admitNetworkIdWorkflowArtifact,
  type AdmittedNetworkIdArtifact,
  confirmedTxHash,
  latestRemovalIntent,
  networkIdForcedArtifactFromPrepared,
  parseMutationLeaseRecovery,
} from "./workflow-adapter.admit-network-id-forced-artifact.js";
import {
  type NetworkIdWorkflowAdapterConfig,
  recoverMutationLease,
} from "./workflow-adapter.create-network-id-raw-l1-observation-port.js";
import { createNetworkIdForcedStepContract } from "./workflow-adapter.forced-step-contract.js";
import {
  action,
  type ActionKind,
  actionKind,
  artifactFromPrepared,
  CATEGORY,
  contentActionId,
  forcedScanKindFor,
  isForcedScanKind,
  requireString,
  type WorkflowContext,
} from "./workflow-adapter.prepared-from-artifact.js";
import {
  createNetworkIdWrongfulRejectionPlanner,
  detectNetworkIdWrongfulRejections,
  NETWORK_ID_WRONGFUL_REJECTION_VIOLATION_ID,
  type PreparedNetworkIdWrongfulRejection,
} from "./wrongful-rejection.js";

/**
 * Concrete last-header adapter. It captures the actual signed transaction at
 * the post-local-evaluation/pre-network boundary, journals that hash through
 * the shared orchestrator, then submits exactly the captured body.
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
  const mutationLeaseByTxHash = new Map<string, StateQueueMutationLease>();
  const category = CATEGORY;

  const { requireForcedStep, requireForcedStepReferenceScript } =
    createNetworkIdForcedStepContract(config);

  const requireForcedScan = () => {
    const forcedScan = config.contracts.forcedScan;
    if (forcedScan === undefined) {
      throw new Error(
        "network-id forced direction requires the deployed forced outputs scan",
      );
    }
    return forcedScan;
  };

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
   * The single scan thread, read directly for the same reason the forced door
   * is: the §10 loop self-loops at one address, so it is not a link the linear
   * raw-L1 family definition can walk.
   */
  const forcedScanThread = async (
    headerHash: string,
  ): Promise<UTxO | undefined> => {
    const forcedScan = requireForcedScan();
    const utxos = await config.lucid.utxosAtWithUnit(
      forcedScan.spendingScriptAddress,
      toUnit(
        config.contracts.computationThread.policyId,
        `${config.category.categoryId}${headerHash}`,
      ),
    );
    if (utxos.length > 1) {
      throw new Error("network-id workflow found duplicate forced-scan UTxOs");
    }
    return utxos[0];
  };

  /**
   * Scan occupancy, read only for the stages whose progress it decides. An
   * accepted artifact, or a deployment without the scan, never pays the query.
   */
  const forcedScanOutRef = async ({
    admitted,
    kind,
  }: {
    readonly admitted: AdmittedNetworkIdArtifact;
    readonly kind: ActionKind;
  }): Promise<string | undefined> => {
    if (admitted.direction !== "forced") return undefined;
    if (
      kind !== "init" &&
      kind !== "forced_step01" &&
      kind !== "forced_bind" &&
      !isForcedScanKind(kind)
    ) {
      return undefined;
    }
    if (config.contracts.forcedScan === undefined) return undefined;
    const utxo = await forcedScanThread(admitted.prepared.headerHash);
    return utxo === undefined ? undefined : outRefLabel(utxo);
  };

  /**
   * The forced door is deliberately not a link in the linear chain the raw-L1
   * family definition walks, so its occupancy is read directly. Nothing here is
   * trusted: `submitNetworkIdForcedBind` re-authenticates the thread token, the
   * handoff datum, the prover and the header before it spends this UTxO.
   */
  const forcedThreadOutRef = async (
    headerHash: string,
  ): Promise<string | undefined> => {
    const forcedStep = requireForcedStep();
    const utxos = await config.lucid.utxosAtWithUnit(
      forcedStep.spendingScriptAddress,
      toUnit(
        config.contracts.computationThread.policyId,
        `${config.category.categoryId}${headerHash}`,
      ),
    );
    if (utxos.length > 1) {
      throw new Error("network-id workflow found duplicate forced-step UTxOs");
    }
    return utxos[0] === undefined ? undefined : outRefLabel(utxos[0]);
  };

  /**
   * Forced-door occupancy, read only for the stages whose progress it decides.
   * An accepted artifact never touches the door, so it never pays the query.
   */
  const forcedDoorOccupied = async ({
    admitted,
    kind,
  }: {
    readonly admitted: AdmittedNetworkIdArtifact;
    readonly kind: ActionKind;
  }): Promise<boolean> => {
    if (admitted.direction !== "forced") return false;
    if (kind !== "init" && kind !== "forced_step01" && kind !== "forced_bind") {
      return false;
    }
    return (
      (await forcedThreadOutRef(admitted.prepared.headerHash)) !== undefined
    );
  };

  const live = async (headerHash: string) => {
    const threadUnit = toUnit(
      config.contracts.computationThread.policyId,
      `${config.category.categoryId}${headerHash}`,
    );
    const proofUnit = toUnit(
      config.contracts.fraudProof.policyId,
      `${config.category.categoryId}${headerHash}`,
    );
    const stateQueueUnit = toUnit(
      config.contracts.stateQueuePolicyId,
      `${STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${headerHash}`,
    );
    const [step01, step02, proofs, stateQueue] = await Promise.all([
      config.lucid.utxosAtWithUnit(
        config.contracts.steps[0].spendingScriptAddress,
        threadUnit,
      ),
      config.lucid.utxosAtWithUnit(
        config.contracts.steps[1].spendingScriptAddress,
        threadUnit,
      ),
      config.lucid.utxosAtWithUnit(
        config.contracts.fraudProof.spendingScriptAddress,
        proofUnit,
      ),
      config.lucid.utxosAtWithUnit(config.stateQueueAddress, stateQueueUnit),
    ]);
    for (const [label, utxos] of [
      ["step-01", step01],
      ["step-02", step02],
      ["proof", proofs],
      ["state-queue", stateQueue],
    ] as const) {
      if (utxos.length > 1) {
        throw new Error(`network-id workflow found duplicate ${label} UTxOs`);
      }
    }
    return {
      threadUnit,
      proofUnit,
      step01: step01[0],
      step02: step02[0],
      proof: proofs[0],
      stateQueue: stateQueue[0],
    };
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
      | PreparedNetworkIdWrongfulRejection;
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
        address: config.signer.address,
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
      const intent = latestRemovalIntent(context);
      if (intent === undefined || intent.kind !== "submission_intent") {
        return {
          kind: "conflict",
          reason:
            "fraudulent header disappeared without a journaled removal intent",
        };
      }
      const removalTxHash = confirmedTxHash(context, intent.actionId);
      if (removalTxHash === undefined) {
        return {
          kind: "conflict",
          reason:
            "confirmed removal facts are unavailable for terminal authentication",
        };
      }
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
            removedStateQueueOutRef: requireString(
              intent.actionInput.nextRemovalOutRef,
              "removed state-queue out-ref",
            ),
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
        // The production funding reservation permit binds the slash authority
        // to the removal action through the shared `nextRemovalOutRef` and
        // `fraudProofOutRef` names.
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
        const missing = await findMissingFaultProofFieldPublication({
          lucid: config.lucid,
          publisherAddress: config.signer.address,
          planned: opening,
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
              contentActionId({ base, entries: context.entries }),
            ),
          };
        }
        if (opening.plan.tier === "Certified") {
          const certificate = await resolveFaultProofFieldPreimageCertificate({
            lucid: config.lucid,
            network: config.network,
            planned: opening,
            certificatePolicyId:
              config.contracts.fieldPreimageCertificatePolicyId,
          });
          if (certificate === undefined) {
            const publications =
              await resolveFaultProofFieldCarriagePublications({
                lucid: config.lucid,
                publisherAddress: config.signer.address,
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
                contentActionId({ base, entries: context.entries }),
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
        const missing = await findMissingFaultProofFieldPublication({
          lucid: config.lucid,
          publisherAddress: config.signer.address,
          planned: opening,
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
              contentActionId({ base, entries: context.entries }),
            ),
          };
        }
        let certificateOutRef: string | undefined;
        if (opening.plan.tier === "Certified") {
          const certificate = await resolveFaultProofFieldPreimageCertificate({
            lucid: config.lucid,
            network: config.network,
            planned: opening,
            certificatePolicyId:
              config.contracts.fieldPreimageCertificatePolicyId,
          });
          if (certificate === undefined) {
            const publications =
              await resolveFaultProofFieldCarriagePublications({
                lucid: config.lucid,
                publisherAddress: config.signer.address,
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
                contentActionId({ base, entries: context.entries }),
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
          admitted.direction === "forced"
            ? action("forced_step01", { threadOutRef: step01OutRef })
            : action("step01", {
                threadOutRef: step01OutRef,
                stateQueueBlockOutRef: stateQueueOutRef,
              }),
      };
    }
    if (admitted.direction === "forced") {
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
      if (
        classification?.category === CATEGORY &&
        classification.selected.violationId ===
          NETWORK_ID_WRONGFUL_REJECTION_VIOLATION_ID
      ) {
        const expectedNetworkId = config.contracts.expectedNetworkId;
        const detections = detectNetworkIdWrongfulRejections({
          block: evidence,
          expectedNetworkId,
        });
        if (
          detections[0]?.detectionId !== classification.selected.detectionId
        ) {
          throw new Error(
            "network-id forced classification does not select the earliest authenticated wrongful rejection",
          );
        }
        return networkIdForcedArtifactFromPrepared(
          await createNetworkIdWrongfulRejectionPlanner(expectedNetworkId)({
            block: evidence,
          }),
        ) as unknown as JournalJsonObject;
      }
      return artifactFromPrepared(
        await prepareNetworkIdFromCanonicalEvidence({
          evidence,
          expectedNetworkId: config.contracts.expectedNetworkId,
        }),
      );
    },
    observe,
    preflight: async (context): Promise<FraudProofWorkflowPreflight> => {
      const admitted = admitNetworkIdWorkflowArtifact(context.artifact);
      const prepared = admitted.prepared;
      const kind = actionKind(context.action);
      let mutationLease: StateQueueMutationLease | undefined;
      const boundaryInvocation = async (
        boundary: FraudProofPreSubmitBoundary,
      ) => {
        if (kind === "init") {
          await submitNetworkIdInit({
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
            preSubmitBoundary: boundary,
            awaitConfirmation: false,
          });
          return;
        }
        if (kind === "step01") {
          if (admitted.direction !== "accepted") {
            throw new Error(
              "network-id forced artifact cannot enter the accepted step-01",
            );
          }
          await submitNetworkIdStep01({
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
            preSubmitBoundary: boundary,
            awaitConfirmation: false,
          });
          return;
        }
        if (kind === "forced_step01") {
          if (admitted.direction !== "forced") {
            throw new Error(
              "network-id accepted artifact cannot enter the forced step-01 handover",
            );
          }
          requireForcedStep();
          await submitNetworkIdForcedStep01({
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
            preSubmitBoundary: boundary,
            awaitConfirmation: false,
          });
          return;
        }
        if (kind === "forced_bind") {
          if (admitted.direction !== "forced") {
            throw new Error(
              "network-id accepted artifact cannot enter the forced door",
            );
          }
          await submitNetworkIdForcedBind({
            lucid: config.lucid,
            contracts: config.contracts,
            categoryId: config.category.categoryId,
            signer: config.signer,
            threadOutRef: requireString(
              context.action.input.threadOutRef,
              "forced-step thread out-ref",
            ),
            prepared: admitted.prepared,
            referenceScriptUtxo: requireForcedStepReferenceScript(),
            preSubmitBoundary: boundary,
            awaitConfirmation: false,
          });
          return;
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
          const scanUtxo = await forcedScanThread(prepared.headerHash);
          if (
            scanUtxo === undefined ||
            outRefLabel(scanUtxo) !== threadOutRef
          ) {
            throw new WorkflowActionChangedError(
              "network-id forced scan thread moved away from the journaled batch out-ref",
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
              lucid: config.lucid,
              publisherAddress: config.signer.address,
              planned: opening,
            },
          );
          if (publications === undefined) {
            throw new Error(
              "network-id forced scan carriage is not observable on L1",
            );
          }
          const certificate =
            opening.plan.tier === "Certified"
              ? await resolveFaultProofFieldPreimageCertificate({
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
            throw new Error(
              "network-id forced scan does not bind the observed field certificate",
            );
          }
          await authenticateFieldInputs({
            prepared,
            publications,
            ...(certificate === undefined ? {} : { certificate }),
          });
          await submitNetworkIdForcedScanAction({
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
            certificateUtxos: certificate === undefined ? [] : [certificate],
            preSubmitBoundary: boundary,
            awaitConfirmation: false,
          });
          return;
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
          if (kind === "publish_field") {
            const missing = await findMissingFaultProofFieldPublication({
              lucid: config.lucid,
              publisherAddress: config.signer.address,
              planned: opening,
            });
            if (
              missing === undefined ||
              context.action.input.publicationDatumCbor !== missing.datumCbor
            ) {
              throw new Error(
                "network-id publication action is not the next missing plan chunk",
              );
            }
            await publishFaultProofFieldCarriage({
              lucid: config.lucid,
              signer: config.signer,
              planned: opening,
              publisherAddress: config.signer.address,
              label: "network-id step-02 outputs",
              preSubmitBoundary: boundary,
            });
            return;
          }
          if (opening.plan.tier !== "Certified") {
            throw new Error(
              "network-id certification action requires tier-3 carriage",
            );
          }
          const publications = await resolveFaultProofFieldCarriagePublications(
            {
              lucid: config.lucid,
              publisherAddress: config.signer.address,
              planned: opening,
            },
          );
          if (publications === undefined) {
            throw new Error(
              "network-id tier-3 publications are not observable on L1",
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
          await certifyFaultProofFieldCarriage({
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
            preSubmitBoundary: boundary,
            awaitConfirmation: false,
          });
          return;
        }
        if (kind === "step02") {
          const opening = finalStepOutputsOpeningPlanFor(admitted);
          const publications =
            opening === undefined
              ? []
              : await resolveFaultProofFieldCarriagePublications({
                  lucid: config.lucid,
                  publisherAddress: config.signer.address,
                  planned: opening,
                });
          if (publications === undefined) {
            throw new Error(
              "network-id field publications are not observable on L1",
            );
          }
          const certificate =
            opening?.plan.tier === "Certified"
              ? await resolveFaultProofFieldPreimageCertificate({
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
            throw new Error(
              "network-id final step does not bind the observed field certificate",
            );
          }
          await authenticateFieldInputs({
            prepared,
            publications,
            ...(certificate === undefined ? {} : { certificate }),
          });
          await submitNetworkIdStep02({
            lucid: config.lucid,
            contracts: config.contracts,
            categoryId: config.category.categoryId,
            signer: config.signer,
            threadOutRef: requireString(
              context.action.input.threadOutRef,
              "step-02 thread out-ref",
            ),
            prepared,
            ...(opening === undefined ? {} : { outputsOpeningPlan: opening }),
            ...(certificate === undefined
              ? {}
              : { certificateUtxos: [certificate] }),
            referenceScriptUtxo: config.stepReferenceScripts[1],
            witnessReferenceScripts: config.witnessReferenceScripts,
            preSubmitBoundary: boundary,
            awaitConfirmation: false,
          });
          return;
        }
        if (
          config.rawL1 !== undefined &&
          config.removal.stateQueueMutationLeaseCoordinator === undefined
        ) {
          throw new Error(
            "production network-id removal requires a state-queue mutation-lease coordinator",
          );
        }
        const retainingCoordinator:
          | StateQueueMutationLeaseCoordinator
          | undefined =
          config.removal.stateQueueMutationLeaseCoordinator === undefined
            ? undefined
            : {
                acquire: async () => {
                  mutationLease =
                    await config.removal.stateQueueMutationLeaseCoordinator!.acquire();
                  return mutationLease;
                },
              };
        await submitRemoveFraudulentBlock({
          lucid: config.lucid,
          blueprint: config.blueprint,
          deploymentInfo: config.removal.deploymentInfo,
          network: config.network,
          signer: config.signer,
          fraudCategory: config.removal.category,
          fraudulentHeaderHash: prepared.headerHash,
          requireReferenceScripts:
            config.removal.requireReferenceScripts ?? true,
          ...(retainingCoordinator === undefined
            ? {}
            : { stateQueueMutationLeaseCoordinator: retainingCoordinator }),
          ...(config.removal.validFrom === undefined
            ? {}
            : { validFrom: config.removal.validFrom }),
          ...(config.removal.validTo === undefined
            ? {}
            : { validTo: config.removal.validTo }),
          preSubmitBoundary: async (transaction) => {
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
            await boundary(transaction);
          },
          awaitConfirmation: false,
        });
      };
      const transaction =
        await captureLocallyEvaluatedTransaction(boundaryInvocation);
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
      // The production funding reservation permit reads the signed body back
      // from the in-memory preflight to reconcile the reserved inputs it
      // actually spends, so the capture is bound beside the journal-safe view.
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
          ...(mutationLease === undefined
            ? {}
            : {
                durableRecovery: {
                  stateQueueMutationLease: {
                    token: mutationLease.token,
                    source: mutationLease.source,
                  },
                },
              }),
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
            "captured locally evaluated transaction is unavailable or differs from the journaled preflight hash",
        };
      }
      const recovery = parseMutationLeaseRecovery(
        context.preflight.durableRecovery,
      );
      if (
        (prepared.mutationLease === undefined) !== (recovery === undefined) ||
        (prepared.mutationLease !== undefined &&
          (prepared.mutationLease.token !== recovery?.token ||
            prepared.mutationLease.source !== recovery.source))
      ) {
        throw new Error(
          "network-id cached mutation lease differs from durable intent",
        );
      }
      try {
        const txHash = await submitCapturedTransaction(prepared.transaction);
        captured.delete(key);
        if (prepared.mutationLease !== undefined) {
          mutationLeaseByTxHash.set(txHash, prepared.mutationLease);
        }
        return { kind: "submitted", txHash };
      } catch (cause) {
        return {
          kind: "ambiguous",
          txHash: prepared.transaction.txHash,
          detail: cause instanceof Error ? cause.message : String(cause),
        };
      }
    },
    reconcile: async (context): Promise<FraudProofWorkflowReconcileResult> => {
      if (context.retirementOnly && context.txHash !== undefined)
        return await reconcileSignedWorkflowTransaction({
          transactionHash: context.txHash,
          signedTransactionCborHex: context.signedTransactionCborHex,
          observe: config.rawL1?.observeSignedTransaction,
          reportInclusion: true,
        });
      const admitted = admitNetworkIdWorkflowArtifact(context.artifact);
      const prepared = admitted.prepared;
      const kind = actionKind(context.action);
      // A journaled submission the chain has not reflected yet is resolved
      // through canonical signed-transaction recovery: still in the mempool is
      // `pending`, expired is `not_found`, and anything the port cannot settle
      // stays `unknown`. Reporting `not_found` directly would make the
      // orchestrator abandon the funding transition and rebuild the identical
      // transaction while the original is still landing.
      const unconfirmed = async (
        lease?: StateQueueMutationLease,
      ): Promise<FraudProofWorkflowReconcileResult> => {
        const observe = config.rawL1?.observeSignedTransaction;
        if (context.txHash === undefined || observe === undefined) {
          return { kind: "not_found" };
        }
        const { authorizeResubmission } = context;
        return await reconcileSignedWorkflowTransaction({
          transactionHash: context.txHash,
          signedTransactionCborHex: context.signedTransactionCborHex,
          observe,
          rebroadcast: config.rawL1?.rebroadcastSignedTransaction,
          authorizeResubmission:
            authorizeResubmission === undefined
              ? undefined
              : async (signed) => {
                  await lease?.renew();
                  await authorizeResubmission(signed);
                },
        });
      };
      let advanced: boolean;
      if (kind === "publish_field" || kind === "certify_field") {
        const opening = outputsOpeningPlanFor(admitted);
        if (opening === undefined) {
          return {
            kind: "conflict",
            reason: "field action exists for a transaction-network fault",
          };
        }
        if (context.action.input.fieldCommitment !== opening.commitment) {
          return {
            kind: "conflict",
            reason: "field action commitment changed during reconciliation",
          };
        }
        if (context.txHash === undefined) {
          return {
            kind: "conflict",
            reason: "publication reconciliation omitted the intended tx hash",
          };
        }
        const observer = config.rawL1?.publications;
        if (observer === undefined) {
          return {
            kind: "conflict",
            reason:
              "production publication reconciliation has no authenticated raw-L1 observer",
          };
        }
        if (kind === "publish_field") {
          const candidates = await config.lucid.utxosAt(config.signer.address);
          const candidate = candidates.find(
            (utxo) =>
              utxo.txHash === context.txHash &&
              utxo.datum === context.action.input.publicationDatumCbor,
          );
          if (candidate === undefined) return await unconfirmed();
          const observation = await observer.observeExact({
            headerHash: prepared.headerHash,
            kind: "field_publication",
            address: config.signer.address,
            expectedOutRef: outRefLabel(candidate),
            expectedDatumCbor: requireString(
              context.action.input.publicationDatumCbor,
              "publication datum",
            ),
          });
          advanced = observation.kind === "confirmed";
        } else {
          const certificate = await resolveFaultProofFieldPreimageCertificate({
            lucid: config.lucid,
            network: config.network,
            planned: opening,
            certificatePolicyId:
              config.contracts.fieldPreimageCertificatePolicyId,
          });
          if (
            certificate === undefined ||
            certificate.txHash !== context.txHash
          ) {
            return await unconfirmed();
          }
          const certification = deriveFieldPreimageCertification(opening.plan);
          const observation = await observer.observeExact({
            headerHash: prepared.headerHash,
            kind: "field_certificate",
            address: fieldPreimageCertificateAddress({
              network: config.network,
              certificatePolicyId:
                config.contracts.fieldPreimageCertificatePolicyId,
            }),
            expectedOutRef: outRefLabel(certificate),
            expectedDatumCbor: certification.datumCbor,
            expectedUnit: `${config.contracts.fieldPreimageCertificatePolicyId}${FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_HEX}`,
          });
          advanced = observation.kind === "confirmed";
        }
      } else if (config.rawL1 !== undefined) {
        if (
          context.txHash === undefined ||
          config.rawL1.transactionConfirmed === undefined
        ) {
          return {
            kind: "conflict",
            reason:
              "production network-id reconciliation requires an intended tx hash and authenticated transaction history",
          };
        }
        const rawStage = await config.rawL1.observe({
          headerHash: prepared.headerHash,
        });
        const forcedOccupied = await forcedDoorOccupied({ admitted, kind });
        const scanOutRef = await forcedScanOutRef({ admitted, kind });
        const scanOccupied = scanOutRef !== undefined;
        const beyondStep01 =
          rawStage.kind === "step"
            ? rawStage.step >= 2
            : rawStage.kind === "proof_token" || rawStage.kind === "removed";
        const tokenMinted =
          rawStage.kind === "proof_token" || rawStage.kind === "removed";
        const stageAdvanced = isForcedScanKind(kind)
          ? scanOutRef !== context.action.input.threadOutRef
          : kind === "init"
            ? rawStage.kind !== "not_started" || forcedOccupied || scanOccupied
            : kind === "step01"
              ? beyondStep01
              : kind === "forced_step01"
                ? forcedOccupied || scanOccupied || beyondStep01
                : kind === "forced_bind"
                  ? !forcedOccupied && (scanOccupied || beyondStep01)
                  : kind === "step02"
                    ? tokenMinted
                    : rawStage.kind === "removed" ||
                      (rawStage.kind === "proof_token" &&
                        rawStage.nextRemovalOutRef !==
                          context.action.input.nextRemovalOutRef);
        const intendedTransactionConfirmed =
          await config.rawL1.transactionConfirmed({
            headerHash: prepared.headerHash,
            txHash: context.txHash,
          });
        if (stageAdvanced && !intendedTransactionConfirmed) {
          return {
            kind: "conflict",
            reason:
              "network-id chain advanced without the journaled transaction in authenticated unit history",
          };
        }
        advanced = stageAdvanced && intendedTransactionConfirmed;
      } else {
        const state = await live(prepared.headerHash);
        const forcedOccupied = await forcedDoorOccupied({ admitted, kind });
        const scanOutRef = await forcedScanOutRef({ admitted, kind });
        const scanOccupied = scanOutRef !== undefined;
        const beyondStep01 =
          state.step01 === undefined &&
          (state.step02 !== undefined ||
            state.proof !== undefined ||
            state.stateQueue === undefined);
        advanced = isForcedScanKind(kind)
          ? scanOutRef !== context.action.input.threadOutRef
          : kind === "init"
            ? state.step01 !== undefined ||
              state.step02 !== undefined ||
              state.proof !== undefined ||
              state.stateQueue === undefined ||
              forcedOccupied ||
              scanOccupied
            : kind === "step01"
              ? beyondStep01
              : kind === "forced_step01"
                ? forcedOccupied || scanOccupied || beyondStep01
                : kind === "forced_bind"
                  ? !forcedOccupied && (scanOccupied || beyondStep01)
                  : kind === "step02"
                    ? state.step02 === undefined &&
                      (state.proof !== undefined ||
                        state.stateQueue === undefined)
                    : state.stateQueue === undefined;
      }
      if (advanced) {
        if (context.txHash === undefined) {
          return {
            kind: "conflict",
            reason:
              "chain advanced but the journal carries no transaction hash",
          };
        }
        const recovery =
          context.txHash === undefined
            ? { kind: "ok" as const, lease: undefined }
            : await recoverMutationLease({
                config,
                txHash: context.txHash,
                durableRecovery: context.durableRecovery,
                mutationLeaseByTxHash,
              });
        if (recovery.kind === "conflict") return recovery;
        await recovery.lease?.release();
        if (context.txHash !== undefined) {
          mutationLeaseByTxHash.delete(context.txHash);
        }
        return { kind: "confirmed", txHash: context.txHash };
      }
      const recovery =
        context.txHash === undefined
          ? { kind: "ok" as const, lease: undefined }
          : await recoverMutationLease({
              config,
              txHash: context.txHash,
              durableRecovery: context.durableRecovery,
              mutationLeaseByTxHash,
            });
      if (recovery.kind === "conflict") return recovery;
      const result = await unconfirmed(recovery.lease);
      if (result.kind === "not_found") {
        await recovery.lease?.release();
        if (context.txHash !== undefined) {
          mutationLeaseByTxHash.delete(context.txHash);
        }
      } else if (result.kind === "conflict") {
        await recovery.lease?.fail(result.reason);
        if (context.txHash !== undefined) {
          mutationLeaseByTxHash.delete(context.txHash);
        }
      } else {
        await recovery.lease?.renew();
      }
      return result;
    },
  };
};
