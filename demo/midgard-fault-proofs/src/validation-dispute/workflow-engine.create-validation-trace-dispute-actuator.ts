import {
  type MidgardValidationTraceProof,
  selectMidgardValidationDisputeReveal,
} from "@al-ft/midgard-core";
import {
  validationMachineStateDataFromCore,
  validationTraceProofCoreFromData,
  validationTraceProofDataFromCore,
} from "@al-ft/midgard-sdk";
import { getAddressDetails, type UTxO } from "@lucid-evolution/lucid";

import { fetchUtxoByOutRef, parseOutRef } from "../runtime.js";
import { submitInit } from "../submit-init.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import {
  captureCursorRemoval,
  type CursorFamilyActionInput,
} from "../workflow/cursor-family-runtime.js";
import { journalJsonDigest } from "../workflow/journal.js";
import { workflowTransactionReferenceInputOutRefs } from "../workflow/transaction-boundary.js";
import { captureLocallyEvaluatedTransaction } from "../workflow/transaction-boundary.js";
import {
  cancelValidationCekContext,
  cancelValidationCekCore,
  cancelValidationCekMaterialTraversal,
  cancelValidationSemanticResolution,
  submitValidationDisputeAward,
  submitValidationDisputeEnterResolution,
  submitValidationDisputeEnterTimeout,
  submitValidationDisputeOpen,
  submitValidationDisputePrepareResolution,
  submitValidationDisputePrepareSelected,
  submitValidationDisputeReveal,
  submitValidationDisputeSemanticResolution,
  submitValidationDisputeTimeout,
  submitValidationDisputeVerifySource,
} from "./submit.js";
import { captureCanonicalCheckpoint } from "./workflow-canonical-continuation.js";
import {
  type ValidationTraceDisputeActuationMaterial,
  type ValidationTraceDisputeActuatorAction,
  type ValidationTraceDisputeActuatorConfig,
  type ValidationTraceDisputeCapturedAction,
  type ValidationTraceDisputeRetainedRouteInput,
} from "./workflow-engine.plan-validation-trace-dispute-move.js";
import {
  capture,
  hex,
  requireGameDispute,
} from "./workflow-engine.recover-validation-trace-state-index.js";
import {
  VALIDATION_TRACE_DISPUTE_CATEGORY,
  VALIDATION_TRACE_DISPUTE_CATEGORY_ID,
} from "./workflow-family.js";
import { createValidationTraceOneStepArgumentResolver } from "./workflow-one-step-argument.js";

/**
 * Family actuator. Builders stop after local UPLC evaluation and signing;
 * the durable production workflow remains the sole submit authority.
 */
export const createValidationTraceDisputeActuator = (
  config: ValidationTraceDisputeActuatorConfig,
) => {
  if (config.categoryId !== VALIDATION_TRACE_DISPUTE_CATEGORY_ID) {
    throw new Error("validationTraceDispute category id changed");
  }
  const now = config.now ?? (() => Date.now());
  const common = {
    lucid: config.lucid,
    blueprint: config.blueprint,
    deploymentInfo: config.deploymentInfo,
    network: config.network,
    signer: config.signer,
  } as const;
  const witnessReferenceScripts: FaultProofWitnessReferenceScripts = {
    computationThreadMint: config.references.witnesses.computationThreadMint,
    fraudProofMint: config.references.witnesses.fraudProofMint,
    phasMembershipWithdraw: config.references.witnesses.phasMembershipWithdraw,
  };
  const threadUtxo = async (threadOutRef: string): Promise<UTxO> =>
    await fetchUtxoByOutRef({
      lucid: config.lucid,
      outRef: parseOutRef(threadOutRef, "validationTraceDispute thread"),
      label: "validationTraceDispute thread UTxO",
    });

  const operatorProofAt = async ({
    material,
    highIndex,
    operatorHighHash,
  }: {
    readonly material: ValidationTraceDisputeActuationMaterial;
    readonly highIndex: number;
    readonly operatorHighHash: Buffer;
  }): Promise<MidgardValidationTraceProof> => {
    const candidates: MidgardValidationTraceProof[] = [
      validationTraceProofCoreFromData(material.claim.initial_state_proof),
      validationTraceProofCoreFromData(material.claim.terminal_state_proof),
      ...(await config.operatorProofs.collect()),
    ];
    const match = candidates.find(
      (proof) =>
        proof.stateIndex === highIndex &&
        hex(proof.stateHash) === hex(operatorHighHash),
    );
    if (match === undefined) {
      throw new Error(
        "validationTraceDispute cannot recover the operator's committed high proof from chain history",
      );
    }
    return match;
  };

  const oneStepArgumentFor = createValidationTraceOneStepArgumentResolver({
    config,
    threadUtxo,
  });

  /**
   * Resolves the published reference-script UTxO for the validator holding
   * the thread, by scanning the manifest-bound deployment entries for the
   * thread address's payment script hash (ruling R4: the runner consumes
   * exactly the deployment entries the submit layer consumes — the entry
   * name is immaterial, the immutable script hash is the identity). Returns
   * `undefined` when no entry carries a published out-ref so the caller's
   * own fail-closed carriage check still decides.
   */
  const publishedThreadScriptReference = async (
    utxo: UTxO,
    label: string,
  ): Promise<UTxO | undefined> => {
    const credential = getAddressDetails(utxo.address).paymentCredential;
    if (credential?.type !== "Script") {
      throw new Error(
        "validationTraceDispute thread is not at a script address",
      );
    }
    const deployed = Object.values(config.resolved.deploymentInfo).find(
      (entry) =>
        entry != null &&
        typeof entry === "object" &&
        (entry as { scriptHash?: string }).scriptHash === credential.hash &&
        (entry as { refScriptUTxO?: unknown }).refScriptUTxO != null,
    ) as { refScriptUTxO: { txHash: string; outputIndex: number } } | undefined;
    if (deployed === undefined) return undefined;
    return await fetchUtxoByOutRef({
      lucid: config.lucid,
      outRef: deployed.refScriptUTxO,
      label,
    });
  };

  const resolveCancelReference = async (utxo: UTxO): Promise<UTxO> => {
    const reference = await publishedThreadScriptReference(
      utxo,
      "validationTraceDispute cancel reference",
    );
    if (reference === undefined) {
      throw new Error(
        "validationTraceDispute cancel target has no published reference script",
      );
    }
    return reference;
  };

  return Object.freeze({
    capture: async ({
      action,
      material,
      retained,
    }: {
      readonly action: ValidationTraceDisputeActuatorAction;
      readonly material: ValidationTraceDisputeActuationMaterial;
      readonly retained?: ValidationTraceDisputeRetainedRouteInput;
    }): Promise<ValidationTraceDisputeCapturedAction> => {
      if (!/^[0-9a-f]{56}$/u.test(material.headerHash)) {
        throw new Error("validationTraceDispute material header changed");
      }
      switch (action.stage) {
        case "init":
          return await capture(async (preSubmitBoundary) => {
            await submitInit({
              ...common,
              fraudCategory: VALIDATION_TRACE_DISPUTE_CATEGORY,
              fraudulentBlockOutRef: action.stateQueueBlockOutRef,
              fraudulentHeaderHash: material.headerHash,
              witnessReferenceScripts,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          });
        case "open":
          return await capture(async (preSubmitBoundary) => {
            await submitValidationDisputeOpen({
              ...common,
              threadOutRef: action.threadOutRef,
              stateQueueBlockOutRef: action.stateQueueBlockOutRef,
              claim: material.claim,
              challengerDescriptor: material.challengerDescriptor,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          });
        case "verify_source":
          return await capture(async (preSubmitBoundary) => {
            await submitValidationDisputeVerifySource({
              ...common,
              threadOutRef: action.threadOutRef,
              sourceReferenceScriptUtxo: config.references.control.source,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          });
        case "reveal": {
          const utxo = await threadUtxo(action.threadOutRef);
          const dispute = requireGameDispute(utxo);
          const move = selectMidgardValidationDisputeReveal({
            dispute,
            role: "challenger",
            proofs: material.challengerTrace.tree.proofs,
          });
          if (move.type !== "revealChallenger") {
            throw new Error(
              "validationTraceDispute reveal planned while the dispute is not awaiting the challenger",
            );
          }
          return await capture(async (preSubmitBoundary) => {
            await submitValidationDisputeReveal({
              ...common,
              threadOutRef: action.threadOutRef,
              role: "challenger",
              proof: move.proof,
              gameReferenceScriptUtxo: config.references.control.game,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          });
        }
        case "enter_timeout":
          return await capture(async (preSubmitBoundary) => {
            await submitValidationDisputeEnterTimeout({
              ...common,
              threadOutRef: action.threadOutRef,
              gameReferenceScriptUtxo: config.references.control.game,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          });
        case "timeout":
          return await capture(async (preSubmitBoundary) => {
            await submitValidationDisputeTimeout({
              ...common,
              threadOutRef: action.threadOutRef,
              timeoutReferenceScriptUtxo: config.references.control.timeout,
              witnessReferenceScripts,
              now: now(),
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          });
        case "enter_resolution":
          return await capture(async (preSubmitBoundary) => {
            await submitValidationDisputeEnterResolution({
              ...common,
              threadOutRef: action.threadOutRef,
              gameReferenceScriptUtxo: config.references.control.game,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          });
        case "prepare_resolution": {
          const utxo = await threadUtxo(action.threadOutRef);
          const dispute = requireGameDispute(utxo);
          if (dispute.turn.type !== "readyForOneStep") {
            throw new Error(
              "validationTraceDispute resolution boundary is not ready for one step",
            );
          }
          const operatorProof = await operatorProofAt({
            material,
            highIndex: dispute.highIndex,
            operatorHighHash: dispute.operatorHighHash,
          });
          const challengerProof =
            material.challengerTrace.tree.proofs[dispute.highIndex];
          const preState = material.challengerTrace.states[dispute.lowIndex];
          if (challengerProof === undefined || preState === undefined) {
            throw new Error(
              "validationTraceDispute local trace is missing the adjudicated positions",
            );
          }
          return await capture(async (preSubmitBoundary) => {
            await submitValidationDisputePrepareResolution({
              ...common,
              threadOutRef: action.threadOutRef,
              preState: validationMachineStateDataFromCore(preState),
              operatorPost: validationTraceProofDataFromCore(operatorProof),
              challengerPost: validationTraceProofDataFromCore(challengerProof),
              boundaryReferenceScriptUtxo: config.references.control.boundary,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          });
        }
        case "prepare_selected": {
          const canonical = await captureCanonicalCheckpoint({
            config,
            material,
            action,
            input: await threadUtxo(action.threadOutRef),
            publishedThreadScriptReference,
          });
          if (canonical !== undefined) return canonical;
          const { argument: oneStepArgument, delivery } =
            await oneStepArgumentFor({
              material,
              threadOutRef: action.threadOutRef,
              retained,
              action,
            });
          // The thread sits at the boundary-selected prepare resolver's own
          // address, so the manifest-bound deployment entries resolve its
          // published reference by script hash. Reference-script carriage is
          // mandatory (owner ruling 2026-08-26): when no publication exists
          // the submit helper's fail-closed carriage check still refuses.
          const prepareReference = await publishedThreadScriptReference(
            await threadUtxo(action.threadOutRef),
            "validationTraceDispute prepare-resolver reference",
          );
          const transaction = await captureLocallyEvaluatedTransaction(
            async (preSubmitBoundary) => {
              await submitValidationDisputePrepareSelected({
                ...common,
                threadOutRef: action.threadOutRef,
                oneStepArgument,
                ...(prepareReference === undefined
                  ? {}
                  : { referenceScriptUtxo: prepareReference }),
                preSubmitBoundary,
                awaitConfirmation: false,
              });
            },
          );
          return Object.freeze({
            transaction,
            durableRouteInput: {
              transitionCborHex: hex(oneStepArgument.transitionCbor),
              auxiliaryCborHex: hex(oneStepArgument.auxiliaryCbor),
              ...(delivery === undefined
                ? {}
                : { fieldCarriageBinding: delivery.durableBinding }),
            },
          });
        }
        case "semantic_resolution": {
          const canonical = await captureCanonicalCheckpoint({
            config,
            material,
            action,
            input: await threadUtxo(action.threadOutRef),
            publishedThreadScriptReference,
          });
          if (canonical !== undefined) return canonical;
          const { argument: oneStepArgument, delivery } =
            await oneStepArgumentFor({
              material,
              threadOutRef: action.threadOutRef,
              retained,
              action,
            });
          const transaction = await captureLocallyEvaluatedTransaction(
            async (preSubmitBoundary) => {
              await submitValidationDisputeSemanticResolution({
                ...common,
                threadOutRef: action.threadOutRef,
                oneStepArgument,
                ...(delivery === undefined
                  ? {}
                  : {
                      carriageMaterial: delivery.material,
                      referenceScriptUtxo: delivery.semanticReference,
                    }),
                ...(action.scriptSourcesItemPreparedCbor === undefined
                  ? {}
                  : {
                      scriptSourcesItemPreparedCbor:
                        action.scriptSourcesItemPreparedCbor,
                    }),
                preSubmitBoundary,
                awaitConfirmation: false,
              });
            },
          );
          if (
            delivery !== undefined &&
            journalJsonDigest(
              [
                ...workflowTransactionReferenceInputOutRefs(transaction.signed),
              ].sort(),
            ) !== journalJsonDigest(delivery.durableBinding.referenceOutRefs)
          )
            throw new Error(
              "validation semantic transaction changed its prepared reference set",
            );
          return Object.freeze({
            transaction,
            durableRouteInput: {
              transitionCborHex: hex(oneStepArgument.transitionCbor),
              auxiliaryCborHex: hex(oneStepArgument.auxiliaryCbor),
              ...(delivery === undefined
                ? {}
                : { fieldCarriageBinding: delivery.durableBinding }),
            },
          });
        }
        case "cancel_semantic_route": {
          const utxo = await threadUtxo(action.threadOutRef);
          if (
            action.group === "cek_material_traversal" ||
            action.group === "cek_core_stage" ||
            action.group === "cek_context_stage" ||
            action.group === "cek_context_item_stage"
          ) {
            const cancel =
              action.group === "cek_material_traversal"
                ? cancelValidationCekMaterialTraversal
                : action.group === "cek_core_stage"
                  ? cancelValidationCekCore
                  : cancelValidationCekContext;
            return await capture(async (preSubmitBoundary) => {
              await cancel({
                ...common,
                threadOutRef: action.threadOutRef,
                witnessReferenceScripts,
                preSubmitBoundary,
                awaitConfirmation: false,
              });
            });
          }
          const reference = await resolveCancelReference(utxo);
          return await capture(async (preSubmitBoundary) => {
            await cancelValidationSemanticResolution({
              ...common,
              threadOutRef: action.threadOutRef,
              referenceScriptUtxo: reference,
              witnessReferenceScripts,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          });
        }
        case "award":
          return await capture(async (preSubmitBoundary) => {
            await submitValidationDisputeAward({
              ...common,
              threadOutRef: action.threadOutRef,
              awardReferenceScriptUtxo: config.references.control.award,
              witnessReferenceScripts,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          });
        case "remove":
          return await captureCursorRemoval({
            category: VALIDATION_TRACE_DISPUTE_CATEGORY,
            ...common,
            headerHash: material.headerHash,
            input: {
              schemaVersion: "midgard-production-cursor-family-action-v1",
              category: VALIDATION_TRACE_DISPUTE_CATEGORY,
              stage: "remove",
              stateQueueBlockOutRef: action.stateQueueBlockOutRef,
              nextRemovalOutRef: action.nextRemovalOutRef,
              fraudProofOutRef: action.fraudProofOutRef,
            } as CursorFamilyActionInput,
            stateQueueMutationLeaseCoordinator:
              config.stateQueueMutationLeaseCoordinator,
            fraudProverRewardLovelace: config.fraudProverRewardLovelace,
          });
      }
    },
  });
};
