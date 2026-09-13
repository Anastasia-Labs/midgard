import {
  type MidgardValidationDispute,
  type MidgardValidationTraceProof,
  selectMidgardValidationDisputeReveal,
} from "@al-ft/midgard-core";
import {
  PreparedValidationResolutionDatum,
  type ValidationClaimWitness,
  validationDisputeCoreFromData,
  ValidationDisputeDatum,
  ValidationGameSpendRedeemer,
  ValidationMachineState,
  validationMachineStateDataFromCore,
  ValidationResolutionDatum,
  type ValidationResolutionState,
  type ValidationTraceDescriptor,
  validationTraceProofCoreFromData,
  validationTraceProofDataFromCore,
} from "@al-ft/midgard-sdk";
import {
  buildValidationOneStepArgument,
  type DeterministicValidationMachineTrace,
  type ValidationOneStepArgument,
} from "@al-ft/midgard-validation";
import {
  CML,
  Data,
  getAddressDetails,
  type LucidEvolution,
  type Network,
  type UTxO,
} from "@lucid-evolution/lucid";

import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import type {
  ResolvedProverSigner,
  ResolvedValidationTraceDisputeDeploymentContracts,
} from "../runtime.js";
import { fetchUtxoByOutRef, parseOutRef } from "../runtime.js";
import { submitInit } from "../submit-init.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import {
  captureCursorRemoval,
  type CursorFamilyActionInput,
} from "../workflow/cursor-family-runtime.js";
import {
  captureLocallyEvaluatedTransaction,
  type LocallyEvaluatedTransaction,
} from "../workflow/transaction-boundary.js";
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
import type {
  ValidationTraceDisputeChainStage,
  ValidationTraceDisputeSemanticGroup,
} from "./workflow-chain-state.js";
import {
  VALIDATION_TRACE_DISPUTE_CATEGORY,
  VALIDATION_TRACE_DISPUTE_CATEGORY_ID,
} from "./workflow-family.js";

/**
 * Ruling R6 ("installed" semantics for the sole interactive family): from
 * every derived chain stage the honest watcher either owns exactly one legal
 * transaction, is deliberately waiting on the counterparty's clock (with the
 * timeout claim armed the moment that clock lapses), or the journey is
 * complete. `planValidationTraceDisputeMove` is total over the cursor type,
 * so the runner can always force progress: detect → initiate → play every
 * honest response → claim timeout when the operator stalls → award →
 * remove. An interrupted multi-transaction semantic route cancels the thread
 * (a legal, always-available single transaction) and restarts from init —
 * progress is never blocked on lost local state, and the cursor is re-derived
 * exclusively from chain state on every invocation.
 */
export type ValidationTraceDisputeActuatorAction =
  | Readonly<{ stage: "init"; stateQueueBlockOutRef: string }>
  | Readonly<{
      stage: "open";
      threadOutRef: string;
      stateQueueBlockOutRef: string;
    }>
  | Readonly<{ stage: "verify_source"; threadOutRef: string }>
  | Readonly<{ stage: "reveal"; threadOutRef: string }>
  | Readonly<{ stage: "enter_timeout"; threadOutRef: string }>
  | Readonly<{ stage: "timeout"; threadOutRef: string }>
  | Readonly<{ stage: "enter_resolution"; threadOutRef: string }>
  | Readonly<{ stage: "prepare_resolution"; threadOutRef: string }>
  | Readonly<{ stage: "prepare_selected"; threadOutRef: string }>
  | Readonly<{
      stage: "semantic_resolution";
      threadOutRef: string;
      scriptSourcesItemPreparedCbor?: string;
    }>
  | Readonly<{
      stage: "cancel_semantic_route";
      threadOutRef: string;
      group: ValidationTraceDisputeSemanticGroup;
    }>
  | Readonly<{ stage: "award"; threadOutRef: string }>
  | Readonly<{
      stage: "remove";
      nextRemovalOutRef: string;
      fraudProofOutRef: string;
    }>;

export type ValidationTraceDisputeMove =
  | Readonly<{ kind: "act"; action: ValidationTraceDisputeActuatorAction }>
  | Readonly<{
      kind: "await_counterparty";
      threadOutRef: string;
      responseDeadline: number;
    }>
  | Readonly<{ kind: "completed" }>;

/**
 * Retained durable material for resuming an interrupted multi-transaction
 * semantic route. Sourced from the journal's last `submission_intent`
 * action input — never from process memory.
 */
export type ValidationTraceDisputeRetainedRouteInput = Readonly<{
  transitionCborHex?: string;
  auxiliaryCborHex?: string;
  scriptSourcesItemPreparedCbor?: string;
}>;

export const planValidationTraceDisputeMove = ({
  stage,
  retained,
}: {
  readonly stage: ValidationTraceDisputeChainStage;
  readonly retained?: ValidationTraceDisputeRetainedRouteInput;
}): ValidationTraceDisputeMove => {
  switch (stage.kind) {
    case "not_started":
      return {
        kind: "act",
        action: {
          stage: "init",
          stateQueueBlockOutRef: stage.stateQueueBlockOutRef,
        },
      };
    case "init":
      return {
        kind: "act",
        action: {
          stage: "open",
          threadOutRef: stage.threadOutRef,
          stateQueueBlockOutRef: stage.stateQueueBlockOutRef,
        },
      };
    case "open_pending_source":
      return {
        kind: "act",
        action: { stage: "verify_source", threadOutRef: stage.threadOutRef },
      };
    case "game":
      if (stage.turn === "ready_for_one_step") {
        return {
          kind: "act",
          action: {
            stage: "enter_resolution",
            threadOutRef: stage.threadOutRef,
          },
        };
      }
      if (stage.turn === "awaiting_challenger") {
        return {
          kind: "act",
          action: { stage: "reveal", threadOutRef: stage.threadOutRef },
        };
      }
      if (stage.timeoutClaimable) {
        return {
          kind: "act",
          action: { stage: "enter_timeout", threadOutRef: stage.threadOutRef },
        };
      }
      return {
        kind: "await_counterparty",
        threadOutRef: stage.threadOutRef,
        responseDeadline: stage.responseDeadline,
      };
    case "timeout_pending":
      return {
        kind: "act",
        action: { stage: "timeout", threadOutRef: stage.threadOutRef },
      };
    case "resolution_boundary":
      return {
        kind: "act",
        action: {
          stage: "prepare_resolution",
          threadOutRef: stage.threadOutRef,
        },
      };
    case "prepare_selected_pending":
      return {
        kind: "act",
        action: { stage: "prepare_selected", threadOutRef: stage.threadOutRef },
      };
    case "semantic_pending":
      return {
        kind: "act",
        action: {
          stage: "semantic_resolution",
          threadOutRef: stage.threadOutRef,
          ...(retained?.scriptSourcesItemPreparedCbor === undefined
            ? {}
            : {
                scriptSourcesItemPreparedCbor:
                  retained.scriptSourcesItemPreparedCbor,
              }),
        },
      };
    case "semantic_in_flight":
      // A staged multi-transaction route interrupted mid-flight is always
      // recoverable without local memory: cancellation is a single legal
      // transaction at every checkpoint (full journal discipline), after
      // which the cursor re-derives `not_started` and the dispute restarts.
      // The retained-DA resume helpers remain operator tooling; their
      // multi-transaction drivers predate the pre-submit boundary seam, so
      // the durable workflow never routes through them.
      return {
        kind: "act",
        action: {
          stage: "cancel_semantic_route",
          threadOutRef: stage.threadOutRef,
          group: stage.group,
        },
      };
    case "award_pending":
      return {
        kind: "act",
        action: { stage: "award", threadOutRef: stage.threadOutRef },
      };
    case "proof_token":
      return {
        kind: "act",
        action: {
          stage: "remove",
          nextRemovalOutRef: stage.nextRemovalOutRef,
          fraudProofOutRef: stage.fraudProofOutRef,
        },
      };
    case "removed":
      return { kind: "completed" };
  }
};

/**
 * The challenger's admitted dispute material: the operator's committed claim
 * witness and the challenger's own deterministic replay, exactly as returned
 * by the workflow challenge authority (`validationTraceMaterial`).
 */
export type ValidationTraceDisputeActuationMaterial = Readonly<{
  headerHash: string;
  claim: ValidationClaimWitness;
  challengerDescriptor: ValidationTraceDescriptor;
  challengerTrace: DeterministicValidationMachineTrace;
}>;

/**
 * On-chain-authenticated source of the operator's revealed bisection proofs
 * (the `RevealOperator` game redeemers). Production implementations decode
 * these from the raw thread-unit transaction history; the emulator journey
 * harness decodes the same redeemer bytes from submitted transactions.
 */
export type ValidationTraceDisputeOperatorProofSource = Readonly<{
  collect: () => Promise<readonly MidgardValidationTraceProof[]>;
}>;

/**
 * Decodes every `Continue(RevealOperator)` game redeemer found in a witness
 * set. The bytes come from authenticated L1 history (raw snapshot witness
 * sets in production, submitted transactions in the emulator journey), so a
 * proof recovered here is the operator's own on-chain commitment.
 */
export const decodeOperatorRevealProofsFromWitnessSet = (
  witnessSetCbor: string,
): readonly MidgardValidationTraceProof[] => {
  const proofs: MidgardValidationTraceProof[] = [];
  const witnesses = CML.TransactionWitnessSet.from_cbor_hex(witnessSetCbor);
  const redeemers = witnesses.redeemers();
  if (redeemers === undefined) return proofs;
  const payloads: string[] = [];
  const legacy = redeemers.as_arr_legacy_redeemer();
  if (legacy !== undefined) {
    for (let index = 0; index < legacy.len(); index += 1) {
      payloads.push(legacy.get(index).data().to_cbor_hex());
    }
  }
  const map = redeemers.as_map_redeemer_key_to_redeemer_val();
  if (map !== undefined) {
    const keys = map.keys();
    for (let index = 0; index < keys.len(); index += 1) {
      const value = map.get(keys.get(index));
      if (value !== undefined) payloads.push(value.data().to_cbor_hex());
    }
  }
  for (const payload of payloads) {
    let decoded: ValidationGameSpendRedeemer;
    try {
      decoded = Data.from(payload, ValidationGameSpendRedeemer);
    } catch {
      continue; // not a validation-game redeemer
    }
    if (typeof decoded !== "object" || !("Continue" in decoded)) continue;
    const action = decoded.Continue[0];
    if (typeof action === "object" && "RevealOperator" in action) {
      proofs.push(
        validationTraceProofCoreFromData(action.RevealOperator.proof),
      );
    }
  }
  return proofs;
};

export type ValidationTraceDisputeWorkflowReferences = Readonly<{
  control: Readonly<{
    source: UTxO;
    game: UTxO;
    boundary: UTxO;
    timeout: UTxO;
    award: UTxO;
  }>;
  witnesses: Readonly<{
    computationThreadMint: UTxO;
    fraudProofMint: UTxO;
    phasMembershipWithdraw: UTxO;
  }>;
}>;

export type ValidationTraceDisputeActuatorConfig = Readonly<{
  lucid: LucidEvolution;
  blueprint: unknown;
  deploymentInfo: unknown;
  network: Network;
  signer: ResolvedProverSigner;
  categoryId: string;
  resolved: ResolvedValidationTraceDisputeDeploymentContracts;
  references: ValidationTraceDisputeWorkflowReferences;
  operatorProofs: ValidationTraceDisputeOperatorProofSource;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  fraudProverRewardLovelace: bigint;
  /** Wall-clock authority for validity ranges and deadline comparisons. */
  now?: () => number;
}>;

export type ValidationTraceDisputeCapturedAction = Readonly<{
  transaction: LocallyEvaluatedTransaction;
  mutationLease?: Awaited<
    ReturnType<StateQueueMutationLeaseCoordinator["acquire"]>
  >;
  /** Durable route input to retain in the journal for resumption. */
  durableRouteInput?: ValidationTraceDisputeRetainedRouteInput;
}>;

const capture = async (
  submit: Parameters<typeof captureLocallyEvaluatedTransaction>[0],
): Promise<ValidationTraceDisputeCapturedAction> =>
  Object.freeze({
    transaction: await captureLocallyEvaluatedTransaction(submit),
  });

const hex = (value: Buffer | Uint8Array): string =>
  Buffer.from(value).toString("hex");

/**
 * Recovers the agreed low state index from an on-chain resolution state: the
 * challenger successor hash locates the candidate index in the local trace,
 * and the full pre-state encoding must match before the index is trusted.
 */
export const recoverValidationTraceStateIndex = ({
  trace,
  resolution,
}: {
  readonly trace: DeterministicValidationMachineTrace;
  readonly resolution: ValidationResolutionState;
}): number => {
  const preStateCbor = Data.to(resolution.pre_state, ValidationMachineState);
  for (let index = 0; index + 1 < trace.tree.stateHashes.length; index += 1) {
    if (
      hex(trace.tree.stateHashes[index + 1]!) !==
      resolution.challenger_successor_hash
    ) {
      continue;
    }
    const local = Data.to(
      validationMachineStateDataFromCore(trace.states[index]!),
      ValidationMachineState,
    );
    if (local === preStateCbor) return index;
  }
  throw new Error(
    "validationTraceDispute resolution pre-state is not a position of the local challenger trace",
  );
};

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

  const disputeFromResolutionInput = async (
    threadOutRef: string,
  ): Promise<ValidationResolutionState> => {
    const utxo = await threadUtxo(threadOutRef);
    if (utxo.datum == null) {
      throw new Error(
        "validationTraceDispute resolution thread lost its datum",
      );
    }
    try {
      const prepared = Data.from(utxo.datum, PreparedValidationResolutionDatum);
      if (prepared.data !== null) return prepared.data.resolution;
    } catch {
      // fall through to the unprepared resolution shape
    }
    const resolution = Data.from(utxo.datum, ValidationResolutionDatum);
    if (resolution.data === null) {
      throw new Error(
        "validationTraceDispute resolution thread carries a null resolution state",
      );
    }
    return resolution.data;
  };

  const oneStepArgumentFor = async ({
    material,
    threadOutRef,
    retained,
  }: {
    readonly material: ValidationTraceDisputeActuationMaterial;
    readonly threadOutRef: string;
    readonly retained?: ValidationTraceDisputeRetainedRouteInput;
  }): Promise<ValidationOneStepArgument> => {
    const resolution = await disputeFromResolutionInput(threadOutRef);
    const stateIndex = recoverValidationTraceStateIndex({
      trace: material.challengerTrace,
      resolution,
    });
    const argument = buildValidationOneStepArgument({
      trace: material.challengerTrace,
      stateIndex,
    });
    if (
      retained?.transitionCborHex !== undefined &&
      hex(argument.transitionCbor) !== retained.transitionCborHex
    ) {
      throw new Error(
        "validationTraceDispute retained route input diverged from the recomputed one-step argument",
      );
    }
    return argument;
  };

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
          const oneStepArgument = await oneStepArgumentFor({
            material,
            threadOutRef: action.threadOutRef,
            retained,
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
            },
          });
        }
        case "semantic_resolution": {
          const oneStepArgument = await oneStepArgumentFor({
            material,
            threadOutRef: action.threadOutRef,
            retained,
          });
          const transaction = await captureLocallyEvaluatedTransaction(
            async (preSubmitBoundary) => {
              await submitValidationDisputeSemanticResolution({
                ...common,
                threadOutRef: action.threadOutRef,
                oneStepArgument,
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
          return Object.freeze({
            transaction,
            durableRouteInput: {
              transitionCborHex: hex(oneStepArgument.transitionCbor),
              auxiliaryCborHex: hex(oneStepArgument.auxiliaryCbor),
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

const requireGameDispute = (utxo: UTxO): MidgardValidationDispute => {
  if (utxo.datum == null) {
    throw new Error("validationTraceDispute game thread lost its datum");
  }
  const datum = Data.from(utxo.datum, ValidationDisputeDatum);
  if (datum.data === null) {
    throw new Error(
      "validationTraceDispute game thread carries a null dispute state",
    );
  }
  return validationDisputeCoreFromData(datum.data.dispute);
};
