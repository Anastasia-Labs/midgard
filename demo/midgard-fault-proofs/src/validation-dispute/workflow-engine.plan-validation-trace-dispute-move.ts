import { type MidgardValidationTraceProof } from "@al-ft/midgard-core";
import {
  type ValidationClaimWitness,
  ValidationGameSpendRedeemer,
  type ValidationTraceDescriptor,
  validationTraceProofCoreFromData,
} from "@al-ft/midgard-sdk";
import { type DeterministicValidationMachineTrace } from "@al-ft/midgard-validation";
import {
  CML,
  Data,
  type LucidEvolution,
  type Network,
  type UTxO,
} from "@lucid-evolution/lucid";

import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import type {
  ResolvedProverSigner,
  ResolvedValidationTraceDisputeDeploymentContracts,
} from "../runtime.js";
import { type LocallyEvaluatedTransaction } from "../workflow/transaction-boundary.js";
import type {
  ValidationTraceDisputeChainStage,
  ValidationTraceDisputeSemanticGroup,
} from "./workflow-chain-state.js";
import { type createValidationTraceFieldCarriageProvider } from "./workflow-field-carriage.js";
import { type ValidationStagedPreparationSource } from "./workflow-staged-route-preparation.js";

/**
 * Ruling R6 ("installed" semantics for the sole interactive family): from
 * every derived chain stage the honest watcher either owns exactly one legal
 * transaction, is deliberately waiting on the counterparty's clock (with the
 * timeout claim armed the moment that clock lapses), or the journey is
 * complete. `planValidationTraceDisputeMove` is total over the cursor type,
 * so the runner can always force progress: detect → initiate → play every
 * honest response → claim timeout when the operator stalls → award →
 * remove. A CEK core or context route, and the split ScriptSources
 * redeemer-item route, advance one stage per move from the checkpoint the
 * previous stage left, bound to the preparation the entry move journaled.
 * Any other interrupted multi-transaction semantic route, or a staged one
 * without that journaled preparation, cancels the thread (a legal,
 * always-available single transaction) and restarts from init — progress is
 * never blocked on lost local state, and the cursor is re-derived
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
      /**
       * Continues a CEK core or context route from its live stage checkpoint:
       * the semantic resolver's prepared-resolution datum the route's first
       * move consumed.
       */
      cekPreparedResolutionCbor?: string;
    }>
  | Readonly<{
      stage: "cancel_semantic_route";
      threadOutRef: string;
      group: ValidationTraceDisputeSemanticGroup;
    }>
  | Readonly<{ stage: "award"; threadOutRef: string }>
  | Readonly<{
      stage: "remove";
      stateQueueBlockOutRef: string;
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
export type ValidationTraceDisputeFieldCarriageBinding = Readonly<{
  stateIndex: number;
  fieldIndex: number;
  transactionId: string;
  fieldCommitment: string;
  referenceOutRefs: readonly string[];
}> &
  (
    | Readonly<{ certificatePolicyId: string }>
    | Readonly<{
        proofItemPublication: Readonly<{
          address: string;
          datumCbor: string;
          outRef: string;
        }>;
      }>
  );

export type ValidationTraceDisputeRetainedRouteInput = Readonly<{
  transitionCborHex?: string;
  auxiliaryCborHex?: string;
  fieldCarriageBinding?: ValidationTraceDisputeFieldCarriageBinding;
  /**
   * The prepared-resolution datum the split ScriptSources redeemer-item
   * route's entry stage consumed; every later stage resumes against it.
   */
  scriptSourcesItemPreparedCbor?: string;
  /**
   * The prepared-resolution datum a CEK core or context route's first stage
   * consumed at the semantic resolver; every later stage resumes against it.
   */
  cekPreparedResolutionCbor?: string;
}>;

/** Semantic groups whose stages the workflow advances one move at a time. */
const RESUMABLE_CEK_GROUPS: ReadonlySet<ValidationTraceDisputeSemanticGroup> =
  new Set(["cek_core_stage", "cek_context_stage", "cek_context_item_stage"]);
/**
 * The groups a split ScriptSources redeemer-item stage classifies as. Its
 * normalizers, source authenticator and shared executors compile to the same
 * addresses as the CEK context item chain's, which the address classifier
 * registers first; only its envelope, settlement and extra executors are its
 * own.
 */
const SCRIPT_SOURCES_ITEM_GROUPS: ReadonlySet<ValidationTraceDisputeSemanticGroup> =
  new Set(["script_sources_item_stage", "cek_context_item_stage"]);

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
      if (
        stage.group === "canonical_decode_item_stage" &&
        stage.canonicalOutput === true
      )
        return {
          kind: "act",
          action: {
            stage: "semantic_resolution",
            threadOutRef: stage.threadOutRef,
          },
        };
      // Each CEK core and context stage reaches the pre-submit boundary, so
      // the workflow submits one stage per move and resumes the next from
      // the checkpoint it leaves, against the journaled preparation.
      if (
        RESUMABLE_CEK_GROUPS.has(stage.group) &&
        retained?.cekPreparedResolutionCbor !== undefined
      )
        return {
          kind: "act",
          action: {
            stage: "semantic_resolution",
            threadOutRef: stage.threadOutRef,
            cekPreparedResolutionCbor: retained.cekPreparedResolutionCbor,
          },
        };
      // The split ScriptSources redeemer-item route reaches the boundary at
      // every stage too, and resumes against the preparation its entry stage
      // consumed.
      if (
        SCRIPT_SOURCES_ITEM_GROUPS.has(stage.group) &&
        retained?.scriptSourcesItemPreparedCbor !== undefined
      )
        return {
          kind: "act",
          action: {
            stage: "semantic_resolution",
            threadOutRef: stage.threadOutRef,
            scriptSourcesItemPreparedCbor:
              retained.scriptSourcesItemPreparedCbor,
          },
        };
      // Any other staged multi-transaction route interrupted mid-flight is
      // recoverable without local memory: cancellation is a single legal
      // transaction at every checkpoint (full journal discipline), after
      // which the cursor re-derives `not_started` and the dispute restarts.
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
          stateQueueBlockOutRef: stage.stateQueueBlockOutRef,
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
  /** Recomputes a staged route's preparation when the journal's is unusable. */
  stagedPreparations: ValidationStagedPreparationSource;
  fieldCarriage?: ReturnType<typeof createValidationTraceFieldCarriageProvider>;
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
