/** The validation-trace dispute workflow adapter: from a detected failing
 * Plutus execution it Inits a thread, Opens the challenger's claim,
 * VerifySources it into a case and its bond, closes the case nobody
 * defended into its fraud-proof record, and removes the convicted block
 * through the shared remove stage (removal.ts), all through the shared
 * workflow orchestrator. On a defended case it plays each thread (threads.ts,
 * moves.ts) up to the one-step resolution, which is not installed. Every
 * output of ours is claimed by out-ref from our own signed body before
 * submission and read back by that out-ref; chain state, not the journal's
 * idea of the step, decides the next action. A thread move needs the claim
 * material, which each run reads back as it validates its saved artifact. */
import type { L1SlotConfig } from "@al-ft/midgard-core/l1-slot-config";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, type LucidEvolution } from "@lucid-evolution/lucid";

import type { ResolvedProverSigner } from "../runtime.js";
import { WorkflowActionChangedError } from "../workflow/action-changed.js";
import { isWorkflowFundingShortfall } from "../workflow/funding-reservation-permit.workflow-funding-shortfall.js";
import type { FraudProofSignedTransactionRecovery } from "../workflow/l1-source.js";
import type { FraudProofWorkflowAdapterContext } from "../workflow/orchestrator.fraud-proof-family-workflow-adapter.js";
import {
  FRAUD_PROOF_WORKFLOW_ADAPTER,
  FRAUD_PROOF_WORKFLOW_SAFETY,
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowObservation,
} from "../workflow/orchestrator.js";
import { readOwnOutputs } from "../workflow/own-outputs.js";
import { reconcileSignedWorkflowTransaction } from "../workflow/signed-transaction-reconciliation.js";
import {
  bindWorkflowPreflightTransaction,
  LOCAL_UPLC_EVALUATOR,
  type LocallyEvaluatedTransaction,
  requireReferenceOnlyScriptWitnesses,
  submitCapturedTransaction,
} from "../workflow/transaction-boundary.js";
import {
  confirmedValidationTraceStages,
  isClaimedOutput,
  parseValidationTraceAction,
  parseValidationTraceRecovery,
  validationTraceAction,
} from "./actions.js";
import { parseValidationTraceDisputeArtifact } from "./artifact.js";
import { validationTraceCaseReads } from "./case-reads.js";
import type { ValidationTraceClaimMaterial } from "./claim-material.js";
import { captureValidationTraceClose } from "./close.js";
import {
  prepareValidationTraceArtifact,
  validateValidationTraceArtifact,
} from "./detected-claim.js";
import {
  captureValidationTraceReveal,
  captureValidationTraceThreadClose,
  captureValidationTraceTimeout,
} from "./moves.js";
import {
  createValidationTraceRemoval,
  VALIDATION_TRACE_CASE_CONVICTED,
  type ValidationTraceRemovalConfig,
} from "./removal.js";
import { observeValidationTraceThreads } from "./threads.js";
import {
  type CapturedValidationTraceStage,
  captureValidationTraceInit,
  captureValidationTraceOpen,
  captureValidationTraceVerifySource,
  type ValidationTraceDisputeDeployment,
} from "./transactions.js";
import {
  validationTraceCaseOpenReason,
  validationTraceDefenseDeadlineMs,
} from "./waiting.js";

const CATEGORY = "validationTraceDispute" as const;

/** A stage transaction that does not build; it is retried on observation. */
export const VALIDATION_TRACE_STAGE_HELD = "validation-trace-stage-held";

export {
  VALIDATION_TRACE_CASE_CONTESTED,
  VALIDATION_TRACE_ONE_STEP_REQUIRED,
  VALIDATION_TRACE_THREAD_AWAITING,
  VALIDATION_TRACE_THREAD_COUNT_HOLD,
  VALIDATION_TRACE_THREAD_DISCOVERY,
} from "./threads.js";
export { VALIDATION_TRACE_CASE_OPEN } from "./waiting.js";

export { VALIDATION_TRACE_CASE_CONVICTED };

/**
 * How long an output of ours the provider does not show after its
 * transaction was confirmed is taken for provider lag rather than spent.
 */
export const VALIDATION_TRACE_PROVIDER_GRACE_MS = 10 * 60_000;

export class ValidationTraceStageHeldError extends WorkflowActionChangedError {
  constructor(
    readonly stage: string,
    override readonly cause: unknown,
  ) {
    super(
      `${VALIDATION_TRACE_STAGE_HELD}: ${stage} does not build: ${cause instanceof Error ? cause.message : String(cause)}`,
    );
    this.name = "ValidationTraceStageHeldError";
  }
}

export const createValidationTraceDisputeWorkflowAdapter = ({
  lucid,
  signer,
  deployment,
  l1SlotConfig,
  removal,
  signedRecovery,
  now = Date.now,
}: {
  readonly lucid: LucidEvolution;
  readonly signer: ResolvedProverSigner;
  readonly deployment: ValidationTraceDisputeDeployment;
  readonly l1SlotConfig: L1SlotConfig;
  /** The convicted block's removal through the shared remove stage. */
  readonly removal: ValidationTraceRemovalConfig;
  /** Where one exact signed transaction stands, and its rebroadcast. */
  readonly signedRecovery?: FraudProofSignedTransactionRecovery;
  readonly now?: () => number;
}): FraudProofFamilyWorkflowAdapter => {
  if (
    deployment.init.contracts.steps[0].spendingScriptHash !==
    deployment.chain.opener.spendingScriptHash
  )
    throw new Error("validation-trace Init must start at the dispute opener");
  const prepared = new Map<string, LocallyEvaluatedTransaction>();
  // Each block's claim material, as its saved artifact was last validated.
  const materials = new Map<string, ValidationTraceClaimMaterial>();
  const removalStage = createValidationTraceRemoval({
    lucid,
    signer,
    deployment,
    removal,
  });
  const read = (outRefs: readonly string[]) =>
    readOwnOutputs((refs) => lucid.utxosByOutRef([...refs]), outRefs);
  const recent = (confirmedAt: string): boolean =>
    now() - Date.parse(confirmedAt) < VALIDATION_TRACE_PROVIDER_GRACE_MS;
  const { recordAt, ourCase, isContested } = validationTraceCaseReads({
    lucid,
    signer,
    deployment,
  });

  const requireIdentity = (context: FraudProofWorkflowAdapterContext) => {
    if (
      context.identity.category !== CATEGORY ||
      context.identity.target.kind !== "state_queue_header"
    )
      throw new Error("validation-trace adapter received another workflow");
    const artifact = parseValidationTraceDisputeArtifact(context.artifact);
    if (artifact.headerHash !== context.identity.target.headerHash)
      throw new Error("validation-trace artifact names another block");
    return artifact;
  };

  const observe = async (
    context: FraudProofWorkflowAdapterContext,
  ): Promise<FraudProofWorkflowObservation> => {
    const artifact = requireIdentity(context);
    const stages = confirmedValidationTraceStages(context.entries);
    const live = await read(
      stages.flatMap(({ outputs }) => outputs.map(({ outRef }) => outRef)),
    );
    const newestFirst = [...stages].reverse();
    const liveClaim = (stage: (typeof stages)[number]["action"]["stage"]) =>
      newestFirst
        .filter((confirmed) => confirmed.action.stage === stage)
        .map(({ outputs }) => outputs[0]!)
        .find((claim) => isClaimedOutput(claim, live.get(claim.outRef)));
    const record = liveClaim("close");
    if (record !== undefined)
      return await removalStage.convicted(context, record);
    // Our case as VerifySource left it: nobody has defended it.
    const openCase = liveClaim("verify_source");
    if (openCase !== undefined) {
      const caseDatum = Data.from(live.get(openCase.outRef)!.datum!, SDK.Case);
      return now() > validationTraceDefenseDeadlineMs(caseDatum)
        ? {
            kind: "action_required",
            action: validationTraceAction({
              stage: "close",
              consumes: openCase.outRef,
            }),
          }
        : {
            kind: "pending",
            reason: validationTraceCaseOpenReason({
              caseUnit: openCase.unit,
              outRef: openCase.outRef,
              caseDatum,
            }),
          };
    }
    const opener = liveClaim("open");
    if (opener !== undefined)
      return {
        kind: "action_required",
        action: validationTraceAction({
          stage: "verify_source",
          consumes: opener.outRef,
        }),
      };
    const thread = liveClaim("init");
    if (thread !== undefined)
      return {
        kind: "action_required",
        action: validationTraceAction({
          stage: "open",
          consumes: thread.outRef,
        }),
      };
    const found = await ourCase(artifact, stages);
    if (found !== undefined)
      return isContested(found.caseDatum)
        ? await observeValidationTraceThreads({
            lucid,
            signer,
            deployment,
            caseUnit: found.caseUnit,
            caseOutRef: found.outRef,
            caseDatum: found.caseDatum,
            caseOriginOutRef: found.originOutRef,
            now: now(),
            material: materials.get(artifact.headerHash),
          })
        : {
            kind: "pending",
            reason: validationTraceCaseOpenReason(found),
          };
    if (newestFirst.some(({ action }) => action.stage === "verify_source")) {
      const recorded = await recordAt(artifact.headerHash);
      if (recorded !== undefined)
        return await removalStage.convicted(context, recorded);
    }
    const newest = newestFirst[0];
    if (newest !== undefined && recent(newest.confirmedAt))
      return {
        kind: "pending",
        reason: `validation-trace output ${newest.outputs[0]!.outRef} is not yet visible to the provider`,
      };
    if (newestFirst.some(({ action }) => action.stage === "verify_source"))
      return {
        kind: "conflict",
        reason: "validation-trace case is closed without this workflow's close",
      };
    const replaced = newestFirst.find(({ action }) => action.stage === "init");
    return {
      kind: "action_required",
      action: validationTraceAction({
        stage: "init",
        consumes: replaced?.outputs[0]!.outRef ?? null,
      }),
    };
  };

  /** The bond VerifySource left beside our case at `caseOutRef`. */
  const bondOf = (
    context: FraudProofWorkflowAdapterContext,
    caseOutRef: string,
  ): string => {
    const verified = confirmedValidationTraceStages(context.entries).find(
      ({ action, outputs }) =>
        action.stage === "verify_source" && outputs[0]!.outRef === caseOutRef,
    );
    if (verified === undefined)
      throw new WorkflowActionChangedError(
        `validation-trace case ${caseOutRef} is not one VerifySource left`,
      );
    return verified.outputs[1]!.outRef;
  };

  const capture = async (
    context: FraudProofWorkflowAdapterContext,
    action: ReturnType<typeof parseValidationTraceAction>,
  ): Promise<CapturedValidationTraceStage> => {
    const input = {
      lucid,
      signer,
      deployment,
      artifact: requireIdentity(context),
    };
    try {
      switch (action.stage) {
        case "init":
          return await captureValidationTraceInit(input);
        case "open":
          return await captureValidationTraceOpen({
            ...input,
            threadOutRef: action.consumes!,
          });
        case "verify_source":
          return await captureValidationTraceVerifySource({
            ...input,
            openerOutRef: action.consumes!,
          });
        case "close":
          return await captureValidationTraceClose({
            ...input,
            caseOutRef: action.consumes!,
            bondOutRef: bondOf(context, action.consumes!),
          });
        case "reveal":
        case "timeout":
        case "close_thread":
          break;
      }
      const found = await ourCase(
        input.artifact,
        confirmedValidationTraceStages(context.entries),
      );
      if (found?.originOutRef === undefined)
        throw new WorkflowActionChangedError(
          `validation-trace ${action.stage} has no case of ours to play`,
        );
      const move = {
        ...input,
        threadOutRef: action.consumes!,
        caseUnit: found.caseUnit,
        caseOriginOutRef: found.originOutRef,
      };
      if (action.stage === "reveal")
        return await captureValidationTraceReveal({
          ...move,
          material: materials.get(input.artifact.headerHash),
        });
      return action.stage === "timeout"
        ? await captureValidationTraceTimeout(move)
        : await captureValidationTraceThreadClose({
            ...move,
            caseOutRef: found.outRef,
          });
    } catch (cause) {
      if (
        cause instanceof WorkflowActionChangedError ||
        isWorkflowFundingShortfall(cause)
      )
        throw cause;
      throw new ValidationTraceStageHeldError(action.stage, cause);
    }
  };

  const adapter: FraudProofFamilyWorkflowAdapter = {
    adapterVersion: FRAUD_PROOF_WORKFLOW_ADAPTER,
    category: CATEGORY,
    safety: FRAUD_PROOF_WORKFLOW_SAFETY,
    prepare: async ({ evidence, replayContext, classification }) =>
      await prepareValidationTraceArtifact({
        evidence,
        context: replayContext,
        classification,
        l1SlotConfig,
      }),
    validatePreparedArtifact: async ({
      evidence,
      replayContext,
      classification,
      artifact,
    }) =>
      await validateValidationTraceArtifact({
        evidence,
        context: replayContext,
        classification,
        artifact,
        l1SlotConfig,
        onMaterial: (material) => materials.set(evidence.headerHash, material),
      }),
    observe,
    preflight: async (context) => {
      const { action, workflowId } = context;
      const parsed = parseValidationTraceAction(action);
      const current = await observe(context);
      if (
        current.kind !== "action_required" ||
        current.action.actionId !== action.actionId
      )
        throw new WorkflowActionChangedError(
          `validation-trace ${action.actionId} is no longer the next action`,
        );
      const key = `${workflowId}\u0000${action.actionId}`;
      // A capture that never reached submit is replaced: submit sends only
      // the body whose hash the journaled preflight names.
      prepared.delete(key);
      const { transaction, durableRecovery } = await capture(context, parsed);
      if (transaction.referenceScripts.length === 0)
        throw new Error(
          `validation-trace ${parsed.stage} used no published reference script`,
        );
      requireReferenceOnlyScriptWitnesses({
        transaction,
        label: `validation-trace ${parsed.stage}`,
      });
      const preflight = bindWorkflowPreflightTransaction(
        {
          actionId: action.actionId,
          txHash: transaction.txHash,
          scriptExecution: "reference_scripts" as const,
          localUplcEvaluation: {
            status: "passed" as const,
            evaluator: LOCAL_UPLC_EVALUATOR,
          },
          referenceScripts: transaction.referenceScripts,
          durableRecovery,
        },
        transaction.signed,
      );
      prepared.set(key, transaction);
      return preflight;
    },
    submit: async ({ workflowId, action, preflight }) => {
      const key = `${workflowId}\u0000${action.actionId}`;
      const transaction = prepared.get(key);
      if (transaction === undefined || transaction.txHash !== preflight.txHash)
        throw new Error(
          `validation-trace ${action.actionId} has no captured body to submit`,
        );
      try {
        return {
          kind: "submitted",
          txHash: await submitCapturedTransaction(transaction),
        };
      } finally {
        prepared.delete(key);
      }
    },
    reconcile: async ({
      action,
      txHash,
      durableRecovery,
      signedTransactionCborHex,
      authorizeResubmission,
    }) => {
      if (txHash === undefined)
        return {
          kind: "conflict",
          reason: "validation-trace action omitted its transaction hash",
        };
      let outputs: ReturnType<typeof parseValidationTraceRecovery>;
      try {
        outputs = parseValidationTraceRecovery({
          value: durableRecovery,
          stage: parseValidationTraceAction(action).stage,
          txHash,
        });
      } catch (cause) {
        return { kind: "conflict", reason: String(cause) };
      }
      if (
        signedTransactionCborHex !== undefined &&
        signedRecovery !== undefined
      ) {
        const reconciled = await reconcileSignedWorkflowTransaction({
          transactionHash: txHash,
          signedTransactionCborHex,
          observe: signedRecovery.observeSignedTransaction,
          rebroadcast: signedRecovery.rebroadcastSignedTransaction,
          authorizeResubmission,
          reportInclusion: true,
        });
        return reconciled.kind === "pending" &&
          reconciled.retirement?.reason === "included"
          ? { kind: "confirmed", txHash }
          : reconciled;
      }
      // Without the signed bytes, any output of this exact transaction still
      // live with its claimed content shows it landed.
      const live = await read(outputs.map(({ outRef }) => outRef));
      return outputs.some((claim) =>
        isClaimedOutput(claim, live.get(claim.outRef)),
      )
        ? { kind: "confirmed", txHash }
        : { kind: "pending", txHash };
    },
  };
  return Object.freeze(removalStage.route(adapter));
};
