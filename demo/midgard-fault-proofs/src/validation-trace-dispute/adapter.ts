/** The validation-trace dispute workflow adapter: from a detected failing
 * Plutus execution it Inits a thread, Opens the challenger's claim,
 * VerifySources it into a case and its bond, closes the case nobody
 * defended into its fraud-proof record, and removes the convicted block
 * through the shared remove stage (removal.ts), all through the shared
 * workflow orchestrator. On a defended case it plays each thread (threads.ts,
 * moves.ts) through the one-step resolution the deployment installs
 * (one-step-moves.ts), closes each thread won into the case, slashes its
 * defender by the thread's receipt (lost-defense-slash.ts) and, once none is
 * open, closes the case as an undefended one. On a deployment that drives
 * CEK script selections, the decode record of every script our trace selects
 * is made before Init (script-decode-records.ts); a move reading a field by
 * reference reads our publications of it (carried-field.ts), and
 * a staged builtin step's operands are staged before it (one-step-staging.ts).
 * The staging sits in front of our thread's reclaim (own-thread-reclaim.ts).
 * Our own outputs are read from the ledger with our node's mempool applied
 * (own-view.chain.ts); that state decides the next action. A thread move needs the claim
 * material, which each run reads back as it prepares or validates its
 * artifact. */
import type { L1SlotConfig } from "@al-ft/midgard-core/l1-slot-config";
import type { LucidEvolution } from "@lucid-evolution/lucid";

import type { ResolvedProverSigner } from "../runtime.js";
import { WorkflowActionChangedError } from "../workflow/action-changed.js";
import {
  FRAUD_PROOF_WORKFLOW_ADAPTER,
  FRAUD_PROOF_WORKFLOW_SAFETY,
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowAdapterContext,
  type FraudProofWorkflowObservation,
} from "../workflow/orchestrator.fraud-proof-family-workflow-adapter.js";
import type { FraudProofAuthenticatedPublicationObserver } from "../workflow/raw-l1-publication-observation.js";
import {
  bindWorkflowPreflightTransaction,
  LOCAL_UPLC_EVALUATOR,
  type LocallyEvaluatedTransaction,
  requireReferenceOnlyScriptWitnesses,
  submitCapturedTransaction,
} from "../workflow/transaction-boundary.js";
import {
  parseValidationTraceAction,
  validationTraceAction,
} from "./actions.js";
import { validationTraceCarriedFields } from "./carried-field.js";
import type { ValidationTraceCaseHistory } from "./case-history.js";
import { validationTraceCaseOrigin } from "./case-origin.js";
import { validationTraceCaseReads } from "./case-reads.js";
import type { ValidationTraceClaimMaterial } from "./claim-material.js";
import { captureValidationTraceClose } from "./close.js";
import { prepareValidationTraceArtifact } from "./detected-claim.js";
import { captureValidationTraceLostDefenseSlash } from "./lost-defense-slash.js";
import {
  dueValidationTraceLostDefenseSlash,
  validationTraceLostDefenseSlashDeadlines,
} from "./lost-defense-slash-due.js";
import {
  captureValidationTraceThreadMove,
  isValidationTraceBuildPassedOn,
} from "./move-dispatch.js";
import { validationTraceStaging } from "./one-step-staging.js";
import { validationTraceChainOwnView } from "./own-view.chain.js";
import type {
  ValidationTraceOwnView,
  ValidationTraceOwnViewOf,
} from "./own-view.js";
import {
  createValidationTraceRemoval,
  VALIDATION_TRACE_CASE_CONVICTED,
  type ValidationTraceRemovalConfig,
} from "./removal.js";
import { withValidationTraceScriptDecodeRecords } from "./script-decode-records.js";
import {
  heldClaimBuildAlarm,
  validationTraceStageUnbuilt,
} from "./stage-unbuilt.js";
import { observeValidationTraceCase } from "./thread-observation.js";
import {
  type CapturedValidationTraceStage,
  captureValidationTraceInit,
  captureValidationTraceOpen,
  captureValidationTraceVerifySource,
  type ValidationTraceDisputeDeployment,
} from "./transactions.js";
import { validationTraceUnfunded } from "./unfunded-move.js";
import { validationTraceArtifactCheck } from "./validated-artifact.js";
const CATEGORY = "validationTraceDispute" as const;

import { VALIDATION_TRACE_DEFENDER_WON } from "./threads.js";
export {
  VALIDATION_TRACE_CASE_CONTESTED,
  VALIDATION_TRACE_DEFENDER_WON,
  VALIDATION_TRACE_MOVE_UNBUILT,
  VALIDATION_TRACE_ONE_STEP_REQUIRED,
  VALIDATION_TRACE_THREAD_AWAITING,
  VALIDATION_TRACE_THREAD_COUNT_HOLD,
  VALIDATION_TRACE_THREAD_DISCOVERY,
} from "./threads.js";
export { VALIDATION_TRACE_CASE_OPEN } from "./waiting.js";

export { VALIDATION_TRACE_CASE_CONVICTED };

export const createValidationTraceDisputeWorkflowAdapter = ({
  lucid,
  signer,
  deployment,
  l1SlotConfig,
  removal,
  releaseDepth,
  caseHistory,
  publications,
  ownThreadReclaim,
  now = Date.now,
  ownView: viewOf,
}: {
  readonly lucid: LucidEvolution;
  readonly signer: ResolvedProverSigner;
  readonly deployment: ValidationTraceDisputeDeployment;
  readonly l1SlotConfig: L1SlotConfig;
  /** The convicted block's removal through the shared remove stage. */
  readonly removal: ValidationTraceRemovalConfig;
  /** How deep the ledger holds a transaction of ours once it is final: at
   * least this many blocks, the tip one. */
  readonly releaseDepth: number;
  /** Our case token's complete L1 history, where its threads are found. */
  readonly caseHistory: ValidationTraceCaseHistory;
  /** Where a script-decode record stands; selection deployments need it. */
  readonly publications?: FraudProofAuthenticatedPublicationObserver;
  /** Our thread's reclaim before a resolution by another record, put in
   * front of the adapter and behind the staging, which sees what it does. */
  readonly ownThreadReclaim?: (
    adapter: FraudProofFamilyWorkflowAdapter,
  ) => FraudProofFamilyWorkflowAdapter;
  readonly now?: () => number;
  /** Where the family reads its own state; when omitted, the chain. */
  readonly ownView?: ValidationTraceOwnViewOf;
}): FraudProofFamilyWorkflowAdapter => {
  if (
    deployment.init.contracts.steps[0].spendingScriptHash !==
    deployment.chain.opener.spendingScriptHash
  )
    throw new Error("validation-trace Init must start at the dispute opener");
  const prepared = new Map<string, LocallyEvaluatedTransaction>();
  const claimHeld = heldClaimBuildAlarm({ lucid, deployment, now });
  // Each block's claim material, as its saved artifact was last validated.
  const materials = new Map<string, ValidationTraceClaimMaterial>();
  const artifactCheck = validationTraceArtifactCheck({
    l1SlotConfig,
    materials,
    held: claimHeld,
  });
  const materialOf = (headerHash: string) => materials.get(headerHash);
  const ownView =
    viewOf ??
    validationTraceChainOwnView({ lucid, signer, deployment, caseHistory });
  const own = { lucid, signer, deployment };
  const withHistory = { ...own, caseHistory };
  const removalStage = createValidationTraceRemoval({ ...own, removal });
  const deadlines = validationTraceLostDefenseSlashDeadlines(withHistory);
  const copy = validationTraceCarriedFields({
    ...withHistory,
    publications,
    releaseDepth,
    materialOf,
    ownView,
  });
  const { recordAt, ourCases, ourCase } = validationTraceCaseReads(withHistory);
  const staging = validationTraceStaging({
    ...withHistory,
    releaseDepth,
    materialOf,
    ownView,
  });
  const observe = async (
    context: FraudProofWorkflowAdapterContext,
  ): Promise<FraudProofWorkflowObservation> => {
    const view = ownView(context);
    const next = await observeNext(context, view);
    // A defender who lost a thread of ours is slashed by its receipt only
    // once nothing else is left: after the conviction's removal, or once a
    // defender's win ends the case for us. It never holds up a thread move,
    // the case close or the removal.
    const settled =
      next.kind === "completed" ||
      (next.kind === "pending" &&
        next.reason.startsWith(`${VALIDATION_TRACE_DEFENDER_WON}:`));
    if (!settled) return next;
    const slash = await dueValidationTraceLostDefenseSlash({
      ...own,
      headerHash: view.artifact().headerHash,
      deadlines,
      receipts: await view.receipts(),
      skipped: context.unbuiltActionIds,
      now: now(),
    });
    if (slash === undefined) return next;
    return slash.kind === "due"
      ? slash.observation
      : { kind: "pending", reason: slash.reason };
  };

  const observeNext = async (
    context: FraudProofWorkflowAdapterContext,
    view: ValidationTraceOwnView,
  ): Promise<FraudProofWorkflowObservation> => {
    const artifact = view.artifact();
    // Read before the removal's view: once it is complete, these play out.
    const live = await ourCases(view);
    const record = await view.record();
    const convicted = record && (await removalStage.convicted(context, record));
    if (convicted && (convicted.kind !== "completed" || live.length === 0))
      return convicted;
    const opener = await view.pendingClaim();
    if (opener !== undefined)
      return {
        kind: "action_required",
        action: validationTraceAction({
          stage: "verify_source",
          consumes: opener.outRef,
        }),
      };
    // One case of ours per block: a thread at the opener while a case of
    // ours stands, or once one stood, is not opened into a second.
    const opened = (await view.cases()).length > 0;
    const thread = opened ? undefined : await view.thread();
    if (thread !== undefined)
      return {
        kind: "action_required",
        action: validationTraceAction({
          stage: "open",
          consumes: thread.outRef,
        }),
      };
    // Each live case of ours, whoever verified it, the nearest deadline
    // first: the first with a step to take takes it, so no case of ours
    // waits on another's wait.
    let waiting: FraudProofWorkflowObservation | undefined;
    for (const found of live) {
      const observed = await observeValidationTraceCase({
        ...withHistory,
        headerHash: artifact.headerHash,
        caseUnit: found.caseUnit,
        caseOutRef: found.outRef,
        caseDatum: found.caseDatum,
        now: now(),
        material: materials.get(artifact.headerHash),
        skipped: context.unbuiltActionIds,
      });
      if (observed.kind !== "pending") return observed;
      waiting ??= observed;
    }
    if (waiting !== undefined) return waiting;
    // Our Open opened the case, whoever verified or closed it: the record
    // names the claim's prover, us.
    if (opened) {
      const recorded = await recordAt(artifact.headerHash);
      if (recorded !== undefined)
        return await removalStage.convicted(context, recorded);
    }
    const unseen = await view.unseen();
    if (unseen !== undefined)
      return {
        kind: "pending",
        reason: `validation-trace transaction ${unseen} is not yet visible to the provider`,
      };
    if (opened)
      return {
        kind: "conflict",
        reason: "validation-trace case is closed without this workflow's close",
      };
    return {
      kind: "action_required",
      action: validationTraceAction({
        stage: "init",
        consumes: (await view.replacedThread()) ?? null,
      }),
    };
  };

  const capture = async (
    context: FraudProofWorkflowAdapterContext,
    action: ReturnType<typeof parseValidationTraceAction>,
  ): Promise<CapturedValidationTraceStage> => {
    const view = ownView(context);
    const input = { ...own, artifact: view.artifact() };
    let found: Awaited<ReturnType<typeof ourCase>> = undefined;
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
        case "reveal":
        case "timeout":
        case "close_thread":
        case "enter_resolution":
        case "resolve":
        case "close_awarded":
          break;
        case "slash_lost_defense":
          return await captureValidationTraceLostDefenseSlash({
            ...own,
            removal,
            receiptOutRef: action.consumes!,
            deadlineMs: async () =>
              await deadlines.ofReceipt(
                input.artifact.headerHash,
                action.consumes!,
              ),
          });
      }
      found = await ourCase(view, action.consumes!);
      if (found === undefined)
        throw new WorkflowActionChangedError(
          `validation-trace ${action.stage} has no case of ours to play`,
        );
      const history = {
        caseHistory,
        headerHash: input.artifact.headerHash,
        caseUnit: found.caseUnit,
      };
      if (action.stage === "close")
        // The case as it stands now, and the bond its VerifySource left, by
        // its L1 history.
        return await captureValidationTraceClose({
          ...input,
          bondOutRef: validationTraceCaseOrigin({
            transactions: await caseHistory.transactions(history),
            caseUnit: found.caseUnit,
          }).bondOutRef,
          caseOutRef: found.outRef,
        });
      return await captureValidationTraceThreadMove({
        caseDatum: found.caseDatum,
        stage: action.stage,
        move: {
          ...input,
          threadOutRef: action.consumes!,
          caseUnit: found.caseUnit,
          material: materials.get(input.artifact.headerHash),
          fieldCarriage: async () => await copy.of(context, action),
          staging: staging.lookup(context),
        },
        caseOutRef: found.outRef,
        history,
      });
    } catch (cause) {
      if (isValidationTraceBuildPassedOn(cause)) throw cause;
      throw await validationTraceStageUnbuilt({
        action,
        lucid,
        deployment,
        headerHash: input.artifact.headerHash,
        found,
        cause,
        lostDefenseDeadlineMs: async (receiptOutRef) =>
          await deadlines.ofReceipt(input.artifact.headerHash, receiptOutRef),
      });
    }
  };

  const adapter: FraudProofFamilyWorkflowAdapter = {
    adapterVersion: FRAUD_PROOF_WORKFLOW_ADAPTER,
    category: CATEGORY,
    safety: FRAUD_PROOF_WORKFLOW_SAFETY,
    prepare: async ({ evidence, replayContext, classification }) =>
      await artifactCheck.prepare(
        { evidence, replayContext, classification },
        async () =>
          await prepareValidationTraceArtifact({
            evidence,
            context: replayContext,
            classification,
            l1SlotConfig,
            onMaterial: (material) => {
              artifactCheck.prepared(evidence.headerHash);
              materials.set(evidence.headerHash, material);
            },
          }).catch(claimHeld(evidence.headerHash)),
      ),
    validatePreparedArtifact: artifactCheck.check,
    observe,
    unfunded: validationTraceUnfunded({ lucid, deadlines, ourCase, ownView }),
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
      // the body whose hash the preflight names.
      prepared.delete(key);
      const { transaction } = await capture(context, parsed);
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
  };
  const routed = withValidationTraceScriptDecodeRecords({
    base: copy.wrap(Object.freeze(removalStage.route(adapter))),
    ...own,
    materialOf,
  });
  return staging.wrap(ownThreadReclaim?.(routed) ?? routed);
};
