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
 * is made before Init (script-decode-records.ts); an observe move of a field
 * too long to carry inline reads our published copy (observe-copy.ts), and
 * a staged builtin step's operands are staged before it (one-step-staging.ts).
 * The staging sits in front of our thread's reclaim (own-thread-reclaim.ts).
 * Every output of ours is claimed by out-ref from our own signed body before
 * submission and read back by that out-ref; chain state, not the journal's
 * idea of the step, decides the next action. A thread move needs the claim
 * material, which each run reads back as it prepares or validates its
 * artifact. */
import type { L1SlotConfig } from "@al-ft/midgard-core/l1-slot-config";
import type { LucidEvolution } from "@lucid-evolution/lucid";

import type { ResolvedProverSigner } from "../runtime.js";
import { WorkflowActionChangedError } from "../workflow/action-changed.js";
import type { FraudProofSignedTransactionRecovery } from "../workflow/l1-source.js";
import type { FraudProofWorkflowAdapterContext } from "../workflow/orchestrator.fraud-proof-family-workflow-adapter.js";
import {
  FRAUD_PROOF_WORKFLOW_ADAPTER,
  FRAUD_PROOF_WORKFLOW_SAFETY,
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowObservation,
} from "../workflow/orchestrator.js";
import { readOwnOutputs } from "../workflow/own-outputs.js";
import type { FraudProofAuthenticatedPublicationObserver } from "../workflow/raw-l1-publication-observation.js";
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
  validationTraceAction,
} from "./actions.js";
import { validationTraceArtifactOf } from "./artifact.js";
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
import { validationTraceObserveCopy } from "./observe-copy.js";
import { validationTraceStaging } from "./one-step-staging.js";
import { validationTraceReconcile } from "./reconcile.js";
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
import { observeValidationTraceThreads } from "./thread-observation.js";
import {
  type CapturedValidationTraceStage,
  captureValidationTraceInit,
  captureValidationTraceOpen,
  captureValidationTraceVerifySource,
  type ValidationTraceDisputeDeployment,
} from "./transactions.js";
import { validationTraceArtifactCheck } from "./validated-artifact.js";
import {
  validationTraceCaseOpenReason,
  validationTraceDefenseDeadlineMs,
} from "./waiting.js";

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

/**
 * How long an output of ours the provider does not show after its
 * transaction was confirmed is taken for provider lag rather than spent.
 */
export const VALIDATION_TRACE_PROVIDER_GRACE_MS = 10 * 60_000;

export const createValidationTraceDisputeWorkflowAdapter = ({
  lucid,
  signer,
  deployment,
  l1SlotConfig,
  removal,
  signedRecovery,
  caseHistory,
  publications,
  ownThreadReclaim,
  now = Date.now,
}: {
  readonly lucid: LucidEvolution;
  readonly signer: ResolvedProverSigner;
  readonly deployment: ValidationTraceDisputeDeployment;
  readonly l1SlotConfig: L1SlotConfig;
  /** The convicted block's removal through the shared remove stage. */
  readonly removal: ValidationTraceRemovalConfig;
  /** Where one exact signed transaction stands, and its rebroadcast. */
  readonly signedRecovery: FraudProofSignedTransactionRecovery;
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
  const own = { lucid, signer, deployment };
  const removalStage = createValidationTraceRemoval({ ...own, removal });
  const deadlines = validationTraceLostDefenseSlashDeadlines({
    ...own,
    caseHistory,
  });
  const copy = validationTraceObserveCopy({
    ...own,
    publications,
    signedRecovery,
    materialOf,
  });
  const recent = (confirmedAt: string): boolean =>
    now() - Date.parse(confirmedAt) < VALIDATION_TRACE_PROVIDER_GRACE_MS;
  const staging = validationTraceStaging({
    ...own,
    signedRecovery,
    materialOf,
    recent,
  });
  const read = (outRefs: readonly string[]) =>
    readOwnOutputs((refs) => lucid.utxosByOutRef([...refs]), outRefs);
  const { recordAt, ourCase, isContested } = validationTraceCaseReads(own);

  const observe = async (
    context: FraudProofWorkflowAdapterContext,
  ): Promise<FraudProofWorkflowObservation> => {
    const stages = confirmedValidationTraceStages(context.entries);
    const live = await read(
      stages.flatMap(({ outputs }) => outputs.map(({ outRef }) => outRef)),
    );
    const next = await observeNext(context, stages, live);
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
      headerHash: validationTraceArtifactOf(context).headerHash,
      deadlines,
      stages,
      live,
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
    stages: ReturnType<typeof confirmedValidationTraceStages>,
    live: Awaited<ReturnType<typeof read>>,
  ): Promise<FraudProofWorkflowObservation> => {
    const artifact = validationTraceArtifactOf(context);
    const newestFirst = [...stages].reverse();
    const liveClaim = (stage: (typeof stages)[number]["action"]["stage"]) =>
      newestFirst
        .filter((confirmed) => confirmed.action.stage === stage)
        .map(({ outputs }) => outputs[0]!)
        .find((claim) => isClaimedOutput(claim, live.get(claim.outRef)));
    const record = liveClaim("close");
    if (record !== undefined)
      return await removalStage.convicted(context, record);
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
    // The case our Open opened, whoever verified it.
    const found = await ourCase(artifact, stages);
    if (found !== undefined)
      return isContested(found.caseDatum)
        ? await observeValidationTraceThreads({
            ...own,
            caseHistory,
            headerHash: artifact.headerHash,
            caseUnit: found.caseUnit,
            caseOutRef: found.outRef,
            caseDatum: found.caseDatum,
            now: now(),
            material: materials.get(artifact.headerHash),
            skipped: context.unbuiltActionIds,
          })
        : // Nobody has defended it: it closes once its deadline has passed.
          now() > validationTraceDefenseDeadlineMs(found.caseDatum)
          ? {
              kind: "action_required",
              action: validationTraceAction({
                stage: "close",
                consumes: found.outRef,
              }),
            }
          : {
              kind: "pending",
              reason: validationTraceCaseOpenReason(found),
            };
    // Our Open opened the case, whoever verified or closed it: the record
    // names the claim's prover, us.
    const opened = newestFirst.some(({ action }) => action.stage === "open");
    if (opened) {
      const recorded = await recordAt(artifact.headerHash);
      if (recorded !== undefined)
        return await removalStage.convicted(context, recorded);
    }
    const newest = newestFirst[0];
    if (newest !== undefined && recent(newest.confirmedAt))
      return {
        kind: "pending",
        reason: `validation-trace transaction ${newest.txHash} is not yet visible to the provider`,
      };
    if (opened)
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

  const capture = async (
    context: FraudProofWorkflowAdapterContext,
    action: ReturnType<typeof parseValidationTraceAction>,
  ): Promise<CapturedValidationTraceStage> => {
    const input = { ...own, artifact: validationTraceArtifactOf(context) };
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
            headerHash: input.artifact.headerHash,
            receiptOutRef: action.consumes!,
            deadlineMs: async () =>
              await deadlines.ofReceipt(
                input.artifact.headerHash,
                action.consumes!,
              ),
          });
      }
      found = await ourCase(
        input.artifact,
        confirmedValidationTraceStages(context.entries),
      );
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
          caseOpenerOutRef: found.openerOutRef,
        });
      return await captureValidationTraceThreadMove({
        caseDatum: found.caseDatum,
        stage: action.stage,
        move: {
          ...input,
          threadOutRef: action.consumes!,
          caseUnit: found.caseUnit,
          caseOpenerOutRef: found.openerOutRef,
          material: materials.get(input.artifact.headerHash),
          observeCopy: async () => await copy.of(context, action),
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
    validatePreparedArtifact: artifactCheck.check,
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
    reconcile: validationTraceReconcile({ signedRecovery, read }),
  };
  const routed = withValidationTraceScriptDecodeRecords({
    base: copy.wrap(Object.freeze(removalStage.route(adapter))),
    ...own,
    publications,
    now,
    materialOf,
  });
  return staging.wrap(ownThreadReclaim?.(routed) ?? routed);
};
