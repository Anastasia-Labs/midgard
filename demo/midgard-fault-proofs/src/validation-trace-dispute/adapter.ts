/** The validation-trace dispute workflow adapter: from a detected failing
 * Plutus execution it Inits a thread, Opens the challenger's claim,
 * VerifySources it into a case and its bond, closes the case nobody
 * defended into its fraud-proof record, and removes the convicted block
 * through the shared remove stage (removal.ts), all through the shared
 * workflow orchestrator. Every output of ours is claimed by out-ref from our
 * own signed body before submission and read back by that out-ref; chain
 * state, not the journal's idea of the step, decides the next action. */
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
import { parseOwnOutRef, readOwnOutputs } from "../workflow/own-outputs.js";
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
import {
  captureValidationTraceClose,
  validationTraceFraudProofUnit,
} from "./close.js";
import {
  prepareValidationTraceArtifact,
  validateValidationTraceArtifact,
} from "./detected-claim.js";
import {
  createValidationTraceRemoval,
  VALIDATION_TRACE_CASE_CONVICTED,
  type ValidationTraceRemovalConfig,
} from "./removal.js";
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

export { VALIDATION_TRACE_CASE_OPEN } from "./waiting.js";

/**
 * A defender joined our case. The challenger's moves are not installed, so
 * the workflow raises this and waits; it never abandons the case.
 */
export const VALIDATION_TRACE_CASE_CONTESTED =
  "validation-trace-case-contested";

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

const isContested = (caseDatum: SDK.Case): boolean =>
  caseDatum.open_threads > 0n ||
  caseDatum.defender_won ||
  caseDatum.defenders_root !== SDK.EMPTY_MERKLE_TREE_ROOT;

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
  // The record a close of our case minted, whoever closed it: the policy
  // mints it once per block, and it names its prover.
  const recordAt = async (headerHash: string) => {
    const unit = validationTraceFraudProofUnit(deployment, headerHash);
    const [found] = (
      await lucid.utxosAtWithUnit(
        deployment.fraudProof.spendingScriptAddress,
        unit,
      )
    ).filter(
      (utxo) =>
        utxo.assets[unit] === 1n &&
        typeof utxo.datum === "string" &&
        Data.from(utxo.datum, SDK.FraudProofTokenDatum).fraud_prover ===
          signer.paymentKeyHash,
    );
    return found === undefined
      ? undefined
      : { unit, outRef: `${found.txHash}#${found.outputIndex.toString()}` };
  };
  const caseAt = async (caseUnit: string) => {
    const [found] = (
      await lucid.utxosAtWithUnit(
        deployment.chain.case.spendingScriptAddress,
        caseUnit,
      )
    ).filter((utxo) => utxo.assets[caseUnit] === 1n);
    return found;
  };

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
    // Our pending claim is gone: the case its out-ref names, by the case
    // policy's unique token, is ours whoever verified it.
    for (const { outputs } of newestFirst.filter(
      ({ action }) => action.stage === "open",
    )) {
      const caseUnit =
        deployment.chain.case.policyId +
        SDK.validationCaseId(
          artifact.headerHash,
          artifact.claim.claimWitness.event_to_step_membership.key,
          parseOwnOutRef(outputs[0]!.outRef),
        );
      const found = await caseAt(caseUnit);
      if (found === undefined || typeof found.datum !== "string") continue;
      const outRef = `${found.txHash}#${found.outputIndex.toString()}`;
      const caseDatum = Data.from(found.datum, SDK.Case);
      return isContested(caseDatum)
        ? {
            kind: "pending",
            reason: `${VALIDATION_TRACE_CASE_CONTESTED}: case ${caseUnit} at ${outRef} has a defender; the challenger's moves are not installed, so it waits`,
          }
        : {
            kind: "pending",
            reason: validationTraceCaseOpenReason({
              caseUnit,
              outRef,
              caseDatum,
            }),
          };
    }
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
      }
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
