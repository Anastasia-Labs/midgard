import { formatUnknownError } from "@al-ft/midgard-core";
import { isFinal } from "@al-ft/midgard-l1-follower/heads";
import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk/core";

import { VALIDATION_TRACE_STAGING_RECLAIM } from "../validation-trace-dispute/one-step-staging-journal.js";
import { WorkflowActionChangedError } from "./action-changed.js";
import { workflowActuationDecisionDigest } from "./actuation-permit.assert-workflow-actuation-permit-identity.js";
import type { WorkflowActuationCheckpoint } from "./actuation-permit.create-workflow-reconciliation-permit-controller.js";
import {
  assertWorkflowRunActuation,
  workflowRunIsReconciliationOnly,
} from "./actuation-permit.js";
import { type InFlightTransaction, WalletCoinHeldError } from "./in-flight.js";
import { inFlightAtAbsence } from "./in-flight-absence.js";
import {
  computeFraudProofWorkflowId,
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  type FraudProofWorkflowTerminal,
  type JournalJsonObject,
  normalizeFraudProofWorkflowIdentity,
  normalizeJournalJson,
} from "./journal.fraud-proof-workflow-terminal.js";
import { isCallerOwnedWorkflowFailure } from "./orchestrator.caller-owned-failure.js";
import {
  FRAUD_PROOF_WORKFLOW_TERMINAL_VERIFIER,
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowAction,
  type FraudProofWorkflowAdapterContext,
  type FraudProofWorkflowObservation,
  type FraudProofWorkflowPreflight,
  type FraudProofWorkflowRegistry,
  type FraudProofWorkflowSubmitResult,
  type FraudProofWorkflowTerminalVerifier,
} from "./orchestrator.fraud-proof-family-workflow-adapter.js";
import {
  FRAUD_PROOF_WORKFLOW_FRESH_SUBMISSION_REQUIRED,
  type FraudProofWorkflowRunResult,
} from "./orchestrator.fraud-proof-workflow-run-result.js";
import { normalizeTxHash } from "./orchestrator.immutable-fraud-proof-workflow-registry.js";
import {
  normalizeWorkflowTerminal,
  validateAction,
  validatePreflight,
} from "./orchestrator.normalize-workflow-terminal.js";
import { adapterWithMaterialsSweep } from "./orchestrator.proof-materials-sweep-adapter.js";
import { resolvedByAnotherRecordResult } from "./orchestrator.resolved-by-another-record.js";
import {
  createUnbuiltWorkflowActions,
  workflowRunOutcomes,
} from "./orchestrator.unbuilt-actions.js";
import { isProofMaterialsSweepAction } from "./proof-materials-sweep.action.js";
import { isFraudProofRawL1DerivationError } from "./raw-l1-family-derivation.state-queue-topology.js";
import type { VerifiedFraudProofReleaseFinalityPolicy } from "./release-finality-policy.js";
import {
  type FraudProofResolvedByAnotherRecord,
  isFraudProofResolvedByAnotherRecordError,
} from "./resolved-by-another-record.js";
import { isFinalAtReleaseDepth } from "./signed-transaction-reconciliation.js";
import {
  workflowPreflightTransaction,
  workflowTransactionCollateralInputOutRefs,
  workflowTransactionInputOutRefs,
} from "./transaction-boundary.js";
import {
  assertWalletOnlyValidityBound,
  faultProofRestartWaitNamedState,
  faultProofRestartWaitReason,
  spendsOnlyWalletCoins,
  type WalletOnlySend,
} from "./wallet-only-send-gate.js";
import { artifactPendingResult } from "./workflow-artifact-pending.js";
import {
  asWorkflowFundingShortfall,
  WorkflowFundingShortfallError,
  WorkflowFundingUnavailableError,
} from "./workflow-funding-shortfall.js";
import type { WorkflowRun } from "./workflow-run.js";
import {
  resubmitWorkflowTransaction,
  withRequiredWalletInputs,
} from "./workflow-wallet.js";

/**
 * The sends reconciliation-only authority admits, by kind: the proof-materials
 * sweep and the spend back of a validation-dispute staging output. Each only
 * returns outputs of ours to our wallet, and what it may select is still the
 * proof-materials view's and the holds' to decide.
 */
const isReconciliationReclaim = (action: FraudProofWorkflowAction): boolean =>
  isProofMaterialsSweepAction(action) ||
  action.input.schemaVersion === VALIDATION_TRACE_STAGING_RECLAIM;

/** Least wall-clock time between two broadcasts of the same signed bytes. */
const REBROADCAST_INTERVAL_MS = 30_000;

/**
 * Where a transaction this process sent stands, as the run acts on it: it
 * landed (final once past rollback recovery), it may still land, it can no
 * longer land and is settled, or the chain cannot say yet.
 */
type FollowedTransaction =
  | Readonly<{ kind: "landed"; final: boolean; depth: bigint }>
  | Readonly<{ kind: "holding" }>
  | Readonly<{ kind: "settled" }>
  | Readonly<{ kind: "unknown"; reason: string }>;

/**
 * Q51/W-O4 single-command core. Every step is derived from authenticated L1
 * observation. A transaction this process sent stays in its in-flight set
 * (`in-flight.ts`) until the chain shows it landed or can no longer land;
 * while one may still land, its action is not built again and no other
 * build of the process spends its inputs. A process that
 * has just started holds a transaction spending only wallet coins for one
 * validity window (`wallet-only-send-gate.ts`), and any other transaction
 * excludes a duplicate by the specific output it spends.
 */
export const runAdmittedFraudProofWorkflow = async ({
  deploymentFingerprint,
  category,
  headerHash,
  prepareFamilyArtifact,
  registry,
  run,
  terminalVerifier,
  releaseFinality,
  maxActions = 64,
  nowMs = Date.now,
  walletOnlySend,
}: {
  readonly deploymentFingerprint: string;
  readonly category: FraudProofCatalogueCategoryName;
  readonly headerHash: string;
  readonly prepareFamilyArtifact: (
    adapter: FraudProofFamilyWorkflowAdapter,
  ) => Promise<JournalJsonObject>;
  readonly registry: FraudProofWorkflowRegistry;
  /** This invocation of the objective: its in-flight transactions, and the
   * actuation permit bound to it. */
  readonly run: WorkflowRun;
  readonly terminalVerifier: FraudProofWorkflowTerminalVerifier;
  readonly releaseFinality: VerifiedFraudProofReleaseFinalityPolicy;
  readonly maxActions?: number;
  /** The wall clock the rebroadcast interval is measured on. */
  readonly nowMs?: () => number;
  /** This process's restart wait and latest wallet-only bound (`wallet-only-send-gate.ts`). */
  readonly walletOnlySend: WalletOnlySend;
}): Promise<FraudProofWorkflowRunResult> => {
  if (!Number.isSafeInteger(maxActions) || maxActions < 1) {
    throw new Error("maxActions must be a positive safe integer");
  }
  if (
    terminalVerifier.verifierVersion !==
      FRAUD_PROOF_WORKFLOW_TERMINAL_VERIFIER ||
    typeof terminalVerifier.observeSignedTransaction !== "function"
  ) {
    throw new Error("workflow requires the authenticated L1 terminal verifier");
  }
  if (releaseFinality.deploymentIdentityDigest !== deploymentFingerprint) {
    throw new Error(
      "release finality authority returned a different deployment identity",
    );
  }
  const { sends } = run;
  if (sends.category !== category || sends.headerHash !== headerHash)
    throw new Error("workflow in-flight set belongs to another objective");
  const adapter = adapterWithMaterialsSweep(registry, category, {
    sends,
    releaseFinality,
  });
  const assertActuation = (checkpoint: WorkflowActuationCheckpoint): void =>
    assertWorkflowRunActuation({
      run,
      deploymentFingerprint,
      category,
      headerHash,
      checkpoint,
    });
  assertActuation("workflow_resume");
  const reconciliationOnly = workflowRunIsReconciliationOnly(run);
  const decisionDigest = workflowActuationDecisionDigest(run);
  const identity = normalizeFraudProofWorkflowIdentity({
    schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
    deploymentFingerprint,
    category,
    target: { kind: "state_queue_header", headerHash },
    ...(decisionDigest === undefined ? {} : { decisionDigest }),
  });
  const workflowId = computeFraudProofWorkflowId(identity);
  // An action that does not build is skipped for the rest of the run, so a
  // lower-ranked move still goes; every result carries it as an alarm.
  const unbuilt = createUnbuiltWorkflowActions();
  const { stalled, pending, resumeOnObservation } = workflowRunOutcomes({
    workflowId,
    identity,
    unbuilt,
  });
  // A stage the family cannot derive from the admitted L1 view stalls this
  // run and is observed again on its next one. Every other failure reaches
  // the caller as before.
  const observeOrStall = async (
    input: FraudProofWorkflowAdapterContext,
  ): Promise<
    | { readonly observation: FraudProofWorkflowObservation }
    | { readonly stalled: FraudProofWorkflowRunResult }
    | { readonly resolved: FraudProofResolvedByAnotherRecord }
  > => {
    try {
      return { observation: await adapter.observe(input) };
    } catch (cause) {
      if (isFraudProofResolvedByAnotherRecordError(cause))
        return { resolved: cause.resolution };
      if (!isFraudProofRawL1DerivationError(cause)) throw cause;
      return {
        stalled: stalled(`observation failed: ${formatUnknownError(cause)}`),
      };
    }
  };

  /**
   * Asks the chain about one transaction of ours. One that landed, or can
   * never land, is settled and its coins released; when it spent only wallet
   * coins and could not land, the next build of the same action spends those
   * coins again, so a rollback that revived it could not land both. One that
   * is absent but may still land is broadcast again, at most once per
   * interval and never under reconciliation-only authority.
   */
  const follow = async (
    transaction: InFlightTransaction,
  ): Promise<FollowedTransaction> => {
    const observed = await terminalVerifier.observeSignedTransaction({
      transactionHash: transaction.txHash,
      signedTransactionCborHex: transaction.signedTransactionCborHex,
    });
    switch (observed.status) {
      case "included": {
        const point = observed.inclusionPoint;
        return {
          kind: "landed",
          final: isFinalAtReleaseDepth(observed),
          depth:
            point === undefined
              ? 0n
              : BigInt(observed.canonicalPoint.blockNo) - BigInt(point.blockNo),
        };
      }
      case "pending":
        return { kind: "holding" };
      case "rebroadcast": {
        const atMs = nowMs();
        if (
          !reconciliationOnly &&
          atMs - transaction.broadcastAtMs >= REBROADCAST_INTERVAL_MS
        ) {
          assertActuation("before_submit");
          sends.rebroadcast(transaction.txHash, atMs);
          try {
            await resubmitWorkflowTransaction(
              sends,
              transaction.signedTransactionCborHex,
            );
          } catch {
            // A failed broadcast leaves the same bytes in flight; the chain
            // says next time whether they landed.
          }
        }
        return { kind: "holding" };
      }
      case "expired":
      case "invalidated":
      case "expired_at_tip":
      case "invalidated_at_tip":
      case "landed_invalid":
        sends.settle(transaction.txHash);
        if (transaction.walletOnly)
          sends.require(transaction.actionId, transaction.walletInputs);
        return { kind: "settled" };
      case "conflict":
      case "unknown":
        return {
          kind: "unknown",
          reason: `transaction ${transaction.txHash} of ${transaction.actionId}: ${observed.status}: ${observed.reason}`,
        };
    }
  };

  /** Follows every transaction in flight, holding nothing: one absent is
   * broadcast again, one that cannot land settles, one landed final settles
   * as landed. A transaction the chain cannot place is followed next run. */
  const followWithoutHolding = async (): Promise<void> => {
    for (const transaction of sends.inFlight()) {
      let followed: FollowedTransaction;
      try {
        followed = await follow(transaction);
      } catch (cause) {
        if (isCallerOwnedWorkflowFailure(cause)) throw cause;
        continue;
      }
      if (followed.kind === "landed" && followed.final)
        sends.settle(transaction.txHash, true);
    }
  };

  /**
   * Follows every transaction this process sent for the objective while
   * `observation` stands. Undefined when none of them holds it; pending while
   * a transaction of the required action may still land, or while that
   * action's own transaction has landed and the observation has not caught
   * up. With no action required (an observation that waits, or a run about
   * to resolve) any transaction that may still land holds it. A transaction
   * of another action never holds a required one: it is followed (broadcast
   * again, settled once it lands or cannot) while the required action is
   * built over other coins, unless the requirement was read from finding no
   * output at an address such a transaction, or one of another objective,
   * may still land at (`in-flight-absence.ts`). A landed transaction of
   * another action stays followed until it is final, so a rollback that
   * undoes it is seen.
   *
   * An action this process saw land final and observed as required again is
   * held while that landing stands within the rollback recovery depth: a
   * read that lags the landing, or a rollback that includes it again, never
   * builds a second copy. Past that depth, or once the landing can no longer
   * stand, the action is built again: what it made is gone from the chain.
   */
  const followInFlight = async (
    observation: FraudProofWorkflowObservation | undefined,
  ): Promise<FraudProofWorkflowRunResult | undefined> => {
    const required =
      observation?.kind === "action_required"
        ? observation.action.actionId
        : undefined;
    let holding: InFlightTransaction | undefined;
    const mayLand: InFlightTransaction[] = [];
    for (const transaction of sends.inFlight()) {
      let followed: FollowedTransaction;
      try {
        followed = await follow(transaction);
      } catch (cause) {
        if (isCallerOwnedWorkflowFailure(cause)) throw cause;
        return resumeOnObservation(
          `transaction ${transaction.txHash} of ${transaction.actionId} awaits a stable observation: ${formatUnknownError(cause)}`,
        );
      }
      if (followed.kind === "unknown")
        return resumeOnObservation(followed.reason);
      if (followed.kind === "settled") continue;
      if (followed.kind === "landed") {
        if (followed.final) {
          sends.settle(transaction.txHash, true);
          continue;
        }
        if (transaction.actionId !== required) continue;
      }
      // One that may still land holds the run only while the observation
      // requires its own action again, or requires none. Another required
      // action is built at once: the in-flight set keeps this one's inputs
      // out of that build.
      if (required === undefined || transaction.actionId === required)
        holding ??= transaction;
      else if (followed.kind === "holding") mayLand.push(transaction);
    }
    if (holding !== undefined)
      return pending(
        `transaction for ${holding.actionId} is pending`,
        holding.actionId,
      );
    // Unless it was read from an address a transaction of ours may still
    // land at: the read may show that transaction's spend and not yet what
    // it makes. Another objective's transactions count too, held until they
    // settle in its own runs.
    if (observation?.kind === "action_required") {
      const others = sends.othersInFlight();
      const at = inFlightAtAbsence({
        absentAt: observation.absentAt ?? [],
        mayLand: [...mayLand, ...others],
        known: [...sends.inFlight(), ...others],
      });
      if (at !== undefined)
        return pending(
          `${observation.action.actionId} waits on transaction ${at.transaction.txHash} of ${at.transaction.actionId}, which may still land at ${at.address}, where it read no output`,
          at.transaction.actionId,
        );
    }
    const landed = required === undefined ? undefined : sends.landed(required);
    if (landed === undefined) return undefined;
    let again: FollowedTransaction;
    try {
      again = await follow(landed);
    } catch (cause) {
      if (isCallerOwnedWorkflowFailure(cause)) throw cause;
      return resumeOnObservation(
        `transaction ${landed.txHash} of ${landed.actionId} awaits a stable observation: ${formatUnknownError(cause)}`,
      );
    }
    if (again.kind === "unknown") return resumeOnObservation(again.reason);
    if (
      again.kind === "holding" ||
      (again.kind === "landed" &&
        !isFinal(Number(again.depth), {
          securityParameter: releaseFinality.policy.automaticRecoveryMaxDepth,
        }))
    )
      return pending(
        `transaction ${landed.txHash} of ${landed.actionId} landed and the observation does not show it yet`,
        landed.actionId,
      );
    return undefined;
  };

  const resolvedByAnotherRecord = async (
    resolution: FraudProofResolvedByAnotherRecord,
  ): Promise<FraudProofWorkflowRunResult> => {
    const waiting = await followInFlight(undefined);
    if (waiting?.kind === "stalled") return waiting;
    // The orchestrator is the guard: no run resolves while the adapter still
    // sees a thread of ours on the header, or cannot list them at all.
    return resolvedByAnotherRecordResult({
      resolution,
      threads:
        resolution.standingThread === undefined
          ? await adapter.observeOwnThreads?.({ headerHash })
          : { standing: resolution.standingThread },
      workflowId,
      identity,
      inFlight: sends.inFlight().length,
      releaseFinality,
      resumeOnObservation,
    });
  };

  let artifact: JournalJsonObject;
  try {
    artifact = normalizeJournalJson(
      await prepareFamilyArtifact(adapter),
      `${category} prepared artifact`,
    ) as JournalJsonObject;
  } catch (cause) {
    return artifactPendingResult(cause, resumeOnObservation);
  }

  for (let actionNumber = 0; actionNumber < maxActions; actionNumber += 1) {
    const context: FraudProofWorkflowAdapterContext = {
      reconciliationOnly,
      identity,
      workflowId,
      artifact,
      unbuiltActionIds: unbuilt.ids,
      inFlight: () => sends.inFlight(),
      attempts: (actionId) => sends.attempts(actionId),
      landed: (actionId) => sends.landed(actionId) !== undefined,
    };
    assertActuation("before_observe");
    const observed = await observeOrStall(context);
    if ("stalled" in observed) return observed.stalled;
    if ("resolved" in observed)
      return await resolvedByAnotherRecord(observed.resolved);
    const observation: FraudProofWorkflowObservation = observed.observation;
    if (observation.kind === "completed") {
      let terminal: FraudProofWorkflowTerminal;
      const inclusionOnly =
        observation.terminal.observedAt.confirmationDepth <=
        releaseFinality.policy.automaticRecoveryMaxDepth + 1;
      try {
        assertActuation("before_terminal_verify");
        const verified = await (
          inclusionOnly
            ? (terminalVerifier.verifyIncluded ?? terminalVerifier.verify)
            : terminalVerifier.verify
        )({
          identity,
          workflowId,
          releaseFinality,
          candidate: observation.terminal,
          artifact,
        });
        terminal = normalizeWorkflowTerminal({
          identity,
          terminal: verified,
          releaseFinality,
          inclusionOnly,
        });
        assertActuation("before_terminal_verify");
      } catch (cause) {
        if (isCallerOwnedWorkflowFailure(cause)) throw cause;
        return stalled(
          `terminal verification failed: ${formatUnknownError(cause)}`,
        );
      }
      // The ending waits on none of ours, but each is still followed: one a
      // rollback undid is broadcast again rather than left unsent behind it.
      await followWithoutHolding();
      if (!inclusionOnly)
        for (const transaction of sends.inFlight())
          sends.settle(transaction.txHash);
      return {
        kind: inclusionOnly ? "terminal_included" : "completed",
        workflowId,
        identity,
        terminal,
      };
    }
    if (observation.kind === "conflict") {
      return stalled(`chain conflict: ${observation.reason}`);
    }
    const waiting = await followInFlight(observation);
    if (waiting !== undefined) return waiting;
    const waited = unbuilt.waiting(observation);
    if (waited !== undefined)
      return resumeOnObservation(
        faultProofRestartWaitReason(waited),
        faultProofRestartWaitNamedState,
      );
    const held = unbuilt.held(observation);
    if (held !== undefined) return resumeOnObservation(held);
    if (observation.kind === "pending") return pending(observation.reason);
    // Under reconciliation-only authority a fresh submission waits for the
    // operator, save a reclaim of our own outputs (`isReconciliationReclaim`).
    if (reconciliationOnly && !isReconciliationReclaim(observation.action))
      return pending(FRAUD_PROOF_WORKFLOW_FRESH_SUBMISSION_REQUIRED);
    const beforePreflight = reconciliationOnly
      ? "before_reclaim_preflight"
      : "before_preflight";
    const beforeSubmit = reconciliationOnly
      ? "before_reclaim_submit"
      : "before_submit";
    const action = validateAction(observation.action);
    let preflight: FraudProofWorkflowPreflight;
    let walletOnly = false;
    let adapterPreflightFailed = false;
    try {
      assertActuation(beforePreflight);
      let captured: FraudProofWorkflowPreflight;
      try {
        captured = await withRequiredWalletInputs(
          sends,
          sends.requiredWalletInputs(action.actionId),
          async () => await adapter.preflight({ ...context, action }),
        );
      } catch (failure) {
        adapterPreflightFailed = true;
        throw await namedShortfall({
          adapter,
          context,
          action,
          cause: asWorkflowFundingShortfall(action.actionId, failure),
        });
      }
      preflight = validatePreflight({ action, preflight: captured });
      walletOnly = spendsOnlyWalletCoins({ sends, preflight });
      if (walletOnly)
        assertWalletOnlyValidityBound({
          actionId: action.actionId,
          preflight,
          latestValidToSlot: walletOnlySend.latestValidToSlot,
        });
    } catch (cause) {
      if (unbuilt.skip(cause)) continue;
      if (
        cause instanceof WorkflowActionChangedError ||
        cause instanceof WorkflowFundingUnavailableError
      )
        return resumeOnObservation(cause.message);
      if (isCallerOwnedWorkflowFailure(cause)) throw cause;
      return stalled(
        `preflight failed for ${action.actionId}: ${formatUnknownError(cause)}`,
        adapterPreflightFailed ? "preflight" : undefined,
      );
    }
    // A held action sends nothing, and the run goes on to the moves ranked
    // after it.
    if (walletOnly && !(await walletOnlySend.open())) {
      unbuilt.hold(action.actionId);
      continue;
    }
    const signed = workflowPreflightTransaction(preflight);
    if (signed === undefined)
      return stalled(
        `preflight of ${action.actionId} omitted its captured signed transaction`,
      );
    const inputs = [
      ...new Set([
        ...workflowTransactionInputOutRefs(signed),
        ...workflowTransactionCollateralInputOutRefs(signed),
      ]),
    ];
    const ttl = signed.toTransaction().body().ttl();
    assertActuation(beforeSubmit);
    try {
      sends.record({
        actionId: action.actionId,
        txHash: preflight.txHash,
        signedTransactionCborHex: signed.toCBOR(),
        inputs,
        walletInputs: inputs.filter((outRef) => sends.isWalletCoin(outRef)),
        walletOnly,
        ...(ttl === undefined ? {} : { validToSlot: Number(ttl) }),
        broadcastAtMs: nowMs(),
      });
    } catch (cause) {
      if (cause instanceof WalletCoinHeldError)
        return resumeOnObservation(
          `${action.actionId} spends a coin another action of this process holds: ${cause.message}`,
        );
      throw cause;
    }
    assertActuation(beforeSubmit);
    let submitted: FraudProofWorkflowSubmitResult;
    try {
      submitted = await adapter.submit({ ...context, action, preflight });
    } catch (cause) {
      submitted = {
        kind: "ambiguous",
        detail: `submit threw: ${formatUnknownError(cause)}`,
      };
    }
    // An ambiguous submission stays in flight like any other: the chain says
    // whether it landed.
    const reported =
      submitted.txHash === undefined
        ? preflight.txHash
        : normalizeTxHash(submitted.txHash, "submitted transaction hash");
    if (reported !== preflight.txHash)
      return stalled(
        `submission for ${action.actionId} reported ${reported}, but its preflight built ${preflight.txHash}`,
      );
  }
  return resumeOnObservation(
    `workflow processed ${maxActions.toString()} actions; objective remains active`,
  );
};

/**
 * A preflight that failed for want of wallet funds, under the action's own
 * alarm where its adapter names one; naming it never turns the pause into a
 * stall.
 */
const namedShortfall = async ({
  adapter,
  context,
  action,
  cause,
}: {
  readonly adapter: FraudProofFamilyWorkflowAdapter;
  readonly context: FraudProofWorkflowAdapterContext;
  readonly action: FraudProofWorkflowAction;
  readonly cause: unknown;
}): Promise<unknown> => {
  if (
    !(cause instanceof WorkflowFundingShortfallError) ||
    adapter.unfunded === undefined
  )
    return cause;
  try {
    return await adapter.unfunded({ ...context, action, shortfall: cause });
  } catch (naming) {
    if (
      naming instanceof WorkflowActionChangedError ||
      isCallerOwnedWorkflowFailure(naming)
    )
      throw naming;
    return cause;
  }
};
