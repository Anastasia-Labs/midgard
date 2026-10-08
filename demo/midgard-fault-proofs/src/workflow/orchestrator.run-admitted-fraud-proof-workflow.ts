import { formatUnknownError } from "@al-ft/midgard-core";
import { type FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";

import { WorkflowActionChangedError } from "./action-changed.js";
import {
  assertWorkflowJournalActuation,
  workflowActuationDecisionDigest,
  workflowJournalIsReconciliationOnly,
} from "./actuation-permit.js";
import {
  assertFinalWorkflowTerminalReceipt,
  isFinalWorkflowCompletion,
} from "./completion-finality.js";
import {
  assertWorkflowFundingAbandonmentHandoffJournal,
  assertWorkflowFundingCompletionHandoffJournal,
  assertWorkflowFundingReservationReadyToSubmit,
  beginWorkflowFundingReservationAction,
  confirmWorkflowFundingReservationTransaction,
  conflictWorkflowFundingReservationTransaction,
  prepareWorkflowFundingReservationTransaction,
  readWorkflowFundingRecovery,
  reconcileWorkflowFundingSubmissionHandoff,
  releaseIdleWorkflowFundingReservation,
  releaseWorkflowFundingReservation,
  type WorkflowFundingCompletionHandoff,
  WorkflowFundingReservationUnavailableError,
  type WorkflowFundingSubmissionHandoff,
  workflowJournalHasFundingReservation,
} from "./funding-reservation-permit.js";
import {
  computeFraudProofWorkflowId,
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
  type FraudProofWorkflowJournalEntry,
  type FraudProofWorkflowJournalEvent,
  type FraudProofWorkflowJournalStore,
  type FraudProofWorkflowTerminal,
  journalJsonDigest,
  type JournalJsonObject,
  normalizeFraudProofWorkflowIdentity,
  normalizeJournalJson,
  validateFraudProofWorkflowJournal,
} from "./journal.js";
import {
  FraudProofL1CheckpointChangedError,
  FraudProofL1UnavailableError,
} from "./l1-source.js";
import { adoptLandedSupersededAttempt } from "./orchestrator.adopt-landed-superseded-attempt.js";
import {
  FRAUD_PROOF_WORKFLOW_TERMINAL_VERIFIER,
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowAdapterContext,
  type FraudProofWorkflowObservation,
  type FraudProofWorkflowPreflight,
  type FraudProofWorkflowReconcileResult,
  type FraudProofWorkflowRegistry,
  type FraudProofWorkflowSubmitResult,
  type FraudProofWorkflowTerminalVerifier,
} from "./orchestrator.fraud-proof-family-workflow-adapter.js";
import {
  attemptCount,
  type FraudProofWorkflowRunResult,
  latestSubmissionIntent,
} from "./orchestrator.fraud-proof-workflow-run-result.js";
import {
  normalizeTxHash,
  persistedArtifact,
  requirePreparedArtifact,
  type WorkflowEvidenceBinding,
} from "./orchestrator.immutable-fraud-proof-workflow-registry.js";
import {
  lastActionEvent,
  lastKnownTxHash,
  normalizeWorkflowTerminal,
  validateAction,
  validatePreflight,
} from "./orchestrator.normalize-workflow-terminal.js";
import { assertWorkflowJournalReconciliation } from "./orchestrator.reconcile-legacy-abandonments.js";
import { reconcileLegacyFraudProofAbandonments } from "./orchestrator.reconcile-legacy-abandonments.js";
import { reobserveRequiredWorkflowParent } from "./orchestrator.reobserve-required-parent.js";
import { supersedeWorkflowFundingAttempt } from "./orchestrator.supersede-funding-attempt.js";
import { type VerifiedFraudProofReleaseFinalityPolicy } from "./release-finality-policy.js";
import { supersededAttemptReadSchedule } from "./superseded-attempt-read-schedule.js";

/**
 * Q51/W-O4 single-command core. Preparation, every submission intent,
 * ambiguous result, reconciliation, submitted hash, and confirmation are
 * durable. An unresolved submission is always reconciled against authenticated
 * L1 state before any retry.
 */
export const runAdmittedFraudProofWorkflow = async ({
  deploymentFingerprint,
  category,
  headerHash,
  evidenceBinding,
  prepareFamilyArtifact,
  validateFamilyArtifact,
  registry,
  journal,
  terminalVerifier,
  releaseFinality,
  maxSubmissionAttempts = 3,
  maxActions = 64,
  now = () => new Date(),
}: {
  readonly deploymentFingerprint: string;
  readonly category: FraudProofCatalogueCategoryName;
  readonly headerHash: string;
  readonly evidenceBinding: WorkflowEvidenceBinding;
  readonly prepareFamilyArtifact: (
    adapter: FraudProofFamilyWorkflowAdapter,
  ) => Promise<JournalJsonObject>;
  readonly validateFamilyArtifact?: (
    adapter: FraudProofFamilyWorkflowAdapter,
    artifact: JournalJsonObject,
  ) => Promise<void>;
  readonly registry: FraudProofWorkflowRegistry;
  readonly journal: FraudProofWorkflowJournalStore;
  readonly terminalVerifier: FraudProofWorkflowTerminalVerifier;
  readonly releaseFinality: VerifiedFraudProofReleaseFinalityPolicy;
  readonly maxSubmissionAttempts?: number;
  readonly maxActions?: number;
  readonly now?: () => Date;
}): Promise<FraudProofWorkflowRunResult> => {
  if (
    !Number.isSafeInteger(maxSubmissionAttempts) ||
    maxSubmissionAttempts < 1
  ) {
    throw new Error("maxSubmissionAttempts must be a positive safe integer");
  }
  if (!Number.isSafeInteger(maxActions) || maxActions < 1) {
    throw new Error("maxActions must be a positive safe integer");
  }
  if (
    terminalVerifier.verifierVersion !== FRAUD_PROOF_WORKFLOW_TERMINAL_VERIFIER
  ) {
    throw new Error("workflow requires the authenticated L1 terminal verifier");
  }
  if (releaseFinality.deploymentIdentityDigest !== deploymentFingerprint) {
    throw new Error(
      "release finality authority returned a different deployment identity",
    );
  }
  const adapter = registry.get(category);
  if (adapter === undefined) {
    throw new Error(`classified family ${category} has no workflow adapter`);
  }
  assertWorkflowJournalActuation({
    journal,
    deploymentFingerprint,
    category,
    headerHash,
    checkpoint: "workflow_resume",
  });
  const decisionDigest = workflowActuationDecisionDigest(journal);
  const identity = normalizeFraudProofWorkflowIdentity({
    schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
    deploymentFingerprint,
    category,
    target: { kind: "state_queue_header", headerHash },
    ...(decisionDigest === undefined ? {} : { decisionDigest }),
  });
  const workflowId = computeFraudProofWorkflowId(identity);
  let entries = [
    ...(await journal.load(workflowId)),
  ] as FraudProofWorkflowJournalEntry[];
  validateFraudProofWorkflowJournal({
    workflowId,
    entries,
    expectedIdentity: identity,
  });

  const append = async (event: FraudProofWorkflowJournalEvent) => {
    const entry: FraudProofWorkflowJournalEntry = {
      schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
      workflowId,
      identity,
      sequence: entries.length,
      recordedAt: now().toISOString(),
      event,
    };
    await journal.append(entry, entries.length);
    entries = [...entries, entry];
  };
  const assertReconcile = () =>
    assertWorkflowJournalReconciliation(journal, identity);
  const stalled = async (
    reason: string,
    phase?: "preflight",
  ): Promise<FraudProofWorkflowRunResult> => {
    await append({ kind: "stalled", reason });
    return {
      kind: "stalled",
      workflowId,
      identity,
      reason,
      entries,
      ...(phase === undefined ? {} : { phase }),
    };
  };
  const resumeOnObservation = (
    reason: string,
  ): FraudProofWorkflowRunResult => ({
    kind: "pending",
    resumeOnObservation: true,
    workflowId,
    identity,
    reason,
    entries,
  });

  if (entries.length === 0) {
    if (workflowJournalIsReconciliationOnly(journal))
      throw new Error(
        "reconciliation authority cannot create a workflow journal",
      );
    await append({ kind: "started" });
  }
  let envelope = requirePreparedArtifact({
    entries,
    evidenceBinding,
    releaseFinality,
  });
  if (envelope === undefined) {
    if (workflowJournalIsReconciliationOnly(journal))
      throw new Error(
        "reconciliation authority requires an existing prepared workflow",
      );
    const familyArtifact = normalizeJournalJson(
      await prepareFamilyArtifact(adapter),
      `${category} prepared artifact`,
    ) as JournalJsonObject;
    envelope = persistedArtifact({
      evidenceBinding,
      releaseFinality,
      familyArtifact,
    });
    await append({
      kind: "prepared",
      artifact: envelope,
      artifactDigest: journalJsonDigest(envelope),
    });
  } else if (
    validateFamilyArtifact !== undefined &&
    !workflowJournalIsReconciliationOnly(journal)
  ) {
    await validateFamilyArtifact(adapter, envelope.familyArtifact);
    assertWorkflowJournalActuation({
      journal,
      deploymentFingerprint,
      category,
      headerHash,
      checkpoint: "workflow_resume",
    });
  }

  let fundingRecovery = await readWorkflowFundingRecovery(journal);
  if (fundingRecovery.submissionHandoff !== null) {
    for (const event of reconcileWorkflowFundingSubmissionHandoff({
      handoff: fundingRecovery.submissionHandoff,
      entries,
    }))
      await append(event);
    validateFraudProofWorkflowJournal({
      workflowId,
      entries,
      expectedIdentity: identity,
    });
  }
  if (fundingRecovery.completionHandoff !== null)
    assertWorkflowFundingCompletionHandoffJournal({
      handoff: fundingRecovery.completionHandoff,
      entries,
    });

  // Limits bound work in this invocation, not the lifetime of the objective.
  // Journal attempt numbers remain monotonic across automatic continuations.
  const attemptsAtStart = entries;
  for (let actionNumber = 0; actionNumber < maxActions; actionNumber += 1) {
    fundingRecovery = await readWorkflowFundingRecovery(journal);
    if (fundingRecovery.abandonmentHandoff !== null)
      assertWorkflowFundingAbandonmentHandoffJournal({
        handoff: fundingRecovery.abandonmentHandoff,
        entries,
      });
    let context: FraudProofWorkflowAdapterContext = {
      reconciliationOnly: workflowJournalIsReconciliationOnly(journal),
      identity,
      workflowId,
      artifact: envelope.familyArtifact,
      entries,
    };

    const landed = await reconcileLegacyFraudProofAbandonments(
      context,
      journal,
      adapter,
      append,
      now().getTime(),
    );
    if (
      landed !== null &&
      (await adoptLandedSupersededAttempt({
        journal,
        entries: () => entries,
        landed,
        append,
      }))
    ) {
      supersededAttemptReadSchedule.forget(landed.transition.transactionHash);
      continue;
    }
    // Reconcile rolled-back parents before pending descendants. An action
    // with its own unresolved intent still reconciles before reobservation.
    const latestJournalEvent = [...entries]
      .reverse()
      .find(({ event }) => event.kind !== "stalled")?.event;
    const unresolvedEvent =
      fundingRecovery.abandonmentHandoff?.submissionIntent ??
      (latestJournalEvent?.kind === "submission_intent" ||
      latestJournalEvent?.kind === "reobserved" ||
      latestJournalEvent?.kind === "submission_ambiguous" ||
      latestJournalEvent?.kind === "submitted" ||
      latestJournalEvent?.kind === "rebroadcast_intent" ||
      (latestJournalEvent?.kind === "reconciled" &&
        latestJournalEvent.outcome === "pending")
        ? latestJournalEvent
        : undefined);
    let currentObservation: FraudProofWorkflowObservation | undefined;
    if (
      !isFinalWorkflowCompletion(latestJournalEvent) &&
      !isFinalWorkflowCompletion(
        fundingRecovery.completionHandoff?.completion,
      ) &&
      entries.some(({ event }) => event.kind === "confirmed")
    ) {
      assertWorkflowJournalActuation({
        journal,
        deploymentFingerprint,
        category,
        headerHash,
        checkpoint: "before_observe",
      });
      const current = await adapter.observe(context);
      currentObservation = current;
      if (
        current.kind === "action_required" &&
        unresolvedEvent?.actionId !== current.action.actionId &&
        (latestJournalEvent?.kind !== "reconciled" ||
          latestJournalEvent.outcome !== "confirmed")
      ) {
        const intent = latestSubmissionIntent(entries, current.action.actionId);
        const last = lastActionEvent(entries, current.action.actionId);
        if (
          intent !== undefined &&
          (last?.kind === "confirmed" ||
            (latestJournalEvent !== undefined &&
              "actionId" in latestJournalEvent &&
              latestJournalEvent.actionId !== current.action.actionId &&
              !(last?.kind === "reconciled" && last.outcome === "not_found")))
        ) {
          const reobservation = await reobserveRequiredWorkflowParent({
            adapter,
            context,
            headerHash,
            journal,
            intent,
            hasUnresolvedDescendant: unresolvedEvent !== undefined,
            // The reobserved intent displaces the pending later attempt. It is
            // superseded, never silently dropped: a dropped attempt would count
            // as live coverage, so a replacement could skip its inputs and both
            // could land, the second failing its script at inclusion.
            supersedeDisplacedAttempt: async () => {
              const displaced = await readWorkflowFundingRecovery(journal);
              const pending = displaced.transition?.transactionHash;
              if (pending !== undefined && pending !== intent.txHash)
                await supersedeWorkflowFundingAttempt({
                  journal,
                  assertReconcile,
                  entries: () => entries,
                  append,
                  transactionHash: pending,
                  retirement: undefined,
                  savedHandoff: displaced.abandonmentHandoff,
                });
            },
            append,
            stalled,
            resumeOnObservation,
          });
          if (reobservation.kind === "reobserved") continue;
          if (reobservation.kind !== "included") return reobservation;
        }
      }
    }

    // Reconciliation precedes family-state observation. Otherwise a tx that
    // reached L1 immediately could make `observe` report completion before its
    // submitted/confirmed journal records had been closed.
    // A diagnostic `stalled` entry does not resolve an in-flight network
    // action.  Resume from the latest lifecycle event so a crash or transient
    // reconciliation failure can never turn uncertainty into a fresh submit.
    if (
      latestJournalEvent?.kind === "reconciled" &&
      latestJournalEvent.outcome === "confirmed"
    ) {
      if (latestJournalEvent.txHash === undefined) {
        return await stalled(
          `confirmed reconciliation for ${latestJournalEvent.actionId} omitted its transaction hash`,
        );
      }
      await append({
        kind: "confirmed",
        actionId: latestJournalEvent.actionId,
        txHash: latestJournalEvent.txHash,
      });
      continue;
    }
    if (unresolvedEvent !== undefined && "actionId" in unresolvedEvent) {
      const intent =
        fundingRecovery.abandonmentHandoff?.submissionIntent ??
        latestSubmissionIntent(entries, unresolvedEvent.actionId);
      if (intent === undefined) {
        return await stalled(
          `unresolved submission ${unresolvedEvent.actionId} has no durable intent`,
        );
      }
      const action = validateAction({
        actionId: intent.actionId,
        input: intent.actionInput,
      });
      const priorTxHash = lastKnownTxHash(entries, action.actionId);
      let reconciled: FraudProofWorkflowReconcileResult;
      let rebroadcastAttempted = false;
      try {
        assertWorkflowJournalReconciliation(journal, identity);
        // A committed supersession is final for this attempt: completing its
        // acknowledgement never waits on a fresh read, and a late landing is
        // adopted separately.
        const reconcile: typeof adapter.reconcile =
          fundingRecovery.abandonmentHandoff !== null &&
          fundingRecovery.abandonmentHandoff.reconciliation.retirement ===
            undefined
            ? async () => ({ kind: "not_found" })
            : (input) => adapter.reconcile(input);
        reconciled = await reconcile({
          ...context,
          action,
          ...(priorTxHash === undefined ? {} : { txHash: priorTxHash }),
          ...(intent.durableRecovery === undefined
            ? {}
            : { durableRecovery: intent.durableRecovery }),
          ...(fundingRecovery.transition === null
            ? {}
            : {
                signedTransactionCborHex:
                  fundingRecovery.transition.signedTransactionCborHex,
                ...(fundingRecovery.abandonmentHandoff !== null ||
                workflowJournalIsReconciliationOnly(journal)
                  ? {}
                  : {
                      authorizeResubmission: async (transaction: {
                        transactionHash: string;
                        signedTransactionCborHex: string;
                      }) => {
                        const recorded =
                          await readWorkflowFundingRecovery(journal);
                        if (
                          recorded.transition === null ||
                          recorded.transition.transactionHash !==
                            intent.txHash ||
                          transaction.transactionHash !== intent.txHash ||
                          transaction.signedTransactionCborHex !==
                            recorded.transition.signedTransactionCborHex
                        )
                          throw new Error(
                            "rebroadcast changed the exact durable transaction intent",
                          );
                        const previousBroadcast = [...entries]
                          .reverse()
                          .find(
                            ({ event }) =>
                              (event.kind === "submission_intent" ||
                                event.kind === "rebroadcast_intent") &&
                              event.txHash === intent.txHash,
                          );
                        // Historical observation catch-up can wake this same
                        // objective repeatedly. Existing journal timestamps
                        // bound network retries; they never establish absence.
                        if (
                          previousBroadcast !== undefined &&
                          now().getTime() -
                            Date.parse(previousBroadcast.recordedAt) <
                            30_000
                        )
                          throw new Error(
                            "Recorded transaction rebroadcast is waiting for its retry interval",
                          );
                        const broadcasts =
                          1 +
                          entries.filter(
                            ({ event }) =>
                              event.kind === "rebroadcast_intent" &&
                              event.txHash === intent.txHash,
                          ).length;
                        await assertWorkflowFundingReservationReadyToSubmit({
                          journal,
                          transactionHash: intent.txHash,
                        });
                        assertWorkflowJournalActuation({
                          journal,
                          deploymentFingerprint,
                          category,
                          headerHash,
                          checkpoint: "before_submit",
                        });
                        await append({
                          kind: "rebroadcast_intent",
                          actionId: action.actionId,
                          txHash: intent.txHash,
                          attempt: broadcasts + 1,
                        });
                        rebroadcastAttempted = true;
                        // Durability is asynchronous; revoke checks must follow it too.
                        assertWorkflowJournalActuation({
                          journal,
                          deploymentFingerprint,
                          category,
                          headerHash,
                          checkpoint: "before_submit",
                        });
                      },
                    }),
              }),
        });
        assertWorkflowJournalReconciliation(journal, identity);
      } catch (cause) {
        // A capture exhausted its bounded retries because the canonical head
        // moved. It establishes neither inclusion nor replacement authority;
        // retain the exact signed intent and yield for a fresh observation.
        if (cause instanceof FraudProofL1CheckpointChangedError)
          return resumeOnObservation(
            `reconciliation awaits a stable boundary for ${action.actionId}: ${cause.message}`,
          );
        if (cause instanceof FraudProofL1UnavailableError) throw cause;
        return await stalled(
          `reconciliation failed for ${action.actionId}: ${formatUnknownError(cause)}`,
        );
      }
      if (
        fundingRecovery.abandonmentHandoff !== null &&
        reconciled.kind !== "not_found"
      ) {
        const reason = `abandoned transaction ${intent.txHash} no longer has authenticated replacement evidence: ${reconciled.kind}; exact outcome remains unresolved`;
        if (workflowJournalIsReconciliationOnly(journal))
          return { kind: "pending", workflowId, identity, reason, entries };
        return await stalled(reason);
      }
      if (reconciled.kind === "unknown") {
        const reason = `reconciliation remains unknown for ${action.actionId}: ${reconciled.reason}`;
        return resumeOnObservation(reason);
      }
      if (reconciled.kind === "conflict") {
        await conflictWorkflowFundingReservationTransaction({
          journal,
          transactionHash: priorTxHash ?? intent.txHash,
        });
        return await stalled(
          `reconciliation conflict for ${action.actionId}: ${reconciled.reason}`,
        );
      }
      if (reconciled.kind === "confirmed") {
        const txHash = normalizeTxHash(
          reconciled.txHash,
          "reconciled transaction hash",
        );
        if (priorTxHash !== undefined && txHash !== priorTxHash) {
          return await stalled(
            `reconciliation for ${action.actionId} returned ${txHash}, expected ${priorTxHash}`,
          );
        }
        await confirmWorkflowFundingReservationTransaction({
          journal,
          transactionHash: txHash,
        });
        await append({
          kind: "reconciled",
          actionId: action.actionId,
          outcome: "confirmed",
          txHash,
        });
        await append({ kind: "confirmed", actionId: action.actionId, txHash });
        continue;
      }
      if (reconciled.kind === "pending") {
        const txHash =
          reconciled.txHash === undefined
            ? priorTxHash
            : normalizeTxHash(reconciled.txHash, "pending transaction hash");
        if (
          priorTxHash !== undefined &&
          txHash !== undefined &&
          priorTxHash !== txHash
        ) {
          return await stalled(
            `pending reconciliation for ${action.actionId} changed transaction hash`,
          );
        }
        await append({
          kind: "reconciled",
          actionId: action.actionId,
          outcome: "pending",
          ...(txHash === undefined ? {} : { txHash }),
        });
        // A fresh admitted observation may retry these exact bytes. Do not
        // repeatedly rebroadcast them inside the runner's one-second poll loop.
        if (rebroadcastAttempted)
          return resumeOnObservation(
            `recorded transaction for ${action.actionId} awaits canonical reconciliation`,
          );
        return {
          kind: "pending",
          workflowId,
          identity,
          reason: `transaction for ${action.actionId} is pending`,
          entries,
        };
      }
      // Owner ruling (whichever lands wins): an attempt that expired or was
      // invalidated at the tip no longer holds the workflow or its funding.
      // The replacement must spend one of its funding inputs. Without a bound
      // funding reservation nothing enforces that, so absence without
      // retirement stays unresolved until retirement past k.
      if (
        reconciled.retirement === undefined &&
        !workflowJournalHasFundingReservation(journal)
      )
        return resumeOnObservation(
          `reconciliation remains unknown for ${action.actionId}: absent without retirement, and no funding reservation keeps a replacement exclusive`,
        );
      await supersedeWorkflowFundingAttempt({
        journal,
        assertReconcile,
        entries: () => entries,
        append,
        transactionHash: intent.txHash,
        retirement: reconciled.retirement,
        savedHandoff: fundingRecovery.abandonmentHandoff,
      });
      fundingRecovery = await readWorkflowFundingRecovery(journal);
      context = { ...context, entries };
      currentObservation = undefined;
    }

    await releaseIdleWorkflowFundingReservation({ journal, workflowId });

    assertWorkflowJournalActuation({
      journal,
      deploymentFingerprint,
      category,
      headerHash,
      checkpoint: "before_observe",
    });
    const observation: FraudProofWorkflowObservation =
      !isFinalWorkflowCompletion(fundingRecovery.completionHandoff?.completion)
        ? (currentObservation ??
          (await adapter.observe({
            ...context,
            reconciliationOnly: workflowJournalIsReconciliationOnly(journal),
          })))
        : {
            kind: "completed",
            terminal: fundingRecovery.completionHandoff.completion.terminal,
          };
    if (observation.kind === "pending") {
      return {
        kind: "pending",
        workflowId,
        identity,
        reason: observation.reason,
        entries,
      };
    }
    if (observation.kind === "completed") {
      let terminal: FraudProofWorkflowTerminal;
      const inclusionOnly =
        observation.terminal.observedAt.confirmationDepth <=
        releaseFinality.policy.automaticRecoveryMaxDepth + 1;
      try {
        assertWorkflowJournalActuation({
          journal,
          deploymentFingerprint,
          category,
          headerHash,
          checkpoint: "before_terminal_verify",
        });
        terminal = normalizeWorkflowTerminal({
          identity,
          terminal: await (
            inclusionOnly
              ? (terminalVerifier.verifyIncluded ?? terminalVerifier.verify)
              : terminalVerifier.verify
          )({
            identity,
            workflowId,
            releaseFinality,
            candidate: observation.terminal,
            artifact: envelope.familyArtifact,
            entries,
          }),
          entries,
          releaseFinality,
          inclusionOnly,
        });
        if (
          isFinalWorkflowCompletion(
            fundingRecovery.completionHandoff?.completion,
          )
        ) {
          const saved = fundingRecovery.completionHandoff.completion.terminal;
          assertFinalWorkflowTerminalReceipt(saved, terminal);
          terminal = normalizeWorkflowTerminal({
            identity,
            terminal: saved,
            entries,
            releaseFinality,
          });
        }
        assertWorkflowJournalActuation({
          journal,
          deploymentFingerprint,
          category,
          headerHash,
          checkpoint: "before_terminal_verify",
        });
      } catch (cause) {
        if (cause instanceof FraudProofL1UnavailableError) throw cause;
        return await stalled(
          `terminal verification failed: ${formatUnknownError(cause)}`,
        );
      }
      const terminalDigest = journalJsonDigest(
        normalizeJournalJson(terminal, "workflow terminal"),
      );
      const kind = inclusionOnly ? "terminal_included" : "completed";
      const savedInclusion = fundingRecovery.completionHandoff;
      if (
        inclusionOnly &&
        savedInclusion?.completion.kind === "terminal_included" &&
        entries.length > savedInclusion.expectedJournalSequence
      ) {
        // Its signed actions and original inclusion handoff already survive a
        // restart. Re-observe next time rather than persisting every depth tick.
        return { kind, workflowId, identity, terminal, entries };
      }
      if (!isFinalWorkflowCompletion(latestJournalEvent)) {
        const handoff: WorkflowFundingCompletionHandoff = (fundingRecovery
          .completionHandoff?.completion.kind === kind
          ? fundingRecovery.completionHandoff
          : null) ?? {
          workflowId,
          identity,
          preparedArtifactDigest: journalJsonDigest(envelope),
          expectedJournalSequence: entries.length,
          completion: { kind, terminal, terminalDigest },
        };
        await releaseWorkflowFundingReservation({ journal, handoff });
        assertWorkflowJournalActuation({
          journal,
          deploymentFingerprint,
          category,
          headerHash,
          checkpoint: "before_terminal_verify",
        });
        await append({ kind, terminal, terminalDigest });
      }
      return {
        kind,
        workflowId,
        identity,
        terminal,
        entries,
      };
    }
    if (observation.kind === "conflict") {
      return await stalled(`chain conflict: ${observation.reason}`);
    }
    if (workflowJournalIsReconciliationOnly(journal))
      return {
        kind: "pending",
        workflowId,
        identity,
        entries,
        reason: "Canonical workflow requires fresh submission authority",
      };
    const action = validateAction(observation.action);
    const latest = lastActionEvent(entries, action.actionId);
    if (latest?.kind === "confirmed") {
      return await stalled(
        `confirmed action ${action.actionId} is still reported as required`,
      );
    }

    const priorAttempts = attemptCount(entries, action.actionId);
    if (
      priorAttempts - attemptCount(attemptsAtStart, action.actionId) >=
      maxSubmissionAttempts
    ) {
      return resumeOnObservation(
        `submission batch exhausted for ${action.actionId}; objective remains active`,
      );
    }
    let preflight: FraudProofWorkflowPreflight;
    let adapterPreflightFailed = false;
    try {
      assertWorkflowJournalActuation({
        journal,
        deploymentFingerprint,
        category,
        headerHash,
        checkpoint: "before_preflight",
      });
      await beginWorkflowFundingReservationAction({
        journal,
        action,
      });
      assertWorkflowJournalActuation({
        journal,
        deploymentFingerprint,
        category,
        headerHash,
        checkpoint: "before_preflight",
      });
      let captured: Awaited<ReturnType<typeof adapter.preflight>>;
      try {
        captured = await adapter.preflight({ ...context, action });
      } catch (cause) {
        adapterPreflightFailed = true;
        throw cause;
      }
      preflight = validatePreflight({ action, preflight: captured });
    } catch (cause) {
      if (
        cause instanceof WorkflowActionChangedError ||
        cause instanceof WorkflowFundingReservationUnavailableError
      )
        return resumeOnObservation(cause.message);
      if (cause instanceof FraudProofL1UnavailableError) throw cause;
      return await stalled(
        `preflight failed for ${action.actionId}: ${formatUnknownError(cause)}`,
        adapterPreflightFailed ? "preflight" : undefined,
      );
    }
    const preflightEvent: WorkflowFundingSubmissionHandoff["preflight"] = {
      kind: "preflight_passed",
      actionId: action.actionId,
      txHash: preflight.txHash,
      localEvaluator: preflight.localUplcEvaluation.evaluator,
      referenceScripts: preflight.referenceScripts,
    };
    const attempt = priorAttempts + 1;
    const submissionIntent: WorkflowFundingSubmissionHandoff["submissionIntent"] =
      {
        kind: "submission_intent",
        actionId: action.actionId,
        actionInput: action.input,
        ...(preflight.durableRecovery === undefined
          ? {}
          : { durableRecovery: preflight.durableRecovery }),
        attempt,
        txHash: preflight.txHash,
      };
    try {
      await prepareWorkflowFundingReservationTransaction({
        journal,
        action,
        preflight,
        handoff: {
          workflowId,
          identity,
          preparedArtifactDigest: journalJsonDigest(envelope),
          expectedJournalSequence: entries.length,
          preflight: preflightEvent,
          submissionIntent,
        },
      });
    } catch (cause) {
      if (cause instanceof WorkflowFundingReservationUnavailableError)
        return resumeOnObservation(cause.message);
      if (cause instanceof FraudProofL1UnavailableError) throw cause;
      return await stalled(
        `funding reservation failed for ${action.actionId}: ${formatUnknownError(cause)}`,
      );
    }
    await append(preflightEvent);
    await append(submissionIntent);
    let submitted: FraudProofWorkflowSubmitResult;
    assertWorkflowJournalActuation({
      journal,
      deploymentFingerprint,
      category,
      headerHash,
      checkpoint: "before_submit",
    });
    try {
      await assertWorkflowFundingReservationReadyToSubmit({
        journal,
        transactionHash: preflight.txHash,
      });
    } catch (cause) {
      if (cause instanceof WorkflowFundingReservationUnavailableError)
        return resumeOnObservation(
          `signed transaction ${preflight.txHash} requires canonical reconciliation: ${cause.message}`,
        );
      throw cause;
    }
    assertWorkflowJournalActuation({
      journal,
      deploymentFingerprint,
      category,
      headerHash,
      checkpoint: "before_submit",
    });
    try {
      submitted = await adapter.submit({
        ...context,
        entries,
        action,
        preflight,
      });
    } catch (cause) {
      submitted = {
        kind: "ambiguous",
        detail: `submit threw after durable intent: ${formatUnknownError(cause)}`,
      };
    }
    if (submitted.kind === "submitted") {
      const submittedTxHash = normalizeTxHash(
        submitted.txHash,
        "submitted transaction hash",
      );
      if (submittedTxHash !== preflight.txHash) {
        return await stalled(
          `submission for ${action.actionId} returned ${submittedTxHash}, but durable intent permits only ${preflight.txHash}`,
        );
      }
      await append({
        kind: "submitted",
        actionId: action.actionId,
        attempt,
        txHash: submittedTxHash,
      });
    } else {
      const ambiguousTxHash =
        submitted.txHash === undefined
          ? preflight.txHash
          : normalizeTxHash(submitted.txHash, "ambiguous transaction hash");
      if (ambiguousTxHash !== preflight.txHash) {
        return await stalled(
          `ambiguous submission for ${action.actionId} reported ${ambiguousTxHash}, but durable intent permits only ${preflight.txHash}`,
        );
      }
      await append({
        kind: "submission_ambiguous",
        actionId: action.actionId,
        attempt,
        txHash: ambiguousTxHash,
        detail: submitted.detail,
      });
    }
    // The next iteration sees an unresolved action and must reconcile before
    // it can create another submission intent.
  }
  return resumeOnObservation(
    `workflow processed ${maxActions.toString()} actions; objective remains active`,
  );
};
