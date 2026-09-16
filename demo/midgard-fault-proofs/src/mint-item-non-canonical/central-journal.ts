import { createHash } from "node:crypto";

import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";

import { assertWorkflowJournalActuation } from "../workflow/actuation-permit.js";
import {
  abandonWorkflowFundingReservationTransaction,
  assertWorkflowFundingCompletionHandoffJournal,
  assertWorkflowFundingReservationReadyToSubmit,
  beginWorkflowFundingReservationAction,
  confirmWorkflowFundingReservationTransaction,
  conflictWorkflowFundingReservationTransaction,
  createWorkflowFundingAbandonmentHandoff,
  createWorkflowFundingSubmissionHandoff,
  prepareWorkflowFundingReservationTransaction,
  readWorkflowFundingRecovery,
  releaseWorkflowFundingReservation,
  type WorkflowFundingCompletionHandoff,
} from "../workflow/funding-reservation-permit.js";
import {
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
  type FraudProofWorkflowIdentity,
  type FraudProofWorkflowJournalEntry,
  type FraudProofWorkflowJournalStore,
  type FraudProofWorkflowTerminal,
  journalJsonDigest,
  normalizeJournalJson,
  validateFraudProofWorkflowJournal,
} from "../workflow/journal.js";
import {
  type FraudProofWorkflowAction,
  type FraudProofWorkflowTerminalVerifier,
  normalizeWorkflowTerminal,
} from "../workflow/orchestrator.js";
import {
  validateVerifiedFraudProofReleaseFinalityPolicy,
  type VerifiedFraudProofReleaseFinalityPolicy,
} from "../workflow/release-finality-policy.js";
import {
  bindWorkflowPreflightTransaction,
  type FraudProofPreSubmitBoundary,
  LOCAL_UPLC_EVALUATOR,
  workflowTransactionInputOutRefs,
  workflowTransactionReferenceInputOutRefs,
} from "../workflow/transaction-boundary.js";
import type {
  MintItemAction,
  MintItemJournal,
  MintItemJournalEntry,
  MintItemStage,
} from "./mint-item-non-canonical.js";

const familyWorkflowId = (identity: FraudProofWorkflowIdentity): string => {
  if (!/^[0-9a-f]{64}$/u.test(identity.deploymentFingerprint))
    throw new Error("mintItemNonCanonical deployment fingerprint is invalid");
  if (
    identity.target.kind !== "state_queue_header" ||
    !/^[0-9a-f]{56}$/u.test(identity.target.headerHash)
  )
    throw new Error("mintItemNonCanonical target header hash is invalid");
  return createHash("sha256")
    .update(
      [
        FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
        identity.deploymentFingerprint,
        "mintItemNonCanonical",
        `header:${identity.target.headerHash}`,
        ...(identity.decisionDigest === undefined
          ? []
          : [`decision:${identity.decisionDigest}`]),
      ].join("\u0000"),
    )
    .digest("hex");
};

type SubmitAction = Exclude<MintItemAction, "done">;
export type MintItemRemovalAction = Readonly<{
  nextRemovalOutRef: string;
  fraudProofOutRef: string;
}>;
type DurableRecovery = Readonly<{
  familyIdentity: string;
  sourceStage: MintItemStage;
  targetStage: MintItemStage;
  auxiliary?: boolean;
}>;

const TX_HASH = /^[0-9a-f]{64}$/u;
const stages: readonly MintItemStage[] = [
  "none",
  "step01",
  "step02",
  "step03",
  "step04",
  "proven",
  "removed",
  "cancelled",
];
const actionId = (action: SubmitAction): string =>
  `mintItemNonCanonical:${action}`;
const now = (): string => new Date().toISOString();

const recoveryFrom = (
  entry: FraudProofWorkflowJournalEntry,
): DurableRecovery => {
  if (entry.event.kind !== "submission_intent") {
    throw new Error("mintItemNonCanonical journal entry is not an intent");
  }
  const value = entry.event.durableRecovery;
  const familyIdentity = value?.familyIdentity;
  const sourceStage = value?.sourceStage;
  const targetStage = value?.targetStage;
  if (
    typeof familyIdentity !== "string" ||
    typeof sourceStage !== "string" ||
    typeof targetStage !== "string" ||
    !stages.includes(sourceStage as MintItemStage) ||
    !stages.includes(targetStage as MintItemStage)
  ) {
    throw new Error("mintItemNonCanonical durable intent is incomplete");
  }
  return {
    familyIdentity,
    sourceStage: sourceStage as MintItemStage,
    targetStage: targetStage as MintItemStage,
    ...(value?.auxiliary === true ? { auxiliary: true } : {}),
  };
};

const actionFinishedAfter = (
  entries: readonly FraudProofWorkflowJournalEntry[],
  sequence: number,
  wantedActionId: string,
): boolean =>
  entries.slice(sequence + 1).some(({ event }) => {
    if (!("actionId" in event) || event.actionId !== wantedActionId) {
      return false;
    }
    return (
      event.kind === "confirmed" ||
      (event.kind === "reconciled" && event.outcome === "not_found")
    );
  });

const unresolvedIntent = (
  entries: readonly FraudProofWorkflowJournalEntry[],
): FraudProofWorkflowJournalEntry | undefined =>
  [...entries].reverse().find((entry) => {
    const event = entry.event;
    return (
      event.kind === "submission_intent" &&
      !actionFinishedAfter(entries, entry.sequence, event.actionId)
    );
  });

/** Central-journal bridge with exact post-restart raw-L1 reconciliation. */
export const createMintItemNonCanonicalCentralJournalAdapter = ({
  store,
  deploymentFingerprint,
  headerHash,
  decisionDigest,
  transactionConfirmed,
}: {
  readonly store: FraudProofWorkflowJournalStore;
  readonly deploymentFingerprint: string;
  readonly headerHash: string;
  readonly decisionDigest: string;
  readonly transactionConfirmed: (txHash: string) => Promise<boolean>;
}) => {
  const identity: FraudProofWorkflowIdentity = {
    schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
    deploymentFingerprint,
    category: "mintItemNonCanonical" as FraudProofCatalogueCategoryName,
    target: { kind: "state_queue_header", headerHash },
    decisionDigest,
  };
  // Byte-for-byte the central workflow-id preimage, kept family-local until
  // the frozen category is admitted to the SDK union.
  const workflowId = familyWorkflowId(identity);
  const entries = async () => await store.load(workflowId);
  const appendEvent = async (
    event: FraudProofWorkflowJournalEntry["event"],
  ): Promise<void> => {
    let current = await entries();
    if (current.length === 0) {
      await store.append(
        {
          schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
          workflowId,
          identity,
          sequence: 0,
          recordedAt: now(),
          event: { kind: "started" },
        },
        0,
      );
      current = await entries();
    }
    await store.append(
      {
        schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
        workflowId,
        identity,
        sequence: current.length,
        recordedAt: now(),
        event,
      },
      current.length,
    );
  };
  const workflowAction = (
    kind: SubmitAction,
    recovery: DurableRecovery,
    removal?: MintItemRemovalAction,
  ): FraudProofWorkflowAction => {
    if (kind === "removeDescendants") {
      if (
        removal === undefined ||
        !/^[0-9a-f]{64}#(0|[1-9][0-9]*)$/u.test(removal.nextRemovalOutRef) ||
        !/^[0-9a-f]{64}#(0|[1-9][0-9]*)$/u.test(removal.fraudProofOutRef)
      )
        throw new Error(
          "mintItemNonCanonical removal omitted authenticated out-refs",
        );
      return {
        actionId: `${actionId(kind)}:${removal.nextRemovalOutRef}:${removal.fraudProofOutRef}`,
        input: { actionKind: "remove", ...recovery, ...removal },
      };
    }
    if (removal !== undefined)
      throw new Error(
        "mintItemNonCanonical non-removal action carries removal out-refs",
      );
    return {
      actionId: actionId(kind),
      input: { actionKind: actionId(kind), ...recovery },
    };
  };
  const assertActuation = (
    checkpoint:
      | "before_preflight"
      | "before_submit"
      | "before_reconcile"
      | "before_terminal_verify",
  ): void =>
    assertWorkflowJournalActuation({
      journal: store,
      deploymentFingerprint,
      category: "mintItemNonCanonical" as FraudProofCatalogueCategoryName,
      headerHash,
      checkpoint,
    });

  const ensurePrepared = async (familyIdentity: string): Promise<void> => {
    const current = await entries();
    const prepared = current.find(({ event }) => event.kind === "prepared");
    const artifact = {
      category: "mintItemNonCanonical",
      familyIdentity,
    };
    const artifactDigest = journalJsonDigest(artifact);
    if (prepared?.event.kind === "prepared") {
      if (prepared.event.artifactDigest !== artifactDigest) {
        throw new Error(
          "mintItemNonCanonical prepared family evidence identity changed",
        );
      }
      return;
    }
    await appendEvent({ kind: "prepared", artifact, artifactDigest });
  };

  const begin = async (
    kind: SubmitAction,
    familyIdentity: string,
    sourceStage: MintItemStage,
    targetStage: MintItemStage,
    removal?: MintItemRemovalAction,
  ): Promise<void> => {
    await ensurePrepared(familyIdentity);
    if (unresolvedIntent(await entries()) !== undefined) {
      throw new Error(
        "mintItemNonCanonical unresolved submission must reconcile before another build",
      );
    }
    assertActuation("before_preflight");
    await beginWorkflowFundingReservationAction({
      journal: store,
      action: workflowAction(
        kind,
        {
          familyIdentity,
          sourceStage,
          targetStage,
        },
        removal,
      ),
    });
  };

  const boundary =
    (
      kind: SubmitAction,
      familyIdentity: string,
      sourceStage: MintItemStage,
      targetStage: MintItemStage,
      removal?: MintItemRemovalAction,
    ): FraudProofPreSubmitBoundary =>
    async (transaction) => {
      if (!TX_HASH.test(transaction.txHash)) {
        throw new Error(
          "mintItemNonCanonical pre-submit hash is not canonical",
        );
      }
      const recovery = { familyIdentity, sourceStage, targetStage } as const;
      const action = workflowAction(kind, recovery, removal);
      if (
        removal !== undefined &&
        (!workflowTransactionInputOutRefs(transaction.signed).includes(
          removal.nextRemovalOutRef,
        ) ||
          !workflowTransactionReferenceInputOutRefs(
            transaction.signed,
          ).includes(removal.fraudProofOutRef))
      )
        throw new Error(
          "mintItemNonCanonical removal changed its authenticated inputs",
        );
      const current = await entries();
      const prior = unresolvedIntent(current);
      if (prior?.event.kind === "submission_intent") {
        if (
          prior.event.actionId !== action.actionId ||
          prior.event.txHash !== transaction.txHash
        ) {
          throw new Error(
            "mintItemNonCanonical transaction identity changed across restart",
          );
        }
        return;
      }
      const attempt =
        current.filter(
          ({ event }) =>
            event.kind === "submission_intent" &&
            event.actionId === action.actionId,
        ).length + 1;
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
          durableRecovery: recovery,
        },
        transaction.signed,
      );
      await beginWorkflowFundingReservationAction({
        journal: store,
        action,
      });
      await prepareWorkflowFundingReservationTransaction({
        journal: store,
        action,
        preflight,
        handoff: createWorkflowFundingSubmissionHandoff({
          entries: await entries(),
          action,
          preflight,
          attempt,
        }),
      });
      await appendEvent({
        kind: "preflight_passed",
        actionId: action.actionId,
        txHash: transaction.txHash,
        localEvaluator: LOCAL_UPLC_EVALUATOR,
        referenceScripts: transaction.referenceScripts,
      });
      await appendEvent({
        kind: "submission_intent",
        actionId: action.actionId,
        actionInput: action.input,
        durableRecovery: recovery,
        attempt,
        txHash: transaction.txHash,
      });
      assertActuation("before_submit");
      await assertWorkflowFundingReservationReadyToSubmit({
        journal: store,
        transactionHash: transaction.txHash,
      });
    };

  const reconcile = async (observedStage: MintItemStage): Promise<void> => {
    const intent = unresolvedIntent(await entries());
    if (intent?.event.kind !== "submission_intent") return;
    assertActuation("before_reconcile");
    const recovery = recoveryFrom(intent);
    const confirmed = await transactionConfirmed(intent.event.txHash);
    if (confirmed && observedStage === recovery.targetStage) {
      await confirmWorkflowFundingReservationTransaction({
        journal: store,
        transactionHash: intent.event.txHash,
      });
      await appendEvent({
        kind: "reconciled",
        actionId: intent.event.actionId,
        outcome: "confirmed",
        txHash: intent.event.txHash,
      });
      await appendEvent({
        kind: "confirmed",
        actionId: intent.event.actionId,
        txHash: intent.event.txHash,
      });
      return;
    }
    if (!confirmed && observedStage === recovery.sourceStage) {
      await abandonWorkflowFundingReservationTransaction({
        journal: store,
        transactionHash: intent.event.txHash,
        handoff: createWorkflowFundingAbandonmentHandoff({
          entries: await entries(),
          transactionHash: intent.event.txHash,
        }),
      });
      await appendEvent({
        kind: "reconciled",
        actionId: intent.event.actionId,
        outcome: "not_found",
      });
      return;
    }
    await conflictWorkflowFundingReservationTransaction({
      journal: store,
      transactionHash: intent.event.txHash,
    });
    throw new Error(
      "mintItemNonCanonical authenticated stage/transaction identity substitution",
    );
  };

  const auxiliaryBoundary =
    (
      kind: "publication" | "certificate",
      familyIdentity: string,
      stage: MintItemStage,
      captured: string[],
    ): FraudProofPreSubmitBoundary =>
    async (transaction) => {
      await ensurePrepared(familyIdentity);
      const pending = unresolvedIntent(await entries());
      if (pending?.event.kind === "submission_intent") {
        if (recoveryFrom(pending).auxiliary !== true) {
          throw new Error(
            "mintItemNonCanonical auxiliary actuation found an unresolved proof transaction",
          );
        }
        await confirmAuxiliary(pending.event.txHash);
      }
      const id = `mintItemNonCanonical:${kind}:${transaction.txHash}`;
      const recovery = {
        familyIdentity,
        sourceStage: stage,
        targetStage: stage,
        auxiliary: true,
      } as const;
      const action: FraudProofWorkflowAction = {
        actionId: id,
        input: {
          actionKind:
            kind === "publication"
              ? "publish_field_carriage"
              : "certify_field_carriage",
          kind,
        },
      };
      const preflight = bindWorkflowPreflightTransaction(
        {
          actionId: id,
          txHash: transaction.txHash,
          scriptExecution: "reference_scripts" as const,
          localUplcEvaluation: {
            status: "passed" as const,
            evaluator: LOCAL_UPLC_EVALUATOR,
          },
          referenceScripts: transaction.referenceScripts,
          durableRecovery: recovery,
        },
        transaction.signed,
      );
      await beginWorkflowFundingReservationAction({
        journal: store,
        action,
      });
      await prepareWorkflowFundingReservationTransaction({
        journal: store,
        action,
        preflight,
        handoff: createWorkflowFundingSubmissionHandoff({
          entries: await entries(),
          action,
          preflight,
          attempt: 1,
        }),
      });
      await appendEvent({
        kind: "preflight_passed",
        actionId: id,
        txHash: transaction.txHash,
        localEvaluator: LOCAL_UPLC_EVALUATOR,
        referenceScripts: transaction.referenceScripts,
      });
      await appendEvent({
        kind: "submission_intent",
        actionId: id,
        actionInput: action.input,
        durableRecovery: recovery,
        attempt: 1,
        txHash: transaction.txHash,
      });
      captured.push(transaction.txHash);
      assertActuation("before_submit");
      await assertWorkflowFundingReservationReadyToSubmit({
        journal: store,
        transactionHash: transaction.txHash,
      });
    };

  const confirmAuxiliary = async (txHash: string): Promise<void> => {
    const current = await entries();
    if (
      current.some(
        ({ event }) => event.kind === "confirmed" && event.txHash === txHash,
      )
    ) {
      return;
    }
    const intent = unresolvedIntent(current);
    if (
      intent?.event.kind !== "submission_intent" ||
      intent.event.txHash !== txHash ||
      recoveryFrom(intent).auxiliary !== true
    ) {
      throw new Error(
        "mintItemNonCanonical auxiliary completion changed transaction identity",
      );
    }
    if (!(await transactionConfirmed(txHash))) {
      throw new Error(
        "mintItemNonCanonical auxiliary transaction is not authenticated on L1",
      );
    }
    await confirmWorkflowFundingReservationTransaction({
      journal: store,
      transactionHash: txHash,
    });
    await appendEvent({
      kind: "submitted",
      actionId: intent.event.actionId,
      attempt: intent.event.attempt,
      txHash,
    });
    await appendEvent({
      kind: "reconciled",
      actionId: intent.event.actionId,
      outcome: "confirmed",
      txHash,
    });
    await appendEvent({
      kind: "confirmed",
      actionId: intent.event.actionId,
      txHash,
    });
  };

  /** Preserve the legacy prepared record while using the standard terminal and funding checks. */
  const finish = async ({
    candidate,
    releaseFinality: policy,
    verifier,
  }: {
    candidate: FraudProofWorkflowTerminal;
    releaseFinality: VerifiedFraudProofReleaseFinalityPolicy;
    verifier: FraudProofWorkflowTerminalVerifier;
  }) => {
    const releaseFinality =
      validateVerifiedFraudProofReleaseFinalityPolicy(policy);
    if (releaseFinality.deploymentIdentityDigest !== deploymentFingerprint)
      throw new Error(
        "mintItemNonCanonical terminal changed deployment finality identity",
      );
    const current = await entries();
    validateFraudProofWorkflowJournal({
      workflowId,
      entries: current,
      expectedIdentity: identity,
    });
    const prepared = current[1]?.event;
    if (prepared?.kind !== "prepared")
      throw new Error(
        "mintItemNonCanonical terminal requires retained prepared evidence",
      );
    assertActuation("before_terminal_verify");
    const inclusionOnly =
      candidate.observedAt.confirmationDepth <
      releaseFinality.policy.confirmationDepth;
    const verify = inclusionOnly ? verifier.verifyIncluded : verifier.verify;
    if (verify === undefined)
      throw new Error(
        "mintItemNonCanonical terminal verifier omitted inclusion verification",
      );
    let terminal = normalizeWorkflowTerminal({
      identity,
      terminal: await verify({
        identity,
        workflowId,
        releaseFinality,
        candidate,
        artifact: prepared.artifact,
        entries: current,
      }),
      entries: current,
      releaseFinality,
      inclusionOnly,
    });
    for (const txHash of [
      terminal.proofToken.createdByTxHash,
      terminal.correction.removalTxHash,
    ])
      if (!(await transactionConfirmed(txHash)))
        throw new Error(
          "mintItemNonCanonical terminal transaction is not authenticated on L1",
        );
    const funding = await readWorkflowFundingRecovery(store);
    const saved = funding.completionHandoff;
    if (saved !== null)
      assertWorkflowFundingCompletionHandoffJournal({
        handoff: saved,
        entries: current,
      });
    const previous = current.find(
      ({ event }) => event.kind === "completed",
    )?.event;
    const completed =
      previous?.kind === "completed"
        ? previous
        : saved?.completion.kind === "completed"
          ? saved.completion
          : undefined;
    const sameFacts = (
      left: FraudProofWorkflowTerminal,
      right: FraudProofWorkflowTerminal,
    ) => {
      const facts = (value: FraudProofWorkflowTerminal) => ({
        ...value,
        observedAt: { ...value.observedAt, confirmationDepth: 0 },
      });
      return (
        journalJsonDigest(normalizeJournalJson(facts(left))) ===
        journalJsonDigest(normalizeJournalJson(facts(right)))
      );
    };
    if (completed !== undefined) {
      if (
        terminal.observedAt.confirmationDepth <
          completed.terminal.observedAt.confirmationDepth ||
        !sameFacts(terminal, completed.terminal)
      )
        throw new Error(
          "mintItemNonCanonical released terminal facts changed on the canonical chain",
        );
      terminal = normalizeWorkflowTerminal({
        identity,
        terminal: completed.terminal,
        entries: current,
        releaseFinality,
      });
    }
    const kind = inclusionOnly
      ? ("terminal_included" as const)
      : ("completed" as const);
    assertActuation("before_terminal_verify");
    const existing = current.some(({ event }) => event.kind === kind);
    if (!existing) {
      const completion: WorkflowFundingCompletionHandoff["completion"] = {
        kind,
        terminal,
        terminalDigest: journalJsonDigest(normalizeJournalJson(terminal)),
      };
      const handoff: WorkflowFundingCompletionHandoff =
        saved?.completion.kind === kind
          ? saved
          : {
              workflowId,
              identity,
              preparedArtifactDigest: prepared.artifactDigest,
              expectedJournalSequence: current.length,
              completion,
            };
      if (handoff === saved) {
        if (
          terminal.observedAt.confirmationDepth <
            saved.completion.terminal.observedAt.confirmationDepth ||
          !sameFacts(terminal, saved.completion.terminal)
        )
          throw new Error(
            "mintItemNonCanonical completion handoff changed its authenticated terminal",
          );
      } else
        await releaseWorkflowFundingReservation({ journal: store, handoff });
      assertActuation("before_terminal_verify");
      await appendEvent(handoff.completion);
    }
    return {
      kind: inclusionOnly ? ("pending" as const) : ("completed" as const),
      workflowId,
      identity,
      terminal,
      entries: await entries(),
      ...(inclusionOnly
        ? {
            resumeOnObservation: true,
            reason: "terminal awaits release finality",
          }
        : {}),
    };
  };

  const familyJournal: MintItemJournal = {
    load: async (familyIdentity) => {
      const current = await entries();
      const result: MintItemJournalEntry[] = [];
      for (const entry of current) {
        const confirmedEvent = entry.event;
        if (confirmedEvent.kind !== "confirmed") continue;
        const intent = [...current]
          .slice(0, entry.sequence)
          .reverse()
          .find(
            ({ event }) =>
              event.kind === "submission_intent" &&
              event.actionId === confirmedEvent.actionId,
          );
        if (intent?.event.kind !== "submission_intent") {
          throw new Error(
            "mintItemNonCanonical confirmed transaction has no durable intent",
          );
        }
        const recovery = recoveryFrom(intent);
        if (recovery.auxiliary === true) continue;
        if (recovery.familyIdentity !== familyIdentity) {
          throw new Error(
            "mintItemNonCanonical family evidence identity changed",
          );
        }
        result.push({
          sequence: result.length,
          identity: familyIdentity,
          stage: recovery.targetStage,
          txHash: confirmedEvent.txHash,
          outputReference: null,
        });
      }
      return result;
    },
    append: async (entry) => {
      const intent = unresolvedIntent(await entries());
      if (
        intent?.event.kind !== "submission_intent" ||
        intent.event.txHash !== entry.txHash
      ) {
        throw new Error(
          "mintItemNonCanonical submission completed without its exact pre-submit intent",
        );
      }
      const recovery = recoveryFrom(intent);
      if (
        recovery.familyIdentity !== entry.identity ||
        recovery.targetStage !== entry.stage
      ) {
        throw new Error(
          "mintItemNonCanonical submission result changed durable recovery identity",
        );
      }
      await appendEvent({
        kind: "submitted",
        actionId: intent.event.actionId,
        attempt: intent.event.attempt,
        txHash: entry.txHash,
      });
    },
  };
  return Object.freeze({
    workflowId,
    identity,
    begin,
    boundary,
    reconcile,
    auxiliaryBoundary,
    confirmAuxiliary,
    finish,
    familyJournal,
  });
};
