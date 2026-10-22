import { assetsToValue, type UTxO } from "@lucid-evolution/lucid";

import {
  assertWorkflowActuationPermitIdentity,
  workflowActuationPermitIsReconciliationOnly,
} from "./actuation-permit.js";
import { isFinalWorkflowCompletion } from "./completion-finality.js";
import { stateForJournal } from "./funding-reservation-permit.create-workflow-funding-reservation-permit.js";
import { parseStateSnapshot } from "./funding-reservation-permit.parse-workflow-funding-snapshot.js";
import {
  type PermitState,
  WorkflowFundingReservationUnavailableError,
} from "./funding-reservation-permit.workflow-funding-reservation-port.js";
import {
  appendWorkflowJournalEvents,
  commitWithJournal,
  type WorkflowJournalWriter,
} from "./funding-reservation-permit.workflow-journal-writer.js";
import {
  type FraudProofWorkflowJournalEntry,
  type FraudProofWorkflowJournalEvent,
  type FraudProofWorkflowJournalStore,
  validateFraudProofWorkflowJournal,
} from "./journal.js";
import type { SignedWorkflowTransactionRetirement } from "./signed-transaction-retirement.js";

const clearAction = (state: PermitState): void => {
  state.preparedTransaction = undefined;
  state.currentActionKind = undefined;
  state.currentActionDigest = undefined;
  state.currentFundingOutRefs = Object.freeze([]);
  state.currentRequiredFundingOutRefs = Object.freeze([]);
  state.currentCollateralOutRefs = Object.freeze([]);
};

type JournaledChange = Readonly<{
  writer: WorkflowJournalWriter;
  events: readonly FraudProofWorkflowJournalEvent[];
}>;

const applyTransition = async ({
  writer,
  events,
  outcome,
  transactionHash,
}: JournaledChange & {
  readonly outcome: "confirmed" | "conflict";
  readonly transactionHash: string;
}): Promise<void> => {
  const state = stateForJournal(writer.journal);
  if (state === undefined) {
    await appendWorkflowJournalEvents(writer, events);
    return;
  }
  if (
    state.pendingTransactionHash !== undefined &&
    state.pendingTransactionHash !== transactionHash
  ) {
    throw new Error(
      "funding reservation reconciliation changed transaction hash",
    );
  }
  const next = await commitWithJournal({
    port: state.port,
    writer,
    events,
    change: async ({ journal }) =>
      outcome === "confirmed"
        ? await state.port.confirm({
            expectedRevision: state.snapshot.revision,
            transactionHash,
            journal,
          })
        : await state.port.markConflict({
            expectedRevision: state.snapshot.revision,
            code: "unexpected_spend",
            journal,
          }),
  });
  state.snapshot = parseStateSnapshot(state, next);
  state.pendingTransactionHash = undefined;
  clearAction(state);
};

/** Confirms the pending attempt; `events` record its confirmation. */
export const confirmWorkflowFundingReservationTransaction = async (
  input: JournaledChange & { readonly transactionHash: string },
): Promise<void> => await applyTransition({ ...input, outcome: "confirmed" });

/** Re-selects a recorded attempt after canonical re-observation, with
 * `events` its `reobserved` record. With `adoption`, re-selects a superseded
 * attempt that landed after a rollback, and `events` record its preflight and
 * intent again. False when another unresolved attempt owns its inputs: then
 * nothing is recorded. */
export const reobserveWorkflowFundingReservationTransaction = async (
  input: JournaledChange & {
    readonly transactionHash: string;
    readonly adoption?: true;
  },
): Promise<boolean> => {
  const state = stateForJournal(input.writer.journal);
  if (state === undefined) {
    await appendWorkflowJournalEvents(input.writer, input.events);
    return true;
  }
  state.idleReleaseAuthorized = false;
  const reobserve = state.port.reobserve?.bind(state.port);
  if (reobserve === undefined)
    throw new Error("funding authority cannot reconcile a reobserved action");
  const observed = await commitWithJournal({
    port: state.port,
    writer: input.writer,
    events: input.events,
    change: async ({ journal, entries }) => {
      const intent = entries.at(-1);
      if (
        input.adoption === true &&
        (intent?.event.kind !== "submission_intent" ||
          intent.event.txHash !== input.transactionHash)
      )
        throw new Error("funding adoption must record its exact intent");
      return await reobserve({
        expectedRevision: state.snapshot.revision,
        transactionHash: input.transactionHash,
        ...(input.adoption === true
          ? { adoption: { intentSequence: intent!.sequence } }
          : {}),
        journal,
      });
    },
  });
  if (observed === null) return false;
  state.snapshot = parseStateSnapshot(state, observed);
  state.pendingTransactionHash = input.transactionHash;
  clearAction(state);
  return true;
};

/** Supersedes the pending attempt, or retires it with `retirement`; `events`
 * record its `not_found` reconciliation. Without a retirement its inputs stay
 * leased and the replacement must spend one of them. */
export const abandonWorkflowFundingReservationTransaction = async (
  input: JournaledChange & {
    readonly transactionHash: string;
    readonly retirement?: SignedWorkflowTransactionRetirement;
  },
): Promise<void> => {
  const state = stateForJournal(input.writer.journal);
  if (state === undefined) {
    await appendWorkflowJournalEvents(input.writer, input.events);
    return;
  }
  if (
    state.pendingTransactionHash !== undefined &&
    state.pendingTransactionHash !== input.transactionHash
  )
    throw new Error(
      "funding abandonment changed its exact transaction identity",
    );
  state.snapshot = parseStateSnapshot(
    state,
    await commitWithJournal({
      port: state.port,
      writer: input.writer,
      events: input.events,
      change: async ({ journal }) =>
        await state.port.abandon({
          expectedRevision: state.snapshot.revision,
          transactionHash: input.transactionHash,
          ...(input.retirement === undefined
            ? {}
            : { retirement: input.retirement }),
          journal,
        }),
    }),
  );
  clearAction(state);
  state.pendingTransactionHash = undefined;
};

const journalHasOnlyResolvedFundingAttempts = (
  entries: readonly FraudProofWorkflowJournalEntry[],
): boolean => {
  const resolved = new Map<string, boolean>();
  for (const { event } of entries) {
    if (event.kind === "submission_intent") resolved.set(event.txHash, false);
    else if (
      "txHash" in event &&
      event.txHash !== undefined &&
      resolved.has(event.txHash)
    ) {
      // A confirmed attempt never holds later actions: retirement beyond the
      // recovery horizon only prunes its record, and its collateral is never
      // at risk because only locally evaluated scripts are submitted. A
      // rollback reopens it through `reobserved` below.
      if (event.kind === "signed_attempt_retired" || event.kind === "confirmed")
        resolved.set(event.txHash, true);
      // Supersession at the tip resolves an attempt as well as retirement.
      else if (event.kind === "reconciled")
        resolved.set(event.txHash, event.outcome === "not_found");
      else if (
        event.kind === "reobserved" ||
        event.kind === "submitted" ||
        event.kind === "rebroadcast_intent" ||
        event.kind === "submission_ambiguous"
      )
        resolved.set(event.txHash, false);
    }
  }
  return [...resolved.values()].every(Boolean);
};

export const releaseIdleWorkflowFundingReservation = async (input: {
  readonly journal: FraudProofWorkflowJournalStore;
  readonly workflowId: string;
}): Promise<void> => {
  const state = stateForJournal(input.journal);
  if (state?.port.releaseIdle === undefined) return;
  const entries = await input.journal.load(input.workflowId);
  validateFraudProofWorkflowJournal({ workflowId: input.workflowId, entries });
  state.idleReleaseAuthorized = journalHasOnlyResolvedFundingAttempts(entries);
  const latestIntent = [...entries]
    .reverse()
    .find(({ event }) => event.kind === "submission_intent")?.event;
  if (
    !state.idleReleaseAuthorized ||
    latestIntent?.kind !== "submission_intent" ||
    !entries.some(
      ({ event }) =>
        event.kind === "reconciled" &&
        event.outcome === "not_found" &&
        (workflowActuationPermitIsReconciliationOnly(state.actuationPermit) ||
          event.txHash === latestIntent.txHash),
    )
  )
    return;
  assertWorkflowActuationPermitIdentity({
    permit: state.actuationPermit,
    category: state.category,
    rollbackGeneration: state.snapshot.rollbackGeneration,
  });
  state.snapshot = parseStateSnapshot(
    state,
    await state.port.releaseIdle({
      expectedRevision: state.snapshot.revision,
    }),
  );
};

/**
 * Releases the inputs of an idle reservation that could not fund the action
 * it was begun for (`cause` is that shortfall), so the next action reserves
 * the wallet as it is then: a top-up resumes the workflow. Idleness is the
 * journal's, read when this iteration began (no attempt is unresolved).
 */
export const releaseUnfundedWorkflowFundingInputs = async (input: {
  readonly journal: object;
  readonly cause: unknown;
}): Promise<void> => {
  const state = stateForJournal(input.journal);
  if (
    !(input.cause instanceof WorkflowFundingReservationUnavailableError) ||
    state?.port.releaseIdle === undefined ||
    !state.idleReleaseAuthorized
  )
    return;
  assertWorkflowActuationPermitIdentity({
    permit: state.actuationPermit,
    category: state.category,
    rollbackGeneration: state.snapshot.rollbackGeneration,
  });
  state.snapshot = parseStateSnapshot(
    state,
    await state.port.releaseIdle({
      expectedRevision: state.snapshot.revision,
    }),
  );
};

/** Marks the pending attempt's inputs as spent by another transaction;
 * `events` record why the workflow stalls, if the journal does not yet. */
export const conflictWorkflowFundingReservationTransaction = async (
  input: JournaledChange & { readonly transactionHash: string },
): Promise<void> => await applyTransition({ ...input, outcome: "conflict" });

/** Records a completion. Only a final completion releases the reservation,
 * in the same commit; an earlier one is recorded alone. */
export const releaseWorkflowFundingReservation = async ({
  writer,
  event,
}: {
  readonly writer: WorkflowJournalWriter;
  readonly event: Extract<
    FraudProofWorkflowJournalEvent,
    { kind: "completed" | "terminal_included" }
  >;
}): Promise<void> => {
  const state = stateForJournal(writer.journal);
  if (state === undefined || !isFinalWorkflowCompletion(event)) {
    await appendWorkflowJournalEvents(writer, [event]);
    return;
  }
  state.snapshot = parseStateSnapshot(
    state,
    await commitWithJournal({
      port: state.port,
      writer,
      events: [event],
      change: async ({ journal }) =>
        await state.port.release({
          expectedRevision: state.snapshot.revision,
          journal,
        }),
    }),
  );
  clearAction(state);
  state.pendingTransactionHash = undefined;
};

export const balanceCbor = (utxos: readonly UTxO[]): string => {
  const assets: Record<string, bigint> = {};
  for (const utxo of utxos) {
    for (const [unit, quantity] of Object.entries(utxo.assets)) {
      assets[unit] = (assets[unit] ?? 0n) + quantity;
    }
  }
  return assetsToValue(assets).to_cbor_hex();
};

/** Retires a superseded attempt; `events` record its retirement. */
export const retireLegacyWorkflowFundingAbandonment = async (
  input: JournaledChange & {
    readonly transactionHash: string;
    readonly retirement: SignedWorkflowTransactionRetirement;
  },
): Promise<void> => {
  const state = stateForJournal(input.writer.journal);
  const retire = state?.port.retireLegacyAbandonment?.bind(state.port);
  if (state === undefined || retire === undefined)
    throw new Error("Funding authority cannot authenticate legacy retirement");
  state.snapshot = parseStateSnapshot(
    state,
    await commitWithJournal({
      port: state.port,
      writer: input.writer,
      events: input.events,
      change: async ({ journal }) =>
        await retire({
          expectedRevision: state.snapshot.revision,
          transactionHash: input.transactionHash,
          retirement: input.retirement,
          journal,
        }),
    }),
  );
};
