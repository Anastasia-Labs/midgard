import { assetsToValue, type UTxO } from "@lucid-evolution/lucid";

import {
  assertWorkflowActuationPermitIdentity,
  workflowActuationPermitIsReconciliationOnly,
} from "./actuation-permit.js";
import { stateForJournal } from "./funding-reservation-permit.create-workflow-funding-reservation-permit.js";
import {
  assertWorkflowFundingAbandonmentHandoffJournal,
  parseWorkflowFundingAbandonmentHandoff,
  parseWorkflowFundingCompletionHandoff,
} from "./funding-reservation-permit.parse-workflow-funding-abandonment-handoff.js";
import { parseStateSnapshot } from "./funding-reservation-permit.reconcile-workflow-funding-submission-handoff.js";
import {
  type WorkflowFundingAbandonmentHandoff,
  type WorkflowFundingCompletionHandoff,
} from "./funding-reservation-permit.workflow-funding-reservation-port.js";
import {
  type FraudProofWorkflowJournalEntry,
  type FraudProofWorkflowJournalStore,
  validateFraudProofWorkflowJournal,
} from "./journal.js";

const applyTransition = async ({
  journal,
  outcome,
  transactionHash,
}: {
  readonly journal: object;
  readonly outcome: "confirmed" | "conflict";
  readonly transactionHash: string;
}): Promise<void> => {
  const state = stateForJournal(journal);
  if (state === undefined) return;
  if (
    state.pendingTransactionHash !== undefined &&
    state.pendingTransactionHash !== transactionHash
  ) {
    throw new Error(
      "funding reservation reconciliation changed transaction hash",
    );
  }
  const next =
    outcome === "confirmed"
      ? await state.port.confirm({
          expectedRevision: state.snapshot.revision,
          transactionHash,
        })
      : await state.port.markConflict({
          expectedRevision: state.snapshot.revision,
          code: "unexpected_spend",
        });
  state.snapshot = parseStateSnapshot(state, next);
  state.pendingTransactionHash = undefined;
  state.preparedTransaction = undefined;
  state.currentActionKind = undefined;
  state.currentActionDigest = undefined;
  state.currentFundingOutRefs = Object.freeze([]);
  state.currentCollateralOutRefs = Object.freeze([]);
};

export const confirmWorkflowFundingReservationTransaction = async (input: {
  readonly journal: object;
  readonly transactionHash: string;
}): Promise<void> => await applyTransition({ ...input, outcome: "confirmed" });

export const reobserveWorkflowFundingReservationTransaction = async (input: {
  readonly journal: object;
  readonly transactionHash: string;
}): Promise<boolean> => {
  const state = stateForJournal(input.journal);
  if (state === undefined) return true;
  state.idleReleaseAuthorized = false;
  if (state.port.reobserve === undefined)
    throw new Error("funding authority cannot reconcile a reobserved action");
  const observed = await state.port.reobserve({
    expectedRevision: state.snapshot.revision,
    transactionHash: input.transactionHash,
  });
  if (observed === null) return false;
  state.snapshot = parseStateSnapshot(state, observed);
  state.pendingTransactionHash = input.transactionHash;
  state.preparedTransaction = undefined;
  state.currentActionKind = undefined;
  state.currentActionDigest = undefined;
  state.currentFundingOutRefs = Object.freeze([]);
  state.currentCollateralOutRefs = Object.freeze([]);
  return true;
};

export const abandonWorkflowFundingReservationTransaction = async (input: {
  readonly journal: object;
  readonly transactionHash: string;
  readonly handoff: WorkflowFundingAbandonmentHandoff;
}): Promise<void> => {
  const state = stateForJournal(input.journal);
  if (state === undefined) return;
  const handoff = parseWorkflowFundingAbandonmentHandoff(input.handoff);
  if (handoff.reconciliation.retirement === undefined)
    throw new Error(
      "Funding cannot retire an attempt without authenticated canonical evidence",
    );
  if (
    handoff.submissionIntent.txHash !== input.transactionHash ||
    (state.pendingTransactionHash !== undefined &&
      state.pendingTransactionHash !== input.transactionHash)
  )
    throw new Error(
      "funding abandonment changed its exact transaction identity",
    );
  state.snapshot = parseStateSnapshot(
    state,
    await state.port.abandon({
      expectedRevision: state.snapshot.revision,
      transactionHash: input.transactionHash,
      handoff,
    }),
  );
  state.preparedTransaction = undefined;
  state.currentActionKind = undefined;
  state.currentActionDigest = undefined;
  state.currentFundingOutRefs = Object.freeze([]);
  state.currentCollateralOutRefs = Object.freeze([]);
  // The recorded bytes remain available until the exact journal outcome is acknowledged.
  state.pendingTransactionHash = input.transactionHash;
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
      else if (event.kind === "reconciled")
        resolved.set(
          event.txHash,
          event.outcome === "not_found" && event.retirement !== undefined,
        );
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

export const acknowledgeWorkflowFundingAbandonment = async (input: {
  readonly journal: FraudProofWorkflowJournalStore;
  readonly handoff: WorkflowFundingAbandonmentHandoff;
}): Promise<void> => {
  const state = stateForJournal(input.journal);
  if (state === undefined) return;
  const handoff = parseWorkflowFundingAbandonmentHandoff(input.handoff);
  const entries = await input.journal.load(handoff.workflowId);
  if (
    !assertWorkflowFundingAbandonmentHandoffJournal({
      handoff,
      entries,
    })
  )
    throw new Error(
      "funding abandonment outcome is not durable in its journal",
    );
  state.snapshot = parseStateSnapshot(
    state,
    await state.port.acknowledgeAbandonment({
      expectedRevision: state.snapshot.revision,
      handoff,
    }),
  );
  state.pendingTransactionHash = undefined;
};

export const conflictWorkflowFundingReservationTransaction = async (input: {
  readonly journal: object;
  readonly transactionHash: string;
}): Promise<void> => await applyTransition({ ...input, outcome: "conflict" });

export const releaseWorkflowFundingReservation = async ({
  journal,
  handoff,
}: {
  readonly journal: object;
  readonly handoff: WorkflowFundingCompletionHandoff;
}): Promise<void> => {
  const state = stateForJournal(journal);
  if (state === undefined) return;
  state.snapshot = parseStateSnapshot(
    state,
    await state.port.release({
      expectedRevision: state.snapshot.revision,
      handoff: parseWorkflowFundingCompletionHandoff(handoff),
    }),
  );
  state.currentActionKind = undefined;
  state.currentActionDigest = undefined;
  state.currentFundingOutRefs = Object.freeze([]);
  state.currentCollateralOutRefs = Object.freeze([]);
  state.pendingTransactionHash = undefined;
  state.preparedTransaction = undefined;
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

export const retireLegacyWorkflowFundingAbandonment = async (input: {
  readonly journal: object;
  readonly transactionHash: string;
  readonly retirement: import("./signed-transaction-retirement.js").SignedWorkflowTransactionRetirement;
}) => {
  const state = stateForJournal(input.journal);
  if (state === undefined || state.port.retireLegacyAbandonment === undefined)
    throw new Error("Funding authority cannot authenticate legacy retirement");
  state.snapshot = parseStateSnapshot(
    state,
    await state.port.retireLegacyAbandonment({
      expectedRevision: state.snapshot.revision,
      transactionHash: input.transactionHash,
      retirement: input.retirement,
    }),
  );
};
