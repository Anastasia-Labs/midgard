import { formatUnknownError } from "@al-ft/midgard-core";
import { CML } from "@lucid-evolution/lucid";

import type {
  FraudProofWorkflowJournalEntry,
  FraudProofWorkflowJournalEvent,
} from "./journal.fraud-proof-workflow-journal-event.js";
import type { FraudProofWorkflowReconcileResult } from "./orchestrator.fraud-proof-family-workflow-adapter.js";

type Stalled = Extract<
  FraudProofWorkflowJournalEvent,
  { readonly kind: "stalled" }
>;

type CollateralForfeited = Extract<
  FraudProofWorkflowJournalEvent,
  { readonly kind: "collateral_forfeited" }
>;

/**
 * A collateral forfeiture, or its reversal, is bookkeeping: it moves no
 * submission, so whatever reads the journal's latest step or an action's
 * latest event passes over it, as over a `stalled` diagnostic.
 */
export const isCollateralForfeitureEvent = (
  event: FraudProofWorkflowJournalEvent,
): boolean =>
  event.kind === "collateral_forfeited" ||
  event.kind === "collateral_forfeiture_reversed";

/**
 * What a phase-2-invalid landing of exact signed bytes takes and creates:
 * every collateral input, sorted, and the collateral return, which the
 * ledger places right after the body's outputs.
 */
export const signedCollateralForfeiture = (
  signedTransactionCborHex: string,
): Readonly<{
  collateralOutRefs: readonly string[];
  collateralReturnOutRef?: string;
}> => {
  const transaction = CML.Transaction.from_cbor_hex(signedTransactionCborHex);
  const body = transaction.body();
  try {
    const collateral = body.collateral_inputs();
    const collateralOutRefs = Array.from(
      { length: collateral?.len() ?? 0 },
      (_, index) => {
        const input = collateral!.get(index);
        return `${input.transaction_id().to_hex()}#${input.index().toString()}`;
      },
    ).sort();
    const transactionHash = CML.hash_transaction(body).to_hex();
    return Object.freeze({
      collateralOutRefs: Object.freeze(collateralOutRefs),
      ...(body.collateral_return() === undefined
        ? {}
        : {
            collateralReturnOutRef: `${transactionHash}#${body
              .outputs()
              .len()
              .toString()}`,
          }),
    });
  } finally {
    body.free();
    transaction.free();
  }
};

/** The forfeiture record of exact signed attempt bytes. */
export const collateralForfeitureEvent = ({
  actionId,
  transactionHash,
  signedTransactionCborHex,
}: {
  readonly actionId: string;
  readonly transactionHash: string;
  readonly signedTransactionCborHex: string;
}): CollateralForfeited => ({
  kind: "collateral_forfeited",
  actionId,
  txHash: transactionHash,
  ...signedCollateralForfeiture(signedTransactionCborHex),
});

/** The forfeitures a rollback has not reversed, by transaction. */
export const activeCollateralForfeitures = (
  entries: readonly FraudProofWorkflowJournalEntry[],
): ReadonlyMap<string, CollateralForfeited> => {
  const active = new Map<string, CollateralForfeited>();
  for (const { event } of entries) {
    if (event.kind === "collateral_forfeited") active.set(event.txHash, event);
    else if (event.kind === "collateral_forfeiture_reversed")
      active.delete(event.txHash);
  }
  return active;
};

/** Whether the journal records these bytes as landed failing phase 2, the
 * forfeiture standing or reversed since. Such bytes are never sent again: a
 * rollback that removes the landing does not make them worth resending. */
export const recordsPhaseTwoInvalidLanding = (
  entries: readonly FraudProofWorkflowJournalEntry[],
  transactionHash: string,
): boolean =>
  entries.some(
    ({ event }) =>
      event.kind === "collateral_forfeited" && event.txHash === transactionHash,
  );

/**
 * The read of a recorded attempt the run acts on. Bytes the journal records
 * as landed failing phase 2 can never land valid: the body and the inputs it
 * resolves fix that verdict. They are never sent again, so once a rollback
 * leaves them absent there is nothing to wait for, with or without an upper
 * validity bound: the read counts as not found, and the attempt is
 * superseded like any other attempt impossible at the tip.
 */
export const notFoundIfNeverResent = (
  entries: readonly FraudProofWorkflowJournalEntry[],
  transactionHash: string,
  read: FraudProofWorkflowReconcileResult,
): FraudProofWorkflowReconcileResult =>
  read.kind === "unknown" &&
  read.absent === true &&
  recordsPhaseTwoInvalidLanding(entries, transactionHash)
    ? { kind: "not_found" }
    : read;

const ACCOUNTING_FAILED = "collateral accounting failed for attempt ";
const ACCOUNTING_MOOT = "collateral accounting no longer needed for attempt ";

/** The diagnostic an attempt's failed forfeiture or reversal records: the
 * read is retried, and the failure is named once. */
export const collateralAccountingFailure = (
  transactionHash: string,
  error: unknown,
): Stalled => ({
  kind: "stalled",
  reason: `${ACCOUNTING_FAILED}${transactionHash}: ${formatUnknownError(error)}`,
});

/** The diagnostic that clears a failed accounting once a read of the attempt
 * leaves nothing to account. */
export const collateralAccountingMoot = (transactionHash: string): Stalled => ({
  kind: "stalled",
  reason: `${ACCOUNTING_MOOT}${transactionHash}`,
});

/**
 * The attempts whose collateral accounting failed and has not been done
 * since, by transaction, with the failure. Each clears once the attempt's
 * forfeiture or reversal is recorded, a read leaves nothing to account, or
 * the attempt retires or lands as its action's result.
 */
export const standingCollateralAccountingFailures = (
  entries: readonly FraudProofWorkflowJournalEntry[],
): ReadonlyMap<string, string> => {
  const failed = new Map<string, string>();
  for (const { event } of entries) {
    if (event.kind === "stalled") {
      if (event.reason.startsWith(ACCOUNTING_FAILED))
        failed.set(
          event.reason.slice(
            ACCOUNTING_FAILED.length,
            ACCOUNTING_FAILED.length + 64,
          ),
          event.reason,
        );
      else if (event.reason.startsWith(ACCOUNTING_MOOT))
        failed.delete(event.reason.slice(ACCOUNTING_MOOT.length));
    } else if (
      event.kind === "collateral_forfeited" ||
      event.kind === "collateral_forfeiture_reversed" ||
      event.kind === "signed_attempt_retired" ||
      event.kind === "confirmed"
    )
      failed.delete(event.txHash);
    else if (
      event.kind === "reconciled" &&
      event.retirement !== undefined &&
      event.txHash !== undefined
    )
      failed.delete(event.txHash);
  }
  return failed;
};

/**
 * Whether a read of an attempt whose forfeiture stands shows its landing
 * gone: the chain answered, the attempt did not land failing phase 2, and
 * the answer is not a retirement. A retirement says nothing of a landing
 * deeper than k, which no rollback undoes, and the follower forgets.
 */
export const readShowsForfeitureReversed = (
  result: FraudProofWorkflowReconcileResult,
): boolean =>
  result.kind === "unknown"
    ? result.absent === true
    : result.kind !== "conflict" &&
      result.retirement === undefined &&
      !(result.kind === "not_found" && result.landedInvalid === true);
