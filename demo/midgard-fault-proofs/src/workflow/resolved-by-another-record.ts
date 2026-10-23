/**
 * A header removed against a fraud-proof record that this run did not
 * create: another prover's, a sibling run's on our key, or another family's.
 * The block is corrected and nothing of this run's remains to prove. Once
 * every signed attempt in this run's journal is settled, the run ends
 * resolved by another record. That is never our conviction: no terminal, and
 * no reward is claimed.
 */
import { CML } from "@lucid-evolution/lucid";

import type { FraudProofWorkflowJournalEntry } from "./journal.js";
import {
  bodyOutputsContainUnit,
  historicalTransactions,
} from "./raw-l1-family-derivation.derive-retained-state-queue-header-observation-from-raw-l1.js";
import {
  type FraudProofRawL1TerminalDefinition,
  mintQuantity,
  outputQuantity,
  rawToUtxo,
} from "./raw-l1-family-derivation.state-queue-topology.js";
import type { FraudProofRawL1Snapshot } from "./raw-l1-snapshot.js";

/** The removal that took the header out of the state queue, the record it
 * cited, and the removal's depth on the observed chain. */
export type FraudProofResolvedByAnotherRecord = Readonly<{
  removalTxHash: string;
  referencedProofTokenOutRef: string;
  confirmationDepth: number;
}>;

/** Thrown by the stage derivation when the header is gone through a record's
 * removal and no terminal of ours stands. The orchestrator reads it as
 * thrown, unwrapped. */
export class FraudProofResolvedByAnotherRecordError extends Error {
  readonly _tag = "FraudProofResolvedByAnotherRecordError";

  constructor(readonly resolution: FraudProofResolvedByAnotherRecord) {
    super(
      `the header was removed in ${resolution.removalTxHash} against another record, ${resolution.referencedProofTokenOutRef}`,
    );
    this.name = "FraudProofResolvedByAnotherRecordError";
  }
}

export const isFraudProofResolvedByAnotherRecordError = (
  error: unknown,
): error is FraudProofResolvedByAnotherRecordError =>
  typeof error === "object" &&
  error !== null &&
  (error as { readonly _tag?: unknown })._tag ===
    "FraudProofResolvedByAnotherRecordError";

/**
 * Finds, in the snapshot's history, the transaction that removed the header
 * against a fraud-proof record of it, whoever's record and whatever family.
 * That transaction:
 *   - spends the header's state-queue node;
 *   - burns the node's unit and recreates none;
 *   - reads as a reference input an output at the fraud-proof address holding
 *     one fraud-proof token named for this header.
 * Only a conviction of this header mints such a token. A merge, or any other
 * exit from the queue, cites no record, so this returns undefined.
 */
export const recordBackedRemoval = ({
  snapshot,
  definition,
  stateUnit,
}: {
  readonly snapshot: FraudProofRawL1Snapshot;
  readonly definition: FraudProofRawL1TerminalDefinition;
  readonly stateUnit: string;
}): FraudProofResolvedByAnotherRecord | undefined => {
  const { policyId, address } = definition.proofToken;
  const namesHeader = (unit: string) =>
    unit.startsWith(policyId) &&
    unit.endsWith(definition.headerHash) &&
    unit.length ===
      policyId.length +
        definition.categoryId.length +
        definition.headerHash.length;
  for (const transaction of historicalTransactions({
    snapshot,
    unit: stateUnit,
  })) {
    const body = CML.TransactionBody.from_cbor_hex(transaction.bodyCbor);
    if (
      transaction.resolvedInputs.filter(
        (candidate) => outputQuantity(candidate, stateUnit) === 1n,
      ).length !== 1 ||
      mintQuantity(body, stateUnit) !== -1n ||
      bodyOutputsContainUnit(body, stateUnit)
    )
      continue;
    const record = transaction.resolvedReferenceInputs.find((candidate) => {
      const utxo = rawToUtxo(candidate);
      return (
        utxo.address === address &&
        Object.entries(utxo.assets).some(
          ([unit, quantity]) => quantity === 1n && namesHeader(unit),
        )
      );
    });
    if (record !== undefined)
      return {
        removalTxHash: transaction.txHash,
        referencedProofTokenOutRef: record.outRef,
        confirmationDepth: transaction.confirmationDepth,
      };
  }
  return undefined;
};

/**
 * Whether every signed attempt in the journal is settled: confirmed on
 * chain, or retired for good. An attempt that is only absent at the tip may
 * still land, so it is not settled. A rollback that reopens a confirmed
 * attempt (`reobserved`) makes it unsettled again.
 */
export const journalAttemptsSettled = (
  entries: readonly FraudProofWorkflowJournalEntry[],
): boolean => {
  const open = new Set<string>();
  for (const { event } of entries) {
    if (event.kind === "submission_intent" || event.kind === "reobserved")
      open.add(event.txHash);
    else if (
      event.kind === "confirmed" ||
      event.kind === "signed_attempt_retired" ||
      (event.kind === "reconciled" &&
        event.retirement !== undefined &&
        event.txHash !== undefined)
    )
      open.delete(event.txHash!);
  }
  return open.size === 0;
};

/** Whether this journal confirms the transaction that created the record a
 * removal cited. If it does not, a sibling run's record removed the header. */
export const journalCreatedRecord = (
  entries: readonly FraudProofWorkflowJournalEntry[],
  createdByTxHash: string,
): boolean =>
  entries.some(
    ({ event }) =>
      event.kind === "confirmed" && event.txHash === createdByTxHash,
  );
