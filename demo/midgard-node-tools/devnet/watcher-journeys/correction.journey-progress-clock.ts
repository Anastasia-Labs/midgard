import {
  confirmedValidationTraceStages,
  type FraudProofWorkflowJournalEntry,
} from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import { CML, coreToTxOutput, Data } from "@lucid-evolution/lucid";

import type { JourneyTransactions } from "./correction.verify-journey-correction.js";

/**
 * The chain time (POSIX ms) until which the workflow legitimately waits on
 * its counterparty, when its journal shows such a wait; undefined otherwise.
 * It is read from durable journal and chain facts only, so a resumed runner
 * reads the same deadline.
 */
export type JourneyCounterpartyWait = (
  records: readonly FraudProofWorkflowJournalEntry[],
) => Promise<number | undefined>;

/**
 * The defense deadline of an open, undefended validation-trace case of ours.
 *
 * The workflow's last move before the wait is its VerifySource: that
 * transaction creates the case, and nothing of ours can move on the case
 * again until its defense deadline, `started_at + C + L_d`
 * (validation-case-v1 `defense_deadline`). The case is waiting while the
 * journal's latest confirmed stage is that VerifySource and the journal
 * records no submission intent after the confirmation. `started_at` is read
 * from the case output of the confirmed VerifySource the journal names, so
 * the deadline is the chain's, not this runner's.
 */
const undefendedCaseDeadline = ({
  defenseWindowMs,
  transactions,
}: {
  defenseWindowMs: number;
  transactions: JourneyTransactions;
}): JourneyCounterpartyWait => {
  const startedAt = new Map<string, Promise<number>>();
  const readStartedAt = async (
    claim: Readonly<{ outRef: string; address: string; unit: string }>,
  ) => {
    const [txHash, index] = claim.outRef.split("#");
    const { cbor } = await transactions.transaction(txHash!);
    const transaction = CML.Transaction.from_cbor_hex(cbor);
    if (CML.hash_transaction(transaction.body()).to_hex() !== txHash)
      throw new Error(`The bytes read for ${txHash!} are another transaction`);
    const outputs = transaction.body().outputs();
    const position = Number(index);
    if (position >= outputs.len())
      throw new Error(`VerifySource ${txHash!} has no case output ${index!}`);
    const output = coreToTxOutput(outputs.get(position));
    if (
      output.address !== claim.address ||
      output.assets[claim.unit] !== 1n ||
      typeof output.datum !== "string"
    )
      throw new Error(
        `VerifySource output ${claim.outRef} is not the case the journal claims`,
      );
    return Number(Data.from(output.datum, SDK.Case).started_at);
  };
  return async (records) => {
    const last = confirmedValidationTraceStages(records).at(-1);
    if (last?.action.stage !== "verify_source") return undefined;
    let movedSince = false;
    for (const { event } of records)
      if (event.kind === "confirmed" && event.txHash === last.txHash)
        movedSince = false;
      else if (event.kind === "submission_intent") movedSince = true;
    if (movedSince) return undefined;
    const claim = last.outputs.find(({ role }) => role === "case");
    if (claim === undefined)
      throw new Error(`VerifySource ${last.txHash} journaled no case output`);
    let read = startedAt.get(claim.outRef);
    if (read === undefined) {
      read = readStartedAt(claim);
      startedAt.set(claim.outRef, read);
      read.catch(() => startedAt.delete(claim.outRef));
    }
    return (await read) + defenseWindowMs;
  };
};

/**
 * The journey's progress clock for a family's timing: one transaction's
 * allowance between durable journal records, and, for a plan that waits out
 * an undefended case, that case's defense deadline (from the deployment's
 * own C + L_d) during which the journal may stay silent.
 */
export const journeyProgressClock = ({
  timing,
  transactions,
}: {
  timing: Readonly<{
    transactionAllowanceMs: number;
    allowances: Readonly<{ undefendedCaseMs: number }>;
  }>;
  transactions: JourneyTransactions;
}): Readonly<{
  progressAllowanceMs: number;
  counterpartyWait?: JourneyCounterpartyWait;
}> =>
  timing.allowances.undefendedCaseMs === 0
    ? { progressAllowanceMs: timing.transactionAllowanceMs }
    : {
        progressAllowanceMs: timing.transactionAllowanceMs,
        counterpartyWait: undefendedCaseDeadline({
          defenseWindowMs: timing.allowances.undefendedCaseMs,
          transactions,
        }),
      };
