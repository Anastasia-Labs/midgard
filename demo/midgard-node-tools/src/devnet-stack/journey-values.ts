import { USER_ROLES, type UserRole } from "./identities.js";

export const ADA = 1_000_000n;

/** Asset quantities by unit (`lovelace`, or policy id + asset name hex). */
export type Value = Record<string, bigint>;

export const addValues = (...values: readonly Value[]): Value => {
  const total: Value = {};
  for (const value of values)
    for (const [unit, amount] of Object.entries(value))
      total[unit] = (total[unit] ?? 0n) + amount;
  return Object.fromEntries(
    Object.entries(total).filter(([, amount]) => amount !== 0n),
  );
};

export const negate = (value: Value): Value =>
  Object.fromEntries(
    Object.entries(value).map(([unit, amount]) => [unit, -amount]),
  );

export const sameValue = (left: Value, right: Value) =>
  Object.keys(addValues(left, negate(right))).length === 0;

export const toValue = (raw: unknown): Value =>
  Object.fromEntries(
    Object.entries((raw ?? {}) as Record<string, unknown>).map(
      ([unit, amount]) => [unit, BigInt(String(amount))],
    ),
  );

export const jsonValue = (value: Value) =>
  Object.fromEntries(
    Object.entries(value).map(([unit, amount]) => [unit, amount.toString()]),
  );

export type Json = Record<string, unknown>;

export type DepositRecord = {
  readonly user: UserRole;
  readonly value: Record<string, string>;
  readonly txHash: string;
  readonly eventId: string;
};
export type TransferRecord = {
  readonly from: UserRole;
  readonly to: UserRole;
  readonly value: Record<string, string>;
  readonly txId: string;
  readonly selectedInputs: readonly string[];
  /** Absent only on records written before transfers had one. */
  readonly submissionId?: string;
  /**
   * The sender outputs the transfer was told never to spend
   * (`--exclude-out-ref`). Absent on records written before it existed,
   * which excluded nothing.
   */
  readonly excludeOutRefs?: readonly string[];
};
export type WithdrawalRecord = {
  readonly user: UserRole;
  readonly l2OutRef: string;
  readonly l1Address: string;
  readonly txHash: string;
  readonly withdrawalEventId: string;
  readonly l2Value: Record<string, string>;
};

export type L2Utxo = { readonly outRef: string; readonly value: Value };

/**
 * What keeps `/pipeline-status` from being drained, one entry per unfinished
 * kind of work: admitted, queued, uncommitted, unmerged or unfinalized local
 * work, and unfinished settlement jobs (named with their last error).
 */
export const drainResidue = (status: Json): string[] => {
  const residue = status.localResidue as Json | undefined;
  const queue = status.stateQueue as Json | undefined;
  const admission = status.durableAdmission as Json | undefined;
  const pending = status.pendingBlockFinalizations as Json | undefined;
  const jobs = status.localMutationJobs as Json | undefined;
  const settlement = status.settlement as Json | undefined;
  const problems: string[] = [];
  const expect = (what: string, actual: unknown, ok: boolean) => {
    if (!ok) problems.push(`${what} is ${JSON.stringify(actual) ?? "missing"}`);
  };
  expect(
    "durableAdmission.backlog",
    admission?.backlog,
    String(admission?.backlog) === "0",
  );
  expect(
    "localResidue.mempoolTxCount",
    residue?.mempoolTxCount,
    String(residue?.mempoolTxCount) === "0",
  );
  expect(
    "localResidue.processedMempoolTxCount",
    residue?.processedMempoolTxCount,
    String(residue?.processedMempoolTxCount) === "0",
  );
  expect(
    "stateQueue.queueLength",
    queue?.queueLength,
    Number(queue?.queueLength) === 0,
  );
  expect(
    "stateQueue.unconfirmedSubmittedBlockTxHash",
    queue?.unconfirmedSubmittedBlockTxHash,
    queue?.unconfirmedSubmittedBlockTxHash === null,
  );
  expect(
    "stateQueue.localFinalizationPending",
    queue?.localFinalizationPending,
    queue?.localFinalizationPending === false,
  );
  expect(
    "pendingBlockFinalizations.oldestActive",
    pending?.oldestActive,
    pending?.oldestActive === null,
  );
  expect(
    "localMutationJobs.unfinished",
    jobs?.unfinished,
    String(jobs?.unfinished) === "0",
  );
  if (String(settlement?.unfinishedJobs) !== "0")
    problems.push(
      `settlement.unfinishedJobs is ${JSON.stringify(settlement?.unfinishedJobs) ?? "missing"}${(
        (settlement?.failingJobs ?? []) as Json[]
      )
        .map(
          (job) =>
            `; ${String(job.kind)} ${String(job.eventId)} ${String(job.phase)} failed ${String(job.failures)} times: ${String(job.lastError)}`,
        )
        .join("")}`,
    );
  return problems;
};

/** No local work and no settlement job left: see drainResidue. */
export const drained = (status: Json) => drainResidue(status).length === 0;

/**
 * Each user's L2 holdings implied by the journaled activity. Native assets
 * are exact; lovelace is exact up to the fees the user paid as a sender,
 * bounded per transfer.
 */
export const expectedHoldings = (
  deposits: readonly DepositRecord[],
  transfers: readonly TransferRecord[],
  withdrawals: readonly WithdrawalRecord[],
  feeBoundPerTransfer = 2n * ADA,
) => {
  const holdings = Object.fromEntries(
    USER_ROLES.map((user) => [user, {}]),
  ) as Record<UserRole, Value>;
  const feeBound = Object.fromEntries(
    USER_ROLES.map((user) => [user, 0n]),
  ) as Record<UserRole, bigint>;
  for (const deposit of deposits)
    holdings[deposit.user] = addValues(
      holdings[deposit.user],
      toValue(deposit.value),
    );
  for (const transfer of transfers) {
    holdings[transfer.from] = addValues(
      holdings[transfer.from],
      negate(toValue(transfer.value)),
    );
    holdings[transfer.to] = addValues(
      holdings[transfer.to],
      toValue(transfer.value),
    );
    feeBound[transfer.from] += feeBoundPerTransfer;
  }
  for (const withdrawal of withdrawals)
    holdings[withdrawal.user] = addValues(
      holdings[withdrawal.user],
      negate(toValue(withdrawal.l2Value)),
    );
  return { holdings, feeBound };
};
