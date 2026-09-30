import { Data as LucidData } from "@lucid-evolution/lucid";

import { DepositsDB, WithdrawalsDB } from "../database/index.js";
import {
  describeHeaderPlacement,
  missingOrderIsImmature,
  payloadDiff,
  placeHeader,
} from "./state-reconciliation.check-state-queue-journal.js";
import { type ReconciliationCheck } from "./state-reconciliation.compares.js";
import {
  type Context,
  finishCheck,
  JOURNAL_STATUS,
  newAccumulator,
  plural,
  skipCheck,
} from "./state-reconciliation.walk-merged-chain.js";

export const checkDeposits = (
  ctx: Context,
  allowInFlight: boolean,
): ReconciliationCheck => {
  if (ctx.l1 === null) {
    return skipCheck("deposits", `L1 unavailable: ${ctx.l1UnavailableReason}`);
  }
  const acc = newAccumulator();
  const sqlById = new Map(
    ctx.sql.deposits.map((row) => [row.payload.eventId, row]),
  );
  for (const order of ctx.l1.deposits) {
    if (order.payload === null) {
      acc.failures.push(
        `L1 deposit ${order.outRef} is undecodable: ${order.decodeError ?? "unknown"}`,
      );
      continue;
    }
    const row = sqlById.get(order.payload.eventId);
    const label = `deposit ${order.payload.eventId} (${order.outRef})`;
    if (row === undefined) {
      if (missingOrderIsImmature(ctx, order.payload.inclusionTimeMs)) {
        acc.inFlight.push(
          `${label} is on L1 but not yet ingested; its inclusion time is after every committed block`,
        );
      } else {
        acc.failures.push(`${label} is on L1 but unknown to SQL`);
      }
      continue;
    }
    const diff = payloadDiff(order.payload, row.payload);
    if (diff.length > 0) {
      acc.failures.push(
        `${label}: SQL payload differs from L1 in ${diff.join(", ")}`,
      );
    }
  }
  const l1EventIds = new Set(
    ctx.l1.deposits.flatMap((order) =>
      order.payload === null ? [] : [order.payload.eventId],
    ),
  );
  let assigned = 0;
  let unmerged = 0;
  for (const row of ctx.sql.deposits) {
    const label = `SQL deposit ${row.payload.eventId}`;
    const header = row.projectedHeaderHash;
    const placement = placeHeader(ctx, header);
    // Only a settlement, which the merge creates, lets a deposit order be
    // spent, so every deposit whose header is not merged keeps its order.
    if (placement !== "merged" && placement !== "unknown") {
      unmerged += 1;
      if (!l1EventIds.has(row.payload.eventId)) {
        if (missingOrderIsImmature(ctx, row.payload.inclusionTimeMs)) {
          acc.inFlight.push(
            `${label} is not among the L1 deposit orders; its inclusion time is after every committed block`,
          );
        } else {
          acc.failures.push(`${label} is not among the L1 deposit orders`);
        }
      }
    }
    if (header === null) continue;
    assigned += 1;
    if (placement === "unknown") {
      acc.failures.push(`${label}: ${describeHeaderPlacement(ctx, header)}`);
      continue;
    }
    if (placement === "active-pending") {
      acc.notes.push(
        `${label} is assigned to active journal ${header}, not on L1 yet`,
      );
    }
    // Header assignment happens with the deposit already projected into the
    // L2 ledger; the merge marks it consumed (an L2 spend may do so earlier).
    if (row.status === DepositsDB.Status.Awaiting) {
      acc.failures.push(
        `${label}: assigned to ${placement} header ${header} but status is ${row.status}`,
      );
    } else if (
      placement === "merged" &&
      row.status !== DepositsDB.Status.Consumed
    ) {
      const message = `${label}: header ${header} is merged but status is ${row.status}, expected consumed`;
      if (ctx.sqlMergeLag) {
        acc.inFlight.push(
          `${message} (SQL confirmed ledger has not applied the latest L1 merge)`,
        );
      } else {
        acc.failures.push(message);
      }
    }
  }
  return finishCheck(
    "deposits",
    acc,
    allowInFlight,
    `${plural(ctx.l1.deposits.length, "L1 deposit order")} match SQL; L1 orders cover ${plural(unmerged, "unmerged SQL deposit")}; ${plural(assigned, "SQL deposit")} with a header assignment point at on-chain or merged headers`,
  );
};

export const checkWithdrawals = (
  ctx: Context,
  allowInFlight: boolean,
): ReconciliationCheck => {
  if (ctx.l1 === null) {
    return skipCheck(
      "withdrawals",
      `L1 unavailable: ${ctx.l1UnavailableReason}`,
    );
  }
  const acc = newAccumulator();
  const sqlById = new Map(
    ctx.sql.withdrawals.map((row) => [row.payload.eventId, row]),
  );
  for (const order of ctx.l1.withdrawals) {
    if (order.payload === null) {
      acc.failures.push(
        `L1 withdrawal ${order.outRef} is undecodable: ${order.decodeError ?? "unknown"}`,
      );
      continue;
    }
    const row = sqlById.get(order.payload.eventId);
    const label = `withdrawal ${order.payload.eventId} (${order.outRef})`;
    if (row === undefined) {
      if (missingOrderIsImmature(ctx, order.payload.inclusionTimeMs)) {
        acc.inFlight.push(
          `${label} is on L1 but not yet ingested; its inclusion time is after every committed block`,
        );
      } else {
        acc.failures.push(`${label} is on L1 but unknown to SQL`);
      }
      continue;
    }
    const diff = payloadDiff(order.payload, row.payload);
    if (diff.length > 0) {
      acc.failures.push(
        `${label}: SQL payload differs from L1 in ${diff.join(", ")}`,
      );
    }
  }
  let assigned = 0;
  for (const row of ctx.sql.withdrawals) {
    const label = `SQL withdrawal ${row.payload.eventId}`;
    const header = row.projectedHeaderHash;
    const placement = placeHeader(ctx, header);
    if (header === null) {
      if (row.status === WithdrawalsDB.Status.Finalized) {
        acc.failures.push(
          `${label}: status is finalized without a header assignment`,
        );
      }
      continue;
    }
    assigned += 1;
    if (placement === "unknown") {
      acc.failures.push(`${label}: ${describeHeaderPlacement(ctx, header)}`);
      continue;
    }
    if (placement === "active-pending") {
      acc.notes.push(
        `${label} is assigned to active journal ${header}, not on L1 yet`,
      );
    }
    // Local finalization of the block (and the merge) marks its withdrawals
    // finalized; before that they are projected.
    const journalFinalized =
      ctx.journals.get(header)?.status === JOURNAL_STATUS.Finalized;
    if (row.status === WithdrawalsDB.Status.Awaiting) {
      acc.failures.push(
        `${label}: assigned to ${placement} header ${header} but status is ${row.status}`,
      );
    } else if (
      journalFinalized &&
      row.status !== WithdrawalsDB.Status.Finalized
    ) {
      acc.failures.push(
        `${label}: journal ${header} is finalized but the withdrawal status is ${row.status}, expected finalized`,
      );
    } else if (
      placement === "merged" &&
      row.status !== WithdrawalsDB.Status.Finalized
    ) {
      const message = `${label}: header ${header} is merged but status is ${row.status}, expected finalized`;
      if (ctx.sqlMergeLag) {
        acc.inFlight.push(
          `${message} (SQL confirmed ledger has not applied the latest L1 merge)`,
        );
      } else {
        acc.failures.push(message);
      }
    }
  }
  return finishCheck(
    "withdrawals",
    acc,
    allowInFlight,
    `${plural(ctx.l1.withdrawals.length, "L1 withdrawal order")} match SQL (payload and l2_value); ${plural(assigned, "SQL withdrawal")} with a header assignment point at on-chain or merged headers`,
  );
};

export const canonicalDataCbor = (hex: string, schema: unknown): string =>
  LucidData.to(LucidData.from(hex, schema as never), schema as never);
