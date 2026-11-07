import { Effect } from "effect";

import {
  DepositsDB,
  ForcedTransactionsDB,
  WithdrawalsDB,
} from "../database/index.js";
import type {
  CommitEventInstant,
  CommitEventLimits,
} from "./utils/commit-block-planner.event-window.js";

/** Each kind returns cap + 1 complete instants, so memory is bounded independently of outage duration. */
export const pendingCommitEventInstants = (
  endTime: Date,
  limits: CommitEventLimits,
) =>
  Effect.gen(function* () {
    const results = yield* Effect.all(
      [
        DepositsDB.retrievePendingHeaderInstantCounts(
          endTime,
          limits.deposits + 1,
        ),
        ForcedTransactionsDB.retrievePendingHeaderInstantCounts(
          endTime,
          limits.forcedTransactions + 1,
        ),
        WithdrawalsDB.retrievePendingHeaderInstantCounts(
          endTime,
          limits.withdrawals + 1,
        ),
      ],
      { concurrency: "unbounded" },
    );
    const groups = new Map<
      number,
      {
        inclusionTimeMs: number;
        deposits: number;
        forcedTransactions: number;
        withdrawals: number;
      }
    >();
    const kinds = ["deposits", "forcedTransactions", "withdrawals"] as const;
    for (const [index, rows] of results.entries())
      for (const row of rows) {
        const group = groups.get(row.inclusionTimeMs) ?? {
          inclusionTimeMs: row.inclusionTimeMs,
          deposits: 0,
          forcedTransactions: 0,
          withdrawals: 0,
        };
        group[kinds[index]!] = row.count;
        groups.set(row.inclusionTimeMs, group);
      }
    return [...groups.values()].sort(
      (a, b) => a.inclusionTimeMs - b.inclusionTimeMs,
    ) satisfies CommitEventInstant[];
  });
