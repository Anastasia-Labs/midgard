import type { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  commitUserEventSourceIdSetsAreExact,
  refreshCommitUserEventSourcesThroughBlockEnd,
} from "../src/workers/commit-block-header/submission.js";
import {
  ingestFollowerViewUnowned,
  modelHorizonLag,
} from "./helpers/follower-view.js";
import { provideDatabaseLayers, resetApplicationTables } from "./utils.js";

const run = <A, E>(effect: Effect.Effect<A, E, SqlClient.SqlClient>) =>
  Effect.runPromise(provideDatabaseLayers(effect));

const exactSources = {
  pendingDepositIds: ["deposit-a"],
  includedDepositIds: ["deposit-a"],
  pendingForcedTransactionIds: ["forced-a"],
  includedForcedTransactionIds: ["forced-a"],
  pendingWithdrawalIds: ["withdrawal-a"],
  includedWithdrawalIds: ["withdrawal-a"],
} as const;

describe("commit source completeness", () => {
  // Deposits and withdrawals are the follower-change driver's (E-N1-2 item 1):
  // the final refresh polls tx orders only, and only after an ingestion.
  it("refreshes only the tx-order source through the exact finalized end, after a follower ingestion", async () => {
    const blockEndTimeMs = Date.parse("2026-01-01T00:07:00.999Z");
    const calls: string[] = [];
    const refresh = refreshCommitUserEventSourcesThroughBlockEnd(
      blockEndTimeMs,
      modelHorizonLag(0),
      {
        txOrder: (upperBound: Date) =>
          Effect.sync(() => {
            calls.push(`tx-order:${upperBound.getTime().toString()}`);
            return upperBound;
          }),
      },
    );
    await run(resetApplicationTables);
    await expect(run(refresh)).rejects.toThrow(
      /exceeds the ingested event horizon/,
    );
    expect(calls).toEqual([]);
    await run(ingestFollowerViewUnowned(100));
    await run(refresh);
    expect(calls).toEqual([`tx-order:${blockEndTimeMs.toString()}`]);
  });

  it("accepts the exact due source sets independent of ordering", () => {
    expect(
      commitUserEventSourceIdSetsAreExact({
        ...exactSources,
        pendingDepositIds: ["deposit-b", "deposit-a"],
        includedDepositIds: ["deposit-a", "deposit-b"],
      }),
    ).toBe(true);
  });

  it("rejects a source that becomes due through the finalized header end", () => {
    expect(
      commitUserEventSourceIdSetsAreExact({
        ...exactSources,
        pendingWithdrawalIds: ["withdrawal-a", "withdrawal-late"],
      }),
    ).toBe(false);
  });

  it("rejects replacement and duplicate source identities", () => {
    expect(
      commitUserEventSourceIdSetsAreExact({
        ...exactSources,
        pendingForcedTransactionIds: ["forced-b"],
      }),
    ).toBe(false);
    expect(
      commitUserEventSourceIdSetsAreExact({
        ...exactSources,
        pendingDepositIds: ["deposit-a", "deposit-a"],
        includedDepositIds: ["deposit-a", "deposit-a"],
      }),
    ).toBe(false);
  });
});
