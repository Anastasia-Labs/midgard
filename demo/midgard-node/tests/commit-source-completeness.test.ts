import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  commitUserEventSourceIdSetsAreExact,
  refreshCommitUserEventSourcesThroughBlockEnd,
} from "../src/workers/commit-block-header/submission.js";

const exactSources = {
  pendingDepositIds: ["deposit-a"],
  includedDepositIds: ["deposit-a"],
  pendingForcedTransactionIds: ["forced-a"],
  includedForcedTransactionIds: ["forced-a"],
  pendingWithdrawalIds: ["withdrawal-a"],
  includedWithdrawalIds: ["withdrawal-a"],
} as const;

describe("commit source completeness", () => {
  it("refreshes deposit, withdrawal, and tx-order sources in order through the exact finalized end", async () => {
    const blockEndTimeMs = Date.parse("2026-01-01T00:07:00.999Z");
    const calls: string[] = [];
    const record =
      (label: string) =>
      (upperBound: Date): Effect.Effect<Date> =>
        Effect.sync(() => {
          calls.push(`${label}:${upperBound.getTime().toString()}`);
          return upperBound;
        });

    await Effect.runPromise(
      refreshCommitUserEventSourcesThroughBlockEnd(blockEndTimeMs, {
        deposit: record("deposit"),
        withdrawal: record("withdrawal"),
        txOrder: record("tx-order"),
      }),
    );

    expect(calls).toEqual([
      `deposit:${blockEndTimeMs.toString()}`,
      `withdrawal:${blockEndTimeMs.toString()}`,
      `tx-order:${blockEndTimeMs.toString()}`,
    ]);
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
