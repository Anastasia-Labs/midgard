import { SqlError } from "@effect/sql";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { DatabaseError } from "../src/database/utils/common.js";
import {
  KupoNotYetIndexed,
  L1SourceUnavailable,
  OgmiosRequestTimeout,
} from "../src/l1-source-unavailable.js";
import { isRecoverableHistorySourceFailure } from "../src/services/event-history-owner.source-failure.js";
import { HistoryRecoverySuperseded } from "../src/services/event-history-recovery.js";

const sql = (code: string) =>
  new DatabaseError({
    table: "event_history_authority",
    message: "Failed to renew history authority",
    cause: new SqlError.SqlError({
      message: "query failed",
      cause: Object.assign(new Error("postgres"), { code }),
    }),
  });
// What the owner's Runtime.runPromise rejects with.
const fiberFailure = (error: unknown) =>
  Effect.runPromise(Effect.fail(error)).catch((cause: unknown) => cause);
// The authority's own refusals carry no SQL cause.
const authority = (message: string) =>
  new DatabaseError({
    table: "event_history_authority",
    message,
    cause: undefined,
  });

describe("history owner failure classification", () => {
  it.each([
    ["socket loss", new L1SourceUnavailable("Ogmios chain-sync socket closed")],
    ["request deadline", new OgmiosRequestTimeout("did not answer")],
    ["index lag", new KupoNotYetIndexed("Kupo has no match")],
    ["recovery fence", new HistoryRecoverySuperseded({ message: "moved" })],
    ["deadline", new DOMException("timed out", "TimeoutError")],
    ["connection closed", sql("CONNECTION_CLOSED")],
    ["connect timeout", sql("CONNECT_TIMEOUT")],
    ["refused", sql("ECONNREFUSED")],
    ["connection failure", sql("08006")],
    ["admin shutdown", sql("57P01")],
    ["too many connections", sql("53300")],
    ["statement timeout", sql("57014")],
    ["serialization", sql("40001")],
  ])("reconnects after %s", async (_label, error) => {
    expect(isRecoverableHistorySourceFailure(error)).toBe(true);
    expect(isRecoverableHistorySourceFailure(await fiberFailure(error))).toBe(
      true,
    );
  });

  it.each([
    [
      "another owner",
      authority("History authority generation or owner changed"),
    ],
    ["lease expired", authority("History authority lease expired")],
    ["suspended", authority("Suspended authority requires revalidation")],
    ["unique violation", sql("23505")],
    [
      "missing intersection",
      new Error('Ogmios chain-sync error: {"code":1000}'),
    ],
    ["genesis mismatch", new Error("History source genesis differs")],
    [
      "broken ancestry",
      new Error("History ChainSync forward breaks retained ancestry"),
    ],
    ["own abort", new DOMException("aborted", "AbortError")],
    ["non-error", "unavailable"],
  ])("stops after %s", async (_label, error) => {
    expect(isRecoverableHistorySourceFailure(error)).toBe(false);
    expect(isRecoverableHistorySourceFailure(await fiberFailure(error))).toBe(
      false,
    );
  });
});
