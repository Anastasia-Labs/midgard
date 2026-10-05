import "./utils.js";

import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { DepositsDB } from "../src/database/index.js";
import {
  assessRetainedForeignTipWindows,
  reconcileOverdueAwaitingEventsAgainstRetainedForeignTips,
  resolveT2ForeignEventEvidence,
  type T2CandidateEventIds,
} from "../src/workers/t2-foreign-event-reconciliation.js";
import { makeDepositEntry } from "./database.test/fixtures.make-deposit-submission-attempt.js";
import {
  depositStatus,
  headerFor,
  IN_WINDOW,
  indexDeposit,
  INGESTED_PAST_WINDOW,
  nonEmptyWindowHeader,
  oneDepositPayload,
  onNode,
  reconciliationRow,
  recordForeignTip,
  storeForeignDa,
  WINDOW_END_MS,
  WINDOW_START_MS,
} from "./foreign-tip-gate.fixtures.js";

const emptyIds = (): T2CandidateEventIds => ({
  deposits: [],
  forcedTransactions: [],
  withdrawals: [],
});

const resolve = async ({
  header,
  candidateIds,
  payload,
}: {
  readonly header: SDK.Header;
  readonly candidateIds: T2CandidateEventIds;
  readonly payload?: SDK.DaPayload;
}) =>
  Effect.runPromise(
    SDK.hashBlockHeader(header).pipe(
      Effect.flatMap((foreignHeaderHash) =>
        resolveT2ForeignEventEvidence({
          foreignHeaderHash,
          header,
          candidateIds,
          payload,
        }),
      ),
    ),
  );

describe("T2 foreign event reconciliation evidence", () => {
  it("proves candidate absence from empty category roots without DA", async () => {
    const header = headerFor();
    const candidateIds = {
      deposits: ["aa".repeat(32)],
      forcedTransactions: ["bb".repeat(32)],
      withdrawals: ["cc".repeat(32)],
    };
    await expect(
      resolve({
        header,
        candidateIds,
      }),
    ).resolves.toEqual({
      type: "Ready",
      absent: candidateIds,
    });
  });

  it("awaits DA for a non-empty category root", async () => {
    const header = headerFor({
      depositsRoot: "33".repeat(32),
      depositCount: 1n,
      totalEventCount: 1n,
    });
    const result = await resolve({
      header,
      candidateIds: { ...emptyIds(), deposits: ["aa".repeat(32)] },
    });
    expect(result.type).toBe("AwaitingForeignDa");
    if (result.type === "AwaitingForeignDa")
      expect(result.reason).toBe("missing");
  });

  it("rejects an empty category root with a nonzero count before any local event is visible", async () => {
    const header = headerFor({
      depositCount: 1n,
      totalEventCount: 1n,
    });
    const result = await resolve({
      header,
      candidateIds: emptyIds(),
    });
    expect(result.type).toBe("AwaitingForeignDa");
    if (result.type === "AwaitingForeignDa") {
      expect(result.reason).toBe("invalid");
      expect(result.detail).toBe(
        "foreign header event root/count evidence is inconsistent",
      );
    }
  });

  it("rejects DA whose header binding or roots are invalid", async () => {
    const { header, payload } = await oneDepositPayload("aa".repeat(32));
    const result = await resolve({
      header,
      candidateIds: { ...emptyIds(), deposits: ["bb".repeat(32)] },
      payload: {
        ...payload,
        block_body: { ...payload.block_body, header_hash: "ff".repeat(28) },
      },
    });
    expect(result.type).toBe("AwaitingForeignDa");
    if (result.type === "AwaitingForeignDa")
      expect(result.reason).toBe("invalid");
  });

  it("defers foreign-present events and releases proven-absent events", async () => {
    const presentId = "aa".repeat(32);
    const absentId = "bb".repeat(32);
    const { header, payload } = await oneDepositPayload(presentId);
    const present = await resolve({
      header,
      candidateIds: { ...emptyIds(), deposits: [presentId] },
      payload,
    });
    expect(present.type).toBe("AwaitingForeignDa");
    if (present.type === "AwaitingForeignDa") {
      expect(present.reason).toBe(
        "foreign_event_present_requires_finalization",
      );
      expect(present.present.deposits).toEqual([presentId]);
    }
    await expect(
      resolve({
        header,
        candidateIds: { ...emptyIds(), deposits: [absentId] },
        payload,
      }),
    ).resolves.toEqual({
      type: "Ready",
      absent: { ...emptyIds(), deposits: [absentId] },
    });
  });
});

const gate = (eventsIngestedThrough: Date = INGESTED_PAST_WINDOW) =>
  reconcileOverdueAwaitingEventsAgainstRetainedForeignTips({
    eventsIngestedThrough,
  });

describe("retained foreign-tip commit gate", () => {
  it("commits past an unverified foreign block whose own window holds no event of the next block", async () => {
    const result = await onNode(
      Effect.gen(function* () {
        const hash = yield* recordForeignTip(nonEmptyWindowHeader());
        // (start, end] is open at the start: these two lie outside it.
        yield* indexDeposit({
          [DepositsDB.Columns.INCLUSION_TIME]: new Date(WINDOW_START_MS),
        });
        yield* indexDeposit({
          [DepositsDB.Columns.INCLUSION_TIME]: new Date(WINDOW_END_MS + 1),
        });
        return {
          gate: yield* gate(),
          row: yield* reconciliationRow(hash),
        };
      }),
    );
    expect(result.gate.type).toBe("Ready");
    // Never marked resolved without verified DA.
    expect(result.row?.status).toBe("awaiting");
    expect(result.row?.evidence_kind).toBe("pending_v1");
  });

  it("still refuses while an event the next block would carry lies inside the window", async () => {
    for (const status of [
      DepositsDB.Status.Awaiting,
      DepositsDB.Status.Projected,
    ]) {
      const result = await onNode(
        Effect.gen(function* () {
          const hash = yield* recordForeignTip(nonEmptyWindowHeader());
          yield* indexDeposit({
            [DepositsDB.Columns.INCLUSION_TIME]: IN_WINDOW,
            [DepositsDB.Columns.STATUS]: status,
          });
          return { hash, gate: yield* gate() };
        }),
      );
      expect(result.gate).toMatchObject({
        type: "AwaitingForeignDa",
        foreignHeaderHash: result.hash,
        reason: "missing",
      });
    }
  });

  it("waits until the window is ingested, proceeds once, and refuses again when a late event lands in it", async () => {
    const result = await onNode(
      Effect.gen(function* () {
        yield* recordForeignTip(nonEmptyWindowHeader());
        const notYetIngested = yield* gate(new Date(WINDOW_END_MS - 1));
        const ingested = yield* gate();
        const ingestedAgain = yield* gate();
        yield* indexDeposit({ [DepositsDB.Columns.INCLUSION_TIME]: IN_WINDOW });
        const late = yield* gate();
        return { notYetIngested, ingested, ingestedAgain, late };
      }),
    );
    expect(result.notYetIngested.type).toBe("AwaitingForeignDa");
    if (result.notYetIngested.type === "AwaitingForeignDa")
      expect(result.notYetIngested.detail).toContain(
        "gate=window_not_yet_ingested",
      );
    expect(result.ingested).toEqual({
      type: "Ready",
      absent: { deposits: [], forcedTransactions: [], withdrawals: [] },
    });
    expect(result.ingestedAgain).toEqual(result.ingested);
    expect(result.late.type).toBe("AwaitingForeignDa");
    if (result.late.type === "AwaitingForeignDa")
      expect(result.late.detail).toContain("gate=pending_event_in_window");
  });

  it("defers until verified DA arrives, releases an absent event exactly once, and keeps refusing a present one", async () => {
    const window = {
      startTime: BigInt(WINDOW_START_MS),
      endTime: BigInt(WINDOW_END_MS),
    };
    const result = await onNode(
      Effect.gen(function* () {
        const absentId = yield* indexDeposit({
          [DepositsDB.Columns.INCLUSION_TIME]: IN_WINDOW,
        });
        const presentEntry = makeDepositEntry({
          [DepositsDB.Columns.INCLUSION_TIME]: IN_WINDOW,
        });
        const presentId = presentEntry[DepositsDB.Columns.ID].toString("hex");
        const { header, payload } = yield* Effect.promise(() =>
          oneDepositPayload(presentId, window),
        );
        const hash = yield* recordForeignTip(header);
        const beforeDa = yield* gate();
        yield* storeForeignDa(header, payload);
        const released = yield* gate();
        const releasedStatus = yield* depositStatus(absentId);
        const again = yield* gate();
        const resolvedRow = yield* reconciliationRow(hash);
        yield* DepositsDB.insertEntries([presentEntry]);
        const present = yield* gate();
        const presentAgain = yield* gate();
        return {
          absentId,
          beforeDa,
          released,
          releasedStatus,
          again,
          resolvedRow,
          present,
          presentAgain,
        };
      }),
    );
    expect(result.beforeDa).toMatchObject({
      type: "AwaitingForeignDa",
      reason: "missing",
    });
    expect(result.released).toEqual({
      type: "Ready",
      absent: {
        deposits: [result.absentId],
        forcedTransactions: [],
        withdrawals: [],
      },
    });
    expect(result.releasedStatus).toBe(DepositsDB.Status.Projected);
    expect(result.again).toEqual({
      type: "Ready",
      absent: { deposits: [], forcedTransactions: [], withdrawals: [] },
    });
    expect(result.resolvedRow?.evidence_kind).toBe("verified_da_v1");
    for (const refusal of [result.present, result.presentAgain]) {
      expect(refusal).toMatchObject({
        type: "AwaitingForeignDa",
        reason: "foreign_event_present_requires_finalization",
      });
    }
  });

  it("gates a speculative build read-only on the same windows", async () => {
    const result = await onNode(
      Effect.gen(function* () {
        const hash = yield* recordForeignTip(nonEmptyWindowHeader());
        const empty = yield* assessRetainedForeignTipWindows({
          eventsIngestedThrough: INGESTED_PAST_WINDOW,
        });
        const eventId = yield* indexDeposit({
          [DepositsDB.Columns.INCLUSION_TIME]: IN_WINDOW,
        });
        const before = yield* reconciliationRow(hash);
        const occupied = yield* assessRetainedForeignTipWindows({
          eventsIngestedThrough: INGESTED_PAST_WINDOW,
        });
        return {
          hash,
          empty,
          occupied,
          before,
          after: yield* reconciliationRow(hash),
          status: yield* depositStatus(eventId),
        };
      }),
    );
    expect(result.empty.type).toBe("Ready");
    expect(result.occupied).toMatchObject({
      type: "AwaitingForeignDa",
      foreignHeaderHash: result.hash,
      reason: "replay_required",
    });
    expect(result.after).toEqual(result.before);
    expect(result.status).toBe(DepositsDB.Status.Awaiting);
  });
});
