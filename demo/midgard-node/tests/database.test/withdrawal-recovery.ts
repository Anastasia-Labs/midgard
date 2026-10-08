import { SqlClient } from "@effect/sql";
import { it } from "@effect/vitest";
import { Effect, Option } from "effect";
import { describe, expect } from "vitest";

import { WithdrawalsDB } from "../../src/database/index.js";
import { resolveIncludedWithdrawalEntriesForWindow } from "../../src/mpf/event-window.js";
import { isolatedDb, makeHistoryWithdrawalEntry } from "./fixtures.js";

/** A withdrawal reopened from `header`, as a disposed block's journal
 * leaves it (`disposeJournals`): unclassified, awaiting, its revision
 * bumped. */
const reopenFrom = (id: Buffer, header: Buffer) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`UPDATE withdrawal_utxos d SET status = 'awaiting',
      projected_header_hash = NULL, reopened_from_header_hash = ${header},
      validity = NULL, validity_detail = '{}'::jsonb,
      settlement_event_info = NULL,
      classification_revision = d.classification_revision + 1,
      updated_at = NOW()
      WHERE d.event_id = ${id}`;
  });

export const registerWithdrawalRecoveryTests = () => {
  describe("withdrawal correction classification recovery", () => {
    it.effect(
      "reclassifies corrected overdue events, fences stale candidates, and restores exact classifications",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            yield* WithdrawalsDB.clear;
            const entry = makeHistoryWithdrawalEntry();
            const id = entry[WithdrawalsDB.Columns.ID];
            const header = Buffer.alloc(28, 91);
            const original = {
              eventId: id,
              expectedClassificationRevision: 0,
              settlementEventInfo: Buffer.from("8101", "hex"),
              validity: WithdrawalsDB.Validity.WithdrawalIsValid,
              validityDetail: { checked: { z: 1, a: 2 } },
            };
            yield* WithdrawalsDB.insertEntries([entry]);
            yield* WithdrawalsDB.setSettlementInfoForEventIds([original]);
            yield* WithdrawalsDB.markAwaitingAsProjected([original]);
            yield* WithdrawalsDB.markProjectedByEventIds([original], header);
            yield* WithdrawalsDB.markFinalizedByEventIds([id], header);
            yield* reopenFrom(id, header);
            let row = Option.getOrThrow(
              yield* WithdrawalsDB.retrieveByEventId(id),
            );
            expect(row[WithdrawalsDB.Columns.RAW_EVENT_INFO]).toEqual(
              entry[WithdrawalsDB.Columns.RAW_EVENT_INFO],
            );
            expect(row[WithdrawalsDB.Columns.SETTLEMENT_EVENT_INFO]).toBeNull();
            expect(row[WithdrawalsDB.Columns.VALIDITY]).toBeNull();
            expect(row[WithdrawalsDB.Columns.CLASSIFICATION_REVISION]).toBe(1);
            expect(
              row[WithdrawalsDB.Columns.REOPENED_FROM_HEADER_HASH],
            ).toEqual(header);
            const selected = yield* resolveIncludedWithdrawalEntriesForWindow({
              currentBlockStartTime: new Date("2026-04-14"),
              effectiveEndTime: new Date("2026-04-15"),
            });
            expect(
              selected.map((item) => item[WithdrawalsDB.Columns.ID]),
            ).toEqual([id]);
            expect(
              (yield* Effect.either(
                WithdrawalsDB.setSettlementInfoForEventIds([original]),
              ))._tag,
            ).toBe("Left");
            expect(
              (yield* Effect.either(
                WithdrawalsDB.markAwaitingAsProjected([original]),
              ))._tag,
            ).toBe("Left");
            const replacement = {
              ...original,
              expectedClassificationRevision: 1,
              settlementEventInfo: Buffer.from("8102", "hex"),
              validity: WithdrawalsDB.Validity.NonExistentWithdrawalUtxo,
              validityDetail: { absent: true },
            };
            yield* WithdrawalsDB.setSettlementInfoForEventIds([replacement]);
            yield* WithdrawalsDB.markAwaitingAsProjected([replacement]);
            expect(
              (yield* Effect.either(
                WithdrawalsDB.markProjectedByEventIds([original], header),
              ))._tag,
            ).toBe("Left");
            yield* WithdrawalsDB.restoreCorrectedClassification(
              [original],
              header,
            );
            yield* WithdrawalsDB.markFinalizedByEventIds([id], header);
            row = Option.getOrThrow(yield* WithdrawalsDB.retrieveByEventId(id));
            // Exact already-assigned reobservation remains idempotent after restoration.
            yield* WithdrawalsDB.markProjectedByEventIds([original], header);
            expect(
              (yield* Effect.either(
                WithdrawalsDB.markProjectedByEventIds([replacement], header),
              ))._tag,
            ).toBe("Left");
            expect(row[WithdrawalsDB.Columns.CLASSIFICATION_REVISION]).toBe(2);
            expect(row[WithdrawalsDB.Columns.SETTLEMENT_EVENT_INFO]).toEqual(
              original.settlementEventInfo,
            );
            expect(row[WithdrawalsDB.Columns.VALIDITY_DETAIL]).toEqual(
              original.validityDetail,
            );
            expect(row[WithdrawalsDB.Columns.PROJECTED_HEADER_HASH]).toEqual(
              header,
            );
            expect(
              (yield* Effect.either(
                WithdrawalsDB.setSettlementInfoForEventIds([replacement]),
              ))._tag,
            ).toBe("Left");
          }),
        ),
    );

    it.effect(
      "refuses ordinary overdue events and refuses restoration over a replacement header",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            yield* WithdrawalsDB.clear;
            const entry = makeHistoryWithdrawalEntry();
            const id = entry[WithdrawalsDB.Columns.ID];
            const header = Buffer.alloc(28, 92);
            const nextHeader = Buffer.alloc(28, 93);
            const assignment = {
              eventId: id,
              expectedClassificationRevision: 0,
              settlementEventInfo: Buffer.from("8101", "hex"),
              validity: WithdrawalsDB.Validity.WithdrawalIsValid,
              validityDetail: {},
            };
            yield* WithdrawalsDB.insertEntries([entry]);
            expect(
              (yield* Effect.either(
                resolveIncludedWithdrawalEntriesForWindow({
                  currentBlockStartTime: new Date("2026-04-14"),
                  effectiveEndTime: new Date("2026-04-15"),
                }),
              ))._tag,
            ).toBe("Left");
            yield* WithdrawalsDB.setSettlementInfoForEventIds([assignment]);
            yield* WithdrawalsDB.markAwaitingAsProjected([assignment]);
            yield* WithdrawalsDB.markProjectedByEventIds([assignment], header);
            yield* reopenFrom(id, header);
            const next = { ...assignment, expectedClassificationRevision: 1 };
            yield* WithdrawalsDB.setSettlementInfoForEventIds([next]);
            yield* WithdrawalsDB.markAwaitingAsProjected([next]);
            yield* WithdrawalsDB.markProjectedByEventIds([next], nextHeader);
            expect(
              (yield* Effect.either(
                WithdrawalsDB.restoreCorrectedClassification(
                  [assignment],
                  header,
                ),
              ))._tag,
            ).toBe("Left");
            const row = Option.getOrThrow(
              yield* WithdrawalsDB.retrieveByEventId(id),
            );
            expect(row[WithdrawalsDB.Columns.PROJECTED_HEADER_HASH]).toEqual(
              nextHeader,
            );
            expect(row[WithdrawalsDB.Columns.CLASSIFICATION_REVISION]).toBe(1);
          }),
        ),
    );
  });
};
