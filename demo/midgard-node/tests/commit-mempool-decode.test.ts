import { describe, expect } from "vitest";
import { Effect } from "effect";
import { it } from "@effect/vitest";
import {
  COMMIT_REJECT_CODE_DECODE_FAILED,
  resolveTxDeltaForCommit,
} from "@/workers/utils/mpt.js";
import * as Tx from "@/database/utils/tx.js";
import * as Ledger from "@/database/utils/ledger.js";

describe("resolveTxDeltaForCommit", () => {
  it.effect(
    "marks malformed mempool tx as rejected instead of throwing",
    (_) =>
      Effect.gen(function* () {
        const txId = Buffer.alloc(32, 7);
        const entry: Tx.EntryWithTimeStamp = {
          [Tx.Columns.TX_ID]: txId,
          [Tx.Columns.TX]: Buffer.alloc(64, 1),
          [Tx.Columns.TIMESTAMPTZ]: new Date(0),
        };

        const result = yield* resolveTxDeltaForCommit(entry, undefined);
        expect(result._tag).toBe("Rejected");
        if (result._tag === "Rejected") {
          expect(result.rejection.tx_id.equals(txId)).toBe(true);
          expect(result.rejection.reject_code).toBe(
            COMMIT_REJECT_CODE_DECODE_FAILED,
          );
          expect(result.rejection.reject_detail).toContain(
            "Failed to decode Midgard-native tx payload",
          );
        }
      }),
  );

  it.effect("uses existing tx delta when available", (_) =>
    Effect.gen(function* () {
      const txId = Buffer.alloc(32, 9);
      const entry: Tx.EntryWithTimeStamp = {
        [Tx.Columns.TX_ID]: txId,
        [Tx.Columns.TX]: Buffer.alloc(8, 3),
        [Tx.Columns.TIMESTAMPTZ]: new Date(0),
      };
      const existingDelta = {
        txId,
        spent: [Buffer.from("01", "hex"), Buffer.from("02", "hex")],
        produced: [
          {
            [Ledger.Columns.OUTREF]: Buffer.from("0a", "hex"),
            [Ledger.Columns.OUTPUT]: Buffer.from("0b", "hex"),
          },
        ],
      } as const;

      const result = yield* resolveTxDeltaForCommit(entry, existingDelta);
      expect(result._tag).toBe("Decoded");
      if (result._tag === "Decoded") {
        expect(result.spent).toStrictEqual(existingDelta.spent);
        expect(result.produced).toStrictEqual(existingDelta.produced);
      }
    }),
  );
});
