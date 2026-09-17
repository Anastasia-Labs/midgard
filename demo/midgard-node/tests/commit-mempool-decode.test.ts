import {
  cardanoTxBytesToMidgardNativeTxCanonicalCbor,
  computeMidgardNativeTxId,
  decodeMidgardNativeTxFullFromCanonicalCbor,
} from "@al-ft/midgard-core/codec";
import { it } from "@effect/vitest";
import { Effect, Metric } from "effect";
import { describe, expect } from "vitest";

import * as Ledger from "../src/database/utils/ledger.js";
import * as Tx from "../src/database/utils/tx.js";
import {
  COMMIT_REJECT_CODE_DECODE_FAILED,
  commitTxDeltaFallbackDecodedCounter,
  resolveTxDeltaForCommit,
} from "../src/mpf/index.js";
import { makeCardanoSignedMapOutputTxBytes } from "./helpers/cardano-native-fixtures.js";

describe("resolveTxDeltaForCommit", () => {
  it.effect("positively decodes valid canonical CBOR after a cache miss", (_) =>
    Effect.gen(function* () {
      const txCbor = cardanoTxBytesToMidgardNativeTxCanonicalCbor(
        makeCardanoSignedMapOutputTxBytes(),
      );
      const txId = computeMidgardNativeTxId(
        decodeMidgardNativeTxFullFromCanonicalCbor(txCbor),
      );
      const before = yield* Metric.value(commitTxDeltaFallbackDecodedCounter);
      const result = yield* resolveTxDeltaForCommit(
        {
          [Tx.Columns.TX_ID]: txId,
          [Tx.Columns.TX]: txCbor,
          [Tx.Columns.TIMESTAMPTZ]: new Date(0),
        },
        undefined,
      );
      const after = yield* Metric.value(commitTxDeltaFallbackDecodedCounter);

      expect(result._tag).toBe("Decoded");
      if (result._tag === "Decoded") {
        expect(result.spent.length).toBeGreaterThan(0);
        expect(result.produced.length).toBeGreaterThan(0);
      }
      // The MPF loop owns the production counter increment. This direct
      // resolver proof must not manufacture a cache-fallback observation.
      expect(after.count).toBe(before.count);
    }),
  );

  it.effect("marks malformed mempool tx as rejected instead of throwing", (_) =>
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
