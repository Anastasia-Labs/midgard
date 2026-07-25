import fs from "node:fs";
import path from "node:path";

import {
  cardanoTxBytesToMidgardNativeTxCanonicalCborV1,
  computeMidgardNativeTxIdV1,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
} from "@al-ft/midgard-core/codec";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { extractStateQueueErrorCode } from "@/commands/listen-response.js";
import {
  diagnoseMissingBlockTxs,
  preflightDecodeBlockTxs,
} from "@/transactions/state-queue/merge-to-confirmed-state.js";

type TxFixture = {
  readonly cborHex: string;
};

const txFixture = JSON.parse(
  fs.readFileSync(path.resolve(__dirname, "./txs/txs_0.json"), "utf8"),
)[0] as TxFixture;

describe("merge error code extraction", () => {
  it("prefers error_code from StateQueueError cause payload", () => {
    const error = new SDK.StateQueueError({
      message: "merge failed",
      cause: {
        error_code: "E_MERGE_UPLC_EVAL_FAILED",
        details: "remote eval rejected",
      },
    });

    expect(extractStateQueueErrorCode(error)).toBe("E_MERGE_UPLC_EVAL_FAILED");
  });

  it("falls back to message prefix when cause has no error_code", () => {
    const error = new SDK.StateQueueError({
      message: "E_MERGE_LAYOUT_DERIVATION_FAILED: could not derive redeemers",
      cause: "missing redeemer index mapping",
    });

    expect(extractStateQueueErrorCode(error)).toBe(
      "E_MERGE_LAYOUT_DERIVATION_FAILED",
    );
  });

  it("returns undefined when neither cause nor message provides a merge code", () => {
    const error = new SDK.StateQueueError({
      message: "merge failed",
      cause: "unknown",
    });

    expect(extractStateQueueErrorCode(error)).toBeUndefined();
  });
});

describe("diagnoseMissingBlockTxs", () => {
  it("accepts blocks with no indexed native tx payloads", () => {
    expect(diagnoseMissingBlockTxs(0, 0)).toBeUndefined();
  });

  it("flags partial ImmutableDB resolution", () => {
    expect(diagnoseMissingBlockTxs(10, 7)).toEqual({
      reason: "IMMUTABLE_DB_TX_LOOKUP_INCOMPLETE",
      txHashesFound: 10,
      txsResolved: 7,
    });
  });

  it("accepts complete tx linkage", () => {
    expect(diagnoseMissingBlockTxs(5, 5)).toBeUndefined();
  });
});

describe("preflightDecodeBlockTxs", () => {
  /**
   * Converts a transaction fixture into a native transaction accepted by the merge tests.
   */
  const toValidNativeTx = () => {
    const nativeTxCbor = cardanoTxBytesToMidgardNativeTxCanonicalCborV1(
      Buffer.from(txFixture.cborHex, "hex"),
    );
    const txId = computeMidgardNativeTxIdV1(
      decodeMidgardNativeTxFullV1FromCanonicalCbor(nativeTxCbor),
    );
    return { txId, txCbor: nativeTxCbor };
  };

  it("fails when a block tx payload is malformed", () => {
    const malformed = {
      txId: Buffer.alloc(32, 7),
      txCbor: Buffer.alloc(64, 1),
    };
    const result = Effect.runSync(
      Effect.either(preflightDecodeBlockTxs([malformed])),
    );
    const txIdHex = malformed.txId.toString("hex");
    expect(result).toHaveProperty("_tag", "Left");
    expect(result).toHaveProperty("left.reason", "DECODE_FAILED");
    expect(result).toHaveProperty("left.txIdHex", txIdHex);
  });

  it("fails when payload tx_id does not match BlocksDB tx_id", () => {
    const valid = toValidNativeTx();
    const mismatchedTxId = Buffer.from(valid.txId);
    mismatchedTxId[0] ^= 0xff;
    const result = Effect.runSync(
      Effect.either(
        preflightDecodeBlockTxs([
          {
            txId: mismatchedTxId,
            txCbor: valid.txCbor,
          },
        ]),
      ),
    );
    const txIdHex = mismatchedTxId.toString("hex");
    const decodedTxIdHex = valid.txId.toString("hex");
    expect(result).toHaveProperty("_tag", "Left");
    expect(result).toHaveProperty("left.reason", "TX_ID_MISMATCH");
    expect(result).toHaveProperty("left.txIdHex", txIdHex);
    expect(result).toHaveProperty("left.decodedTxIdHex", decodedTxIdHex);
  });

  it("accepts decodable tx payloads with matching tx_id", () => {
    const valid = toValidNativeTx();
    const result = Effect.runSync(
      preflightDecodeBlockTxs([{ txId: valid.txId, txCbor: valid.txCbor }]),
    );
    expect(result).toHaveLength(1);
    expect(result[0].txId.equals(valid.txId)).toBe(true);
  });
});
