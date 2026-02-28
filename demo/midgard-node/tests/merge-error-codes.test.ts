import { describe, expect, it } from "vitest";
import * as SDK from "@al-ft/midgard-sdk";
import fs from "node:fs";
import path from "node:path";
import { Effect } from "effect";
import { extractStateQueueErrorCode } from "@/commands/listen.js";
import {
  diagnoseMissingBlockTxs,
  preflightDecodeBlockTxs,
} from "@/transactions/state-queue/merge-to-confirmed-state.js";
import {
  cardanoTxBytesToMidgardNativeTxFullBytes,
  computeMidgardNativeTxIdFromFull,
  decodeMidgardNativeTxFull,
} from "@/midgard-tx-codec/index.js";

type TxFixture = {
  readonly cborHex: string;
  readonly txId: string;
};

const fixturePath = path.resolve(__dirname, "./txs/txs_0.json");
const txFixture = (
  JSON.parse(fs.readFileSync(fixturePath, "utf8")) as readonly TxFixture[]
)[0];

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
  it("flags missing BlocksDB links", () => {
    expect(diagnoseMissingBlockTxs(0, 0)).toEqual({
      reason: "NO_BLOCKS_DB_TX_HASHES",
      txHashesFound: 0,
      txsResolved: 0,
    });
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
  const toValidNativeTx = () => {
    const nativeTxCbor = cardanoTxBytesToMidgardNativeTxFullBytes(
      Buffer.from(txFixture.cborHex, "hex"),
    );
    const txId = computeMidgardNativeTxIdFromFull(
      decodeMidgardNativeTxFull(nativeTxCbor),
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
    expect(result._tag).toBe("Left");
    if (result._tag === "Left") {
      expect(result.left.reason).toBe("DECODE_FAILED");
      expect(result.left.txIdHex).toBe(malformed.txId.toString("hex"));
    }
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
    expect(result._tag).toBe("Left");
    if (result._tag === "Left") {
      expect(result.left.reason).toBe("TX_ID_MISMATCH");
      expect(result.left.txIdHex).toBe(mismatchedTxId.toString("hex"));
      expect(result.left.decodedTxIdHex).toBe(valid.txId.toString("hex"));
    }
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
