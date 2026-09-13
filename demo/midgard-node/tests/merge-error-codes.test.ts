import fs from "node:fs";
import path from "node:path";

import {
  cardanoTxBytesToMidgardNativeTxCanonicalCbor,
  computeMidgardNativeTxId,
  decodeMidgardNativeTxFullFromCanonicalCbor,
} from "@al-ft/midgard-core/codec";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { extractStateQueueErrorCode } from "../src/commands/listen-response.js";
import {
  diagnoseMissingBlockTxs,
  preflightDecodeBlockTxs,
  synchronizeCommitMpfAfterConfirmedMerge,
} from "../src/transactions/state-queue/merge-to-confirmed-state.js";

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
    const nativeTxCbor = cardanoTxBytesToMidgardNativeTxCanonicalCbor(
      Buffer.from(txFixture.cborHex, "hex"),
    );
    const txId = computeMidgardNativeTxId(
      decodeMidgardNativeTxFullFromCanonicalCbor(nativeTxCbor),
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

describe("confirmed merge MPF synchronization", () => {
  const ownerDiagnostics = (durableRoot: string) => ({
    ownerEpoch: Buffer.alloc(16),
    durableRoot,
    residentNodes: 1,
    residentEdges: 0,
    residentBytes: 64,
    activeGenerations: 0,
    generatedNodes: 0,
    generatedBytes: 0,
    rssBytes: 1024,
    peakRssBytes: 1024,
    childRestarts: 0,
  });

  it("uses the live Architecture G owner without running persistent-store synchronization", async () => {
    const durableRoot = "11".repeat(32);
    const confirmedLedgerRoot = "22".repeat(32);
    let ownerCalls = 0;
    let persistentStoreCalls = 0;

    const result = await Effect.runPromise(
      synchronizeCommitMpfAfterConfirmedMerge({
        mpfEngine: "architecture_g",
        nativeMpfOwner: {
          diagnostics: async () => {
            ownerCalls += 1;
            return ownerDiagnostics(durableRoot);
          },
        },
        confirmedLedgerEntryCount: 7,
        confirmedLedgerRoot,
        synchronizePersistentStores: Effect.sync(() => {
          persistentStoreCalls += 1;
          return {
            ledgerEntryCount: 7,
            ledgerRoot: confirmedLedgerRoot,
            transactionsRoot: "33".repeat(32),
          };
        }),
      }),
    );

    expect(ownerCalls).toBe(1);
    expect(persistentStoreCalls).toBe(0);
    expect(result).toEqual({
      mode: "architecture_g_owner",
      confirmedLedgerEntryCount: 7,
      confirmedLedgerRoot,
      durableLedgerRoot: durableRoot,
      activeGenerations: 0,
    });
  });

  it("fails closed when the Architecture G owner is missing", async () => {
    await expect(
      Effect.runPromise(
        synchronizeCommitMpfAfterConfirmedMerge({
          mpfEngine: "architecture_g",
          nativeMpfOwner: undefined,
          confirmedLedgerEntryCount: 1,
          confirmedLedgerRoot: "11".repeat(32),
          synchronizePersistentStores: Effect.succeed({
            ledgerEntryCount: 1,
            ledgerRoot: "11".repeat(32),
            transactionsRoot: "22".repeat(32),
          }),
        }),
      ),
    ).rejects.toThrow("Architecture G native owner is not initialized");
  });

  it("fails closed on a malformed Architecture G durable root", async () => {
    await expect(
      Effect.runPromise(
        synchronizeCommitMpfAfterConfirmedMerge({
          mpfEngine: "architecture_g",
          nativeMpfOwner: {
            diagnostics: async () => ownerDiagnostics("not-a-root"),
          },
          confirmedLedgerEntryCount: 1,
          confirmedLedgerRoot: "11".repeat(32),
          synchronizePersistentStores: Effect.die(
            "persistent synchronization must remain unevaluated",
          ),
        }),
      ),
    ).rejects.toThrow("Architecture G durable root");
  });

  it("preserves persistent-store synchronization for other MPF engines", async () => {
    let persistentStoreCalls = 0;
    const result = await Effect.runPromise(
      synchronizeCommitMpfAfterConfirmedMerge({
        mpfEngine: "overlay",
        nativeMpfOwner: undefined,
        confirmedLedgerEntryCount: 3,
        confirmedLedgerRoot: "11".repeat(32),
        synchronizePersistentStores: Effect.sync(() => {
          persistentStoreCalls += 1;
          return {
            ledgerEntryCount: 3,
            ledgerRoot: "11".repeat(32),
            transactionsRoot: "22".repeat(32),
          };
        }),
      }),
    );

    expect(persistentStoreCalls).toBe(1);
    expect(result).toEqual({
      mode: "persistent_stores",
      ledgerEntryCount: 3,
      ledgerRoot: "11".repeat(32),
      transactionsRoot: "22".repeat(32),
    });
  });
});
