import { readFile } from "node:fs/promises";

import {
  computeFraudProofRawL1PointId,
  LOCAL_KUPMIOS_RAW_BLOCK_AT_POINT,
  type LocalKupmiosRawBlockAtPoint,
} from "@al-ft/midgard-fault-proofs";
import { describe, expect, it } from "vitest";

import {
  assertWatcherLocalKupmiosNativeObservation,
  unsafeAssertNativeKupmiosAgreementForTest,
  type WatcherLocalKupmiosNativeObservation,
} from "../../src/l1/local-kupmios-native-observation.js";
import { admitWatcherNativeRollForwardBlock } from "../../src/l1/native-block-admission.js";
import {
  WATCHER_NATIVE_CHAIN_SYNC_SCHEMA_VERSION,
  type WatcherNativeChainSyncRollForward,
} from "../../src/l1/native-chain-sync.js";

const METADATA = Object.freeze({
  blockHash: "27807a70215e3e018eec9be8c619c692e06a78ebcb63daf90d7abe823f3bbf47",
  blockNo: "12069665",
  blockType: "7",
  prevHash: "ff51732269af51a2efaa2a7ad4a2ff5647af5629013a446511249e837be617a0",
  slot: "159835207",
});

const fixture = async () => {
  const event: WatcherNativeChainSyncRollForward = Object.freeze({
    ...METADATA,
    kind: "roll_forward",
    rawBlockCbor: (
      await readFile(
        new URL("../support/conway-block.hex", import.meta.url),
        "utf8",
      )
    ).trim(),
    schemaVersion: WATCHER_NATIVE_CHAIN_SYNC_SCHEMA_VERSION,
    tip: Object.freeze({
      blockHash: METADATA.blockHash,
      blockNo: METADATA.blockNo,
      kind: "point" as const,
      slot: METADATA.slot,
    }),
  });
  const native = admitWatcherNativeRollForwardBlock(event);
  const point = Object.freeze({
    blockHash: native.blockHash,
    blockNo: native.blockNo,
    slot: native.slot,
    pointId: computeFraudProofRawL1PointId({
      blockHash: native.blockHash,
      blockNo: native.blockNo,
      slot: native.slot,
    }),
  });
  const raw: LocalKupmiosRawBlockAtPoint = Object.freeze({
    schemaVersion: LOCAL_KUPMIOS_RAW_BLOCK_AT_POINT,
    sourceId: "fixture",
    point,
    parentBlockHash: native.prevHash.length === 0 ? null : native.prevHash,
    kupoCheckpoint: Object.freeze({
      slot: Number(native.slot),
      blockHash: native.blockHash,
    }),
    transactions: Object.freeze(
      native.transactionIds.map((txHash, index) =>
        Object.freeze({
          txHash,
          transactionCbor: native.transactionCbors[index]!,
        }),
      ),
    ),
  });
  return { native, raw };
};

describe("native Kupo/Ogmios agreement", () => {
  it("rejects a structural local observation that the live source did not admit", async () => {
    const { native } = await fixture();
    expect(() =>
      assertWatcherLocalKupmiosNativeObservation(
        Object.freeze({}) as WatcherLocalKupmiosNativeObservation,
        native,
      ),
    ).toThrow("is not admitted for the native block");
  });

  it("accepts one exact point and ordered transaction-CBOR vector", async () => {
    const { native, raw } = await fixture();
    expect(() =>
      unsafeAssertNativeKupmiosAgreementForTest(native, raw),
    ).not.toThrow();
  });

  it.each(["point", "checkpoint", "tx_hash", "tx_cbor", "tx_order"] as const)(
    "rejects hostile %s substitution",
    async (attack) => {
      const { native, raw } = await fixture();
      const transactions = raw.transactions.map((transaction) => ({
        ...transaction,
      }));
      let candidate: LocalKupmiosRawBlockAtPoint;
      if (attack === "point") {
        candidate = {
          ...raw,
          point: { ...raw.point, blockNo: "12069666" },
        };
      } else if (attack === "checkpoint") {
        candidate = {
          ...raw,
          kupoCheckpoint: { ...raw.kupoCheckpoint, slot: 159835208 },
        };
      } else {
        if (attack === "tx_hash") transactions[0]!.txHash = "00".repeat(32);
        if (attack === "tx_cbor") transactions[0]!.transactionCbor = "80";
        if (attack === "tx_order") {
          [transactions[0], transactions[1]] = [
            transactions[1]!,
            transactions[0]!,
          ];
        }
        candidate = { ...raw, transactions };
      }
      expect(() =>
        unsafeAssertNativeKupmiosAgreementForTest(native, candidate),
      ).toThrow(
        "local Kupo/Ogmios observation differs from the native chain-sync block",
      );
    },
  );
});
