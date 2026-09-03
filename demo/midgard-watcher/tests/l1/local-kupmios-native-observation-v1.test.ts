import { readFile } from "node:fs/promises";

import {
  computeFraudProofRawL1PointIdV1,
  LOCAL_KUPMIOS_RAW_BLOCK_AT_POINT_V1,
  type LocalKupmiosRawBlockAtPointV1,
} from "@al-ft/midgard-fault-proofs";
import { describe, expect, it } from "vitest";

import {
  assertWatcherLocalKupmiosNativeObservationV1,
  unsafeAssertNativeKupmiosAgreementForTest,
  type WatcherLocalKupmiosNativeObservationV1,
} from "../../src/l1/local-kupmios-native-observation-v1.js";
import { admitWatcherNativeRollForwardBlockV1 } from "../../src/l1/native-block-admission-v1.js";
import {
  WATCHER_NATIVE_CHAIN_SYNC_V1_SCHEMA_VERSION,
  type WatcherNativeChainSyncRollForwardV1,
} from "../../src/l1/native-chain-sync-v1.js";

const METADATA = Object.freeze({
  blockHash: "27807a70215e3e018eec9be8c619c692e06a78ebcb63daf90d7abe823f3bbf47",
  blockNo: "12069665",
  blockType: "7",
  prevHash: "ff51732269af51a2efaa2a7ad4a2ff5647af5629013a446511249e837be617a0",
  slot: "159835207",
});

const fixture = async () => {
  const event: WatcherNativeChainSyncRollForwardV1 = Object.freeze({
    ...METADATA,
    kind: "roll_forward",
    rawBlockCbor: (
      await readFile(
        new URL("../support/conway-block.hex", import.meta.url),
        "utf8",
      )
    ).trim(),
    schemaVersion: WATCHER_NATIVE_CHAIN_SYNC_V1_SCHEMA_VERSION,
    tip: Object.freeze({
      blockHash: METADATA.blockHash,
      blockNo: METADATA.blockNo,
      kind: "point" as const,
      slot: METADATA.slot,
    }),
  });
  const native = admitWatcherNativeRollForwardBlockV1(event);
  const point = Object.freeze({
    blockHash: native.blockHash,
    blockNo: native.blockNo,
    slot: native.slot,
    pointId: computeFraudProofRawL1PointIdV1({
      blockHash: native.blockHash,
      blockNo: native.blockNo,
      slot: native.slot,
    }),
  });
  const raw: LocalKupmiosRawBlockAtPointV1 = Object.freeze({
    schemaVersion: LOCAL_KUPMIOS_RAW_BLOCK_AT_POINT_V1,
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
      assertWatcherLocalKupmiosNativeObservationV1(
        Object.freeze({}) as WatcherLocalKupmiosNativeObservationV1,
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
      let candidate: LocalKupmiosRawBlockAtPointV1;
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
