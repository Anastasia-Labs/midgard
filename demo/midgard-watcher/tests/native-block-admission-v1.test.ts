import { readFile } from "node:fs/promises";

import { describe, expect, it } from "vitest";

import { admitWatcherNativeRollForwardBlockV1 } from "../src/native-block-admission-v1.js";
import {
  WATCHER_NATIVE_CHAIN_SYNC_V1_SCHEMA_VERSION,
  type WatcherNativeChainSyncRollForwardV1,
} from "../src/native-chain-sync-v1.js";

const FIXTURE_METADATA = Object.freeze({
  blockHash: "27807a70215e3e018eec9be8c619c692e06a78ebcb63daf90d7abe823f3bbf47",
  blockNo: "12069665",
  blockType: "7",
  prevHash: "ff51732269af51a2efaa2a7ad4a2ff5647af5629013a446511249e837be617a0",
  slot: "159835207",
});

const fixtureEvent = async (): Promise<WatcherNativeChainSyncRollForwardV1> => {
  const rawBlockCbor = (
    await readFile(
      new URL("./support/conway-block.hex", import.meta.url),
      "utf8",
    )
  ).trim();
  return Object.freeze({
    ...FIXTURE_METADATA,
    kind: "roll_forward",
    rawBlockCbor,
    schemaVersion: WATCHER_NATIVE_CHAIN_SYNC_V1_SCHEMA_VERSION,
    tip: Object.freeze({
      blockHash: FIXTURE_METADATA.blockHash,
      blockNo: FIXTURE_METADATA.blockNo,
      kind: "point",
      slot: FIXTURE_METADATA.slot,
    }),
  });
};

describe("native block admission", () => {
  it("independently derives the era, header identity, ancestry, height and ordered transaction ids", async () => {
    const admitted = admitWatcherNativeRollForwardBlockV1(await fixtureEvent());
    expect(admitted).toMatchObject({
      ...FIXTURE_METADATA,
      protocolMajor: "10",
      rawHeaderCbor: expect.stringMatching(/^[0-9a-f]+$/u),
      schemaVersion: "midgard-watcher-native-block-admission-v1",
    });
    expect(admitted.transactionIds).toHaveLength(8);
    expect(admitted.transactionCbors).toHaveLength(8);
    expect(new Set(admitted.transactionIds).size).toBe(8);
    expect(admitted.transactionIds).toEqual(
      expect.arrayContaining([expect.stringMatching(/^[0-9a-f]{64}$/u)]),
    );
  });

  it.each([
    ["blockType", "8"],
    ["blockHash", "00".repeat(32)],
    ["prevHash", "00".repeat(32)],
    ["slot", "159835208"],
    ["blockNo", "12069666"],
  ] as const)("rejects forged helper %s metadata", async (field, value) => {
    const event = await fixtureEvent();
    expect(() =>
      admitWatcherNativeRollForwardBlockV1({ ...event, [field]: value }),
    ).toThrow("native chain-sync block admission failed");
  });

  it("rejects substituted raw block bytes even when all metadata claims are retained", async () => {
    const event = await fixtureEvent();
    expect(() =>
      admitWatcherNativeRollForwardBlockV1({
        ...event,
        rawBlockCbor: `${event.rawBlockCbor.slice(0, -2)}00`,
      }),
    ).toThrow("native chain-sync block admission failed");
  });
});
