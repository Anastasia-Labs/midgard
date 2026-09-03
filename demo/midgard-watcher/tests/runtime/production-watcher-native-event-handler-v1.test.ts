import { describe, expect, it, vi } from "vitest";

import { WATCHER_NATIVE_CHAIN_SYNC_V1_SCHEMA_VERSION } from "../../src/l1/native-chain-sync-v1.js";
import type { WatcherProductionOperationsSinkV1 } from "../../src/runtime/production-operations-observability-v1.js";
import { createWatcherProductionNativeEventHandlerV1 } from "../../src/runtime/production-watcher-runtime-v1.js";

const hash = (byte: string): string => byte.repeat(64);

describe("watcher production native event delivery V1", () => {
  it("delivers each roll-forward and rollback event exactly once", async () => {
    const handle = vi.fn(async () => undefined);
    const onCaughtUp = vi.fn();
    const recordL1Source = vi.fn();
    const setAlert = vi.fn();
    const operationsSink = Object.freeze({
      recordL1Source,
      setAlert,
    }) as unknown as WatcherProductionOperationsSinkV1;
    const onEvent = createWatcherProductionNativeEventHandlerV1({
      coordinator: Promise.resolve({ handle }),
      onCaughtUp,
      operationsSink,
      sourceIdentityDigest: hash("d"),
      nowMs: () => 1_000n,
    });
    const rollForward = Object.freeze({
      schemaVersion: WATCHER_NATIVE_CHAIN_SYNC_V1_SCHEMA_VERSION,
      kind: "roll_forward" as const,
      blockHash: hash("a"),
      blockType: "conway",
      prevHash: hash("b"),
      slot: "101",
      blockNo: "51",
      rawBlockCbor: "80",
      tip: Object.freeze({
        kind: "point" as const,
        blockHash: hash("a"),
        slot: "101",
        blockNo: "51",
      }),
    });
    const rollback = Object.freeze({
      schemaVersion: WATCHER_NATIVE_CHAIN_SYNC_V1_SCHEMA_VERSION,
      kind: "roll_backward" as const,
      point: Object.freeze({
        kind: "point" as const,
        blockHash: hash("c"),
        slot: "99",
      }),
      tip: Object.freeze({
        kind: "point" as const,
        blockHash: hash("c"),
        slot: "99",
        blockNo: "49",
      }),
    });

    await onEvent(rollForward);
    await onEvent(rollback);

    expect(handle).toHaveBeenCalledTimes(2);
    expect(handle.mock.calls).toEqual([[rollForward], [rollback]]);
    expect(onCaughtUp).toHaveBeenCalledTimes(2);
    expect(recordL1Source).toHaveBeenCalledTimes(2);
    expect(recordL1Source.mock.calls).toEqual([
      [
        {
          sourceIdentityDigest: hash("d"),
          sourceMode: "local_node",
          status: "consistent",
          blockHash: hash("a"),
          blockNo: "51",
          slot: "101",
          observedAtMs: "1000",
        },
      ],
      [
        {
          sourceIdentityDigest: hash("d"),
          sourceMode: "local_node",
          status: "consistent",
          blockHash: hash("c"),
          blockNo: "49",
          slot: "99",
          observedAtMs: "1000",
        },
      ],
    ]);
    expect(setAlert.mock.calls).toEqual([
      [
        {
          code: "chain_rollback",
          subjectDigest: hash("d"),
          active: false,
          observedAtMs: "1000",
        },
      ],
      [
        {
          code: "chain_rollback",
          subjectDigest: hash("d"),
          active: true,
          observedAtMs: "1000",
        },
      ],
    ]);
  });
});
