import "node:crypto";
import "node:url";
import "vitest";
import "../../src/l1/l1-adapter.js";
import "../../src/l1/native-chain-sync.js";
import "../../src/runtime/config.js";
import "../../src/storage/durable-store.js";
import "./native-chain-sync.config.js";
import "./native-chain-sync.fake-transport.js";

import { createHash } from "node:crypto";

import { describe, expect, it } from "vitest";

import {
  closeWatcherL1TransportAttestationContext,
  establishWatcherLocalNodeAuthorityTransport,
  watcherL1TransportAttestationDetails,
} from "../../src/l1/l1-adapter.js";
import {
  parseWatcherNativeChainSyncEvent,
  readWatcherNativeChainSyncEventReceipt,
  startWatcherNativeChainSync,
  startWatcherNativeChainSyncWithRetry,
  WATCHER_NATIVE_CHAIN_SYNC_SCHEMA_VERSION,
  watcherNativeChainSyncAuthorityDetails,
  type WatcherNativeChainSyncEvent,
  type WatcherNativeChainSyncEventReceipt,
  watcherNativeChainSyncEventReceipt,
} from "../../src/l1/native-chain-sync.js";
import { watcherCanonicalJson } from "../../src/storage/durable-store.js";
import {
  config,
  GENESIS,
  GENESIS_CONFIG_BYTES,
  GENESIS_CONFIG_PATH,
  INTERSECTION,
  NODE_CONFIG_BYTES,
  NODE_CONFIG_PATH,
  readIdentityFixture,
  waitFor,
} from "./native-chain-sync.config.js";
import {
  fakeNodeTransport,
  start,
} from "./native-chain-sync.fake-transport.js";

describe("native Cardano node-to-client chain-sync supervisor", () => {
  it.each(["matching", "magic", "zeroTime", "slotLength", "hardFork"])(
    "binds a custom chain clock and magic to its actual genesis: %s",
    async (variant) => {
      const customNetwork = {
        networkMagic: 424242,
        slotConfig: { zeroTime: 1789041600000, zeroSlot: 0, slotLength: 1000 },
      };
      const genesisBytes = new TextEncoder().encode(
        JSON.stringify({
          networkMagic: variant === "magic" ? 424243 : 424242,
          networkId: "Testnet",
          systemStart: new Date(
            customNetwork.slotConfig.zeroTime +
              (variant === "zeroTime" ? 1000 : 0),
          ).toISOString(),
          slotLength: variant === "slotLength" ? 2 : 1,
        }),
      );
      const nodeBytes = new TextEncoder().encode(
        JSON.stringify({
          ShelleyGenesisFile: GENESIS_CONFIG_PATH,
          TestShelleyHardForkAtEpoch: 0,
          TestConwayHardForkAtEpoch: variant === "hardFork" ? 1 : 0,
        }),
      );
      const base = config();
      if (base.l1.source.sourceMode !== "local_node")
        throw new Error("local fixture required");
      const genesisIdentitySha256 = createHash("sha256")
        .update(genesisBytes)
        .digest("hex");
      const transport = await fakeNodeTransport("honest", {
        magic: customNetwork.networkMagic,
      });
      const pending = startWatcherNativeChainSync({
        binaryPath: transport.binaryPath,
        watcherConfig: {
          ...base,
          targetNetwork: "Custom",
          customNetwork,
          l1: {
            ...base.l1,
            source: {
              ...base.l1.source,
              chainSync: { ...base.l1.source.chainSync, genesisIdentitySha256 },
            },
          },
        },
        intersection: INTERSECTION,
        startupTimeoutMs: 2000,
        onEvent: async () => {},
        unsafeReadIdentityFileForTest: async (path) => {
          if (path === NODE_CONFIG_PATH) return nodeBytes;
          if (path === GENESIS_CONFIG_PATH) return genesisBytes;
          throw new Error("unexpected identity path");
        },
      });
      if (variant === "matching") {
        const runtime = await pending;
        try {
          expect(
            watcherNativeChainSyncAuthorityDetails(runtime.authority),
          ).toMatchObject({
            network: "Custom",
            genesisIdentitySha256,
          });
        } finally {
          await runtime.close();
        }
        expect(transport.journal()).toContain("step honest");
      } else {
        await expect(pending).rejects.toThrow(
          variant === "magic" ? "network magic differs" : "slot clock differs",
        );
        expect(transport.journal()).toEqual([]);
      }
    },
  );

  it("seals the exact startup identity and admits ordered roll-forward/rollback", async () => {
    const events: WatcherNativeChainSyncEvent[] = [];
    const receipts: WatcherNativeChainSyncEventReceipt[] = [];
    const captured: ReturnType<
      typeof readWatcherNativeChainSyncEventReceipt
    >[] = [];
    const runtime = await start("honest", async (event) => {
      const receipt = watcherNativeChainSyncEventReceipt(event);
      if (receipt === null) throw new Error("callback event has no receipt");
      expect(watcherNativeChainSyncEventReceipt({ ...event })).toBeNull();
      expect(
        watcherNativeChainSyncEventReceipt(
          parseWatcherNativeChainSyncEvent(event),
        ),
      ).toBeNull();
      expect(() =>
        readWatcherNativeChainSyncEventReceipt({ ...receipt }),
      ).toThrow("absent or stale");
      // A rollback past the intersection acknowledgement revokes the
      // receipts delivered before it.
      if (event.kind === "roll_backward" && events.length > 0) {
        expect(watcherNativeChainSyncEventReceipt(events[1]!)).toBeNull();
        expect(() =>
          readWatcherNativeChainSyncEventReceipt(receipts[1]!),
        ).toThrow("absent or stale");
      }
      events.push(event);
      receipts.push(receipt);
      captured.push(readWatcherNativeChainSyncEventReceipt(receipt));
    });
    try {
      await waitFor(() => events.length === 3);
      expect(events).toMatchObject([
        { kind: "roll_backward", point: INTERSECTION },
        {
          kind: "roll_forward",
          blockType: "6",
          prevHash: INTERSECTION.blockHash,
        },
        { kind: "roll_backward", point: INTERSECTION },
      ]);
      for (const [index, value] of captured.entries()) {
        expect(value.authority).toBe(runtime.authority);
        expect(value.startupDigest).toBe(
          watcherNativeChainSyncAuthorityDetails(runtime.authority)
            ?.startupDigest,
        );
        expect(value.event).toBe(events[index]);
        expect(value.eventDigest).toBe(
          createHash("sha256")
            .update(watcherCanonicalJson(events[index]!), "utf8")
            .digest("hex"),
        );
        expect(Object.isFrozen(value)).toBe(true);
        expect(Object.isFrozen(value.event)).toBe(true);
        expect(Object.isFrozen(value.event.tip)).toBe(true);
      }
      expect(readWatcherNativeChainSyncEventReceipt(receipts[2]!)).toBe(
        captured[2],
      );
      expect(watcherNativeChainSyncAuthorityDetails(runtime.authority)).toEqual(
        {
          network: "Preprod",
          authorityNodeId: "watcher-node",
          operation: { kind: "stream" },
          genesisIdentitySha256: GENESIS,
          socketPath: "/run/cardano/node.socket",
          startupDigest: expect.stringMatching(/^[0-9a-f]{64}$/u),
          selectedIntersection: INTERSECTION,
          currentTip: {
            blockHash: "44".repeat(32),
            blockNo: "12",
            kind: "point",
            slot: "103",
          },
        },
      );
      const context = establishWatcherLocalNodeAuthorityTransport(
        runtime.authority,
      );
      try {
        expect(watcherL1TransportAttestationDetails(context)).toMatchObject({
          provider: {
            network: "Preprod",
            providerId: "watcher-node",
            source: {
              sourceMode: "local_node",
              authorityNodeId: "watcher-node",
              surface: "chain_sync",
            },
            authentication: {
              kind: "cardano_node_genesis_v1",
              publicIdentitySha256: GENESIS,
            },
          },
          transportEndpoint: "/run/cardano/node.socket",
        });
      } finally {
        closeWatcherL1TransportAttestationContext(context);
      }
    } finally {
      await runtime.close();
    }
    expect(watcherNativeChainSyncEventReceipt(events[2]!)).toBeNull();
    expect(() => readWatcherNativeChainSyncEventReceipt(receipts[2]!)).toThrow(
      "absent or stale",
    );
  });

  it("revokes a receipt at close entry while its callback remains pending", async () => {
    let release!: () => void;
    const gate = new Promise<void>((resolve) => {
      release = resolve;
    });
    const receipts: WatcherNativeChainSyncEventReceipt[] = [];
    let callbackCompleted = false;
    const runtime = await start("honest", async (event) => {
      if (event.kind !== "roll_forward") return;
      const receipt = watcherNativeChainSyncEventReceipt(event);
      if (receipt === null) throw new Error("callback event has no receipt");
      receipts.push(receipt);
      await gate;
      callbackCompleted = true;
    });
    try {
      await waitFor(() => receipts.length === 1);
      const receipt = receipts[0]!;
      const { event } = readWatcherNativeChainSyncEventReceipt(receipt);
      const shutdown = runtime.close();
      try {
        expect(callbackCompleted).toBe(false);
        expect(watcherNativeChainSyncEventReceipt(event)).toBeNull();
        expect(() => readWatcherNativeChainSyncEventReceipt(receipt)).toThrow(
          "absent or stale",
        );
      } finally {
        release();
        await shutdown;
      }
    } finally {
      release();
      await runtime.close();
    }
  });

  it.each(["sidecar_exit", "stream_failure"] as const)(
    "revokes provenance when a %s is observed during a pending callback",
    async (lifecycle) => {
      const transport = await fakeNodeTransport();
      let release!: () => void;
      const gate = new Promise<void>((resolve) => {
        release = resolve;
      });
      const receipts: WatcherNativeChainSyncEventReceipt[] = [];
      let callbackCompleted = false;
      const runtime = await start(
        "honest",
        async (event) => {
          if (event.kind !== "roll_forward") return;
          const receipt = watcherNativeChainSyncEventReceipt(event);
          if (receipt === null)
            throw new Error("callback event has no receipt");
          receipts.push(receipt);
          await gate;
          callbackCompleted = true;
        },
        transport,
      );
      try {
        await waitFor(() => receipts.length === 1);
        const receipt = receipts[0]!;
        const { event } = readWatcherNativeChainSyncEventReceipt(receipt);
        if (lifecycle === "sidecar_exit")
          process.kill(transport.pid(), "SIGKILL");
        else transport.failStreams();
        await waitFor(() => watcherNativeChainSyncEventReceipt(event) === null);
        expect(callbackCompleted).toBe(false);
        expect(() => readWatcherNativeChainSyncEventReceipt(receipt)).toThrow(
          "absent or stale",
        );
        release();
        await expect(runtime.done).rejects.toThrow(
          lifecycle === "sidecar_exit"
            ? "transport sidecar ended"
            : "node_connection_lost",
        );
      } finally {
        release();
        await runtime.close();
      }
    },
  );

  it("reports a stream failure after startup with its cause", async () => {
    const events: WatcherNativeChainSyncEvent[] = [];
    const runtime = await start("runtime_failure", async (event) => {
      events.push(event);
    });
    try {
      await expect(runtime.done).rejects.toThrow(
        "native chain-sync runtime failed: chain-sync stream failed: node_connection_lost: actual underlying socket failure",
      );
      // Only the intersection acknowledgement was delivered.
      expect(events).toMatchObject([
        { kind: "roll_backward", point: INTERSECTION },
      ]);
      expect(
        watcherNativeChainSyncAuthorityDetails(runtime.authority),
      ).toBeNull();
    } finally {
      await runtime.close();
    }
  });

  it("revokes a delivered receipt when the callback fails", async () => {
    const receipts: WatcherNativeChainSyncEventReceipt[] = [];
    const runtime = await start("honest", async (event) => {
      const receipt = watcherNativeChainSyncEventReceipt(event);
      if (receipt === null) throw new Error("callback event has no receipt");
      receipts.push(receipt);
      throw new Error("native fixture callback failed");
    });
    try {
      await expect(runtime.done).rejects.toThrow("callback failed");
      expect(receipts).toHaveLength(1);
      expect(() =>
        readWatcherNativeChainSyncEventReceipt(receipts[0]!),
      ).toThrow("absent or stale");
    } finally {
      await runtime.close();
    }
  });

  it("derives genesis identity from the exact node config before opening a stream", async () => {
    const transport = await fakeNodeTransport();
    const invoke = async (
      readIdentityFile: (path: string) => Promise<Uint8Array>,
    ) =>
      await startWatcherNativeChainSync({
        binaryPath: transport.binaryPath,
        watcherConfig: config(),
        intersection: INTERSECTION,
        startupTimeoutMs: 2_000,
        onEvent: async () => undefined,
        unsafeReadIdentityFileForTest: readIdentityFile,
      });

    await expect(
      invoke(async (path) =>
        path === NODE_CONFIG_PATH
          ? new TextEncoder().encode(
              `{"ShelleyGenesisFile":"${GENESIS_CONFIG_PATH}","ShelleyGenesisFile":"${GENESIS_CONFIG_PATH}"}`,
            )
          : GENESIS_CONFIG_BYTES,
      ),
    ).rejects.toThrow(/duplicate_field/u);
    await expect(
      invoke(async (path) =>
        path === NODE_CONFIG_PATH
          ? NODE_CONFIG_BYTES
          : new TextEncoder().encode(JSON.stringify({ networkMagic: 2 })),
      ),
    ).rejects.toThrow("network magic differs");
    expect(transport.journal()).toEqual([]);
  });

  it.each(["reordered", "first_slot_regression", "unknown_rollback"])(
    "terminates on hostile %s output",
    async (mode) => {
      const receipts: WatcherNativeChainSyncEventReceipt[] = [];
      const runtime = await start(mode, async (event) => {
        const receipt = watcherNativeChainSyncEventReceipt(event);
        if (receipt === null) throw new Error("callback event has no receipt");
        receipts.push(receipt);
      });
      await expect(runtime.done).rejects.toThrow(
        /out of order|not durable history/u,
      );
      // The intersection acknowledgement precedes the hostile event.
      expect(receipts).toHaveLength(mode === "unknown_rollback" ? 2 : 1);
      for (const receipt of receipts) {
        expect(() => readWatcherNativeChainSyncEventReceipt(receipt)).toThrow(
          "absent or stale",
        );
      }
      await runtime.close();
    },
  );

  it("surfaces a sidecar crash after authenticated startup", async () => {
    const runtime = await start("crash", async () => undefined);
    await expect(runtime.done).rejects.toThrow(
      "transport sidecar ended (code 23",
    );
    await runtime.close();
  });

  it("offers every durable ancestor in one intersection and binds explicit Origin", async () => {
    const transport = await fakeNodeTransport("retry_intersection");
    const events: WatcherNativeChainSyncEvent[] = [];
    const runtime = await startWatcherNativeChainSyncWithRetry({
      binaryPath: transport.binaryPath,
      watcherConfig: config(),
      intersectionCandidates: [INTERSECTION, { kind: "origin" }],
      startupTimeoutMs: 2_000,
      onEvent: async (event) => {
        events.push(event);
      },
      unsafeReadIdentityFileForTest: readIdentityFixture,
    });
    try {
      expect(transport.journal()).toEqual(["step retry_intersection"]);
      expect(
        watcherNativeChainSyncAuthorityDetails(runtime.authority)
          ?.selectedIntersection,
      ).toEqual({ kind: "origin" });
      // The node intersected below the first candidate, so the transport
      // restates the intersection as a rollback; the read delivers it once.
      await expect.poll(() => events.length).toBe(1);
      await new Promise((resolve) => setTimeout(resolve, 200));
      expect(events).toMatchObject([
        { kind: "roll_backward", point: { kind: "origin" } },
      ]);
    } finally {
      await runtime.close();
    }
  });

  it("strictly parses canonical bounded event shapes", () => {
    const event = {
      blockHash: "bb".repeat(32),
      blockNo: "10",
      blockType: "6",
      kind: "roll_forward",
      prevHash: "aa".repeat(32),
      rawBlockCbor: "80",
      schemaVersion: WATCHER_NATIVE_CHAIN_SYNC_SCHEMA_VERSION,
      slot: "101",
      tip: {
        blockHash: "cc".repeat(32),
        blockNo: "11",
        kind: "point",
        slot: "102",
      },
    };
    const parsed = parseWatcherNativeChainSyncEvent(event);
    expect(parsed).toEqual(event);
    expect(watcherNativeChainSyncEventReceipt(parsed)).toBeNull();
    expect(() =>
      parseWatcherNativeChainSyncEvent({ ...event, trusted: true }),
    ).toThrow("unknown or missing");
    expect(() =>
      parseWatcherNativeChainSyncEvent({ ...event, slot: "0101" }),
    ).toThrow("slot is invalid");
    expect(() =>
      parseWatcherNativeChainSyncEvent({ ...event, rawBlockCbor: "0" }),
    ).toThrow("CBOR is invalid");
    expect(
      parseWatcherNativeChainSyncEvent({
        kind: "roll_backward",
        point: { kind: "origin" },
        schemaVersion: WATCHER_NATIVE_CHAIN_SYNC_SCHEMA_VERSION,
        tip: { kind: "origin" },
      }),
    ).toMatchObject({ kind: "roll_backward", point: { kind: "origin" } });
  });
});
