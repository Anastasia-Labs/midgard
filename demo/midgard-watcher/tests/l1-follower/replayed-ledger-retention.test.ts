import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import { openSqliteFactStore } from "@al-ft/midgard-l1-follower";
import {
  SIM_ORIGIN,
  SimChain,
  simStoreOptions,
  simUniverse,
} from "@al-ft/midgard-l1-follower/testing";
import {
  EVENT_WAIT_DURATION_MS,
  MAX_EVENT_VALIDITY_RANGE_LENGTH_MS,
  MAX_VALIDITY_RANGE_LENGTH_MS,
} from "@al-ft/midgard-sdk/core";
import { describe, expect, it } from "vitest";

import { watcherProjection } from "../../src/l1-follower/projection.js";
import { createWatcherReplayRetention } from "../../src/l1-follower/replay-retention.js";
import {
  createWatcherLedgerWindowReader,
  watcherL1TipTimeMs,
} from "../../src/l1-follower/replay-source.window.js";
import { loadWatcherVerifiedDeploymentAuthority } from "../../src/runtime/deployment-authority.js";
import { createWatcherRuntimeReplayHost } from "../../src/runtime/watcher-runtime.replayed-ledger.js";
import {
  computeWatcherRuleBundleCommitment,
  makeWatcherCanonicalRuleBundle,
} from "../../src/verification/rule-bundle.parse-watcher-rule-bundle.js";
import { WATCHER_TEST_CARDANO_PROTOCOL_PARAMETERS } from "../support/deployment-authority-fixture.build-watcher-authority-contracts.js";
import { followerUserEventsDeployment } from "../support/follower-user-events-fixture.js";
import {
  commitTx,
  initTx,
  mergeTx,
  queueState,
} from "../support/l1-follower-state-queue-traffic.js";
import {
  applyAll,
  D,
  K,
  openStore,
  RELEASE_DEPTH,
  RETAINED_DEPTH,
} from "../support/l1-follower-store-reset.js";
import { replaySource } from "../support/replay-source-fixture.js";

const retention = () =>
  createWatcherReplayRetention({
    slotConfig: { zeroTime: 0, zeroSlot: 0, slotLength: 1000 },
    eventWaitMs: 2000n,
    maximumAdmissionRangeMs: 4000n,
    maximumProtectionWaitMs: 3000n,
    ledgerRetentionMs: 900_000n,
    maturityMs: 900_000n,
  });

describe("replay admission retention", () => {
  it("retains the signed event admission window when it exceeds the general transaction range", async () => {
    const construction = await followerUserEventsDeployment();
    const slotConfig = NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod;
    const rules = makeWatcherCanonicalRuleBundle({
      constructionIdentity: {
        l1SlotConfig: slotConfig,
        hubOraclePolicyId: construction.deploymentIdentity.hubOraclePolicyId,
        network: construction.deploymentIdentity.network,
        blueprintHash: construction.deploymentIdentity.blueprintHash,
        programCommitments: construction.deploymentIdentity.programCommitments,
      },
      targetParameterSnapshot: WATCHER_TEST_CARDANO_PROTOCOL_PARAMETERS,
    });
    const deployment = await followerUserEventsDeployment(
      computeWatcherRuleBundleCommitment(rules),
    );
    const authority = await loadWatcherVerifiedDeploymentAuthority({
      path: "/test/replay-authority.json",
      ruleBundlePath: "/test/replay-rules.json",
      unsafeReadFileForTest: async (path) =>
        Buffer.from(
          JSON.stringify(
            path === "/test/replay-rules.json"
              ? rules
              : {
                  signedIdentity: deployment.deployment.signedIdentity,
                  policy: deployment.deployment.policy,
                  trustRoots: deployment.deployment.trustRoots,
                },
          ),
        ),
    });
    const host = createWatcherRuntimeReplayHost(
      authority,
      deployment.scripts,
      "/unused/replay-checkpoint",
      { setAlert: () => undefined },
    );
    // A shorter commit range cannot prove the earliest user-event admission.
    expect(MAX_EVENT_VALIDITY_RANGE_LENGTH_MS).toBeGreaterThan(
      MAX_VALIDITY_RANGE_LENGTH_MS,
    );
    const store = openStore(":memory:");
    try {
      await store.start();
      await store.initialize(SIM_ORIGIN);
      const lowerSlot = BigInt(SIM_ORIGIN.point.slot + 10);
      const lowerTime =
        BigInt(slotConfig.zeroTime) +
        (lowerSlot - BigInt(slotConfig.zeroSlot)) *
          BigInt(slotConfig.slotLength);
      host.retention.checkpointDurable(
        lowerTime +
          MAX_EVENT_VALIDITY_RANGE_LENGTH_MS +
          BigInt(EVENT_WAIT_DURATION_MS) +
          BigInt(deployment.scripts.maximumProtectionDurationMs),
      );
      const floor = await store.transaction("read", (tx) =>
        host.retention.floor.floor({ tx, dialect: store.dialect }),
      );
      // Preserve the admission's lower endpoint and the inclusive slot pad.
      expect(floor).toBe(Number(lowerSlot - 1n));
    } finally {
      await store.close();
    }
  });
  it("holds the origin until a durable checkpoint, retains the admission range and resets at restart", async () => {
    const store = openStore(":memory:");
    try {
      await store.start();
      await store.initialize(SIM_ORIGIN);
      const r = retention();
      const floor = () =>
        store.transaction("read", (tx) =>
          r.floor.floor({ tx, dialect: store.dialect }),
        );
      expect(await floor()).toBe(SIM_ORIGIN.point.slot);
      r.checkpointDurable(BigInt(SIM_ORIGIN.point.slot + 20) * 1000n);
      expect(await floor()).toBe(SIM_ORIGIN.point.slot + 10);
      // An older checkpoint cannot release or move the floor backward.
      r.checkpointDurable(BigInt(SIM_ORIGIN.point.slot + 10) * 1000n);
      expect(await floor()).toBe(SIM_ORIGIN.point.slot + 10);
      r.reset();
      expect(await floor()).toBe(SIM_ORIGIN.point.slot);
      const restarted = retention();
      expect(
        await store.transaction("read", (tx) =>
          restarted.floor.floor({ tx, dialect: store.dialect }),
        ),
      ).toBe(SIM_ORIGIN.point.slot);
    } finally {
      await store.close();
    }
  });

  it("keeps a merged block's row while the block is inside a ledger window longer than maturity", async () => {
    const origin = SIM_ORIGIN.point.slot;
    // Slot `origin` starts at 0 ms, so the committed block_time (2 ms) sits
    // at the origin; maturity is ten slots and the window thirty-five.
    const r = createWatcherReplayRetention({
      slotConfig: { zeroTime: 0, zeroSlot: origin, slotLength: 1000 },
      eventWaitMs: 1n,
      maximumAdmissionRangeMs: 1n,
      maximumProtectionWaitMs: 0n,
      ledgerRetentionMs: 35_000n,
      maturityMs: 10_000n,
    });
    const store = openSqliteFactStore({
      ...simStoreOptions([watcherProjection(D)], K, "sqlite"),
      retainedDepth: RETAINED_DEPTH,
      path: ":memory:",
      pruneFloors: [r.floor],
    });
    try {
      await store.start();
      await store.initialize(SIM_ORIGIN);
      const chain = new SimChain(simUniverse(), SIM_ORIGIN);
      const forward = (txs: Parameters<SimChain["forward"]>[0]) =>
        applyAll(store, [chain.forward(txs).event]);
      await forward([initTx(D)]);
      await forward([commitTx(queueState(chain, D)!, D)]);
      const source = replaySource(store, RELEASE_DEPTH, r);
      const committed = await source.ledgerWindow();
      expect(committed.blocks).toHaveLength(1);
      // The merge's validity starts at block_time + maturity.
      while (chain.nextSlot() < origin + 11) await forward([]);
      await forward([mergeTx(queueState(chain, D)!, D)]);
      while (chain.nextSlot() < origin + 30) await forward([]);
      expect((await source.ledgerWindow()).blocks).toEqual(committed.blocks);
      // The replay checkpoint reaches the tip: the admission rule alone would
      // prune every row closed below it, the merged block's last row too.
      r.checkpointDurable(BigInt(chain.nextSlot() - origin) * 1000n);
      for (;;) {
        const pruned = await store.prune(1_000);
        if ("kind" in pruned) throw new Error(`prune: ${pruned.kind}`);
        if (pruned.done) break;
      }
      expect((await source.ledgerWindow()).blocks).toEqual(committed.blocks);
    } finally {
      await store.close();
    }
  });
});

describe("the follower's ledger window and tip time", () => {
  const origin = SIM_ORIGIN.point.slot;
  // Slot `origin` starts at 1_000_000 ms; one slot is one second.
  const slotConfig = {
    zeroTime: 1_000_000,
    zeroSlot: origin,
    slotLength: 1000,
  };
  const following = async (
    run: (follow: {
      store: ReturnType<typeof openStore>;
      chain: SimChain;
      forward: (txs: Parameters<SimChain["forward"]>[0]) => Promise<void>;
    }) => Promise<void>,
  ) => {
    const store = openStore(":memory:");
    try {
      await store.start();
      await store.initialize(SIM_ORIGIN);
      const chain = new SimChain(simUniverse(), SIM_ORIGIN);
      await run({
        store,
        chain,
        forward: (txs) => applyAll(store, [chain.forward(txs).event]),
      });
    } finally {
      await store.close();
    }
  };
  const reader = (
    store: ReturnType<typeof openStore>,
    ledgerRetentionMs = 900_000,
  ) =>
    createWatcherLedgerWindowReader({ store, slotConfig, ledgerRetentionMs });

  it("reads no tip time before the follower holds a cursor, then the POSIX time of the cursor's slot", async () => {
    const store = openStore(":memory:");
    try {
      await store.start();
      expect(await watcherL1TipTimeMs(store, slotConfig)).toBeUndefined();
      await store.initialize(SIM_ORIGIN);
      expect(await watcherL1TipTimeMs(store, slotConfig)).toBe(1_000_000);
      const chain = new SimChain(simUniverse(), SIM_ORIGIN);
      await applyAll(store, [chain.forward([]).event]);
      await applyAll(store, [chain.forward([]).event]);
      expect(chain.tip.point.slot).toBeGreaterThan(origin);
      expect(await watcherL1TipTimeMs(store, slotConfig)).toBe(
        1_000_000 + (chain.tip.point.slot - origin) * 1000,
      );
    } finally {
      await store.close();
    }
  });

  it("names the open tail block's root as the newest, and none for an empty queue", async () => {
    await following(async ({ store, chain, forward }) => {
      const window = reader(store);
      await forward([initTx(D)]);
      expect(await window()).toEqual({ blocks: [], newestRoot: undefined });
      const first = "a1".repeat(32);
      const second = "b2".repeat(32);
      // Inside the window: committed at the origin's time.
      await forward([
        commitTx(queueState(chain, D)!, D, undefined, {
          endTime: 1_000_000n,
          utxosRoot: first,
        }),
      ]);
      expect((await window()).newestRoot).toBe(first);
      await forward([
        commitTx(queueState(chain, D)!, D, undefined, {
          endTime: 1_000_000n,
          prevUtxosRoot: first,
          utxosRoot: second,
        }),
      ]);
      const read = await window();
      expect(read.newestRoot).toBe(second);
      expect(read.blocks.map(({ utxosRoot }) => utxosRoot).sort()).toEqual([
        first,
        second,
      ]);
    });
  });

  it("leaves a block outside the window exactly at tip == block_time + ledger_retention_ms, inside one millisecond before", async () => {
    await following(async ({ store, chain, forward }) => {
      await forward([initTx(D)]);
      // block_time is the header's endTime.
      const blockTimeMs = 1_000_500;
      await forward([
        commitTx(queueState(chain, D)!, D, undefined, {
          endTime: BigInt(blockTimeMs),
        }),
      ]);
      await forward([]);
      const tipMs = 1_000_000 + (chain.tip.point.slot - origin) * 1000;
      const retentionAtTip = tipMs - blockTimeMs;
      expect(retentionAtTip).toBeGreaterThan(0);
      expect((await reader(store, retentionAtTip)()).blocks).toEqual([]);
      expect((await reader(store, retentionAtTip + 1)()).blocks).toHaveLength(
        1,
      );
    });
  });
});
