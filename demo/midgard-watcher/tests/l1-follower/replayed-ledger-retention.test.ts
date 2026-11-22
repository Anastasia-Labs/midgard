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
        manifestId: construction.deploymentIdentity.manifestId,
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
                  durableMarker: deployment.deployment.marker,
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
