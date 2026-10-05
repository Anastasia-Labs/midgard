import { describe, expect, it } from "vitest";

import {
  startWatcherNativeChainSync,
  type WatcherNativeChainSyncEvent,
} from "../../src/l1/native-chain-sync.js";
import {
  guardRecoveryEvent,
  retryQuarantinedRecovery,
} from "../../src/runtime/chain-coordinator.recovery-evidence.js";
import type { WatcherDurableRuntime } from "../../src/storage/durable-runtime.js";
import {
  config,
  INTERSECTION,
  readIdentityFixture,
  spawnFixture,
} from "../l1/native-chain-sync.config.js";

const hash = (byte: string) => byte.repeat(64);

describe("quarantine retry native lease", () => {
  it("preserves actual supervisor receipt revocation even when only MAC history supplies the recovery path", async () => {
    let resolveEvent!: (event: WatcherNativeChainSyncEvent) => void;
    const eventReady = new Promise<WatcherNativeChainSyncEvent>((resolve) => {
      resolveEvent = resolve;
    });
    const native = await startWatcherNativeChainSync({
      binaryPath: "/test/native-chain-sync",
      watcherConfig: config(),
      intersection: INTERSECTION,
      startupTimeoutMs: 10_000,
      unsafeSpawnForTest: spawnFixture("below_intersection"),
      unsafeReadIdentityFileForTest: readIdentityFixture,
      onEvent: async (event) => {
        if (
          event.kind === "roll_backward" &&
          event.point.kind === "point" &&
          event.point.slot === "90"
        )
          resolveEvent(event);
      },
    });
    try {
      const event = await eventReady;
      if (event.kind !== "roll_backward" || event.point.kind !== "point")
        throw new Error("expected actual lower rollback receipt");
      const guard = guardRecoveryEvent(event, () => true);
      guard();
      const ancestor = {
        blockHash: event.point.blockHash,
        blockNo: "10",
        slot: event.point.slot,
      };
      const released = {
        blockHash: hash("2"),
        blockNo: "11",
        slot: String(BigInt(event.point.slot) + 1n),
      };
      const priorDigest = hash("3");
      const triggerDigest = hash("4");
      let attempts = 0;
      let commits = 0;
      const agreement = (point: typeof ancestor, digest: string) => ({
        status: "agreed",
        protocolDecision: "allowed",
        consistencyDigest: digest,
        agreement: { ...point, minimumDepth: "30" },
      });
      const durable = {
        readFinality: () => ({ phase: "quarantined" }),
        read: () => ({
          currentFinalityState: {
            finalized: { ...released, lastSeenConsistencyDigest: priorDigest },
            incident: { triggerConsistencyDigest: triggerDigest },
          },
          authenticatedConsistencyHistory: [
            agreement(ancestor, hash("5")),
            agreement(released, priorDigest),
            agreement(released, triggerDigest),
          ],
        }),
        persistPostFinalityRecovery: async (input: {
          assertCurrent?: () => void;
        }) => {
          attempts += 1;
          await native.close();
          input.assertCurrent?.();
          commits += 1;
          return {
            persistence: "committed",
            result: { protocolDecision: "resume_replay" },
          };
        },
      } as unknown as WatcherDurableRuntime;
      await expect(
        retryQuarantinedRecovery(durable, event, guard),
      ).rejects.toThrow(/native.*stale/);
      expect(attempts).toBe(1);
      expect(commits).toBe(0);
    } finally {
      await native.close();
    }
  });
});
