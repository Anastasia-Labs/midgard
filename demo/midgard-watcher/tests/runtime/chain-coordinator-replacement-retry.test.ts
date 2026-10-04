import "../support/chain-coordinator-unit-fixture.js";

import { describe, expect, it } from "vitest";

import type { WatcherFinalityPolicy } from "../../src/l1/finality-engine.js";
import type { WatcherLocalKupmiosNativeObservationRuntime } from "../../src/l1/local-kupmios-native-observation.js";
import type { WatcherNativeBlockAdmission } from "../../src/l1/native-block-admission.js";
import type { WatcherNativeChainSyncEvent } from "../../src/l1/native-chain-sync.js";
import { unsafeCreateWatcherChainCoordinatorForTest } from "../../src/runtime/chain-coordinator.js";
import type { WatcherDurableRuntime } from "../../src/storage/durable-runtime.js";
import { WatcherDurableAuthorityConflict } from "../../src/storage/durable-runtime.load-published-authority.js";

const hash = (byte: string) => byte.repeat(64);
const replacement = (
  byte: string,
  parent: string,
  blockNo: string,
  slot: string,
) =>
  ({
    schemaVersion: "midgard-watcher-native-block-admission-v1",
    blockType: "7",
    protocolMajor: "10",
    blockHash: hash(byte),
    prevHash: hash(parent),
    blockNo,
    slot,
    rawBlockCbor: "80",
    rawHeaderCbor: "80",
    transactionIds: [],
    transactionCbors: [],
  }) as WatcherNativeBlockAdmission;
const first = replacement("2", "1", "11", "101");
const next = replacement("3", "2", "12", "102");
const forward = (
  block: WatcherNativeBlockAdmission,
): WatcherNativeChainSyncEvent =>
  ({
    ...block,
    schemaVersion: "midgard-watcher-native-chain-sync-v1",
    kind: "roll_forward",
    tip: {
      kind: "point",
      blockHash: next.blockHash,
      blockNo: "40",
      slot: "140",
    },
  }) as WatcherNativeChainSyncEvent;

describe("independent rollback replacement retry", () => {
  it("keeps retrying the buffered rollback child after a newer forward arrives", async () => {
    let attempts = 0;
    let reconciliations = 0;
    const state = {
      phase: "finalized",
      pending: null,
      finalized: { blockHash: hash("4"), blockNo: "13", slot: "103" },
    };
    const coordinator = unsafeCreateWatcherChainCoordinatorForTest(
      {
        policy: { confirmationDepth: "30" } as WatcherFinalityPolicy,
        durable: {
          readFinality: () => state,
          read: () => ({
            currentFinalityState: state,
            currentStore: undefined,
          }),
          reconcile: async () => {
            reconciliations += 1;
          },
          persistObservation: async () => {
            attempts += 1;
            throw new WatcherDurableAuthorityConflict(
              "publication temporarily conflicted",
            );
          },
        } as unknown as WatcherDurableRuntime,
        observation: {
          observe: async () => ({
            block: {},
            observations: [],
            transportAttestations: [],
            consistency: {},
          }),
        } as unknown as WatcherLocalKupmiosNativeObservationRuntime,
        hooks: {
          onRollback: async () => undefined,
          onFinalized: async () => undefined,
        },
      },
      {
        admitRollForward: (event) =>
          event.blockHash === first.blockHash ? first : next,
      },
    );
    try {
      await coordinator.handle({
        schemaVersion: "midgard-watcher-native-chain-sync-v1",
        kind: "roll_backward",
        point: { kind: "point", blockHash: hash("1"), slot: "100" },
        tip: {
          kind: "point",
          blockHash: hash("4"),
          slot: "103",
          blockNo: "13",
        },
      });
      await coordinator.handle(forward(first));
      await coordinator.handle(forward(next));
      expect(attempts).toBe(2);
      expect(coordinator.status()).toMatchObject({
        integrityHold: "durable_authority_conflict",
        bufferedBlockCount: 2,
      });
      // A retry should attempt first again and retain its named conflict.
      // Frozen source instead tries next against the earlier rollback point.
      await expect(coordinator.resume()).resolves.toBeUndefined();
      expect(attempts).toBe(3);
      expect(reconciliations).toBe(2);
    } finally {
      await coordinator.stop();
    }
  });
  it("holds before durable mutation while the rollback replacement child is missing", async () => {
    const orphan = replacement("4", "9", "12", "103");
    const state = {
      phase: "pending",
      pending: {
        blockHash: orphan.blockHash,
        blockNo: orphan.blockNo,
        slot: orphan.slot,
      },
      finalized: null,
    };
    const observation = {
      observe: async () => {
        throw new Error("must not observe");
      },
      close: () => undefined,
    } as unknown as WatcherLocalKupmiosNativeObservationRuntime;
    const durable = {
      readFinality: () => state,
      read: () => ({
        currentFinalityState: state,
        currentStore: undefined,
      }),
    } as unknown as WatcherDurableRuntime;
    const coordinator = unsafeCreateWatcherChainCoordinatorForTest(
      {
        policy: { confirmationDepth: "30" } as WatcherFinalityPolicy,
        durable,
        observation,
      },
      { admitRollForward: () => orphan },
    );
    await coordinator.handle({
      schemaVersion: "midgard-watcher-native-chain-sync-v1",
      kind: "roll_backward",
      point: { kind: "point", blockHash: hash("1"), slot: "101" },
      tip: { kind: "point", blockHash: hash("2"), slot: "103", blockNo: "13" },
    });
    try {
      await expect(
        coordinator.handle(forward(orphan)),
      ).resolves.toBeUndefined();
      expect(coordinator.status()).toMatchObject({
        integrityHold: "rollback_evidence_rejected",
        deliveryHeld: true,
        bufferedBlockCount: 1,
      });
    } finally {
      await coordinator.stop();
    }
  });
});
