import "../support/chain-coordinator-unit-fixture.js";

import { describe, expect, it, vi } from "vitest";

import type { WatcherFinalityPolicy } from "../../src/l1/finality-engine.js";
import type { WatcherLocalKupmiosNativeObservationRuntime } from "../../src/l1/local-kupmios-native-observation.js";
import type { WatcherNativeBlockAdmission } from "../../src/l1/native-block-admission.js";
import type { WatcherNativeChainSyncEvent } from "../../src/l1/native-chain-sync.js";
import { unsafeCreateWatcherChainCoordinatorForTest } from "../../src/runtime/chain-coordinator.js";
import type { WatcherDurableRuntime } from "../../src/storage/durable-runtime.js";

const h32 = (byte: string): string => byte.repeat(64);

const block = (
  hashByte: string,
  parentByte: string,
  slot: string,
  blockNo: string,
): WatcherNativeBlockAdmission =>
  Object.freeze({
    schemaVersion: "midgard-watcher-native-block-admission-v1",
    blockType: "7",
    protocolMajor: "10",
    blockHash: h32(hashByte),
    prevHash: h32(parentByte),
    slot,
    blockNo,
    rawBlockCbor: "80",
    rawHeaderCbor: "80",
    transactionIds: Object.freeze([]),
    transactionCbors: Object.freeze([]),
  });

const forward = (
  admitted: WatcherNativeBlockAdmission,
  tipBlockNo = admitted.blockNo,
): WatcherNativeChainSyncEvent =>
  Object.freeze({
    schemaVersion: "midgard-watcher-native-chain-sync-v1",
    kind: "roll_forward",
    blockHash: admitted.blockHash,
    blockType: admitted.blockType,
    prevHash: admitted.prevHash,
    slot: admitted.slot,
    blockNo: admitted.blockNo,
    rawBlockCbor: admitted.rawBlockCbor,
    tip: Object.freeze({
      kind: "point",
      blockHash: admitted.blockHash,
      slot: admitted.slot,
      blockNo: tipBlockNo,
    }),
  });

const finalityState = (
  phase: "unobserved" | "pending" | "finalized" | "quarantined",
  admitted?: WatcherNativeBlockAdmission,
) => ({
  phase,
  pending:
    phase === "pending" && admitted !== undefined
      ? {
          blockHash: admitted.blockHash,
          slot: admitted.slot,
          blockNo: admitted.blockNo,
        }
      : null,
  finalized:
    phase === "finalized" && admitted !== undefined
      ? {
          blockHash: admitted.blockHash,
          slot: admitted.slot,
          blockNo: admitted.blockNo,
        }
      : null,
});

const policy = Object.freeze({
  confirmationDepth: "30",
}) as WatcherFinalityPolicy;

describe("coordinator authority reconciliation", () => {
  it("holds a conflicted canonical write and resumes with freshly captured evidence", async () => {
    const admitted = block("2", "1", "101", "11");
    let state = finalityState("unobserved") as ReturnType<
      WatcherDurableRuntime["readFinality"]
    >;
    let observations = 0;
    let attempts = 0;
    let reconciliations = 0;
    let delivered = 0;
    const observation = {
      observe: async () => {
        observations += 1;
        return {
          block: {},
          observations: [],
          transportAttestations: [],
          consistency: { consistencyDigest: h32("8") },
        };
      },
    } as unknown as WatcherLocalKupmiosNativeObservationRuntime;
    const durable = {
      readFinality: () => state,
      read: () => ({ currentFinalityState: state }),
      reconcile: async () => {
        reconciliations += 1;
      },
      persistCanonicalProgress: async () => {
        attempts += 1;
        if (attempts === 1) return { persistence: "conflict" };
        state = finalityState("finalized", admitted) as typeof state;
        return {
          persistence: "committed",
          finalityResult: { action: "finalize" },
        };
      },
    } as unknown as WatcherDurableRuntime;
    const coordinator = unsafeCreateWatcherChainCoordinatorForTest(
      {
        policy,
        durable,
        observation,
        hooks: {
          onRollback: async () => undefined,
          onFinalized: async () => {
            delivered += 1;
          },
        },
      },
      { admitRollForward: () => admitted },
    );
    vi.useFakeTimers();
    try {
      await expect(
        coordinator.handle(forward(admitted, "40")),
      ).resolves.toBeUndefined();
      expect(coordinator.status()).toMatchObject({
        integrityHold: "durable_authority_conflict",
        deliveryHeld: true,
      });
      expect(delivered).toBe(0);
      await vi.advanceTimersByTimeAsync(1_000);
      expect(reconciliations).toBe(1);
      expect(observations).toBe(2);
      expect(attempts).toBe(2);
      expect(delivered).toBe(1);
      expect(coordinator.status()).toMatchObject({
        integrityHold: null,
        deliveryHeld: false,
      });
    } finally {
      await coordinator.stop();
      vi.useRealTimers();
    }
  });

  it("retains a live hold when independent reconciliation cannot authenticate the head", async () => {
    const admitted = block("2", "1", "101", "11");
    let writes = 0;
    const state = finalityState("unobserved");
    const coordinator = unsafeCreateWatcherChainCoordinatorForTest(
      {
        policy,
        durable: {
          readFinality: () => state,
          read: () => ({ currentFinalityState: state }),
          reconcile: async () => {
            throw new Error("head mismatch");
          },
          persistCanonicalProgress: async () => {
            writes += 1;
            return { persistence: "conflict" };
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
          onFinalized: async () => {
            throw new Error("must remain held");
          },
        },
      },
      { admitRollForward: () => admitted },
    );
    try {
      await coordinator.handle(forward(admitted, "40"));
      await coordinator.resume();
      expect(writes).toBe(1);
      expect(coordinator.status()).toMatchObject({
        integrityHold: "durable_authority_conflict",
        deliveryHeld: true,
      });
    } finally {
      await coordinator.stop();
    }
  });
});
