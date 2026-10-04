import { describe, expect, it } from "vitest";

import type { WatcherFinalityPolicy } from "../../src/l1/finality-engine.js";
import type { WatcherLocalKupmiosNativeObservationRuntime } from "../../src/l1/local-kupmios-native-observation.js";
import type { WatcherMultiProviderConsistency } from "../../src/l1/multi-provider-consistency.js";
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

describe("native coordinator integrity hold", () => {
  it("reopens an integrity hold after authenticated recovery without requiring a second rewind", async () => {
    const ancestor = block("1", "0", "100", "10");
    const released = block("2", "1", "101", "11");
    const digest = h32("4");
    const trigger = h32("5");
    let state = {
      ...finalityState("quarantined", released),
      finalized: { ...released, lastSeenConsistencyDigest: digest },
      incident: { triggerConsistencyDigest: trigger },
    } as unknown as ReturnType<WatcherDurableRuntime["readFinality"]>;
    const consistency = (
      admitted: WatcherNativeBlockAdmission,
      consistencyDigest: string,
    ) =>
      ({
        status: "agreed",
        protocolDecision: "allowed",
        consistencyDigest,
        agreement: { ...admitted, minimumDepth: "30" },
      }) as unknown as WatcherMultiProviderConsistency;
    let recoveries = 0;
    const coordinator = unsafeCreateWatcherChainCoordinatorForTest(
      {
        policy,
        durable: {
          readFinality: () => state,
          read: () => ({
            currentFinalityState: state,
            authenticatedConsistencyHistory: [
              consistency(ancestor, h32("3")),
              consistency(released, digest),
              {
                ...consistency(released, trigger),
                agreement: { ...released, minimumDepth: "1" },
              },
            ],
          }),
          persistPostFinalityRecovery: async () => {
            recoveries += 1;
            state = finalityState("unobserved") as typeof state;
            return {
              persistence: "committed",
              result: { protocolDecision: "resume_replay" },
            };
          },
        } as unknown as WatcherDurableRuntime,
        observation: {} as WatcherLocalKupmiosNativeObservationRuntime,
        hooks: {
          onRollback: async () => undefined,
          onFinalized: async () => undefined,
        },
      },
      { admitRollForward: () => released },
    );
    const event: WatcherNativeChainSyncEvent = {
      schemaVersion: "midgard-watcher-native-chain-sync-v1",
      kind: "roll_backward",
      point: {
        kind: "point",
        blockHash: ancestor.blockHash,
        slot: ancestor.slot,
      },
      tip: {
        kind: "point",
        blockHash: released.blockHash,
        blockNo: released.blockNo,
        slot: released.slot,
      },
    };
    await coordinator.handle(event);
    expect(recoveries).toBe(1);
    expect(coordinator.status()).toMatchObject({
      quarantined: false,
      rollbackPoint: null,
    });
    await coordinator.stop();
  });

  it("holds live native intake during finality quarantine without releasing consumers", async () => {
    const admitted = block("2", "1", "101", "11");
    const state = finalityState("quarantined");
    let delivered = 0;
    const coordinator = unsafeCreateWatcherChainCoordinatorForTest(
      {
        policy,
        durable: {
          readFinality: () => state,
          read: () => ({ currentFinalityState: state }),
        } as WatcherDurableRuntime,
        observation: {} as WatcherLocalKupmiosNativeObservationRuntime,
        hooks: {
          onRollback: async () => undefined,
          onFinalized: async () => {
            delivered += 1;
          },
        },
      },
      { admitRollForward: () => admitted },
    );
    await expect(
      coordinator.handle(forward(admitted)),
    ).resolves.toBeUndefined();
    await expect(coordinator.resume()).resolves.toBeUndefined();
    expect(coordinator.status().quarantined).toBe(true);
    expect(delivered).toBe(0);
    await coordinator.stop();
  });
});
