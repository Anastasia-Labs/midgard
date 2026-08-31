import { describe, expect, it } from "vitest";

import type { WatcherFinalityPolicyV1 } from "../src/finality-engine.js";
import type { WatcherLocalKupmiosNativeObservationRuntimeV1 } from "../src/local-kupmios-native-observation-v1.js";
import type { WatcherNativeBlockAdmissionV1 } from "../src/native-block-admission-v1.js";
import type { WatcherNativeChainSyncEventV1 } from "../src/native-chain-sync-v1.js";
import { unsafeCreateWatcherProductionChainCoordinatorForTestV1 } from "../src/production-chain-coordinator-v1.js";
import type { WatcherProductionDurableRuntimeV1 } from "../src/production-durable-runtime-v1.js";

const h32 = (byte: string): string => byte.repeat(64);

const block = (
  hashByte: string,
  parentByte: string,
  slot: string,
  blockNo: string,
): WatcherNativeBlockAdmissionV1 =>
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
  admitted: WatcherNativeBlockAdmissionV1,
  tipBlockNo = admitted.blockNo,
): WatcherNativeChainSyncEventV1 =>
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
  admitted?: WatcherNativeBlockAdmissionV1,
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
}) as WatcherFinalityPolicyV1;

describe("production native-chain coordinator", () => {
  it("replays a release-final queue hook when the durable snapshot is already ahead", async () => {
    const replayed = block("1", "0", "100", "10");
    const durableHead = block("9", "8", "200", "100");
    const observed: string[] = [];
    const finalized: string[] = [];
    const observation = {
      observe: async ({
        block: candidate,
      }: {
        readonly block: WatcherNativeBlockAdmissionV1;
      }) => {
        observed.push(candidate.blockHash);
        return {
          block: { chainPoint: { blockNo: candidate.blockNo } },
          consistency: {},
          transportAttestations: [],
        };
      },
      close: () => undefined,
    } as unknown as WatcherLocalKupmiosNativeObservationRuntimeV1;
    const durable = {
      read: () => ({
        currentFinalityState: finalityState("finalized", durableHead),
        currentStore: {},
      }),
      persistCanonicalProgress: async () => {
        throw new Error("durable finality must not rewind to replayed history");
      },
    } as unknown as WatcherProductionDurableRuntimeV1;
    const coordinator = unsafeCreateWatcherProductionChainCoordinatorForTestV1(
      {
        policy,
        durable,
        observation,
        hooks: {
          onRollback: async () => undefined,
          onFinalized: async ({ nativeBlock }) => {
            finalized.push(nativeBlock.blockHash);
          },
        },
      },
      { admitRollForward: () => replayed },
    );

    await coordinator.handle(forward(replayed, "100"));

    expect(observed).toEqual([replayed.blockHash]);
    expect(finalized).toEqual([replayed.blockHash]);
  });

  it("reobserves the pending block at the native tip before admitting its child", async () => {
    const first = block("1", "0", "100", "10");
    const second = block("2", "1", "101", "11");
    let state = finalityState("unobserved");
    const observed: string[] = [];
    const persisted: string[] = [];
    const observation = {
      observe: async ({
        block: candidate,
        depth,
      }: {
        readonly block: WatcherNativeBlockAdmissionV1;
        readonly depth: string;
      }) => {
        observed.push(`${candidate.blockNo}:${depth}`);
        return {
          block: { chainPoint: { blockNo: candidate.blockNo } },
          consistency: {},
          transportAttestations: [],
        };
      },
      close: () => undefined,
    } as unknown as WatcherLocalKupmiosNativeObservationRuntimeV1;
    const durable = {
      read: () => ({ currentFinalityState: state, currentStore: {} }),
      persistCanonicalProgress: async (input: {
        readonly block: { readonly chainPoint: { readonly blockNo: string } };
      }) => {
        persisted.push(input.block.chainPoint.blockNo);
        if (
          input.block.chainPoint.blockNo === first.blockNo &&
          state.phase === "pending"
        ) {
          state = finalityState("finalized", first);
          return {
            persistence: "committed",
            finalityResult: { action: "finalize" },
          };
        }
        state = finalityState(
          "pending",
          input.block.chainPoint.blockNo === first.blockNo ? first : second,
        );
        return {
          persistence: "committed",
          finalityResult: { action: "observe_pending" },
        };
      },
    } as unknown as WatcherProductionDurableRuntimeV1;
    const coordinator = unsafeCreateWatcherProductionChainCoordinatorForTestV1(
      { policy, durable, observation },
      {
        admitRollForward: (event) =>
          event.blockHash === first.blockHash ? first : second,
      },
    );

    await coordinator.handle(forward(first));
    await coordinator.handle(forward(second));

    expect(observed).toEqual(["10:1", "10:2", "11:1"]);
    expect(persisted).toEqual(["10", "10", "11"]);
    expect(coordinator.status()).toMatchObject({
      quarantined: false,
      bufferedBlockCount: 1,
    });
  });

  it("journals a direct-child replacement before applying a pending rollback", async () => {
    const orphan = block("3", "1", "102", "12");
    const replacement = block("4", "1", "103", "12");
    let state = finalityState("pending", orphan);
    const order: string[] = [];
    const observation = {
      observe: async () => ({
        block: {},
        consistency: {},
        transportAttestations: [],
      }),
      close: () => undefined,
    } as unknown as WatcherLocalKupmiosNativeObservationRuntimeV1;
    const durable = {
      read: () => ({ currentFinalityState: state, currentStore: {} }),
      persistObservation: async () => {
        order.push("observation");
        return { persistence: "committed" };
      },
      persistRollback: async () => {
        order.push("rollback");
        state = finalityState("pending", replacement);
        return {
          persistence: "committed",
          result: {
            action: "apply_rewind",
            protocolDecision: "resume_pending",
          },
        };
      },
      persistCanonicalProgress: async () => ({
        persistence: "unchanged",
        finalityResult: { action: "duplicate" },
      }),
    } as unknown as WatcherProductionDurableRuntimeV1;
    const coordinator = unsafeCreateWatcherProductionChainCoordinatorForTestV1(
      { policy, durable, observation },
      { admitRollForward: () => replacement },
    );

    await coordinator.handle({
      schemaVersion: "midgard-watcher-native-chain-sync-v1",
      kind: "roll_backward",
      point: { kind: "point", blockHash: h32("1"), slot: "101" },
      tip: { kind: "point", blockHash: h32("2"), slot: "103", blockNo: "13" },
    });
    await coordinator.handle(forward(replacement, "13"));

    expect(order).toEqual(["observation", "rollback"]);
    expect(coordinator.status().rollbackPoint).toBeNull();
  });

  it("fails before durable mutation when a replacement is not anchored to the rollback point", async () => {
    const replacement = block("4", "9", "103", "12");
    const observation = {
      observe: async () => {
        throw new Error("must not observe");
      },
      close: () => undefined,
    } as unknown as WatcherLocalKupmiosNativeObservationRuntimeV1;
    const durable = {
      read: () => ({
        currentFinalityState: finalityState("pending", replacement),
        currentStore: {},
      }),
    } as unknown as WatcherProductionDurableRuntimeV1;
    const coordinator = unsafeCreateWatcherProductionChainCoordinatorForTestV1(
      { policy, durable, observation },
      { admitRollForward: () => replacement },
    );
    await coordinator.handle({
      schemaVersion: "midgard-watcher-native-chain-sync-v1",
      kind: "roll_backward",
      point: { kind: "point", blockHash: h32("1"), slot: "101" },
      tip: { kind: "point", blockHash: h32("2"), slot: "103", blockNo: "13" },
    });
    await expect(
      coordinator.handle(forward(replacement, "13")),
    ).rejects.toThrow("not the child");
  });

  it("derives bounded post-finality paths from sidecar-authenticated history and resumes only after recovery", async () => {
    const ancestor = block("5", "4", "200", "20");
    const orphan = block("6", "5", "201", "21");
    const replacement = block("7", "5", "202", "21");
    const consistency = (
      digestByte: string,
      candidate: WatcherNativeBlockAdmissionV1,
    ) =>
      ({
        schemaVersion: "midgard-watcher-multi-provider-consistency-v1",
        status: "agreed",
        protocolDecision: "allowed",
        consistencyDigest: h32(digestByte),
        agreement: {
          blockHash: candidate.blockHash,
          blockNo: candidate.blockNo,
          slot: candidate.slot,
          minimumDepth: "30",
        },
      }) as never;
    const ancestorConsistency = consistency("a", ancestor);
    const orphanConsistency = consistency("b", orphan);
    const replacementConsistency = consistency("c", replacement);
    type RecoveryState = Omit<ReturnType<typeof finalityState>, "finalized"> & {
      finalized:
        | (NonNullable<ReturnType<typeof finalityState>["finalized"]> & {
            lastSeenConsistencyDigest: string;
          })
        | null;
    };
    let state: RecoveryState = {
      ...finalityState("finalized", orphan),
      finalized: {
        ...finalityState("finalized", orphan).finalized!,
        lastSeenConsistencyDigest: h32("b"),
      },
    };
    let history = [ancestorConsistency, orphanConsistency];
    let recoveryCalls = 0;
    const observation = {
      observe: async () => ({
        block: {},
        observations: [],
        consistency: replacementConsistency,
        transportAttestations: [],
      }),
      close: () => undefined,
    } as unknown as WatcherLocalKupmiosNativeObservationRuntimeV1;
    const durable = {
      read: () => ({
        currentFinalityState: state,
        currentStore: {},
        authenticatedConsistencyHistory: history,
      }),
      persistObservation: async () => {
        history = [...history, replacementConsistency];
        return { persistence: "committed" };
      },
      persistRollback: async () => {
        state = { ...finalityState("quarantined"), finalized: null };
        return {
          persistence: "committed",
          result: {
            action: "quarantine_incident",
            protocolDecision: "quarantined",
          },
        };
      },
      persistPostFinalityRecovery: async ({
        previousCanonicalPath,
        replacementCanonicalPath,
      }: {
        readonly previousCanonicalPath: readonly unknown[];
        readonly replacementCanonicalPath: readonly unknown[];
      }) => {
        recoveryCalls += 1;
        expect(previousCanonicalPath).toEqual([
          ancestorConsistency,
          orphanConsistency,
        ]);
        expect(replacementCanonicalPath).toEqual([
          ancestorConsistency,
          replacementConsistency,
        ]);
        state = { ...finalityState("unobserved"), finalized: null };
        return {
          persistence: "committed",
          result: {
            action: "rewind_and_replay",
            protocolDecision: "resume_replay",
          },
        };
      },
      persistCanonicalProgress: async () => ({
        persistence: "committed",
        finalityResult: { action: "observe_pending" },
      }),
    } as unknown as WatcherProductionDurableRuntimeV1;
    const coordinator = unsafeCreateWatcherProductionChainCoordinatorForTestV1(
      { policy, durable, observation },
      { admitRollForward: () => replacement },
    );
    await coordinator.handle({
      schemaVersion: "midgard-watcher-native-chain-sync-v1",
      kind: "roll_backward",
      point: {
        kind: "point",
        blockHash: ancestor.blockHash,
        slot: ancestor.slot,
      },
      tip: {
        kind: "point",
        blockHash: replacement.blockHash,
        slot: replacement.slot,
        blockNo: replacement.blockNo,
      },
    });
    await coordinator.handle(forward(replacement));

    expect(recoveryCalls).toBe(1);
    expect(coordinator.status().quarantined).toBe(false);
  });

  it("reconciles an authenticated quarantined snapshot from the selected native ancestor after restart", async () => {
    const ancestor = block("8", "7", "300", "30");
    const orphan = block("9", "8", "301", "31");
    const replacement = block("a", "8", "302", "31");
    const consistency = (
      digestByte: string,
      candidate: WatcherNativeBlockAdmissionV1,
    ) =>
      ({
        schemaVersion: "midgard-watcher-multi-provider-consistency-v1",
        status: "agreed",
        protocolDecision: "allowed",
        consistencyDigest: h32(digestByte),
        agreement: {
          blockHash: candidate.blockHash,
          blockNo: candidate.blockNo,
          slot: candidate.slot,
          minimumDepth: "30",
        },
      }) as never;
    const ancestorConsistency = consistency("d", ancestor);
    const orphanConsistency = consistency("e", orphan);
    const triggerConsistency = consistency("f", replacement);
    let state: {
      phase: "unobserved" | "pending" | "finalized" | "quarantined";
      pending: ReturnType<typeof finalityState>["pending"];
      finalized: Readonly<{
        blockHash: string;
        slot: string;
        blockNo: string;
        lastSeenConsistencyDigest: string;
      }> | null;
      incident: Readonly<{ triggerConsistencyDigest: string }> | null;
    } = {
      ...finalityState("quarantined"),
      finalized: {
        blockHash: orphan.blockHash,
        slot: orphan.slot,
        blockNo: orphan.blockNo,
        lastSeenConsistencyDigest: h32("e"),
      },
      incident: { triggerConsistencyDigest: h32("f") },
    };
    const order: string[] = [];
    const observation = {
      observe: async () => {
        order.push("observe");
        return {
          block: {},
          observations: [],
          consistency: triggerConsistency,
          transportAttestations: [],
        };
      },
      close: () => undefined,
    } as unknown as WatcherLocalKupmiosNativeObservationRuntimeV1;
    const durable = {
      read: () => ({
        currentFinalityState: state,
        currentStore: {},
        authenticatedConsistencyHistory: [
          ancestorConsistency,
          orphanConsistency,
          triggerConsistency,
        ],
      }),
      persistPostFinalityRecovery: async () => {
        order.push("restart-recovery");
        state = {
          ...finalityState("unobserved"),
          finalized: null,
          incident: null,
        };
        return {
          persistence: "committed",
          result: {
            action: "rewind_and_replay",
            protocolDecision: "resume_replay",
          },
        };
      },
      persistCanonicalProgress: async () => {
        order.push("canonical-progress");
        return {
          persistence: "committed",
          finalityResult: { action: "observe_pending" },
        };
      },
    } as unknown as WatcherProductionDurableRuntimeV1;
    const coordinator = unsafeCreateWatcherProductionChainCoordinatorForTestV1(
      {
        policy,
        durable,
        observation,
        restartIntersection: {
          kind: "point",
          blockHash: ancestor.blockHash,
          slot: ancestor.slot,
        },
      },
      { admitRollForward: () => replacement },
    );

    await coordinator.handle(forward(replacement));

    expect(order).toEqual([
      "restart-recovery",
      "observe",
      "canonical-progress",
    ]);
    expect(coordinator.status().quarantined).toBe(false);
  });
});
