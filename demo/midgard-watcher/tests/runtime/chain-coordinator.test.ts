import { describe, expect, it } from "vitest";

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

describe("production native-chain coordinator", () => {
  it("treats only the initial exact intersection acknowledgement as startup when finality is unobserved", async () => {
    const first = block("2", "1", "102", "12");
    const replacement = block("3", "1", "103", "12");
    const intersection = {
      kind: "point" as const,
      blockHash: h32("1"),
      slot: "101",
    };
    let state = finalityState("unobserved");
    const order: string[] = [];
    const observation = {
      observe: async () => ({
        block: {},
        consistency: {},
        transportAttestations: [],
      }),
      close: () => undefined,
    } as unknown as WatcherLocalKupmiosNativeObservationRuntime;
    const durable = {
      readFinality: () => state,
      read: () => ({ currentFinalityState: state, currentStore: {} }),
      persistCanonicalProgress: async () => {
        if (state.phase === "unobserved") {
          order.push("first-visibility");
          state = finalityState("pending", first);
          return {
            persistence: "committed",
            finalityResult: { action: "observe_pending" },
          };
        }
        return {
          persistence: "unchanged",
          finalityResult: { action: "duplicate" },
        };
      },
      persistObservation: async () => {
        order.push("observation");
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
    } as unknown as WatcherDurableRuntime;
    const coordinator = unsafeCreateWatcherChainCoordinatorForTest(
      {
        policy,
        durable,
        observation,
        restartIntersection: intersection,
        hooks: {
          onRollback: async () => {
            order.push("revoke");
          },
          onFinalized: async () => undefined,
        },
      },
      {
        admitRollForward: (event) =>
          event.blockHash === first.blockHash ? first : replacement,
      },
    );
    const backward = {
      schemaVersion: "midgard-watcher-native-chain-sync-v1" as const,
      kind: "roll_backward" as const,
      point: intersection,
      tip: {
        kind: "point" as const,
        blockHash: first.blockHash,
        slot: first.slot,
        blockNo: first.blockNo,
      },
    };
    await coordinator.handle(backward);
    expect(order).toEqual([]);
    expect(coordinator.status().rollbackPoint).toBeNull();
    await coordinator.handle(forward(first));
    expect(order).toEqual(["first-visibility"]);
    await coordinator.handle(backward);
    await coordinator.handle(forward(replacement));
    expect(order).toEqual([
      "first-visibility",
      "revoke",
      "observation",
      "rollback",
    ]);
  });

  it.each(["blockHash", "slot"] as const)(
    "does not suppress a first rollback with another intersection %s",
    async (field) => {
      const intersection = {
        kind: "point" as const,
        blockHash: h32("1"),
        slot: "101",
      };
      const point = {
        ...intersection,
        [field]: field === "blockHash" ? h32("2") : "100",
      };
      const revoked: unknown[] = [];
      const coordinator = unsafeCreateWatcherChainCoordinatorForTest(
        {
          policy,
          durable: {
            readFinality: () => finalityState("unobserved"),
            read: () => ({ currentFinalityState: finalityState("unobserved") }),
          } as unknown as WatcherDurableRuntime,
          observation: {} as WatcherLocalKupmiosNativeObservationRuntime,
          restartIntersection: intersection,
          hooks: {
            onRollback: async (point) => {
              revoked.push(point);
            },
            onFinalized: async () => undefined,
          },
        },
        {
          admitRollForward: () => {
            throw new Error("not used");
          },
        },
      );
      await coordinator.handle({
        schemaVersion: "midgard-watcher-native-chain-sync-v1",
        kind: "roll_backward",
        point,
        tip: { kind: "point", blockHash: h32("3"), slot: "102", blockNo: "12" },
      });
      expect(revoked).toEqual([point]);
      expect(coordinator.status().rollbackPoint).toEqual(point);
    },
  );

  const retainedPrefixFixture = (
    phase: "pending" | "finalized",
    emptyPrefix = false,
    intersectionAtRetained = false,
  ) => {
    const first = block("1", "0", "100", "10");
    const second = block("2", "1", "101", "11");
    const retained = block("3", "2", "102", "12");
    const intersection = {
      kind: "point" as const,
      blockHash: intersectionAtRetained
        ? retained.blockHash
        : emptyPrefix
          ? second.blockHash
          : h32("0"),
      slot: intersectionAtRetained
        ? retained.slot
        : emptyPrefix
          ? second.slot
          : "99",
    };
    const delivered: string[] = [];
    const rolledBack: unknown[] = [];
    const persisted: string[] = [];
    const candidates = new Map(
      [first, second, retained].map((item) => [item.blockHash, item]),
    );
    const state = finalityState(phase, retained);
    const coordinator = unsafeCreateWatcherChainCoordinatorForTest(
      {
        policy,
        restartIntersection: intersection,
        durable: {
          readFinality: () => state,
          read: () => ({
            currentFinalityState: state,
            currentStore: {},
            authenticatedConsistencyHistory: [],
          }),
          persistCanonicalProgress: async (input: {
            block: { chainPoint: { blockNo: string } };
          }) => {
            persisted.push(input.block.chainPoint.blockNo);
            if (
              phase === "finalized" &&
              input.block.chainPoint.blockNo === "13"
            ) {
              return {
                persistence: "committed",
                finalityResult: { action: "observe_pending" },
              };
            }
            if (
              phase !== "pending" ||
              input.block.chainPoint.blockNo !== retained.blockNo
            ) {
              throw new Error(
                "Retained history must not rewind canonical progress",
              );
            }
            return {
              persistence: "unchanged",
              finalityResult: { action: "duplicate" },
            };
          },
        } as unknown as WatcherDurableRuntime,
        observation: {
          observe: async ({
            block: candidate,
          }: {
            block: WatcherNativeBlockAdmission;
          }) => ({
            block: { chainPoint: { blockNo: candidate.blockNo } },
            consistency: {},
            transportAttestations: [],
          }),
          close: () => undefined,
        } as unknown as WatcherLocalKupmiosNativeObservationRuntime,
        hooks: {
          onRollback: async (point) => {
            rolledBack.push(point);
          },
          onFinalized: async ({ nativeBlock }) => {
            delivered.push(nativeBlock.blockNo);
          },
        },
      },
      {
        admitRollForward: (event) => {
          const candidate = candidates.get(event.blockHash);
          if (candidate === undefined) throw new Error("Unknown fixture block");
          return candidate;
        },
      },
    );
    const acknowledge = async () =>
      await coordinator.handle({
        schemaVersion: "midgard-watcher-native-chain-sync-v1",
        kind: "roll_backward",
        point: intersection,
        tip: {
          kind: "point",
          blockHash: retained.blockHash,
          slot: "1000",
          blockNo: "100",
        },
      });
    return {
      coordinator,
      first,
      second,
      retained,
      candidates,
      delivered,
      rolledBack,
      persisted,
      acknowledge,
    };
  };

  it.each(["pending", "finalized"] as const)(
    "checks retained %s ancestry and predecessor finality before releasing recovered blocks",
    async (phase) => {
      const fixture = retainedPrefixFixture(phase);
      await fixture.acknowledge();
      expect(fixture.rolledBack).toEqual([]);
      await fixture.coordinator.handle(forward(fixture.first, "100"));
      expect(fixture.delivered).toEqual([]);
      await fixture.coordinator.handle(forward(fixture.second, "100"));
      expect(fixture.delivered).toEqual([]);
      if (phase === "pending") {
        await expect(
          fixture.coordinator.handle(forward(fixture.retained, "100")),
        ).rejects.toThrow();
        expect(fixture.delivered).toEqual([]);
        expect(fixture.persisted).toEqual([]);
        return;
      }
      await fixture.coordinator.handle(forward(fixture.retained, "100"));
      expect(fixture.delivered).toEqual(["10", "11", "12"]);
      await fixture.coordinator.handle(forward(fixture.retained, "100"));
      expect(fixture.delivered).toEqual(["10", "11", "12"]);
      expect(fixture.persisted.every((height) => height === "12")).toBe(true);
    },
  );

  it("acknowledges an initial pending point without inferring any finalized ancestor when the replay prefix is empty", async () => {
    const fixture = retainedPrefixFixture("pending", true);
    await fixture.acknowledge();
    expect(fixture.rolledBack).toEqual([]);
    await fixture.coordinator.handle(forward(fixture.retained, "100"));
    expect(fixture.delivered).toEqual([]);
    expect(fixture.persisted).toEqual(["12"]);
  });

  it("starts the finalized head's child when the restart intersection already equals that head", async () => {
    const fixture = retainedPrefixFixture("finalized", false, true);
    const child = block("4", "3", "103", "13");
    fixture.candidates.set(child.blockHash, child);
    await fixture.acknowledge();
    expect(fixture.rolledBack).toEqual([]);
    await fixture.coordinator.handle(forward(child, "100"));
    expect(fixture.delivered).toEqual([]);
    expect(fixture.persisted).toEqual(["13"]);
  });

  it.each(["wrong_boundary", "wrong_parent", "missing_block"] as const)(
    "rejects retained replay with %s before releasing any queue hook",
    async (variant) => {
      const fixture = retainedPrefixFixture("finalized");
      await fixture.acknowledge();
      await fixture.coordinator.handle(forward(fixture.first, "100"));
      expect(fixture.delivered).toEqual([]);
      if (variant !== "missing_block") {
        const middle =
          variant === "wrong_parent"
            ? { ...fixture.second, prevHash: h32("8") }
            : fixture.second;
        fixture.candidates.set(middle.blockHash, middle);
        await fixture.coordinator.handle(forward(middle, "100"));
        expect(fixture.delivered).toEqual([]);
      }
      const boundary =
        variant === "wrong_boundary"
          ? { ...fixture.retained, blockHash: h32("9") }
          : fixture.retained;
      fixture.candidates.set(boundary.blockHash, boundary);
      await expect(
        fixture.coordinator.handle(forward(boundary, "100")),
      ).rejects.toThrow();
      expect(fixture.delivered).toEqual([]);
      expect(fixture.persisted).toEqual([]);
    },
  );

  it.each([0, 5_000])(
    "captures blocks %i behind the live tip and persists pending finality before the child",
    async (lag) => {
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
          readonly block: WatcherNativeBlockAdmission;
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
      } as unknown as WatcherLocalKupmiosNativeObservationRuntime;
      const durable = {
        readFinality: () => state,
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
      } as unknown as WatcherDurableRuntime;
      const coordinator = unsafeCreateWatcherChainCoordinatorForTest(
        { policy, durable, observation },
        {
          admitRollForward: (event) =>
            event.blockHash === first.blockHash ? first : second,
        },
      );

      await coordinator.handle(
        forward(first, String(Number(first.blockNo) + lag)),
      );
      await coordinator.handle(
        forward(second, String(Number(second.blockNo) + lag)),
      );

      if (lag === 0) {
        // Below confirmation depth a pending block is not re-observed or
        // re-persisted: the second observation waits for depth 30.
        expect(observed).toEqual(["10:1", "11:1"]);
        expect(persisted).toEqual(["10"]);
        expect(coordinator.status()).toMatchObject({
          quarantined: false,
          bufferedBlockCount: 2,
        });
        return;
      }
      expect(observed).toEqual([
        `10:${lag + 1}`,
        `11:${lag + 1}`,
        `10:${lag + 2}`,
      ]);
      expect(persisted).toEqual(["10", "10", "11"]);
      expect(coordinator.status()).toMatchObject({
        quarantined: false,
        bufferedBlockCount: 1,
      });
    },
  );

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
    } as unknown as WatcherLocalKupmiosNativeObservationRuntime;
    const durable = {
      readFinality: () => state,
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
    } as unknown as WatcherDurableRuntime;
    const coordinator = unsafeCreateWatcherChainCoordinatorForTest(
      {
        policy,
        durable,
        observation,
        hooks: {
          onRollback: async () => {
            order.push("revoke");
          },
          onFinalized: async () => {
            throw new Error("Replacement must retain first visibility");
          },
        },
        restartIntersection: {
          kind: "point",
          blockHash: h32("1"),
          slot: "101",
        },
      },
      { admitRollForward: () => replacement },
    );

    await coordinator.handle({
      schemaVersion: "midgard-watcher-native-chain-sync-v1",
      kind: "roll_backward",
      point: { kind: "point", blockHash: h32("1"), slot: "101" },
      tip: { kind: "point", blockHash: h32("2"), slot: "103", blockNo: "13" },
    });
    await coordinator.handle(forward(replacement, "13"));

    expect(order).toEqual(["revoke", "observation", "rollback"]);
    expect(coordinator.status().rollbackPoint).toBeNull();
  });

  it("fails before durable mutation when a replacement is not anchored to the rollback point", async () => {
    const replacement = block("4", "9", "103", "12");
    const observation = {
      observe: async () => {
        throw new Error("must not observe");
      },
      close: () => undefined,
    } as unknown as WatcherLocalKupmiosNativeObservationRuntime;
    const durable = {
      readFinality: () => finalityState("pending", replacement),
      read: () => ({
        currentFinalityState: finalityState("pending", replacement),
        currentStore: {},
      }),
    } as unknown as WatcherDurableRuntime;
    const coordinator = unsafeCreateWatcherChainCoordinatorForTest(
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
      candidate: WatcherNativeBlockAdmission,
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
    } as unknown as WatcherLocalKupmiosNativeObservationRuntime;
    const durable = {
      readFinality: () => state,
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
    } as unknown as WatcherDurableRuntime;
    const coordinator = unsafeCreateWatcherChainCoordinatorForTest(
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
      candidate: WatcherNativeBlockAdmission,
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
    } as unknown as WatcherLocalKupmiosNativeObservationRuntime;
    const durable = {
      readFinality: () => state,
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
    } as unknown as WatcherDurableRuntime;
    const coordinator = unsafeCreateWatcherChainCoordinatorForTest(
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
    expect(coordinator.status().rollbackPoint).toBeNull();
    await coordinator.handle(forward(replacement));

    expect(order).toEqual([
      "restart-recovery",
      "observe",
      "canonical-progress",
    ]);
    expect(coordinator.status().quarantined).toBe(false);
  });
});
