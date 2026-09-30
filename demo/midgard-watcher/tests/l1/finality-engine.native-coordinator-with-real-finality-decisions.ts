import "./finality-engine.canonical-release-bound-watcher-finality.js";

import { describe, expect, it } from "vitest";

import {
  evaluateWatcherFinality,
  makeWatcherFinalityBootstrapState,
} from "../../src/l1/finality-engine.js";
import type {
  WatcherLocalKupmiosNativeObservation,
  WatcherLocalKupmiosNativeObservationRuntime,
} from "../../src/l1/local-kupmios-native-observation.js";
import type { WatcherNativeBlockAdmission } from "../../src/l1/native-block-admission.js";
import { unsafeCreateWatcherChainCoordinatorForTest } from "../../src/runtime/chain-coordinator.js";
import type { WatcherDurableRuntime } from "../../src/storage/durable-runtime.js";
import { externalSource, policy } from "./finality-engine.config.js";
import {
  agreement,
  evaluateWatcherMultiProviderConsistency,
  observation,
  type ObservationOptions,
  pendingAt,
  provider,
} from "./finality-engine.policy-over-providers.js";

describe("native coordinator with real finality decisions", () => {
  it.each(["fixed_tip", "one_tip_growth", "continued_tip_growth"] as const)(
    "releases buffered empty blocks at two increasing depths once in order: %s",
    async (mode) => {
      const finalityPolicy = policy(30);
      let state = makeWatcherFinalityBootstrapState(finalityPolicy);
      if (state === null) throw new Error("Fixture bootstrap rejected");
      const delivered: number[] = [];
      const hash = (height: number) => height.toString(16).padStart(64, "0");
      const block = (height: number): WatcherNativeBlockAdmission => ({
        schemaVersion: "midgard-watcher-native-block-admission-v1",
        blockType: "7",
        protocolMajor: "10",
        blockHash: hash(height),
        prevHash: hash(height - 1),
        slot: String(height * 10),
        blockNo: String(height),
        rawBlockCbor: "80",
        rawHeaderCbor: "80",
        transactionIds: [],
        transactionCbors: [],
      });
      const durable = {
        readFinality: () => state,
        read: () => ({
          currentFinalityState: state,
          authenticatedConsistencyHistory: [],
          currentStore: {},
        }),
        persistCanonicalProgress: async (
          observed: WatcherLocalKupmiosNativeObservation,
        ) => {
          if (state === null) throw new Error("Fixture state rejected");
          let evaluationState = state;
          const point = observed.block.chainPoint;
          // Reproduce durable canonical progress's direct-child/bootstrap
          // selection; the finality decision itself uses the real evaluator.
          if (
            state.phase === "finalized" &&
            state.finalized?.pointDigest !== point.pointDigest
          ) {
            const finalized = state.finalized;
            if (
              finalized === null ||
              point.parentBlockHash !== finalized.blockHash ||
              BigInt(point.blockNo) !== BigInt(finalized.blockNo) + 1n ||
              BigInt(point.slot) <= BigInt(finalized.slot)
            ) {
              throw new Error("Canonical progress is not a direct child");
            }
            const bootstrap = makeWatcherFinalityBootstrapState(finalityPolicy);
            if (bootstrap === null)
              throw new Error("Fixture bootstrap rejected");
            evaluationState = bootstrap;
          }
          const finalityResult = evaluateWatcherFinality(
            finalityPolicy,
            evaluationState,
            observed.consistency,
          );
          if (
            finalityResult.state === null ||
            ["reject", "rewind_pending", "quarantine_incident"].includes(
              finalityResult.action,
            )
          ) {
            throw new Error(
              `Canonical progress rejected: ${finalityResult.reasonCodes.join(",")}`,
            );
          }
          state = finalityResult.state;
          return {
            persistence:
              finalityResult.action === "duplicate" ? "unchanged" : "committed",
            finalityResult,
          };
        },
      } as unknown as WatcherDurableRuntime;
      const localObservation = {
        observe: async ({
          block: nativeBlock,
          depth,
        }: {
          readonly block: WatcherNativeBlockAdmission;
          readonly depth: string;
        }): Promise<WatcherLocalKupmiosNativeObservation> => {
          const options = {
            blockHash: nativeBlock.blockHash,
            parentBlockHash: nativeBlock.prevHash,
            slot: nativeBlock.slot,
            blockNo: nativeBlock.blockNo,
            depth,
          };
          const blocks = [
            observation("provider-a", "a1", options),
            observation("provider-b", "b2", options),
          ] as const;
          return {
            schemaVersion:
              "midgard-watcher-local-kupmios-native-observation-v1",
            block: blocks[0],
            ogmiosBlock: blocks[0],
            kupoCheckpoint: blocks[1],
            observations: blocks,
            transportAttestations: [
              provider("provider-a", "a1"),
              provider("provider-b", "b2"),
            ],
            consistency: evaluateWatcherMultiProviderConsistency(
              externalSource(),
              blocks,
            ),
          };
        },
        close: () => undefined,
      } as unknown as WatcherLocalKupmiosNativeObservationRuntime;
      const coordinator = unsafeCreateWatcherChainCoordinatorForTest(
        {
          policy: finalityPolicy,
          durable,
          observation: localObservation,
          hooks: {
            onRollback: async () => undefined,
            onFinalized: async ({ nativeBlock }) => {
              delivered.push(Number(nativeBlock.blockNo));
            },
          },
        },
        { admitRollForward: (event) => block(Number(event.blockNo)) },
      );
      const send = async (height: number, tip: number) => {
        const admitted = block(height);
        await coordinator.handle({
          schemaVersion: "midgard-watcher-native-chain-sync-v1",
          kind: "roll_forward",
          blockHash: admitted.blockHash,
          blockType: admitted.blockType,
          prevHash: admitted.prevHash,
          slot: admitted.slot,
          blockNo: admitted.blockNo,
          rawBlockCbor: admitted.rawBlockCbor,
          tip: {
            kind: "point",
            blockHash: hash(tip),
            slot: String(tip * 10),
            blockNo: String(tip),
          },
        });
      };
      for (let height = 100; height <= 105; height += 1) {
        await send(height, height);
      }
      expect(delivered).toEqual([]);
      for (let height = 106; height <= 110; height += 1) {
        await send(height, 140);
      }
      // The earlier native arrivals supply the first real observation for
      // 100..105. Blocks106..110 have only been seen at tip140 and must wait.
      if (mode === "fixed_tip") {
        expect(delivered).toEqual([100, 101, 102, 103, 104, 105]);
        return;
      }
      await send(111, 141);
      if (mode === "one_tip_growth") {
        expect(delivered).toEqual(
          Array.from({ length: 11 }, (_, i) => 100 + i),
        );
        return;
      }
      for (let height = 112; height <= 118; height += 1) {
        await send(height, 140 + height - 110);
      }
      expect(delivered).toEqual(Array.from({ length: 18 }, (_, i) => 100 + i));
    },
  );

  it.each(["increasing_depth", "same_depth"] as const)(
    "requires real predecessor finality before recovering a nonempty pending prefix: %s",
    async (historyKind) => {
      const finalityPolicy = policy(30);
      const hash = (height: number) => height.toString(16).padStart(64, "0");
      const block = (height: number): WatcherNativeBlockAdmission => ({
        schemaVersion: "midgard-watcher-native-block-admission-v1",
        blockType: "7",
        protocolMajor: "10",
        blockHash: hash(height),
        prevHash: hash(height - 1),
        slot: String(height * 10),
        blockNo: String(height),
        rawBlockCbor: "80",
        rawHeaderCbor: "80",
        transactionIds: [],
        transactionCbors: [],
      });
      const pointOptions = (height: number): ObservationOptions => ({
        blockHash: hash(height),
        parentBlockHash: hash(height - 1),
        slot: String(height * 10),
        blockNo: String(height),
      });
      const history = [
        agreement("39", pointOptions(101)),
        agreement(
          historyKind === "increasing_depth" ? "40" : "39",
          pointOptions(101),
        ),
      ];
      const first = evaluateWatcherFinality(finalityPolicy, null, history[0]);
      const second = evaluateWatcherFinality(
        finalityPolicy,
        first.state,
        history[1],
      );
      expect(second.action).toBe(
        historyKind === "increasing_depth" ? "finalize" : "duplicate",
      );
      let state = pendingAt(finalityPolicy, "39", pointOptions(102));
      const delivered: string[] = [];
      const observationRuntime = {
        observe: async ({
          block: native,
          depth,
        }: {
          block: WatcherNativeBlockAdmission;
          depth: string;
        }): Promise<WatcherLocalKupmiosNativeObservation> => {
          const options = { ...pointOptions(Number(native.blockNo)), depth };
          const blocks = [
            observation("provider-a", "a1", options),
            observation("provider-b", "b2", options),
          ] as const;
          return {
            schemaVersion:
              "midgard-watcher-local-kupmios-native-observation-v1",
            block: blocks[0],
            ogmiosBlock: blocks[0],
            kupoCheckpoint: blocks[1],
            observations: blocks,
            transportAttestations: [
              provider("provider-a", "a1"),
              provider("provider-b", "b2"),
            ],
            consistency: evaluateWatcherMultiProviderConsistency(
              externalSource(),
              blocks,
            ),
          };
        },
        close: () => undefined,
      } as unknown as WatcherLocalKupmiosNativeObservationRuntime;
      const intersection = {
        kind: "point" as const,
        blockHash: hash(99),
        slot: "990",
      };
      const coordinator = unsafeCreateWatcherChainCoordinatorForTest(
        {
          policy: finalityPolicy,
          restartIntersection: intersection,
          observation: observationRuntime,
          durable: {
            readFinality: () => state,
            read: () => ({
              currentFinalityState: state,
              authenticatedConsistencyHistory: history,
              currentStore: {},
            }),
            persistCanonicalProgress: async (
              observed: WatcherLocalKupmiosNativeObservation,
            ) => {
              expect(observed.block.chainPoint.blockNo).toBe("102");
              const finalityResult = evaluateWatcherFinality(
                finalityPolicy,
                state,
                observed.consistency,
              );
              expect(finalityResult.action).toBe("duplicate");
              if (finalityResult.state === null)
                throw new Error("Fixture state rejected");
              state = finalityResult.state;
              return { persistence: "unchanged", finalityResult };
            },
          } as unknown as WatcherDurableRuntime,
          hooks: {
            onRollback: async () => {
              throw new Error("Initial acknowledgement must not rewind");
            },
            onFinalized: async ({ nativeBlock }) => {
              delivered.push(nativeBlock.blockNo);
            },
          },
        },
        { admitRollForward: (event) => block(Number(event.blockNo)) },
      );
      const tip = {
        kind: "point" as const,
        blockHash: hash(140),
        slot: "1400",
        blockNo: "140",
      };
      await coordinator.handle({
        schemaVersion: "midgard-watcher-native-chain-sync-v1",
        kind: "roll_backward",
        point: intersection,
        tip,
      });
      const send = async (height: number) => {
        const native = block(height);
        await coordinator.handle({
          schemaVersion: "midgard-watcher-native-chain-sync-v1",
          kind: "roll_forward",
          blockHash: native.blockHash,
          prevHash: native.prevHash,
          slot: native.slot,
          blockNo: native.blockNo,
          blockType: native.blockType,
          rawBlockCbor: native.rawBlockCbor,
          tip,
        });
      };
      await send(100);
      expect(delivered).toEqual([]);
      await send(101);
      expect(delivered).toEqual([]);
      if (historyKind === "same_depth") {
        await expect(send(102)).rejects.toThrow();
        expect(delivered).toEqual([]);
      } else {
        await send(102);
        expect(delivered).toEqual(["100", "101"]);
        expect(state.phase).toBe("pending");
        await send(102);
        expect(delivered).toEqual(["100", "101"]);
      }
    },
  );
});
