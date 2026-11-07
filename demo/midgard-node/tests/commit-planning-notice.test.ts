import { Worker } from "node:worker_threads";

import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import {
  credentialToAddress,
  scriptHashToCredential,
} from "@lucid-evolution/lucid";
import { Effect, Ref } from "effect";
import { describe, expect, it } from "vitest";

import {
  beginDriverRecompute,
  publishDriverView,
} from "../src/services/follower-write-gate.driver.js";
import {
  followerViewOf,
  type GateState,
  readFollowerWriteGate,
} from "../src/services/follower-write-gate.js";
import { Globals } from "../src/services/globals.js";
import type { notifyCommitPlanningSelection } from "../src/workers/commit-block-header.notify-planning-selection.js";
import {
  applyCommitPlanningNotice,
  commitPlanningReadiness,
  type CommitPlanningSelection,
  makeCommitPlanningNotice,
  parseCommitPlanningNotice,
} from "../src/workers/commit-planning-notice.js";
import { serializeStateQueueUTxO } from "../src/workers/utils/commit-block-header.js";
import { provideDatabaseLayers } from "./utils.js";

const selected: CommitPlanningSelection = {
  mode: "catch_up",
  observedAtMs: 5_000,
  authority: {
    view: { generation: 7, slot: 101, height: 90, hash: "aa".repeat(32) },
    epoch: "1",
    baseHeaderHash: "bb".repeat(28),
    baseUtxosRoot: "cc".repeat(32),
    deploymentFingerprint: "dd".repeat(32),
  },
  coverageStartTimeMs: 100,
  selectedEndTimeMs: 200,
  blockTimeMs: 1_000,
  canonicalHorizonMs: 900,
  eligibleCutoffMs: 900,
  eligibleEventCountLowerBounds: {
    deposits: 4,
    withdrawals: 0,
    forcedTransactions: 0,
  },
  selectedEventCounts: { deposits: 2, withdrawals: 0, forcedTransactions: 0 },
  includedInstantCount: 1,
  selectedOrdinaryTxCount: 0,
  rescue: null,
};
const gate: GateState = {
  epoch: selected.authority.epoch,
  applied: selected.authority.view,
  pending: undefined,
};
const project = (
  snapshot: Parameters<typeof commitPlanningReadiness>[0]["snapshot"],
  overrides: Partial<Parameters<typeof commitPlanningReadiness>[0]> = {},
) =>
  commitPlanningReadiness({
    snapshot,
    gate,
    localEpoch: gate.epoch,
    recomputing: false,
    deploymentFingerprint: selected.authority.deploymentFingerprint!,
    nowMs: 5_050,
    maxAgeMs: 100,
    ...overrides,
  });

it("real driver epochs cross the compiled planning notifier and dispatcher losslessly", async () => {
  const notifierUrl = new URL(
    "../dist/commit-block-header.notify-planning-selection.js",
    import.meta.url,
  ).href;
  const consumerUrl = new URL(
    "../dist/commit-planning-notice.js",
    import.meta.url,
  ).href;
  const dispatcherUrl = new URL(
    "../dist/commit-worker-message.js",
    import.meta.url,
  ).href;
  const compiled = (await import(
    consumerUrl
  )) as typeof import("../src/workers/commit-planning-notice.js");
  const dispatcher = (await import(
    dispatcherUrl
  )) as typeof import("../src/workers/commit-worker-message.js");
  await Effect.runPromise(
    provideDatabaseLayers(
      Effect.gen(function* () {
        const globals = yield* Globals;
        const sql = yield* SqlClient.SqlClient;
        // Only this invocation's disposable shard is seeded. Production driver
        // increments and emits the string; the fixture cannot round it to Number.
        yield* sql`UPDATE node_follower_write_gate SET epoch = 9007199254740992 WHERE singleton`;
        const epoch = yield* beginDriverRecompute(
          "planning-regression",
          "initial published view",
        );
        yield* publishDriverView(
          epoch,
          followerViewOf(selected.authority.view),
        );
        const currentGate = yield* readFollowerWriteGate;
        expect(epoch).toBe("9007199254740993");
        expect(currentGate?.epoch).toBe(epoch);
        expect((yield* Ref.get(globals.FOLLOWER_WRITE_GATE)).epoch).toBe(epoch);
        const input: Omit<
          Parameters<typeof notifyCommitPlanningSelection>[0],
          "notify"
        > = {
          workerInput: {
            history: { view: currentGate!.applied!, epoch: currentGate!.epoch },
            data: {
              availableConfirmedBlock: "",
              availableLocalFinalizationBlock: "",
              currentBlockStartTimeMs: 100,
              localFinalizationPending: false,
              ledgerStoreLeaseOwner:
                "commit:12345678-1234-4123-8123-123456789abc",
              mempoolTxsCountSoFar: 0,
              sizeOfProcessedTxsSoFar: 0,
            },
          },
          parent: {
            assetName: SDK.STATE_QUEUE_ROOT_ASSET_NAME,
            utxo: {
              txHash: "44".repeat(32),
              outputIndex: 0,
              address: credentialToAddress(
                "Preprod",
                scriptHashToCredential("bb".repeat(28)),
              ),
              assets: {
                lovelace: 2_000_000n,
                ["bb".repeat(28) + SDK.STATE_QUEUE_ROOT_ASSET_NAME]: 1n,
              },
            },
            datum: {
              key: "Empty",
              next: "Empty",
              data: SDK.castConfirmedStateToData({
                headerHash: selected.authority.baseHeaderHash,
                prevHeaderHash: "00".repeat(28),
                utxoRoot: selected.authority.baseUtxosRoot,
                startTime: 0n,
                endTime: 100n,
                protocolVersion: 1n,
              }) as SDK.LinkedListNodeView["data"],
            },
          },
          identity: {
            kind: "derived",
            manifestId: selected.authority.deploymentFingerprint!,
            consensusProfile: MIDGARD_CONSENSUS_PROFILE,
          },
          baseRoot: selected.authority.baseUtxosRoot,
          startTimeMs: 100,
          blockTimeMs: 1_000,
          canonicalHorizonMs: 900,
          eligibleCutoffMs: 900,
          groups: [
            {
              inclusionTimeMs: 200,
              deposits: 2,
              withdrawals: 0,
              forcedTransactions: 0,
            },
            {
              inclusionTimeMs: 300,
              deposits: 2,
              withdrawals: 0,
              forcedTransactions: 0,
            },
          ],
          plan: {
            kind: "selected",
            window: {
              mode: "catch_up",
              endTimeMs: 200,
              counts: selected.selectedEventCounts,
              includedInstantCount: 1,
            },
            candidateCount: 0,
            upperBoundBytes: 0n,
          },
        };
        const serializedParent = yield* serializeStateQueueUTxO({
          ...input.parent!,
          utxo: {
            ...input.parent!.utxo,
            datum: SDK.encodeLinkedListNodeView(input.parent!.datum),
          },
        });
        const parentCodecUrl = new URL(
          "../dist/utils/commit-block-header.js",
          import.meta.url,
        ).href;
        const notice = yield* Effect.tryPromise({
          try: async () => {
            const worker = new Worker(
              new URL(
                "./helpers/commit-planning-compiled-producer.mjs",
                import.meta.url,
              ),
              {
                // The compiled worker uses production import conditions, not
                // Vitest's inherited source-resolution condition.
                execArgv: [],
                workerData: {
                  notifierUrl,
                  parentCodecUrl,
                  input: { ...input, parent: serializedParent },
                },
              },
            );
            try {
              return await new Promise<unknown>((resolve, reject) => {
                worker.once("message", resolve);
                worker.once("error", reject);
                worker.once("exit", (code) => {
                  if (code !== 0) reject(new Error(`producer exited ${code}`));
                });
              });
            } finally {
              await worker.terminate();
            }
          },
          catch: (error) => error,
        });
        const parsed = compiled.parseCommitPlanningNotice(notice);
        expect(parsed).toMatchObject({
          status: "selected",
          selection: { authority: { epoch } },
        });
        expect(parsed?.status).toBe("selected");
        if (parsed?.status !== "selected")
          throw new Error("compiled notifier emitted no selected plan");
        expect(
          dispatcher.takeCommitWorkerOutput(globals, parsed, 10),
        ).toBeUndefined();
        const snapshot = yield* Ref.get(globals.COMMIT_PLANNING);
        expect(snapshot?.selection.authority.epoch).toBe(epoch);
        const readiness = (
          gate: GateState | undefined,
          localEpoch: string | undefined,
        ) =>
          compiled.commitPlanningReadiness({
            snapshot,
            gate,
            localEpoch,
            recomputing: false,
            deploymentFingerprint: selected.authority.deploymentFingerprint!,
            nowMs: Date.now(),
            maxAgeMs: 10_000,
          });
        expect(
          readiness(
            currentGate,
            (yield* Ref.get(globals.FOLLOWER_WRITE_GATE)).epoch,
          ),
        ).toMatchObject({
          status: "current",
          reason: null,
          planned: { mode: "catch_up" },
        });
        const nextEpoch = yield* beginDriverRecompute(
          "planning-regression",
          "next published view",
        );
        yield* publishDriverView(
          nextEpoch,
          followerViewOf(selected.authority.view),
        );
        expect(nextEpoch).toBe("9007199254740994");
        expect(
          readiness(
            yield* readFollowerWriteGate,
            (yield* Ref.get(globals.FOLLOWER_WRITE_GATE)).epoch,
          ),
        ).toMatchObject({
          status: "stale",
          reason: "follower_authority_changed",
          planned: { authority: { epoch } },
        });
      }).pipe(Effect.provide(Globals.Default)),
    ),
  );
});

describe("planning authority and freshness", () => {
  it.each(["0", "1", "9007199254740993", "9223372036854775807"])(
    "preserves canonical PostgreSQL history epoch %s exactly",
    (epoch) => {
      const message = makeCommitPlanningNotice({
        ...selected,
        authority: { ...selected.authority, epoch },
      });
      expect(parseCommitPlanningNotice(structuredClone(message))).toMatchObject(
        {
          status: "selected",
          selection: { authority: { epoch } },
        },
      );
    },
  );
  it.each([
    ["UUID", "12345678-1234-4123-8123-123456789abc"],
    ["empty", ""],
    ["leading zero", "01"],
    ["repeated zero", "00"],
    ["negative", "-1"],
    ["plus", "+1"],
    ["leading space", " 1"],
    ["trailing space", "1 "],
    ["trailing newline", "1\n"],
    ["fraction", "1.0"],
    ["exponent", "1e1"],
    ["PostgreSQL overflow", "9223372036854775808"],
    ["large overflow", "9999999999999999999"],
    ["unbounded digits", "1".repeat(1_000)],
    ["number", 1],
  ])("clears mode on malformed history epoch: %s", async (_label, epoch) => {
    const message = {
      type: "CommitPlanningNotice",
      status: "selected",
      selection: { ...selected, authority: { ...selected.authority, epoch } },
    };
    const globals = {
      COMMIT_PLANNING: await Effect.runPromise(
        Ref.make({ selection: selected, receivedAtMs: 5_010 } as {
          selection: CommitPlanningSelection;
          receivedAtMs: number;
        } | null),
      ),
    };
    expect(parseCommitPlanningNotice(message)).toBeUndefined();
    await Effect.runPromise(applyCommitPlanningNotice(globals, message));
    expect(
      project(Effect.runSync(Ref.get(globals.COMMIT_PLANNING))),
    ).toMatchObject({
      status: "unavailable",
      reason: "no_selected_plan",
      planned: null,
    });
  });
  it("distinguishes adjacent epochs that Number would collapse", () => {
    const epoch = "9007199254740992";
    const currentGate = { ...gate, epoch };
    const snapshot = {
      selection: { ...selected, authority: { ...selected.authority, epoch } },
      receivedAtMs: 5_010,
    };
    expect(
      project(snapshot, { gate: currentGate, localEpoch: epoch }),
    ).toMatchObject({ status: "current", reason: null });
    expect(
      project(snapshot, {
        gate: { ...currentGate, epoch: "9007199254740993" },
        localEpoch: "9007199254740993",
      }),
    ).toMatchObject({ status: "stale", reason: "follower_authority_changed" });
  });

  it("marks a prior plan stale after a rewind, forward view, new epoch, recompute or deployment change", () => {
    const snapshot = { selection: selected, receivedAtMs: 5_010 };
    expect(project(snapshot)).toMatchObject({
      status: "current",
      planned: { mode: "catch_up" },
    });
    for (const overrides of [
      { gate: { ...gate, applied: { ...gate.applied!, generation: 8 } } },
      { gate: { ...gate, applied: { ...gate.applied!, slot: 102 } } },
      { localEpoch: "2" },
      { recomputing: true },
      { deploymentFingerprint: "ee".repeat(32) },
      { nowMs: 5_101 },
      { nowMs: 4_999 },
    ])
      expect(project(snapshot, overrides)).toMatchObject({ status: "stale" });
  });
  it("refuses malformed clocks, partial count authority and ordinary work in catch-up, clearing the old mode", async () => {
    const globals = {
      COMMIT_PLANNING: await Effect.runPromise(
        Ref.make({ selection: selected, receivedAtMs: 5_010 } as {
          selection: CommitPlanningSelection;
          receivedAtMs: number;
        } | null),
      ),
    };
    for (const altered of [
      { ...selected, selectedEndTimeMs: 901 },
      { ...selected, blockTimeMs: -1 },
      { ...selected, selectedOrdinaryTxCount: 1 },
      {
        ...selected,
        selectedEventCounts: {
          deposits: 5,
          withdrawals: 0,
          forcedTransactions: 0,
        },
      },
      {
        ...selected,
        authority: {
          ...selected.authority,
          view: { ...selected.authority.view, hash: "bad" },
        },
      },
    ]) {
      const message = structuredClone({
        type: "CommitPlanningNotice",
        status: "selected",
        selection: altered,
      });
      expect(parseCommitPlanningNotice(message)).toBeUndefined();
      await Effect.runPromise(applyCommitPlanningNotice(globals, message));
      expect(
        project(Effect.runSync(Ref.get(globals.COMMIT_PLANNING))),
      ).toMatchObject({ status: "unavailable", planned: null });
    }
  });
  it("clears a selected mode on the next attempt and preserves the approved equal-clock whole-instant rescue", async () => {
    const globals = {
      COMMIT_PLANNING: await Effect.runPromise(
        Ref.make({ selection: selected, receivedAtMs: 5_010 } as {
          selection: CommitPlanningSelection;
          receivedAtMs: number;
        } | null),
      ),
    };
    await Effect.runPromise(
      applyCommitPlanningNotice(globals, {
        type: "CommitPlanningNotice",
        status: "unavailable",
        reason: "planning_started",
      }),
    );
    expect(Effect.runSync(Ref.get(globals.COMMIT_PLANNING))).toBeNull();
    const rescue = makeCommitPlanningNotice({
      ...selected,
      selectedEndTimeMs: 1_000,
      canonicalHorizonMs: 1_000,
      eligibleCutoffMs: 1_000,
      rescue: "whole_instant",
    });
    expect(parseCommitPlanningNotice(structuredClone(rescue))).toEqual(rescue);
  });
});
