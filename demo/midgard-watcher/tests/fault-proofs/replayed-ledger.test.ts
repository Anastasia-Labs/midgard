import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import {
  FraudProofL1CheckpointChangedError,
  FraudProofL1UnavailableError,
  type LedgerCheckpoint,
} from "@al-ft/midgard-fault-proofs";
import { buildCanonicalBlockFixture } from "@al-ft/midgard-fault-proofs/test-support/canonical-block-evidence-fixture";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import { afterEach, describe, expect, it } from "vitest";

import { createWatcherReplayedLedger } from "../../src/fault-proofs/replayed-ledger.js";
import type { WatcherFollowerReplaySource } from "../../src/l1-follower/replay-source.js";
import { observation } from "./fault-decision-bridge.observation.js";

const directories: string[] = [];
afterEach(async () => {
  for (const directory of directories.splice(0))
    await rm(directory, { recursive: true, force: true });
});
const setup = async (input: { failWrite?: boolean } = {}) => {
  const directory = await mkdtemp(join(tmpdir(), "watcher-replay-checkpoint-"));
  directories.push(directory);
  let state = SDK.makeGenesisConfirmedState(1000n);
  let current = true;
  let stored: LedgerCheckpoint | undefined;
  const durable: bigint[] = [];
  let writes = 0;
  const follower: WatcherFollowerReplaySource = {
    epoch: () => 0,
    confirmed: async () => ({ state, current: () => current }),
    materialFor: async () => {
      throw new Error("genesis never needs replay material");
    },
    checkpointDurable: (end) => durable.push(end),
    close: () => undefined,
  };
  const keeper = () =>
    createWatcherReplayedLedger({
      follower,
      l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
      checkpointPath: join(directory, "ledger.json"),
      unsafeCheckpointStoreForTest: {
        read: async () => stored,
        write: async (checkpoint) => {
          if (input.failWrite) throw new Error("checkpoint fsync failed");
          writes += 1;
          stored = checkpoint;
        },
      },
    });
  return {
    keeper,
    follower,
    durable,
    writes: () => writes,
    changeRoot: () => {
      state = { ...state, utxoRoot: "00".repeat(32) };
    },
    retire: () => {
      current = false;
    },
  };
};

describe("the production replay keeper", () => {
  it("persists a checked genesis head, restores it at restart, and refuses a changed root under the same hash", async () => {
    const h = await setup();
    const keeper = h.keeper();
    expect(await keeper.advance(observation([]), [])).toEqual([]);
    expect(h.writes()).toBe(1);
    expect(h.durable).toEqual([1000n]);
    expect(await h.keeper().advance(observation([]), [])).toEqual([]);
    expect(h.writes()).toBe(1);
    h.changeRoot();
    expect(await keeper.advance(observation([]), [])).toMatchObject([
      { reason: "replayed_ledger_checkpoint_pending" },
    ]);
    expect(h.durable).toEqual([1000n, 1000n]);
  });
  it("does not release admission history when persistence fails", async () => {
    const h = await setup({ failWrite: true });
    await expect(h.keeper().advance(observation([]), [])).rejects.toThrow(
      "checkpoint fsync failed",
    );
    expect(h.durable).toEqual([]);
  });
  it("names unavailable L1 and a reset during restore instead of trusting missing material", async () => {
    const h = await setup();
    h.retire();
    expect(await h.keeper().advance(observation([]), [])).toMatchObject([
      { reason: "replayed_ledger_source_pending" },
    ]);
    expect(h.durable).toEqual([]);
    expect(h.writes()).toBe(0);
    const directory = directories[0]!;
    const keeper = createWatcherReplayedLedger({
      follower: {
        ...h.follower,
        confirmed: async () => {
          throw new FraudProofL1UnavailableError(
            "the follower is reconnecting",
          );
        },
      },
      l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
      checkpointPath: join(directory, "unavailable.json"),
    });
    expect(await keeper.advance(observation([]), [])).toMatchObject([
      {
        reason: "replayed_ledger_source_pending",
        detail: "the follower is reconnecting",
      },
    ]);
  });
});

it("re-reads authenticated replay material after a follower generation changes", async () => {
  const directory = await mkdtemp(join(tmpdir(), "watcher-replay-epoch-"));
  directories.push(directory);
  const block = await buildCanonicalBlockFixture({
    transactions: [],
    utxos: [],
    prevHeaderHash: SDK.GENESIS_HEADER_HASH,
  });
  let epoch = 0;
  let materialReads = 0;
  const follower: WatcherFollowerReplaySource = {
    epoch: () => epoch,
    confirmed: async () => ({
      state: SDK.makeGenesisConfirmedState(1000n),
      current: () => true,
    }),
    materialFor: async () => {
      materialReads += 1;
      if (epoch !== 0)
        throw new FraudProofL1UnavailableError(
          "admission belongs to the prior generation",
        );
      return {
        blockTimeMs: 2,
        replayUserEvent: () => Effect.die("empty block has no user events"),
        verifyForcedSource: () => Effect.die("empty block has no forced order"),
      };
    },
    checkpointDurable: () => undefined,
    close: () => undefined,
  };
  const keeper = createWatcherReplayedLedger({
    follower,
    l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
    checkpointPath: join(directory, "ledger.json"),
  });
  const ledgerAt = keeper.ledgerAt([
    {
      sourceId: "epoch-fixture",
      fetchPayloadByHeaderHash: async () => ({
        ok: true,
        sourceId: "epoch-fixture",
        sourcePeerId: "peer",
        payloadEnvelopeCbor: block.payloadEnvelopeCbor,
        provenance: {
          trustClass: "public_or_permissionless_da",
          sourceId: "epoch-fixture/peer",
          grade: "security",
        },
        attempts: [],
      }),
    },
  ]);
  expect(await ledgerAt(block.headerHash)).toEqual([]);
  expect(materialReads).toBe(1);
  epoch += 1;
  await expect(ledgerAt(block.headerHash)).rejects.toThrow(
    "admission belongs to the prior generation",
  );
  expect(materialReads).toBe(2);
});

it("refuses a ledger lookup when its follower generation changes during replay", async () => {
  const directory = await mkdtemp(
    join(tmpdir(), "watcher-replay-during-epoch-"),
  );
  directories.push(directory);
  const block = await buildCanonicalBlockFixture({
    transactions: [],
    utxos: [],
    prevHeaderHash: SDK.GENESIS_HEADER_HASH,
  });
  let epoch = 0;
  const follower: WatcherFollowerReplaySource = {
    epoch: () => epoch,
    confirmed: async () => ({
      state: SDK.makeGenesisConfirmedState(1000n),
      current: () => true,
    }),
    materialFor: async () => {
      epoch += 1;
      return {
        blockTimeMs: 2,
        replayUserEvent: () => Effect.die("no events"),
        verifyForcedSource: () => Effect.die("no forced order"),
      };
    },
    checkpointDurable: () => undefined,
    close: () => undefined,
  };
  const keeper = createWatcherReplayedLedger({
    follower,
    l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
    checkpointPath: join(directory, "ledger.json"),
  });
  const ledgerAt = keeper.ledgerAt([
    {
      sourceId: "during-epoch-fixture",
      fetchPayloadByHeaderHash: async () => ({
        ok: true,
        sourceId: "during-epoch-fixture",
        sourcePeerId: "peer",
        payloadEnvelopeCbor: block.payloadEnvelopeCbor,
        provenance: {
          trustClass: "public_or_permissionless_da",
          sourceId: "during-epoch-fixture/peer",
          grade: "security",
        },
        attempts: [],
      }),
    },
  ]);
  await expect(ledgerAt(block.headerHash)).rejects.toBeInstanceOf(
    FraudProofL1CheckpointChangedError,
  );
});
