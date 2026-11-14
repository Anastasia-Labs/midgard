import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import {
  FraudProofL1CheckpointChangedError,
  FraudProofL1UnavailableError,
  type LedgerCheckpoint,
  ReplayLocalFault,
  type RetainedDaPayloadSource,
} from "@al-ft/midgard-fault-proofs";
import { buildCanonicalBlockFixture } from "@al-ft/midgard-fault-proofs/test-support/canonical-block-evidence-fixture";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import { afterEach, describe, expect, it } from "vitest";

import { createWatcherReplayedLedger } from "../../src/fault-proofs/replayed-ledger.js";
import type { WatcherFollowerReplaySource } from "../../src/l1-follower/replay-source.js";
import { observation } from "./fault-decision-bridge.observation.js";

const availabilityAuthority = () => ({
  deploymentIdentity: "c7".repeat(28),
  responseGeometry: SDK.availabilityResponseGeometry({
    chunkByteLength: 4096,
    trancheByteLength: 64 * 1024,
    maxTrancheCount: 1024,
  }),
});
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
    availabilityStatus: async () => "Unattested",
    checkpointDurable: (end) => durable.push(end),
    close: () => undefined,
  };
  const keeper = () =>
    createWatcherReplayedLedger({
      follower,
      l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
      availabilityAuthority,
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
      availabilityAuthority,
      checkpointPath: join(directory, "unavailable.json"),
    });
    expect(await keeper.advance(observation([]), [])).toMatchObject([
      {
        reason: "replayed_ledger_source_pending",
        detail: "the follower is reconnecting",
      },
    ]);
  });
  it("holds a local replay fault under its own reason, and advances once it clears", async () => {
    const h = await setup();
    let faulted = true;
    const keeper = createWatcherReplayedLedger({
      follower: {
        ...h.follower,
        confirmed: async (current) => {
          if (faulted) {
            faulted = false;
            throw new ReplayLocalFault({
              message: "withdrawal classification failed locally",
              cause: undefined,
            });
          }
          return h.follower.confirmed(current);
        },
      },
      l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
      availabilityAuthority,
      checkpointPath: join(directories[0]!, "local-fault.json"),
    });
    expect(await keeper.advance(observation([]), [])).toEqual([
      {
        reason: "replayed_ledger_local_fault",
        detail: "withdrawal classification failed locally",
      },
    ]);
    expect(h.durable).toEqual([]);
    expect(await keeper.advance(observation([]), [])).toEqual([]);
    expect(h.durable).toEqual([1000n]);
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
    availabilityStatus: async () => "Unattested",
    checkpointDurable: () => undefined,
    close: () => undefined,
  };
  const keeper = createWatcherReplayedLedger({
    follower,
    l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
    availabilityAuthority,
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
    availabilityStatus: async () => "Unattested",
    checkpointDurable: () => undefined,
    close: () => undefined,
  };
  const keeper = createWatcherReplayedLedger({
    follower,
    l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
    availabilityAuthority,
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

/**
 * The keeper checks each retained copy against the availability commitment
 * its block's node carries: once it carries one, a copy that does not
 * rebuild it is passed over within the fetch; while it is Unattested a copy
 * that does not replay only makes the advance wait, and the next advance asks
 * past it.
 */
describe("the production replay keeper's availability binding", () => {
  const setupBound = async () => {
    const directory = await mkdtemp(join(tmpdir(), "watcher-replay-bound-"));
    directories.push(directory);
    const block = await buildCanonicalBlockFixture({
      transactions: [],
      utxos: [],
      prevHeaderHash: SDK.GENESIS_HEADER_HASH,
    });
    // The block's header over preimages of no transaction: the header does
    // not commit that section.
    const stray = await wrapDaPayload(
      SDK.encodeDaPayload({
        ...block.payload,
        block_body: {
          ...block.payload.block_body,
          transaction_preimages: [["ab".repeat(32), "00"]],
        },
      }),
      { mode: "identity" },
    );
    const node: { status: SDK.DaAvailabilityStateQueueStatus } = {
      status: "Unattested",
    };
    const requests: string[] = [];
    const served: readonly (readonly [string, Uint8Array])[] = [
      ["s0", stray],
      ["s1", block.payloadEnvelopeCbor],
    ];
    const sources: RetainedDaPayloadSource[] = served.map(([id, bytes]) => ({
      sourceId: id,
      fetchPayloadByHeaderHash: async () => {
        requests.push(id);
        return {
          ok: true,
          sourceId: id,
          sourcePeerId: "peer",
          payloadEnvelopeCbor: Buffer.from(bytes),
          provenance: {
            trustClass: "public_or_permissionless_da",
            sourceId: `${id}/peer`,
            grade: "security",
          },
          attempts: [],
        };
      },
    }));
    const statuses: string[] = [];
    let stored: LedgerCheckpoint | undefined;
    const keeper = createWatcherReplayedLedger({
      follower: {
        epoch: () => 0,
        confirmed: async () => ({
          state: {
            ...SDK.makeGenesisConfirmedState(1000n),
            headerHash: block.headerHash,
            utxoRoot: block.header.utxosRoot,
          },
          current: () => true,
        }),
        materialFor: async (payload) => ({
          blockTimeMs: Number(payload.block_body.header.endTime),
          replayUserEvent: () => Effect.die("the block has no user event"),
          verifyForcedSource: () => Effect.die("the block has no forced order"),
        }),
        availabilityStatus: async (headerHash) => {
          statuses.push(headerHash);
          return node.status;
        },
        checkpointDurable: () => undefined,
        close: () => undefined,
      },
      l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
      availabilityAuthority,
      checkpointPath: join(directory, "ledger.json"),
      unsafeCheckpointStoreForTest: {
        read: async () => stored,
        write: async (checkpoint) => {
          stored = checkpoint;
        },
      },
    });
    const commitment = SDK.buildDaAvailabilityCommitment({
      ...availabilityAuthority(),
      headerHash: block.headerHash,
      payload: block.payloadEnvelopeCbor,
    });
    return {
      block,
      node,
      requests,
      statuses,
      sources,
      keeper,
      stored: () => stored,
      attest: () => {
        node.status = {
          Attested: {
            commitment_hash: SDK.daAvailabilityCommitmentHash(commitment),
          },
        };
      },
    };
  };

  it("passes over a copy the Attested commitment does not bind within one advance", async () => {
    const h = await setupBound();
    h.attest();
    expect(await h.keeper.advance(observation([]), h.sources)).toEqual([]);
    expect(h.requests).toEqual(["s0", "s1"]);
    expect(h.statuses).toEqual([h.block.headerHash]);
    expect(h.stored()?.headerHash).toBe(h.block.headerHash);
  });

  it("waits on an Unattested copy that does not replay, then asks past it", async () => {
    const h = await setupBound();
    expect(await h.keeper.advance(observation([]), h.sources)).toMatchObject([
      {
        reason: "replayed_ledger_source_pending",
        detail: expect.stringContaining(
          "is not bound to an attested commitment yet",
        ),
      },
    ]);
    expect(h.stored()).toBeUndefined();
    expect(await h.keeper.advance(observation([]), h.sources)).toEqual([]);
    expect(h.requests).toEqual(["s0", "s0", "s1"]);
    expect(h.stored()?.headerHash).toBe(h.block.headerHash);
  });
});
