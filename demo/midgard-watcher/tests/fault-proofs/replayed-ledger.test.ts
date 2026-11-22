import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import { ReplayLocalFault } from "@al-ft/midgard-fault-proofs/block-replay";
import {
  createLedgerStore,
  createMemoryLedgerStorage,
  type LedgerStorage,
} from "@al-ft/midgard-fault-proofs/ledger-store";
import {
  replayedLedgerChain,
  type ReplayedLedgerChainBlock,
} from "@al-ft/midgard-fault-proofs/test-support/replayed-ledger-chain";
import type { RetainedDaPayloadSource } from "@al-ft/midgard-fault-proofs/transition-trace/fetch";
import {
  FraudProofL1CheckpointChangedError,
  FraudProofL1UnavailableError,
} from "@al-ft/midgard-fault-proofs/workflow/l1-source";
import * as SDK from "@al-ft/midgard-sdk/core";
import * as SDKDa from "@al-ft/midgard-sdk/da";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { createWatcherReplayedLedger } from "../../src/fault-proofs/replayed-ledger.js";
import type { WatcherFollowerReplaySource } from "../../src/l1-follower/replay-source.js";
import { observation } from "./fault-decision-bridge.observation.js";

const availabilityAuthority = () => ({
  deploymentIdentity: "c7".repeat(28),
  responseGeometry: SDKDa.availabilityResponseGeometry({
    chunkByteLength: 4096,
    trancheByteLength: 64 * 1024,
    maxTrancheCount: 1024,
  }),
});

/** A memory ledger store whose writes fail while `failing` is set. */
const ledgerStore = () => {
  const memory = createMemoryLedgerStorage();
  const control = { failing: false };
  const storage: LedgerStorage = {
    ...memory,
    write: async (change) => {
      if (control.failing) throw new Error("ledger store fsync failed");
      await memory.write(change);
    },
  };
  return { store: createLedgerStore(storage), control };
};

/** A retained-DA source serving `served` by header hash; each request
 * records the source's id in `requests`. */
const retainedSource = (
  sourceId: string,
  served: ReadonlyMap<string, Uint8Array>,
  requests: string[] = [],
): RetainedDaPayloadSource => ({
  sourceId,
  fetchPayloadByHeaderHash: async (headerHash) => {
    requests.push(sourceId);
    const bytes = served.get(headerHash);
    if (bytes === undefined) throw new Error(`${sourceId} has no payload`);
    return {
      ok: true,
      sourceId,
      sourcePeerId: "peer",
      payloadEnvelopeCbor: Buffer.from(bytes),
      provenance: {
        trustClass: "public_or_permissionless_da",
        sourceId: `${sourceId}/peer`,
        grade: "security",
      },
      attempts: [],
    };
  },
});

const chainSource = (
  blocks: readonly ReplayedLedgerChainBlock[],
  requests?: string[],
) =>
  retainedSource(
    "chain",
    new Map(
      blocks.map((block) => [block.headerHash, block.payloadEnvelopeCbor]),
    ),
    requests,
  );

/** A follower over `blocks` whose ConfirmedState is genesis. */
const followerOver = (
  blocks: readonly ReplayedLedgerChainBlock[] = [],
): WatcherFollowerReplaySource => ({
  epoch: () => 0,
  headerAt: async (headerHash) =>
    blocks.find((block) => block.headerHash === headerHash)?.header,
  ledgerWindow: async () => ({ blocks: [], newestRoot: undefined }),
  confirmed: async () => ({
    state: SDK.makeGenesisConfirmedState(1000n),
    current: () => true,
  }),
  materialFor: async (payload) => ({
    blockTimeMs: Number(payload.block_body.header.endTime),
    replayUserEvent: () => Effect.die("the block has no user event"),
    verifyForcedSource: () => Effect.die("the block has no forced order"),
  }),
  availabilityStatus: async () => "Unattested",
  checkpointDurable: () => undefined,
  close: () => undefined,
});

/** One chain block above a ledger the store holds. */
const storedChain = async (length = 1) => {
  const chain = await replayedLedgerChain(length);
  const { store, control } = ledgerStore();
  await store.persist(await store.admit(chain.base, chain.baseRoot));
  const held = (root: string) =>
    store.at(root).then(
      () => true,
      () => false,
    );
  return { chain, store, control, held };
};

const setup = () => {
  let state = SDK.makeGenesisConfirmedState(1000n);
  let current = true;
  const durable: bigint[] = [];
  const { store } = ledgerStore();
  const follower: WatcherFollowerReplaySource = {
    ...followerOver(),
    confirmed: async () => ({ state, current: () => current }),
    materialFor: async () => {
      throw new Error("genesis never needs replay material");
    },
    checkpointDurable: (end) => durable.push(end),
  };
  const keeper = () =>
    createWatcherReplayedLedger({
      follower,
      l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
      availabilityAuthority,
      store,
    });
  return {
    keeper,
    follower,
    store,
    durable,
    changeRoot: () => {
      state = { ...state, utxoRoot: "00".repeat(32) };
    },
    retire: () => {
      current = false;
    },
  };
};

describe("the production replay keeper", { timeout: 120_000 }, () => {
  it("holds a checked genesis head, holds it again at restart, and refuses a changed root under the same hash", async () => {
    const h = setup();
    const keeper = h.keeper();
    expect(await keeper.advance(observation([]), [])).toEqual([]);
    expect(h.durable).toEqual([1000n]);
    expect(await h.keeper().advance(observation([]), [])).toEqual([]);
    h.changeRoot();
    expect(await keeper.advance(observation([]), [])).toMatchObject([
      { reason: "replayed_ledger_checkpoint_pending" },
    ]);
    expect(h.durable).toEqual([1000n, 1000n]);
  });
  it("does not release admission history when persistence fails", async () => {
    const { chain, store, control } = await storedChain();
    const block = chain.blocks[0]!;
    const durable: bigint[] = [];
    const keeper = createWatcherReplayedLedger({
      follower: {
        ...followerOver(chain.blocks),
        confirmed: async () => ({
          state: {
            ...SDK.makeGenesisConfirmedState(1000n),
            headerHash: block.headerHash,
            utxoRoot: block.header.utxosRoot,
          },
          current: () => true,
        }),
        checkpointDurable: (end) => durable.push(end),
      },
      l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
      availabilityAuthority,
      store,
    });
    control.failing = true;
    expect(
      await keeper.advance(observation([]), [chainSource(chain.blocks)]),
    ).toMatchObject([
      {
        reason: "replayed_ledger_local_fault",
        detail: expect.stringContaining("ledger store fsync failed"),
      },
    ]);
    expect(durable).toEqual([]);
  });
  it("names unavailable L1 and a reset during the confirmed read instead of trusting missing material", async () => {
    const h = setup();
    h.retire();
    expect(await h.keeper().advance(observation([]), [])).toMatchObject([
      { reason: "replayed_ledger_source_pending" },
    ]);
    expect(h.durable).toEqual([]);
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
      store: h.store,
    });
    expect(await keeper.advance(observation([]), [])).toMatchObject([
      {
        reason: "replayed_ledger_source_pending",
        detail: "the follower is reconnecting",
      },
    ]);
  });
  it("holds a local replay fault under its own reason, and advances once it clears", async () => {
    const h = setup();
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
      store: h.store,
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

describe(
  "the production replay keeper across follower generations",
  { timeout: 120_000 },
  () => {
    it("keeps a stored ledger across a generation change and reads a new block's material from the current generation", async () => {
      const { chain, store } = await storedChain(2);
      const [first, second] = chain.blocks as [
        ReplayedLedgerChainBlock,
        ReplayedLedgerChainBlock,
      ];
      let epoch = 0;
      let materialReads = 0;
      const requests: string[] = [];
      const base = followerOver(chain.blocks);
      const keeper = createWatcherReplayedLedger({
        follower: {
          ...base,
          epoch: () => epoch,
          materialFor: async (payload) => {
            materialReads += 1;
            if (epoch !== 0)
              throw new FraudProofL1UnavailableError(
                "admission belongs to the prior generation",
              );
            return base.materialFor(payload);
          },
        },
        l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
        availabilityAuthority,
        store,
      });
      const ledgerAt = keeper.ledgerAt([chainSource(chain.blocks, requests)]);
      const entries = await ledgerAt(first.headerHash);
      expect(entries).toBeDefined();
      expect(materialReads).toBe(1);
      epoch += 1;
      // The ledger stored before the change still answers, with no payload
      // or material read.
      requests.length = 0;
      expect(await ledgerAt(first.headerHash)).toEqual(entries);
      expect(requests).toEqual([]);
      expect(materialReads).toBe(1);
      // A block not stored yet reads its admission from the new generation.
      await expect(ledgerAt(second.headerHash)).rejects.toThrow(
        "admission belongs to the prior generation",
      );
      expect(materialReads).toBe(2);
    });

    it("refuses a ledger lookup when its follower generation changes during replay, and keeps the ledger it stored", async () => {
      const { chain, store, held } = await storedChain();
      const block = chain.blocks[0]!;
      let epoch = 0;
      const base = followerOver(chain.blocks);
      const keeper = createWatcherReplayedLedger({
        follower: {
          ...base,
          epoch: () => epoch,
          materialFor: async (payload) => {
            epoch += 1;
            return base.materialFor(payload);
          },
        },
        l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
        availabilityAuthority,
        store,
      });
      const requests: string[] = [];
      const ledgerAt = keeper.ledgerAt([chainSource(chain.blocks, requests)]);
      await expect(ledgerAt(block.headerHash)).rejects.toBeInstanceOf(
        FraudProofL1CheckpointChangedError,
      );
      // The replayed ledger opened its header's root before it was stored, so
      // the rewind cannot have made it wrong: the next lookup answers from it.
      expect(await held(block.header.utxosRoot)).toBe(true);
      requests.length = 0;
      expect(await ledgerAt(block.headerHash)).toBeDefined();
      expect(requests).toEqual([]);
    });
  },
);

/**
 * The keeper checks each retained copy against the availability commitment
 * its block's node carries: once it carries one, a copy that does not
 * rebuild it is passed over within the fetch; while it is Unattested a copy
 * that does not replay only makes the advance wait, and the next advance asks
 * past it.
 */
describe(
  "the production replay keeper's availability binding",
  { timeout: 120_000 },
  () => {
    const setupBound = async () => {
      const { chain, store, held } = await storedChain();
      const block = chain.blocks[0]!;
      // The block's header over a transaction preimage section it does not
      // commit.
      const stray = await wrapDaPayload(
        SDKDa.encodeDaPayload({
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
      const sources = served.map(([id, bytes]) =>
        retainedSource(id, new Map([[block.headerHash, bytes]]), requests),
      );
      const statuses: string[] = [];
      const keeper = createWatcherReplayedLedger({
        follower: {
          ...followerOver(chain.blocks),
          confirmed: async () => ({
            state: {
              ...SDK.makeGenesisConfirmedState(1000n),
              headerHash: block.headerHash,
              utxoRoot: block.header.utxosRoot,
            },
            current: () => true,
          }),
          availabilityStatus: async (headerHash) => {
            statuses.push(headerHash);
            return node.status;
          },
        },
        l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
        availabilityAuthority,
        store,
      });
      const commitment = SDKDa.buildDaAvailabilityCommitment({
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
        stored: () => held(block.header.utxosRoot),
        attest: () => {
          node.status = {
            Attested: {
              commitment_hash: SDKDa.daAvailabilityCommitmentHash(commitment),
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
      expect(await h.stored()).toBe(true);
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
      expect(await h.stored()).toBe(false);
      expect(await h.keeper.advance(observation([]), h.sources)).toEqual([]);
      expect(h.requests).toEqual(["s0", "s0", "s1"]);
      expect(await h.stored()).toBe(true);
    });
  },
);
