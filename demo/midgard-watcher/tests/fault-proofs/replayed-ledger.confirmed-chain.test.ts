/**
 * The production replay keeper verifying the confirmed chain by block: a
 * confirmed head whose events do not replay to the root it commits holds the
 * checkpoint and is recorded as the confirmed head's root mismatch even when
 * the store already holds that root, a confirmed block that can no longer
 * be replayed is passed and reported, and a catch-up over many blocks holds
 * the follower's window, not every ledger it derived.
 */
import { DaRequestResponseProtocol } from "@al-ft/midgard-core/da-transport";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import {
  createLedgerStore,
  createMemoryLedgerStorage,
} from "@al-ft/midgard-fault-proofs/ledger-store";
import {
  memoryVerifiedHead,
  type ReplayedLedgerUnverified,
} from "@al-ft/midgard-fault-proofs/replayed-ledger";
import {
  REPLAYED_LEDGER_CHAIN_PARENT,
  replayedLedgerChain,
  type ReplayedLedgerChainBlock,
  replayedLedgerChainBlockWith,
} from "@al-ft/midgard-fault-proofs/test-support/replayed-ledger-chain";
import type { RetainedDaPayloadSource } from "@al-ft/midgard-fault-proofs/transition-trace/fetch";
import * as SDK from "@al-ft/midgard-sdk/core";
import * as SDKDa from "@al-ft/midgard-sdk/da";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { createWatcherReplayLostRoots } from "../../src/fault-proofs/replay-lost-roots.js";
import { createWatcherReplayRefusals } from "../../src/fault-proofs/replay-refusals.js";
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

/** A source serving `blocks`; any other payload it answers it does not hold. */
const sourceOf = (
  blocks: readonly ReplayedLedgerChainBlock[],
): RetainedDaPayloadSource => ({
  sourceId: "chain",
  fetchPayloadByHeaderHash: async (headerHash) => {
    const block = blocks.find((entry) => entry.headerHash === headerHash);
    if (block === undefined)
      return {
        ok: false,
        sourceId: "chain",
        attempts: [
          {
            sourceId: "chain",
            sourcePeerId: "peer",
            protocol: DaRequestResponseProtocol.payloadByHeader,
            status: "not_found",
            detail: "no payload",
          },
        ],
      };
    return {
      ok: true,
      sourceId: "chain",
      sourcePeerId: "peer",
      payloadEnvelopeCbor: Buffer.from(block.payloadEnvelopeCbor),
      provenance: {
        trustClass: "public_or_permissionless_da",
        sourceId: "chain/peer",
        grade: "security",
      },
      attempts: [],
    };
  },
});

/** A follower over `blocks`, each attested to its own payload, whose stable
 * ConfirmedState head is `head` and whose ledger window is `window`. */
const followerAt = (
  blocks: readonly ReplayedLedgerChainBlock[],
  head: ReplayedLedgerChainBlock,
  durable: bigint[],
  window: readonly ReplayedLedgerChainBlock[] = [],
): WatcherFollowerReplaySource => ({
  epoch: () => 0,
  headerAt: async (headerHash) =>
    blocks.find((block) => block.headerHash === headerHash)?.header,
  ledgerWindow: async () => ({
    blocks: window.map(({ header }) => header),
    newestRoot: window.at(-1)?.header.utxosRoot,
  }),
  confirmed: async () => ({
    state: {
      ...SDK.makeGenesisConfirmedState(1000n),
      headerHash: head.headerHash,
      utxoRoot: head.header.utxosRoot,
    },
    current: () => true,
  }),
  materialFor: async (payload) => ({
    blockTimeMs: Number(payload.block_body.header.endTime),
    replayUserEvent: () => Effect.die("the block has no user event"),
    verifyForcedSource: () => Effect.die("the block has no forced order"),
  }),
  availabilityStatus: async (headerHash) => {
    const block = blocks.find((entry) => entry.headerHash === headerHash);
    if (block === undefined) return "Unattested";
    return {
      Attested: {
        commitment_hash: SDKDa.daAvailabilityCommitmentHash(
          SDKDa.buildDaAvailabilityCommitment({
            ...availabilityAuthority(),
            headerHash,
            payload: block.payloadEnvelopeCbor,
          }),
        ),
      },
    };
  },
  checkpointDurable: (end) => durable.push(end),
  close: () => undefined,
});

/** A store holding the ledger below the chain and the ledger after its
 * first block, and a verified head at that block's parent. */
const scene = async (
  chain: Awaited<ReturnType<typeof replayedLedgerChain>>,
) => {
  const first = chain.blocks[0]!;
  const store = createLedgerStore(createMemoryLedgerStorage());
  await store.admit(chain.base, chain.baseRoot);
  await store.admit(first.entries, first.header.utxosRoot);
  const verifiedHead = memoryVerifiedHead();
  await verifiedHead.write(first.header.prevHeaderHash);
  return { store, verifiedHead };
};

describe(
  "the production replay keeper verifying the confirmed chain",
  { timeout: 120_000 },
  () => {
    it("holds the checkpoint on a confirmed head whose events do not replay to the held root it commits", async () => {
      const chain = await replayedLedgerChain(1);
      // The block's events under a header that commits its parent's root.
      const liar = await replayedLedgerChainBlockWith(chain.blocks[0]!, {
        utxosRoot: chain.baseRoot,
      });
      const { store, verifiedHead } = await scene(chain);
      const durable: bigint[] = [];
      const logged: Record<string, unknown>[] = [];
      const refusals = createWatcherReplayRefusals({
        nowMs: () => 100_000,
        log: (line) => logged.push(JSON.parse(line) as Record<string, unknown>),
      });
      const keeper = createWatcherReplayedLedger({
        follower: followerAt([liar], liar, durable),
        l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
        availabilityAuthority,
        store,
        verifiedHead,
        refusals,
      });
      expect(
        await keeper.advance(observation([]), [sourceOf([liar])]),
      ).toMatchObject([{ reason: "replayed_ledger_checkpoint_pending" }]);
      expect(durable).toEqual([]);
      expect(refusals.status().rootMismatch).toMatchObject({
        phase: "confirmed",
        kind: "root-mismatch",
        headerHash: liar.headerHash,
      });
      expect(logged).toEqual([
        expect.objectContaining({
          level: "error",
          reason: "replayed_ledger_root_mismatch",
          headerHash: liar.headerHash,
        }),
      ]);
    });

    it("passes a confirmed block below the head whose payload no source retains, reports it once, and verifies the head", async () => {
      const chain = await replayedLedgerChain(2);
      const [b0, b1] = chain.blocks as [
        ReplayedLedgerChainBlock,
        ReplayedLedgerChainBlock,
      ];
      const { store, verifiedHead } = await scene(chain);
      const durable: bigint[] = [];
      const unverified: ReplayedLedgerUnverified[] = [];
      const keeper = createWatcherReplayedLedger({
        follower: followerAt(chain.blocks, b1, durable),
        l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
        availabilityAuthority,
        store,
        verifiedHead,
        onUnverified: (block) => unverified.push(block),
      });
      // The source holds b1's payload and answers that it has none of b0.
      const sources = [sourceOf([b1])];
      expect(await keeper.advance(observation([]), sources)).toEqual([]);
      expect(await keeper.advance(observation([]), sources)).toEqual([]);
      expect(unverified).toEqual([
        { headerHash: b0.headerHash, cause: "payload-not-retained" },
      ]);
      expect(durable).toEqual([1000n, 1000n]);
      expect(await verifiedHead.read()).toBe(b1.headerHash);
    });

    it("holds the window's ledgers through a catch-up over many blocks, and reports no lost root", async () => {
      const length = 12;
      const windowLength = 3;
      const chain = await replayedLedgerChain(length);
      const tip = chain.blocks[length - 1]!;
      const window = chain.blocks.slice(length - windowLength);
      const storage = createMemoryLedgerStorage();
      const store = createLedgerStore(storage);
      await store.admit(chain.base, chain.baseRoot);
      const verifiedHead = memoryVerifiedHead();
      await verifiedHead.write(REPLAYED_LEDGER_CHAIN_PARENT);
      const logged: string[] = [];
      const keeper = createWatcherReplayedLedger({
        follower: followerAt(chain.blocks, tip, [], window),
        l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
        availabilityAuthority,
        store,
        verifiedHead,
        lostRoots: createWatcherReplayLostRoots({
          nowMs: () => 0,
          log: (line) => logged.push(line),
        }),
      });
      // The roots the store holds each time the catch-up fetches a payload.
      const held: number[] = [];
      const served = sourceOf(chain.blocks);
      const source: RetainedDaPayloadSource = {
        ...served,
        fetchPayloadByHeaderHash: async (headerHash, ...rest) => {
          held.push((await storage.counts()).roots);
          return await served.fetchPayloadByHeaderHash(headerHash, ...rest);
        },
      };

      expect(await keeper.advance(observation([]), [source])).toEqual([]);

      expect(held).toHaveLength(length);
      // The window's ledgers and the one below it, never every ledger walked.
      expect(Math.max(...held)).toBeLessThanOrEqual(windowLength + 1);
      expect((await storage.counts()).roots).toBe(windowLength + 1);
      for (const block of chain.blocks.slice(length - windowLength - 1))
        expect((await store.at(block.header.utxosRoot)).root).toBe(
          block.header.utxosRoot,
        );
      expect(logged).toEqual([]);
      expect(await verifiedHead.read()).toBe(tip.headerHash);
    });
  },
);
