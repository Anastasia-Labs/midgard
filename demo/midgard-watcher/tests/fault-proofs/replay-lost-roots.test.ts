/**
 * The `replayed_ledger_root_lost` alert through the production replay keeper
 * (`replay-lost-roots.ts`): a window root this process saw held that the
 * store no longer holds raises it and is logged once; a window root never
 * held does not; it clears once the root is held again or leaves the window.
 */
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import {
  createLedgerStore,
  createMemoryLedgerStorage,
} from "@al-ft/midgard-fault-proofs/ledger-store";
import { memoryVerifiedHead } from "@al-ft/midgard-fault-proofs/replayed-ledger";
import { replayedLedgerChain } from "@al-ft/midgard-fault-proofs/test-support/replayed-ledger-chain";
import type { RetainedDaPayloadSource } from "@al-ft/midgard-fault-proofs/transition-trace/fetch";
import * as SDK from "@al-ft/midgard-sdk/core";
import * as SDKDa from "@al-ft/midgard-sdk/da";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { createWatcherReplayLostRoots } from "../../src/fault-proofs/replay-lost-roots.js";
import { createWatcherReplayedLedger } from "../../src/fault-proofs/replayed-ledger.js";
import type { WatcherFollowerReplaySource } from "../../src/l1-follower/replay-source.js";
import { observation } from "./fault-decision-bridge.observation.js";
import { operations } from "./fault-decision-bridge.released-fixture.js";

describe("the replayed ledger's lost-root alert", { timeout: 120_000 }, () => {
  it("raises for a window root it saw held that the store lost, never for one it never held, and clears as the root is held again or leaves the window", async () => {
    const chain = await replayedLedgerChain(3);
    const [b0, b1, b2] = chain.blocks as [
      (typeof chain.blocks)[number],
      (typeof chain.blocks)[number],
      (typeof chain.blocks)[number],
    ];
    const store = createLedgerStore(createMemoryLedgerStorage());
    await store.persist(await store.admit(chain.base, chain.baseRoot));
    // b2 is in the window, but its ledger is never derived.
    let window = {
      blocks: [b0.header, b1.header, b2.header],
      newestRoot: b2.header.utxosRoot as string | undefined,
    };
    const follower: WatcherFollowerReplaySource = {
      epoch: () => 0,
      headerAt: async (headerHash) =>
        chain.blocks.find((block) => block.headerHash === headerHash)?.header,
      ledgerWindow: async () => window,
      // The confirmed head is genesis, whose ledger is always held.
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
    };
    const source: RetainedDaPayloadSource = {
      sourceId: "chain",
      fetchPayloadByHeaderHash: async (headerHash) => {
        const block = chain.blocks.find(
          (block) => block.headerHash === headerHash,
        );
        if (block === undefined) throw new Error("no payload");
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
    };
    const observability = operations();
    const logged: string[] = [];
    const keeper = createWatcherReplayedLedger({
      verifiedHead: memoryVerifiedHead(),
      follower,
      l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
      availabilityAuthority: () => ({
        deploymentIdentity: "c7".repeat(28),
        responseGeometry: SDKDa.availabilityResponseGeometry({
          chunkByteLength: 4096,
          trancheByteLength: 64 * 1024,
          maxTrancheCount: 1024,
        }),
      }),
      store,
      lostRoots: createWatcherReplayLostRoots({
        sink: observability.sink,
        nowMs: () => 0,
        log: (line) => {
          const { reason, root } = JSON.parse(line) as {
            reason: string;
            root: string;
          };
          logged.push(`${reason} ${root}`);
        },
      }),
    });
    const lost = () =>
      observability.api
        .status()
        .activeAlerts.some(({ code }) => code === "replayed_ledger_root_lost");
    const advance = async () =>
      expect(await keeper.advance(observation([]), [source])).toEqual([]);

    await keeper.ledgerAt([source])(b1.headerHash);
    await advance();
    // b2's root is kept and not held, but this process never held it.
    expect(lost()).toBe(false);
    expect(logged).toEqual([]);

    // The store loses b1's ledger behind the keeper's back.
    await store.retain(new Set([chain.baseRoot, b0.header.utxosRoot]));
    await advance();
    expect(lost()).toBe(true);
    await advance();
    expect(logged).toEqual([
      `replayed_ledger_root_lost ${b1.header.utxosRoot}`,
    ]);

    // Derived again on the next lookup: the alert clears.
    await keeper.ledgerAt([source])(b1.headerHash);
    expect(lost()).toBe(false);

    // Lost again, then out of the window: the alert clears.
    await store.retain(new Set([chain.baseRoot, b0.header.utxosRoot]));
    await advance();
    expect(lost()).toBe(true);
    window = { blocks: [b0.header], newestRoot: b0.header.utxosRoot };
    await advance();
    expect(lost()).toBe(false);
    expect(logged).toEqual([
      `replayed_ledger_root_lost ${b1.header.utxosRoot}`,
      `replayed_ledger_root_lost ${b1.header.utxosRoot}`,
    ]);
  });
});
