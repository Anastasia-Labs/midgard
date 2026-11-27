/**
 * `confirmed_ledger_behind` across a history outage, on the node database:
 * the merged queue root passed a block this node never processed while the
 * retained queue history could not reach it, so processing holds the fold
 * as `confirmed_ledger_behind` (a verdict on the view, re-read on the next
 * follower change, no timer) and writes nothing. Once the history serves the
 * lineage again, the next run folds through it, and the commit base on the
 * merged confirmed state is `confirmed_ledger` at its root. A merge that
 * landed while the node took its ledger in is walked and folded the same way.
 */
import * as SDK from "@al-ft/midgard-sdk/core";
import { Effect, Either } from "effect";
import { describe, expect, it } from "vitest";

import { isRetriedHold } from "../src/l1-events/driver.js";
import type { LandedStateQueueElement } from "../src/l1-state-queue/index.js";
import {
  CONFIRMED_LEDGER_BEHIND,
  LANDED_BLOCK_REBASE_PENDING,
  LANDED_BLOCK_REPLAY_FAILED,
} from "../src/landed-blocks/holds.js";
import { processLandedQueue } from "../src/landed-blocks/process.js";
import { Frontier, markApplied } from "../src/landed-blocks/store.js";
import { withFollowerWrite } from "../src/services/follower-write-gate.js";
import {
  ledgerAnchorOf,
  takeInLedger,
} from "../src/services/ledger-snapshot-start.js";
import {
  confirmedTipOf,
  hexEntries,
  resolve,
} from "./commit-base-resolver.fixture.js";
import { DEPLOYMENT } from "./da-payload-libp2p-producer.manifest-fixture.js";
import { peerOver, storeLookup } from "./helpers/ledger-install.js";
import {
  AFTER_A,
  confirmedAt,
  confirmedKeys,
  fixture,
  GENESIS,
  harness,
  inInstalledNode,
  inNode,
  ledgerStoreOf,
  ports,
  queueOf,
  sortedKeys,
} from "./landed-blocks-own.fixture.js";

/** A queue output as the history retains it, created at `slot`. */
const createdAt = (
  element: LandedStateQueueElement,
  slot: number,
): LandedStateQueueElement => ({ ...element, created: { slot, txIndex: 0 } });

describe("confirmed_ledger behind across a queue history outage", () => {
  it("holds the fold while the history cannot reach the merged root, and folds and serves the commit base once it can", async () => {
    const { a, genesisState, journalA } = await fixture();
    const state = harness();
    state.journals.set(a.hash, journalA);
    const genesisQueue = queueOf(genesisState, []);
    const landedQueue = queueOf(genesisState, [a]);
    const merged = queueOf(confirmedAt(a, ""), []);
    await inInstalledNode(state)(
      Effect.gen(function* () {
        yield* processLandedQueue(ports(state), genesisQueue);
        expect((yield* Frontier.retrieve)?.headerHash).toBe(
          SDK.GENESIS_HEADER_HASH,
        );

        // The outage: `a` landed and merged while this node read nothing,
        // and the history read itself fails. The run holds on the failure
        // and writes nothing.
        const failed = yield* processLandedQueue(
          {
            ...ports(state),
            queueHistory: () => Effect.fail(new Error("history unreachable")),
          },
          merged,
        );
        expect(failed?.reason).toBe(LANDED_BLOCK_REPLAY_FAILED);
        expect(failed?.detail).toContain("history unreachable");
        expect((yield* Frontier.retrieve)?.headerHash).toBe(
          SDK.GENESIS_HEADER_HASH,
        );
        expect(yield* confirmedKeys).toEqual(sortedKeys(GENESIS));

        // The history answers, retaining none of the lineage: the fold is
        // held behind, a verdict the next follower change re-reads.
        const behind = yield* processLandedQueue(ports(state), merged);
        expect(behind?.reason).toBe(CONFIRMED_LEDGER_BEHIND);
        expect(isRetriedHold(behind!)).toBe(false);
        expect((yield* Frontier.retrieve)?.headerHash).toBe(
          SDK.GENESIS_HEADER_HASH,
        );
        expect(yield* confirmedKeys).toEqual(sortedKeys(GENESIS));

        // The history recovers with the lineage genesis -> a -> merge: the
        // next run walks it and processes `a`; once the rebase applied it,
        // the run after folds it.
        state.history = [
          createdAt(landedQueue.root!, 1),
          createdAt(landedQueue.nodes[0]!, 2),
          createdAt(merged.root!, 3),
        ];
        const walked = yield* processLandedQueue(ports(state), merged);
        expect(walked?.reason).not.toBe(CONFIRMED_LEDGER_BEHIND);
        yield* markApplied([a.hash]);
        expect(yield* processLandedQueue(ports(state), merged)).toBeUndefined();
        expect(yield* Frontier.retrieve).toEqual({
          headerHash: a.hash,
          utxosRoot: a.header.utxosRoot,
        });
        expect(yield* confirmedKeys).toEqual(sortedKeys(AFTER_A));
      }),
    );

    // The commit base on the merged confirmed state, as the next commit
    // resolves it once a verification passed at the new frontier.
    const outcome = await resolve(
      await confirmedTipOf(confirmedAt(a, "")),
      a.header.utxosRoot,
      { ledgers: [AFTER_A] },
    );
    expect(Either.isRight(outcome)).toBe(true);
    const base = Either.getOrThrow(outcome);
    expect(base.source).toBe(`confirmed:${a.hash}`);
    expect(base.root).toBe(a.header.utxosRoot);
    expect(base.entries).toEqual(hexEntries(AFTER_A).sort());
  }, 120_000);

  it("folds a merge that landed while the node took its ledger in", async () => {
    const { a, genesisRoot, genesisState, journalA } = await fixture();
    // A node with no ledger, whose store holds only the empty ledger.
    const state = { ...harness(), ledgers: ledgerStoreOf() };
    state.journals.set(a.hash, journalA);
    const genesisQueue = queueOf(genesisState, []);
    const landedQueue = queueOf(genesisState, [a]);
    const merged = queueOf(confirmedAt(a, ""), []);
    const peer = peerOver(
      "snapshot-peer",
      storeLookup(await ledgerStoreOf(GENESIS)),
    );
    await inNode(
      Effect.gen(function* () {
        // The node takes its ledger in at the queue's confirmed head when it
        // started, genesis, from a peer serving that head's ledger.
        yield* takeInLedger({
          store: yield* Effect.promise(() => state.ledgers),
          anchor: yield* ledgerAnchorOf(genesisQueue),
          peers: Effect.succeed({
            deploymentFingerprint: DEPLOYMENT,
            peers: [peer],
            excluded: new Set<string>(),
          }),
          write: withFollowerWrite,
        });
        expect(peer.requests).toBeGreaterThan(0);
        expect(yield* Frontier.retrieve).toEqual({
          headerHash: SDK.GENESIS_HEADER_HASH,
          utxosRoot: genesisRoot,
        });

        // Meanwhile `a` landed and merged: the queue's confirmed root has
        // passed the anchor. The run walks genesis -> a -> merge and
        // processes `a`; once the rebase applied it, the run after folds it.
        state.history = [
          createdAt(landedQueue.root!, 1),
          createdAt(landedQueue.nodes[0]!, 2),
          createdAt(merged.root!, 3),
        ];
        const walked = yield* processLandedQueue(ports(state), merged);
        expect(walked?.reason).toBe(LANDED_BLOCK_REBASE_PENDING);
        yield* markApplied([a.hash]);
        expect(yield* processLandedQueue(ports(state), merged)).toBeUndefined();
        expect(yield* Frontier.retrieve).toEqual({
          headerHash: a.hash,
          utxosRoot: a.header.utxosRoot,
        });
        expect(yield* confirmedKeys).toEqual(sortedKeys(AFTER_A));
      }),
    );
  }, 120_000);
});
