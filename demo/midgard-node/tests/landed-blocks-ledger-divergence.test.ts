/**
 * Landed-block processing on a `confirmed_ledger` that does not derive the
 * frontier's root (`local-ledger-divergence.ts`): the parent ledger a landed
 * block is adopted on is this node's, so a block on it, foreign and honest
 * or this node's own with a matching journal, is held as a local fault on
 * its header (retried within the runner's bound, with the alarm), never
 * `landed_block_invalid` or `landed_block_own_journal_mismatch`. Once the
 * ledger derives the root again, the same block is adopted. An attested
 * foreign block's replay keeps the local fault: only `invalid` is demoted
 * for an unbound copy.
 */
import type { FactStore } from "@al-ft/midgard-l1-follower";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import { afterEach, beforeEach, describe, expect, it } from "vitest";

import { ConfirmedLedgerDB } from "../src/database/index.js";
import type * as Ledger from "../src/database/utils/ledger.js";
import { isRetriedHold, localFaultSubject } from "../src/l1-events/driver.js";
import {
  LANDED_BLOCK_INVALID,
  LANDED_BLOCK_LOCAL_FAULT,
  LANDED_BLOCK_OWN_JOURNAL_MISMATCH,
  LANDED_BLOCK_REBASE_PENDING,
} from "../src/landed-blocks/holds.js";
import { ledgerRows } from "../src/landed-blocks/ledger.js";
import type { LandedBlockPorts } from "../src/landed-blocks/ports.js";
import { processLandedQueue } from "../src/landed-blocks/process.js";
import { Frontier, retrieveRows } from "../src/landed-blocks/store.js";
import { withFollowerWrite } from "../src/services/follower-write-gate.js";
import { simDigest, simOutput } from "./helpers/landed-blocks-sim.universe.js";
import { NODE_PREFIX, nodeDatum } from "./helpers/state-queue-sim.fixtures.js";
import {
  element,
  fixture,
  GENESIS,
  harness,
  inNode as inOwnNode,
  ports,
  queueOf,
} from "./landed-blocks-own.fixture.js";
import { attested } from "./landed-blocks-replay-foreign.availability.js";
import {
  emptyBlock,
  followChain,
  inNode,
  inputFor,
  replayerFor,
  retain,
  retainedRow,
} from "./landed-blocks-replay-foreign.fixture.js";
import { makeOutRefCbor } from "./midgard-output-helpers.js";
import { resetApplicationTables } from "./utils.js";

/** A `confirmed_ledger` row the frontier's root does not hold. */
const STRAY: Ledger.MinimalEntry = {
  outref: makeOutRefCbor(simDigest("divergence:stray"), 0),
  output: simOutput(2_000_000n),
};

/** `confirmed_ledger` holds `entries` and the frontier is `frontier`. */
const seed = (
  entries: readonly Ledger.MinimalEntry[],
  frontier: Readonly<{ headerHash: string; utxosRoot: string }>,
) =>
  withFollowerWrite(
    Effect.gen(function* () {
      yield* ConfirmedLedgerDB.insertMultiple([
        ...(yield* ledgerRows(entries, new Map())),
      ]);
      yield* Frontier.upsert(frontier);
    }),
  );

const dropStray = withFollowerWrite(
  ConfirmedLedgerDB.clearUTxOs([Buffer.from(STRAY.outref)]),
);

describe("an own landed block on a parent ledger that does not derive the parent's root", () => {
  it("is held as a local fault on it, not a journal mismatch, and adopted once the ledger derives the root", async () => {
    const { a, genesisRoot, genesisState, journalA } = await fixture();
    const state = harness();
    state.journals.set(a.hash, journalA);
    await inOwnNode(
      Effect.gen(function* () {
        yield* seed([...GENESIS, STRAY], {
          headerHash: SDK.GENESIS_HEADER_HASH,
          utxosRoot: genesisRoot,
        });
        const process = () =>
          processLandedQueue(ports(state), queueOf(genesisState, [a]));
        const held = yield* process();
        expect(held?.reason).toBe(LANDED_BLOCK_LOCAL_FAULT);
        expect(held?.reason).not.toBe(LANDED_BLOCK_OWN_JOURNAL_MISMATCH);
        expect(isRetriedHold(held!)).toBe(true);
        expect(localFaultSubject(held!)).toBe(a.hash);
        expect(held?.detail).toContain(
          `local ledger divergence (own_parent): the ledger at ${SDK.GENESIS_HEADER_HASH}`,
        );
        expect(yield* retrieveRows).toHaveLength(0);

        yield* dropStray;
        expect((yield* process())?.reason).toBe(LANDED_BLOCK_REBASE_PENDING);
        expect((yield* retrieveRows).map((row) => row.headerHash)).toEqual([
          a.hash,
        ]);
      }),
    );
  }, 120_000);
});

describe("an honest foreign landed block on a parent ledger that does not derive the parent's root", () => {
  const opened: FactStore[] = [];
  beforeEach(async () => {
    await inNode(resetApplicationTables);
  });
  afterEach(async () => {
    await Promise.all(opened.splice(0).map((store) => store.close()));
  });

  const follow = async () => {
    const followed = await followChain();
    opened.push(followed.store);
    return followed;
  };

  it("replays to a local fault, not invalid, for an attested block, and replays on the parent's ledger", async () => {
    const { store } = await follow();
    const payload = await emptyBlock();
    await retain(await retainedRow(payload));
    const input = await inputFor(store, payload, await attested(payload));
    const diverged = await inNode(
      replayerFor(store)({ ...input, parentEntries: [STRAY] }),
    );
    expect(diverged).toMatchObject({ kind: "local_fault" });
    expect(diverged.kind === "local_fault" && diverged.detail).toContain(
      "local ledger divergence (landed_parent)",
    );
    const replayed = await inNode(replayerFor(store)(input));
    expect(replayed.kind).toBe("replayed");
  }, 120_000);

  it("is held as a local fault on it, never landed_block_invalid, and adopted once the ledger derives the root", async () => {
    const { store } = await follow();
    const payload = await emptyBlock();
    const { header, header_hash: hash } = payload.block_body;
    await retain(await retainedRow(payload));
    // The frontier is the block's parent at its root, but `confirmed_ledger`
    // holds a row that root does not.
    await inNode(
      seed([STRAY], {
        headerHash: header.prevHeaderHash,
        utxosRoot: header.prevUtxosRoot,
      }),
    );
    const root: SDK.ConfirmedState = {
      headerHash: header.prevHeaderHash,
      prevHeaderHash: SDK.GENESIS_HEADER_HASH,
      utxoRoot: header.prevUtxosRoot,
      startTime: 0n,
      endTime: header.startTime,
      protocolVersion: 1n,
    };
    const queue = {
      ...queueOf(root, [{ hash, header }]),
      view: (await store.currentView())!,
      nodes: [
        element(
          1,
          nodeDatum(header, await attested(payload), null),
          NODE_PREFIX + hash,
          hash,
          header.endTime,
        ),
      ],
    };
    // The fixture's ports, with the production replayer.
    const replayer = replayerFor(store);
    const production: LandedBlockPorts<
      Effect.Effect.Context<ReturnType<typeof replayer>>
    > = {
      ...ports(harness()),
      write: (work) => withFollowerWrite(work),
      replay: replayer,
    };

    const held = await inNode(processLandedQueue(production, queue));
    expect(held?.reason).toBe(LANDED_BLOCK_LOCAL_FAULT);
    expect(held?.reason).not.toBe(LANDED_BLOCK_INVALID);
    expect(isRetriedHold(held!)).toBe(true);
    expect(localFaultSubject(held!)).toBe(hash);
    expect(held?.detail).toContain("local ledger divergence (landed_parent)");
    expect(await inNode(retrieveRows)).toHaveLength(0);

    await inNode(dropStray);
    const adopted = await inNode(processLandedQueue(production, queue));
    expect(adopted?.reason).toBe(LANDED_BLOCK_REBASE_PENDING);
    expect(
      (await inNode(retrieveRows)).map((row) => [row.headerHash, row.kind]),
    ).toEqual([[hash, "foreign"]]);
  }, 120_000);
});
