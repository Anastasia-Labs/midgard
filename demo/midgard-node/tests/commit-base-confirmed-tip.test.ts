/**
 * The commit base on a confirmed-state tip: the ledger
 * store's ledger at the state's root, once the native root is that root,
 * this node's own merged block or another operator's. Its outputs are read
 * by out-ref, from the landed row that produced them before landed-block
 * processing folds the merge into `confirmed_ledger` and from
 * `confirmed_ledger` after; resolving it reads no whole `confirmed_ledger`.
 * While the native root is another, the commit waits
 * (`LANDED_COMMIT_BASE_PENDING`). At genesis the state's root is the
 * deployment's genesis root, and the configured genesis UTxOs give the
 * outputs no table holds.
 */
import type { View } from "@al-ft/midgard-l1-follower";
import * as SDK from "@al-ft/midgard-sdk/core";
import type { UTxO } from "@lucid-evolution/lucid";
import { Effect, Either, Metric } from "effect";
import { beforeEach, describe, expect, it } from "vitest";

import { fullScanCounter } from "../src/database/confirmedLedger.js";
import { foldToRoot } from "../src/landed-blocks/fold.js";
import { utxoToLedgerInsertMaterial } from "../src/mpf/ledger-hydration.js";
import {
  address,
  confirmedTipOf,
  E0,
  E1,
  expectPending,
  fixture,
  FRONTIER_HASH,
  hexEntries,
  resolve,
  rootOf,
  run,
  seed,
} from "./commit-base-resolver.fixture.js";
import { foldPorts } from "./helpers/fold-ports.js";
import { simCommitAnchor } from "./helpers/landed-blocks-sim.own.js";
import { prepareUnderTestLease } from "./helpers/mutation-lease.js";
import {
  header as journalHeader,
  journalFixture,
} from "./helpers/own-journal-fixture.js";
import { resetApplicationTables } from "./utils.js";

beforeEach(async () => {
  await run(resetApplicationTables);
});

/** The confirmed state a merge of the block `headerHash` (on the frontier,
 * at root `utxoRoot`) leaves on L1. */
const mergedAt = (
  headerHash: string,
  utxoRoot: string,
): SDK.ConfirmedState => ({
  headerHash,
  prevHeaderHash: FRONTIER_HASH,
  utxoRoot,
  startTime: 1n,
  endTime: 2n,
  protocolVersion: 1n,
});

const scans = () =>
  Effect.runPromise(Metric.value(fullScanCounter)).then(({ count }) => count);

/** The commit base on `tip` at the native root `r1`, before and after
 * landed-block processing folds the merge of `headerHash`: the ledger at
 * `r1` holding `E1` both times, read with no whole `confirmed_ledger`. */
const buildsBeforeAndAfterTheFold = async (
  tip: Awaited<ReturnType<typeof confirmedTipOf>>,
  headerHash: string,
  r1: string,
) => {
  const expectBase = async () => {
    const before = await scans();
    const outcome = await resolve(tip, r1, { verified: "none" });
    expect(await scans()).toBe(before);
    expect(Either.isRight(outcome)).toBe(true);
    const base = Either.getOrThrow(outcome);
    expect(base.source).toBe(`confirmed:${headerHash}`);
    expect(base.root).toBe(r1);
    expect(base.entries).toEqual(hexEntries([E1]));
  };
  // `E1`'s output is the landed row's until the fold.
  await expectBase();
  expect(
    await run(
      foldToRoot(foldPorts, {} as View, { headerHash, utxosRoot: r1 }, () =>
        Effect.succeed(null),
      ),
    ),
  ).toEqual({ kind: "at_root" });
  // `confirmed_ledger`'s after it.
  await expectBase();
};

describe("commit base on a confirmed-state tip", { concurrent: false }, () => {
  it("waits while the native root is not another operator's merged root, and builds on the ledger at it before and after the fold", async () => {
    const { headerHash, r0, r1 } = await fixture({ frontier: true, row: {} });
    const tip = await confirmedTipOf(mergedAt(headerHash, r1));
    expectPending(await resolve(tip, r0, { verified: "none" }), headerHash);
    await buildsBeforeAndAfterTheFold(tip, headerHash, r1);
  });

  it("builds on this node's own merged block's root before and after the fold", async () => {
    const own = journalHeader("own-merged");
    const OWN = own.toString("hex");
    const { r0, r1 } = await fixture({
      frontier: true,
      row: { headerHash: OWN, kind: "own" },
    });
    // The block's journal on the frontier, its delta the landed row's.
    const journal = journalFixture(own);
    await run(
      prepareUnderTestLease(
        {
          ...journal,
          ledgerDelta: { spent: [E0.outref], produced: [E1] },
          metadata: {
            ...journal.metadata,
            baseTailHeaderHash: Buffer.from(FRONTIER_HASH, "hex"),
            baseRoots: { ...journal.metadata.baseRoots, utxosRoot: r0 },
            expectedRoots: {
              ...journal.metadata.expectedRoots,
              utxosRoot: r1,
            },
          },
        },
        { beforeJournalInsert: Effect.orDie(simCommitAnchor) },
      ),
    );
    await buildsBeforeAndAfterTheFold(
      await confirmedTipOf(mergedAt(OWN, r1)),
      OWN,
      r1,
    );
  });

  it("builds at genesis on the genesis root, the configured genesis UTxOs giving the outputs no table holds", async () => {
    const genesisUtxo: UTxO = {
      txHash: "61".repeat(32),
      outputIndex: 0,
      address,
      assets: { lovelace: 5_000_000n },
    };
    const { ledgerOp, outputCbor } = await Effect.runPromise(
      utxoToLedgerInsertMaterial(genesisUtxo),
    );
    const genesis = { outref: Buffer.from(ledgerOp.key), output: outputCbor };
    const genesisRoot = await rootOf([genesis]);
    await seed([], {
      headerHash: SDK.GENESIS_HEADER_HASH,
      utxosRoot: genesisRoot,
    });
    const tip = await confirmedTipOf({
      ...mergedAt(SDK.GENESIS_HEADER_HASH, genesisRoot),
      prevHeaderHash: SDK.GENESIS_HEADER_HASH,
    });
    const outcome = await resolve(tip, genesisRoot, {
      genesisUtxos: [genesisUtxo],
      ledgers: [[genesis]],
      verified: "none",
    });
    expect(Either.isRight(outcome)).toBe(true);
    const base = Either.getOrThrow(outcome);
    expect(base.source).toBe(`confirmed:${SDK.GENESIS_HEADER_HASH}`);
    expect(base.root).toBe(genesisRoot);
    expect(base.entries).toEqual(hexEntries([genesis]));
  });
});
