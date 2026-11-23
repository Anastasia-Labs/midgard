/**
 * The commit base on a confirmed-state tip (plan §7.3, N3, N5) the
 * confirmed-ledger frontier lags: while the fold of the merge has not taken
 * `confirmed_ledger` to the confirmed state, the commit waits
 * (`LANDED_COMMIT_BASE_PENDING`); once landed-block processing has folded
 * it, this node's own block or another operator's, the next resolution
 * builds on the state's post-state with no other step. The configured genesis ledger is the base only at the
 * genesis state while `confirmed_ledger` is empty.
 */
import type { View } from "@al-ft/midgard-l1-follower";
import * as SDK from "@al-ft/midgard-sdk/core";
import type { UTxO } from "@lucid-evolution/lucid";
import { Effect, Either } from "effect";
import { beforeEach, describe, expect, it } from "vitest";

import { foldToRoot } from "../src/landed-blocks/fold.js";
import {
  address,
  confirmedTipOf,
  E0,
  E1,
  expectDivergence,
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

describe(
  "commit base on a confirmed-state tip the frontier lags",
  { concurrent: false },
  () => {
    it("waits while another operator's merged block is not folded, and builds on its post-state once landed-block processing folds it", async () => {
      const { headerHash, r1 } = await fixture({ frontier: true, row: {} });
      const tip = await confirmedTipOf(mergedAt(headerHash, r1));
      expectPending(await resolve(tip, r1), headerHash);

      // Landed-block processing folds up to the merged root; nothing else
      // is written.
      expect(
        await run(
          foldToRoot(foldPorts, {} as View, { headerHash, utxosRoot: r1 }, () =>
            Effect.succeed(null),
          ),
        ),
      ).toEqual({ kind: "at_root" });
      const outcome = await resolve(tip, r1);
      expect(Either.isRight(outcome)).toBe(true);
      const base = Either.getOrThrow(outcome);
      expect(base.source).toBe("confirmed_ledger");
      expect(base.root).toBe(r1);
      expect(hexEntries(base.entries ?? [])).toEqual(hexEntries([E1]));
    });

    it("waits while this node's own merged block is not folded, and builds on its post-state once landed-block processing folds it", async () => {
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
      const tip = await confirmedTipOf(mergedAt(OWN, r1));
      expectPending(await resolve(tip, r1), OWN);

      // Landed-block processing folds the own block as it folds any other.
      expect(
        await run(
          foldToRoot(
            foldPorts,
            {} as View,
            { headerHash: OWN, utxosRoot: r1 },
            () => Effect.succeed(null),
          ),
        ),
      ).toEqual({ kind: "at_root" });
      const outcome = await resolve(tip, r1);
      expect(Either.isRight(outcome)).toBe(true);
      const base = Either.getOrThrow(outcome);
      expect(base.source).toBe("confirmed_ledger");
      expect(base.root).toBe(r1);
      expect(hexEntries(base.entries ?? [])).toEqual(hexEntries([E1]));
    });
  },
);

describe(
  "the configured genesis ledger as commit base",
  { concurrent: false },
  () => {
    const genesisUtxo: UTxO = {
      txHash: "61".repeat(32),
      outputIndex: 0,
      address,
      assets: { lovelace: 5_000_000n },
    };

    it("is the base at the genesis state while confirmed_ledger is empty", async () => {
      await seed([], {
        headerHash: SDK.GENESIS_HEADER_HASH,
        utxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      });
      const tip = await confirmedTipOf({
        ...mergedAt(SDK.GENESIS_HEADER_HASH, SDK.EMPTY_MERKLE_TREE_ROOT),
        prevHeaderHash: SDK.GENESIS_HEADER_HASH,
      });
      const outcome = await resolve(tip, SDK.EMPTY_MERKLE_TREE_ROOT, [
        genesisUtxo,
      ]);
      expect(Either.isRight(outcome)).toBe(true);
      const base = Either.getOrThrow(outcome);
      expect(base.source).toBe("genesis");
      expect(base.entries).toHaveLength(1);
    });

    it("is no base at any other confirmed state: an empty confirmed_ledger there is a divergence", async () => {
      const r0 = await rootOf([E0]);
      const state = mergedAt("c1".repeat(28), r0);
      await seed([], { headerHash: state.headerHash, utxosRoot: r0 });
      expectDivergence(
        await resolve(await confirmedTipOf(state), r0, [genesisUtxo]),
        {
          check: "commit_base_confirmed_ledger",
          headerHash: state.headerHash,
          root: r0,
          derivedRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
        },
      );
    });
  },
);
