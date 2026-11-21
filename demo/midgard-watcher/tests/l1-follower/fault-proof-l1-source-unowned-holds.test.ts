/**
 * A lost-defense slash's read of a closed case, over the follower's own
 * retention: the slash-due capture holds the receipt with the case token
 * under the header, so once a close that is no conviction burns the case
 * token, the receipt still live keeps the hold, and the case's closed
 * history stays readable past the retained depth with no objective owning
 * the hold.
 */
import type { FraudProofRawL1Snapshot } from "@al-ft/midgard-fault-proofs/workflow/raw-l1-snapshot";
import type { SimTx } from "@al-ft/midgard-l1-follower/testing";
import { afterEach, describe, expect, it } from "vitest";

import { createWatcherFaultProofL1Source } from "../../src/l1-follower/fault-proof-l1-source.js";
import { L1_PROOF_HOLDS_UNOWNED_LIVE } from "../../src/l1-follower/proof-retention.js";
import { createWatcherProofRosterRetention } from "../../src/l1-follower/proof-roster-retention.js";
import { WATCHER_UNIT_HISTORY_TABLE } from "../../src/l1-follower/tables.js";
import { K, X } from "../support/l1-follower-raw-reads-fixture.js";
import {
  pendingOver,
  RELEASE,
  requestFor,
  SOURCE_ID,
} from "../support/l1-follower-raw-source-fixture.js";
import {
  closeRemovedHeaders,
  FOLLOWED_UNIT,
  type Removed,
  removedHeader,
} from "../support/proof-retention-removed-header.js";
import { advance, passAtTip } from "../support/proof-roster.js";

afterEach(closeRemovedHeaders);

const FOLLOWED = FOLLOWED_UNIT.slice(0, 56);
const CASE = `${FOLLOWED}c1`;
const RECEIPT = `${FOLLOWED}e1`;
const at = (): Buffer =>
  Buffer.concat([Buffer.of(0x70), Buffer.from(FOLLOWED, "hex")]);
const assets = (name: string, quantity = 1n) =>
  new Map([[FOLLOWED, new Map([[name, quantity]])]]);

const forward = async (r: Removed, tx: SimTx): Promise<string> => {
  const hash = (await r.h.forward([tx])).hashes[0]!;
  // Its inputs are stored at ingest, while the node's window still has them.
  const failed = await r.resolver.step();
  expect(failed.filter(({ txHash }) => txHash === hash)).toEqual([]);
  return hash;
};

/** An output at X, in the node's ledger, for a mint to spend. */
const funded = async (r: Removed) => {
  const tx: SimTx = {
    inputs: [r.h.chain.outsideInput()],
    outputs: [{ address: X, lovelace: 4_000_000n }],
    nonce: r.h.chain.nonce(),
  };
  const [hash] = (await r.h.forward([tx])).hashes as [string];
  return { txHash: Buffer.from(hash, "hex"), index: 0 };
};

/** The slash-due read: the case token's history, with the receipt held alongside. */
const slashDueCapture = (r: Removed) =>
  createWatcherFaultProofL1Source({
    store: r.h.store,
    rawReads: r.h.reads(true),
    pending: pendingOver(r.h.store),
    sourceId: SOURCE_ID,
    proofRetention: r.retention,
  })
    .snapshotAuthority({
      releaseFinality: RELEASE,
      observationDepth: "inclusion",
    })
    .capture(
      requestFor(r.header, { scopes: [], historyUnits: [CASE, RECEIPT] }),
    )
    .then(({ snapshot }) => snapshot as FraudProofRawL1Snapshot);

const caseHistoryOf = (snapshot: FraudProofRawL1Snapshot) =>
  snapshot.history.find(({ unit }) => unit === CASE)?.transactionHashes;

describe("a closed case's history, held by its live receipt", () => {
  it("stays readable past the retained depth after a close that is no conviction burns the case token", async () => {
    const r = await removedHeader({
      pin: true,
      resolveAtIngest: true,
      followUnit: true,
      roster: createWatcherProofRosterRetention(),
    });
    const funding = await funded(r);
    const opened = await forward(r, {
      inputs: [funding],
      outputs: [{ address: at(), lovelace: 2_000_000n, assets: assets("c1") }],
      mint: assets("c1"),
      nonce: r.h.chain.nonce(),
    });
    // The close burns the case token and leaves the receipt live.
    const closed = await forward(r, {
      inputs: [{ txHash: Buffer.from(opened, "hex"), index: 0 }],
      outputs: [{ address: at(), lovelace: 2_000_000n, assets: assets("e1") }],
      mint: new Map([
        [
          FOLLOWED,
          new Map([
            ["c1", -1n],
            ["e1", 1n],
          ]),
        ],
      ]),
      nonce: r.h.chain.nonce(),
    });
    const first = await slashDueCapture(r);
    expect(caseHistoryOf(first)).toEqual([opened, closed].sort());

    // The objective is gone (its record lost); passes at tip run on.
    expect(await passAtTip(r)).toBe(true);
    await advance(r, K + 1);
    expect(
      await r.retention.releaseUnowned({
        owned: () => false,
        queued: new Set(),
      }),
    ).toEqual([]);
    expect(r.retention.degradations()).toContainEqual(
      expect.objectContaining({
        reason: L1_PROOF_HOLDS_UNOWNED_LIVE,
        count: 1,
      }),
    );
    await r.h.pruneAll();
    expect(await r.count(WATCHER_UNIT_HISTORY_TABLE, "unit", CASE)).toBe(2);

    const again = await slashDueCapture(r);
    expect(caseHistoryOf(again)).toEqual([opened, closed].sort());
    expect(
      again.history.find(({ unit }) => unit === RECEIPT)?.transactionHashes,
    ).toEqual([closed]);
  });
});
