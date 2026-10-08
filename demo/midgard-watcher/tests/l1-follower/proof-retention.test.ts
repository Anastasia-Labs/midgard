/**
 * Proof retention on the fork simulator with pruning on (E1 ruling): a
 * removed header's L1 history is held past k while a proof objective over
 * it is open and pruned on schedule once released (or when never pinned);
 * the inputs of every tx a unit history records are stored at ingest, so a
 * commit funded by an untracked operator UTxO still resolves after the
 * node's ledger window has moved past its inclusion block.
 */
import { afterEach, describe, expect, it } from "vitest";

import {
  WATCHER_TX_INPUTS_TABLE,
  WATCHER_UNIT_HISTORY_TABLE,
} from "../../src/l1-follower/tables.js";
import {
  L1_TX_INPUTS_UNRESOLVABLE,
  L1_TX_INPUTS_UNRESOLVED,
} from "../../src/l1-follower/tx-inputs.js";
import {
  K,
  okValue,
  reasonOf,
} from "../support/l1-follower-raw-reads-fixture.js";
import {
  closeRemovedHeaders,
  departedRows,
  FOLLOWED_UNIT,
  historyRows,
  nodeUnit,
  type Removed,
  removedHeader,
  txRows,
} from "../support/proof-retention-removed-header.js";

afterEach(closeRemovedHeaders);

describe("proof retention: a removed header's history past k", () => {
  it("holds the pinned history, txs and departed row while the proof runs past K blocks", async () => {
    const r = await removedHeader({ pin: true, resolveAtIngest: true });
    expect(await departedRows(r)).toBe(1);
    // The proof runs: the tip moves well past K, pruning runs on schedule.
    await r.passK(2 * K + 4);

    expect(await historyRows(r)).toBeGreaterThan(0);
    expect(await departedRows(r)).toBe(1);
    expect(await txRows(r, r.commitHash)).toBe(1);
    expect(await txRows(r, r.removalHash)).toBe(1);
    const reads = r.h.reads(true);
    const history = okValue(
      await reads.unitHistoryAtPoint(nodeUnit(r.header), r.h.tipPoint()),
    );
    expect(history.transactions.map(({ txHash }) => txHash)).toEqual([
      r.commitHash,
      r.removalHash,
    ]);
    for (const { txHash, inclusionPoint } of history.transactions)
      expect(
        okValue(await reads.rawTransaction(txHash, inclusionPoint)).transaction
          .txHash,
      ).toBe(txHash);
  });

  it("releases the pin once the objective completes, and the rows prune", async () => {
    const r = await removedHeader({ pin: true, resolveAtIngest: true });
    await r.passK();
    expect(await historyRows(r)).toBeGreaterThan(0);

    await r.retention.release(r.target);
    expect(await r.retention.pinned()).toEqual([]);
    await r.passK(1);
    expect(await historyRows(r)).toBe(0);
    expect(await departedRows(r)).toBe(0);
    expect(await txRows(r, r.commitHash)).toBe(0);
    expect(
      reasonOf(
        await r.h
          .reads()
          .unitHistoryAtPoint(nodeUnit(r.header), r.h.tipPoint()),
      ),
    ).toBe("beyond_retention");
    // The stored inputs go with the tx they belong to.
    expect(await r.resolver.step()).toEqual([]);
    expect(
      await r.count(WATCHER_TX_INPUTS_TABLE, "tx_hash", r.commitHash),
    ).toBe(0);
  });

  it("prunes an unpinned removed header's history on schedule (negative control)", async () => {
    const r = await removedHeader({ pin: false, resolveAtIngest: true });
    await r.passK();
    expect(await historyRows(r)).toBe(0);
    expect(await departedRows(r)).toBe(0);
    expect(await txRows(r, r.commitHash)).toBe(0);
    expect(
      reasonOf(
        await r.h
          .reads()
          .unitHistoryAtPoint(nodeUnit(r.header), r.h.tipPoint()),
      ),
    ).toBe("beyond_retention");
  });

  it("holds the followed units a capture names while the header is pinned, and releases them with it", async () => {
    const r = await removedHeader({
      pin: true,
      resolveAtIngest: true,
      followUnit: true,
    });
    const held = await r.retention.holdUnits(r.header, [FOLLOWED_UNIT]);
    expect(held).toEqual({ kind: "held" });
    await r.passK();
    expect(
      await r.count(WATCHER_UNIT_HISTORY_TABLE, "unit", FOLLOWED_UNIT),
    ).toBe(2);
    const history = okValue(
      await r.h.reads().unitHistoryAtPoint(FOLLOWED_UNIT, r.h.tipPoint()),
    );
    expect(history.transactions.map(({ txHash }) => txHash)).toEqual(r.unitTxs);

    await r.retention.release(r.target);
    await r.passK(1);
    expect(
      await r.count(WATCHER_UNIT_HISTORY_TABLE, "unit", FOLLOWED_UNIT),
    ).toBe(0);
  });

  it("pins a removed header whose history is closed but not yet k deep", async () => {
    const r = await removedHeader({ pin: false, resolveAtIngest: true });
    expect(await r.retention.pin(r.target)).toEqual({ kind: "pinned" });
    await r.passK();
    expect(await historyRows(r)).toBeGreaterThan(0);
    expect(await departedRows(r)).toBe(1);
  });

  it("pins a second category on a header another pin already holds past k", async () => {
    const r = await removedHeader({ pin: true, resolveAtIngest: true });
    await r.passK();
    const second = { category: "otherCategory", headerHash: r.header };
    expect(await r.retention.pin(second)).toEqual({ kind: "pinned" });
    await r.retention.release(r.target);
    await r.passK(1);
    expect(await historyRows(r)).toBeGreaterThan(0);
  });
});

describe("tx inputs stored at ingest (facet 2)", () => {
  it("resolves a commit's untracked operator input after its parent is more than k deep", async () => {
    const r = await removedHeader({ pin: true, resolveAtIngest: true });
    await r.passK();
    // The node can no longer acquire the commit's parent.
    const raw = okValue(
      await r.h.reads(true).rawTransaction(r.commitHash, r.commitPoint),
    );
    expect(raw.unresolvedInputs).toEqual([]);
    expect(raw.unresolvedReferenceInputs).toEqual([]);
    expect(
      raw.transaction.resolvedInputs.map(({ outRef }) => outRef),
    ).toContain(r.operatorUtxo);
  });

  it("leaves the operator input unresolved when nothing stored it at ingest", async () => {
    const r = await removedHeader({ pin: true, resolveAtIngest: false });
    await r.passK();
    const raw = okValue(
      await r.h.reads(true).rawTransaction(r.commitHash, r.commitPoint),
    );
    expect(raw.unresolvedInputs.map(({ outRef }) => outRef)).toEqual([
      r.operatorUtxo,
    ]);
  });

  it("holds a named retrying reason while the node is down, pinned or not, and clears it once resolved", async () => {
    const r = await removedHeader({ pin: false, resolveAtIngest: false });
    r.ledger.down(true);
    const unresolved = await r.resolver.step();
    expect(
      unresolved.map(({ txHash, cause, permanent }) => [
        txHash,
        cause,
        permanent,
      ]),
    ).toContainEqual([r.commitHash, "unavailable", false]);
    const held = await r.resolver.assess();
    expect(held.readiness.map(({ reason }) => reason)).toEqual([
      L1_TX_INPUTS_UNRESOLVED,
    ]);
    expect(held.readiness[0]!.detail).toContain("(retrying)");
    expect(held.degradations).toEqual([]);

    r.ledger.down(false);
    expect(await r.resolver.step()).toEqual([]);
    expect(await r.resolver.assess()).toEqual({
      readiness: [],
      degradations: [],
    });
    expect(
      await r.count(WATCHER_TX_INPUTS_TABLE, "tx_hash", r.commitHash),
    ).toBeGreaterThan(0);
  });
});

describe("permanently unresolvable inputs: degraded unless a proof pin holds them", () => {
  const permanentOf = async (r: Removed, txHash: string) =>
    (await r.resolver.step()).find((entry) => entry.txHash === txHash);

  it("reports a deep catch-up's unresolvable inputs in unpinned histories as a degradation and stays ready", async () => {
    // Caught up from the origin past k: neither the init nor the commit was
    // resolved while its parent was in the node's window.
    const r = await removedHeader({
      pin: false,
      resolveAtIngest: false,
      resolveInit: false,
    });
    for (let i = 0; i < K + 1; i += 1) await r.h.forward([]);
    expect(await permanentOf(r, r.commitHash)).toMatchObject({
      cause: "too_old",
      permanent: true,
    });
    const deep = await r.resolver.assess();
    expect(deep.readiness).toEqual([]);
    expect(deep.degradations).toMatchObject([
      { reason: L1_TX_INPUTS_UNRESOLVABLE, count: 2 },
    ]);
    expect(deep.degradations[0]!.detail).toContain("no open proof needs them");
    // Never skipped: it holds across passes, and the commit's history pruning
    // drops it while the init's still-open hub-oracle history keeps the init.
    await r.passK();
    expect(
      (await r.resolver.step()).map(({ cause, permanent }) => [
        cause,
        permanent,
      ]),
    ).toEqual([["too_old", true]]);
    expect(await r.resolver.assess()).toMatchObject({
      readiness: [],
      degradations: [{ reason: L1_TX_INPUTS_UNRESOLVABLE, count: 1 }],
    });
  });

  it("fails readiness by name while a proof pin holds the tx's header, and clears when the pin releases", async () => {
    const r = await removedHeader({ pin: true, resolveAtIngest: false });
    for (let i = 0; i < K + 1; i += 1) await r.h.forward([]);
    expect(await permanentOf(r, r.commitHash)).toMatchObject({
      cause: "too_old",
      permanent: true,
    });
    const pinned = await r.resolver.assess();
    expect(pinned.readiness.map(({ reason }) => reason)).toEqual([
      L1_TX_INPUTS_UNRESOLVABLE,
    ]);
    expect(pinned.readiness[0]!.detail).toContain(r.commitHash);
    expect(pinned.degradations).toEqual([]);

    // The release clears the reason at once, with no pass and no restart.
    await r.retention.release(r.target);
    const released = await r.resolver.assess();
    expect(released.readiness).toEqual([]);
    expect(released.degradations).toMatchObject([
      { reason: L1_TX_INPUTS_UNRESOLVABLE, count: 1 },
    ]);
    await r.passK();
    expect(await r.resolver.step()).toEqual([]);
    expect(await r.resolver.assess()).toEqual({
      readiness: [],
      degradations: [],
    });
  });

  it("fails readiness while a pinned header's unit hold names the tx's unit, and clears when the pin releases", async () => {
    const r = await removedHeader({
      pin: true,
      resolveAtIngest: true,
      followUnit: true,
    });
    const [minted] = r.unitTxs as [string, string];
    // The mint spends an outside input no ledger state holds.
    expect(await permanentOf(r, minted)).toMatchObject({
      cause: "absent_at_parent",
      permanent: true,
    });
    expect((await r.resolver.assess()).readiness).toEqual([]);
    await r.retention.holdUnits(r.header, [FOLLOWED_UNIT]);
    const held = await r.resolver.assess();
    expect(held.readiness.map(({ reason }) => reason)).toEqual([
      L1_TX_INPUTS_UNRESOLVABLE,
    ]);
    expect(held.readiness[0]!.detail).toContain(minted);

    await r.retention.release(r.target);
    expect(await r.resolver.assess()).toMatchObject({
      readiness: [],
      degradations: [{ reason: L1_TX_INPUTS_UNRESOLVABLE, count: 1 }],
    });
  });
});
