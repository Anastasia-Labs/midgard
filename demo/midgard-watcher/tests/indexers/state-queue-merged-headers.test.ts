import { describe, expect, it } from "vitest";

import { unsafeResolveMergedWatcherStateQueueHeadersForTest as resolveMergedHeaders } from "../../src/indexers/authenticated-state-queue-observation.js";
import {
  headerFixture,
  observation,
} from "../fault-proofs/fault-decision-bridge.observation.js";
import { h32 } from "../support/deployment-authority-fixture.js";
import {
  authority,
  behind,
  chain,
  grind,
  type MergeOptions,
  mergeTx,
  nodeAsset,
  queueTx,
  RELEASE_DEPTH,
  removalRedeemer,
  resolve,
  resolveReleased,
  ROOT_ASSET,
} from "./state-queue-merged-headers.fixture.js";

describe("state-queue headers merged on L1", () => {
  it("follows the confirmed-state root through consecutive release-final merges", async () => {
    const current = behind();
    const [first, second] = current.finalizedHeaders;
    const root = current.finalizedQueue[0]!.outRef;
    // Each merge spends the root and the head's node, so the second is
    // first reached through the second header's untouched node.
    const one = mergeTx(root, first!.queueOutRef, first!.headerHash, 150);
    const two = mergeTx(
      `${one.txHash}#0`,
      second!.queueOutRef,
      second!.headerHash,
      160,
    );
    const readers = chain([
      [root, one],
      [first!.queueOutRef, one],
      [`${one.txHash}#0`, two],
      [second!.queueOutRef, two],
    ]);
    const merged = await resolveMergedHeaders({
      observation: current,
      authority,
      readers,
    });
    expect([...merged.keys()]).toEqual([first!.headerHash, second!.headerHash]);
    expect(merged.get(first!.headerHash)).toEqual({
      headerHash: first!.headerHash,
      mergeTransactionHash: one.txHash,
      mergeBlockHash: one.inclusionPoint.blockHash,
      mergeSlot: "1500",
      mergeBlockNo: "150",
      confirmationDepth: RELEASE_DEPTH.toString(),
    });
    expect(readers.readTransaction).toHaveBeenCalledTimes(2);
  });

  it("applies two merges of one block in chain order, whatever their hashes", async () => {
    const current = behind();
    const [first, second] = current.finalizedHeaders;
    const root = current.finalizedQueue[0]!.outRef;
    const one = mergeTx(root, first!.queueOutRef, first!.headerHash, 150);
    // The later merge sorts first by hash, so hash order alone would apply
    // it before the merge whose root it spends.
    const two = grind(
      (fee) =>
        mergeTx(
          `${one.txHash}#0`,
          second!.queueOutRef,
          second!.headerHash,
          150,
          { fee },
        ),
      (tx) => tx.txHash < one.txHash,
    );
    expect(
      await resolve(
        chain([
          [root, one],
          [first!.queueOutRef, one],
          [`${one.txHash}#0`, two],
          [second!.queueOutRef, two],
        ]),
        current,
      ),
    ).toEqual([
      { headerHash: first!.headerHash, mergeTransactionHash: one.txHash },
      { headerHash: second!.headerHash, mergeTransactionHash: two.txHash },
    ]);
  });

  it("proves only the merges the walk reaches when a root spend between them is missing", async () => {
    const current = behind();
    const [first, second] = current.finalizedHeaders;
    const root = current.finalizedQueue[0]!.outRef;
    const one = mergeTx(root, first!.queueOutRef, first!.headerHash, 150);
    // The second merge spends a root no reached transaction produced.
    const two = mergeTx(
      `${h32("36")}#0`,
      second!.queueOutRef,
      second!.headerHash,
      160,
    );
    expect(
      await resolve(
        chain([
          [root, one],
          [first!.queueOutRef, one],
          [second!.queueOutRef, two],
        ]),
        current,
      ),
    ).toEqual([
      { headerHash: first!.headerHash, mergeTransactionHash: one.txHash },
    ]);
  });

  it("does not treat a merge shallower than release finality as proven", async () => {
    const current = behind();
    const [first, second] = current.finalizedHeaders;
    const root = current.finalizedQueue[0]!.outRef;
    const one = mergeTx(root, first!.queueOutRef, first!.headerHash, 150);
    const two = mergeTx(
      `${one.txHash}#0`,
      second!.queueOutRef,
      second!.headerHash,
      199,
      { depth: RELEASE_DEPTH - 1 },
    );
    expect(
      await resolve(
        chain([
          [root, one],
          [first!.queueOutRef, one],
          [`${one.txHash}#0`, two],
          [second!.queueOutRef, two],
        ]),
        current,
      ),
    ).toEqual([
      { headerHash: first!.headerHash, mergeTransactionHash: one.txHash },
    ]);
  });

  it.each([
    ["another header's node key", { headerKey: "ab".repeat(28) }],
    ["another confirmed-state input", { consumed: `${h32("34")}#0` }],
    ["no node burn", { burn: false }],
    ["no confirmed state at its output index", { rootOutput: false }],
    ["the root missing from its inputs", { spendsRoot: false }],
  ] satisfies [string, MergeOptions][])(
    "ends the walk at a root spend with %s",
    async (_label, options) => {
      const current = behind();
      const root = current.finalizedQueue[0]!.outRef;
      const [first] = current.finalizedHeaders;
      const merge = mergeTx(
        root,
        first!.queueOutRef,
        first!.headerHash,
        150,
        options,
      );
      const readers = chain([
        [root, merge],
        [first!.queueOutRef, merge],
      ]);
      expect(await resolve(readers, current)).toEqual([]);
    },
  );

  it("proves no merge when a correction spends the root", async () => {
    const current = observation([headerFixture("01")]);
    const root = current.finalizedQueue[0]!.outRef;
    const [only] = current.finalizedHeaders;
    // RemoveUnattestedBlockAfterTimeout of the last header, anchored at the
    // root: the root continues and the header is removed, not merged.
    const correction = queueTx({
      inputs: [root, only!.queueOutRef],
      outputs: [ROOT_ASSET],
      mint: [[nodeAsset(only!.headerHash), -1n]],
      redeemer: removalRedeemer.lastUnattested(root, only!.headerHash),
      blockNo: 150,
    });
    const readers = chain([
      [root, correction],
      [only!.queueOutRef, correction],
    ]);
    await expect(resolve(readers, current)).resolves.toEqual([]);
    expect(
      (await resolveReleased(readers, current)).get(only!.headerHash),
    ).toMatchObject({
      removalTransactionHash: correction.txHash,
      removalKind: "RemoveUnattestedBlockAfterTimeout",
    });
  });

  it("ends the walk at a reported spend whose body consumes no followed output", async () => {
    const current = behind();
    const root = current.finalizedQueue[0]!.outRef;
    // An inconsistent reader names a spender that never spends the root.
    const unrelated = queueTx({
      inputs: [`${h32("37")}#0`],
      outputs: [ROOT_ASSET],
      mint: [],
      redeemer: removalRedeemer.timedOutHead(root),
      blockNo: 150,
    });
    const readers = chain([[root, unrelated]]);
    expect(await resolve(readers, current)).toEqual([]);
    expect(readers.readOutRefs).toHaveBeenCalledOnce();
  });

  it("ends the walk at an unspent root", async () => {
    const readers = chain([]);
    expect(await resolve(readers)).toEqual([]);
    expect(readers.readOutRefs).toHaveBeenCalledOnce();
    expect(readers.readTransaction).not.toHaveBeenCalled();
  });

  it("reads nothing for an observation at or after the release boundary", async () => {
    const current = behind();
    const root = current.finalizedQueue[0]!.outRef;
    const [first] = current.finalizedHeaders;
    const readers = chain(
      [[root, mergeTx(root, first!.queueOutRef, first!.headerHash, 90)]],
      100,
    );
    expect(await resolve(readers, current)).toEqual([]);
    expect(readers.readOutRefs).not.toHaveBeenCalled();
  });

  it("classifies a header again after a rollback un-merges it", async () => {
    const current = behind();
    const root = current.finalizedQueue[0]!.outRef;
    const [first] = current.finalizedHeaders;
    const merge = mergeTx(root, first!.queueOutRef, first!.headerHash, 150);
    expect(
      await resolve(
        chain([
          [root, merge],
          [first!.queueOutRef, merge],
        ]),
        current,
      ),
    ).toHaveLength(1);
    // The same observation read after L1 rolled the merge back.
    expect(await resolve(chain([], 160), current)).toEqual([]);
  });
});
