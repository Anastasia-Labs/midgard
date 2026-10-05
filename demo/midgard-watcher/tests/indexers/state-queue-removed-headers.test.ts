import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  headerFixture,
  observation,
} from "../fault-proofs/fault-decision-bridge.observation.js";
import {
  behind,
  chain,
  grind,
  mergeTx,
  nodeAsset,
  queueTx,
  RELEASE_DEPTH,
  removalRedeemer,
  resolveReleased,
  ROOT_ASSET,
} from "./state-queue-merged-headers.fixture.js";

/** Each released header as `{ headerHash, kind, transactionHash }`. */
const released = async (...input: Parameters<typeof resolveReleased>) =>
  [...(await resolveReleased(...input)).values()].map((proof) =>
    "mergeTransactionHash" in proof
      ? {
          headerHash: proof.headerHash,
          kind: "merged",
          transactionHash: proof.mergeTransactionHash,
        }
      : {
          headerHash: proof.headerHash,
          kind: proof.removalKind,
          transactionHash: proof.removalTransactionHash,
        },
  );

/** RemoveTimedOutHead of the queue head, anchored at the root. */
const timedOutHead = (
  rootOutRef: string,
  headOutRef: string,
  headerHash: string,
  blockNo: number,
  depth?: number,
) =>
  queueTx({
    inputs: [rootOutRef, headOutRef],
    outputs: [ROOT_ASSET],
    mint: [[nodeAsset(headerHash), -1n]],
    redeemer: removalRedeemer.timedOutHead(rootOutRef),
    blockNo,
    ...(depth === undefined ? {} : { depth }),
  });

/** RemoveLastUnattestedBlock of the tail, anchored at its predecessor. */
const lastUnattested = (
  predecessorOutRef: string,
  predecessorHash: string,
  tailOutRef: string,
  headerHash: string,
  blockNo: number,
) =>
  queueTx({
    inputs: [predecessorOutRef, tailOutRef],
    outputs: [nodeAsset(predecessorHash)],
    mint: [[nodeAsset(headerHash), -1n]],
    redeemer: removalRedeemer.lastUnattested(predecessorOutRef, headerHash),
    blockNo,
  });

/** A spend of `nodeOutRef` that puts `headerHash`'s node back at `#0`. */
const reOutput = (
  nodeOutRef: string,
  headerHash: string,
  blockNo: number,
  fee?: bigint,
) =>
  queueTx({
    inputs: [nodeOutRef],
    outputs: [nodeAsset(headerHash)],
    mint: [],
    redeemer: Data.to(
      {
        RecordCompletedFraud: {
          state_queue_input_index: 0n,
          state_queue_output_index: 0n,
          fraud_proof_asset_name: "cc".repeat(32),
        },
      },
      SDK.StateQueueSpendRedeemer,
    ),
    blockNo,
    ...(fee === undefined ? {} : { fee }),
  });

/** RemoveLastFraudulentBlock of the tail, anchored at its predecessor. */
const lastFraudulent = (
  anchorOutRef: string,
  anchorHash: string,
  tailOutRef: string,
  headerHash: string,
  blockNo: number,
  fee?: bigint,
) =>
  queueTx({
    inputs: [anchorOutRef, tailOutRef],
    outputs: [nodeAsset(anchorHash)],
    mint: [[nodeAsset(headerHash), -1n]],
    redeemer: removalRedeemer.lastFraudulent(anchorOutRef, headerHash),
    blockNo,
    ...(fee === undefined ? {} : { fee }),
  });

describe("state-queue headers removed on L1", () => {
  it("proves a removed head and keeps proving merges past it", async () => {
    const current = behind();
    const root = current.finalizedQueue[0]!.outRef;
    const [first, second] = current.finalizedHeaders;
    const removal = timedOutHead(
      root,
      first!.queueOutRef,
      first!.headerHash,
      150,
    );
    // The merge is first reached through the second header's untouched
    // node, before the walk follows the root out of the removal.
    const merge = mergeTx(
      `${removal.txHash}#0`,
      second!.queueOutRef,
      second!.headerHash,
      160,
    );
    const readers = chain([
      [root, removal],
      [first!.queueOutRef, removal],
      [`${removal.txHash}#0`, merge],
      [second!.queueOutRef, merge],
    ]);
    const proofs = await resolveReleased(readers, current);
    expect(proofs.get(first!.headerHash)).toEqual({
      headerHash: first!.headerHash,
      removalTransactionHash: removal.txHash,
      removalKind: "RemoveUnavailableBlockAfterTimeout",
      removalBlockHash: removal.inclusionPoint.blockHash,
      removalSlot: "1500",
      removalBlockNo: "150",
      confirmationDepth: RELEASE_DEPTH.toString(),
    });
    expect(await released(readers, current)).toEqual([
      {
        headerHash: first!.headerHash,
        kind: "RemoveUnavailableBlockAfterTimeout",
        transactionHash: removal.txHash,
      },
      {
        headerHash: second!.headerHash,
        kind: "merged",
        transactionHash: merge.txHash,
      },
    ]);
  });

  it("follows a removal's anchor node to the header's later merge", async () => {
    const current = behind();
    const root = current.finalizedQueue[0]!.outRef;
    const [first, second] = current.finalizedHeaders;
    // The tail is removed through its predecessor, which continues at a new
    // output; the predecessor then merges over the untouched root.
    const removal = lastUnattested(
      first!.queueOutRef,
      first!.headerHash,
      second!.queueOutRef,
      second!.headerHash,
      150,
    );
    const merge = mergeTx(root, `${removal.txHash}#0`, first!.headerHash, 160);
    const readers = chain([
      [first!.queueOutRef, removal],
      [second!.queueOutRef, removal],
      [root, merge],
      [`${removal.txHash}#0`, merge],
    ]);
    expect(await released(readers, current)).toEqual([
      {
        headerHash: second!.headerHash,
        kind: "RemoveUnattestedBlockAfterTimeout",
        transactionHash: removal.txHash,
      },
      {
        headerHash: first!.headerHash,
        kind: "merged",
        transactionHash: merge.txHash,
      },
    ]);
  });

  it("does not treat a removal shallower than release finality as proven", async () => {
    const current = behind();
    const root = current.finalizedQueue[0]!.outRef;
    const [first, second] = current.finalizedHeaders;
    const removal = timedOutHead(
      root,
      first!.queueOutRef,
      first!.headerHash,
      199,
      RELEASE_DEPTH - 1,
    );
    const merge = mergeTx(
      `${removal.txHash}#0`,
      second!.queueOutRef,
      second!.headerHash,
      199,
    );
    expect(
      await released(
        chain([
          [root, removal],
          [first!.queueOutRef, removal],
          [`${removal.txHash}#0`, merge],
          [second!.queueOutRef, merge],
        ]),
        current,
      ),
    ).toEqual([]);
  });

  it("proves nothing for a burn under a redeemer that removes nothing", async () => {
    const current = behind();
    const root = current.finalizedQueue[0]!.outRef;
    const [first, second] = current.finalizedHeaders;
    // A merge redeemer naming another header cannot remove the head.
    const burn = mergeTx(root, first!.queueOutRef, first!.headerHash, 150, {
      headerKey: second!.headerHash,
    });
    expect(
      await released(
        chain([
          [root, burn],
          [first!.queueOutRef, burn],
        ]),
        current,
      ),
    ).toEqual([]);
  });

  it("proves no removal when a redeemer that removes nothing burns a node it spends", async () => {
    const current = behind();
    const root = current.finalizedQueue[0]!.outRef;
    const [first] = current.finalizedHeaders;
    const deinit = queueTx({
      inputs: [root, first!.queueOutRef],
      outputs: [ROOT_ASSET],
      mint: [[nodeAsset(first!.headerHash), -1n]],
      redeemer: Data.to("Deinit", SDK.StateQueueRedeemer),
      blockNo: 150,
    });
    const readers = chain([
      [root, deinit],
      [first!.queueOutRef, deinit],
    ]);
    expect(await released(readers, current)).toEqual([]);
  });

  it("follows an unrelated spend of a queued node and proves nothing", async () => {
    const current = observation([headerFixture("01")]);
    const [only] = current.finalizedHeaders;
    // An attestation-style spend that keeps the node's unit.
    const relink = queueTx({
      inputs: [only!.queueOutRef],
      outputs: [null, nodeAsset(only!.headerHash)],
      mint: [],
      redeemer: removalRedeemer.timedOutHead(only!.queueOutRef),
      blockNo: 150,
    });
    const readers = chain([[only!.queueOutRef, relink]]);
    expect(await released(readers, current)).toEqual([]);
    expect(readers.readOutRefs).toHaveBeenLastCalledWith(
      [current.finalizedQueue[0]!.outRef, `${relink.txHash}#1`],
      expect.anything(),
    );
  });

  it("returns a header to the queue after a rollback un-removes it", async () => {
    const current = behind();
    const root = current.finalizedQueue[0]!.outRef;
    const [first] = current.finalizedHeaders;
    const removal = timedOutHead(
      root,
      first!.queueOutRef,
      first!.headerHash,
      150,
    );
    expect(
      await released(
        chain([
          [root, removal],
          [first!.queueOutRef, removal],
        ]),
        current,
      ),
    ).toHaveLength(1);
    // The same observation read after L1 rolled the removal back.
    expect(await released(chain([], 160), current)).toEqual([]);
  });

  it("proves a fraud removal reached through its anchor after the target node was re-output", async () => {
    const current = behind();
    const [first, second] = current.finalizedHeaders;
    // RecordCompletedFraud moves the target; the removal then spends the
    // moved node with the anchor the walk still follows from the start.
    const recorded = reOutput(second!.queueOutRef, second!.headerHash, 150);
    const removal = lastFraudulent(
      first!.queueOutRef,
      first!.headerHash,
      `${recorded.txHash}#0`,
      second!.headerHash,
      160,
    );
    expect(
      await released(
        chain([
          [second!.queueOutRef, recorded],
          [first!.queueOutRef, removal],
          [`${recorded.txHash}#0`, removal],
        ]),
        current,
      ),
    ).toEqual([
      {
        headerHash: second!.headerHash,
        kind: "RemoveFraudulentBlockHeader",
        transactionHash: removal.txHash,
      },
    ]);
  });

  it("waits for every earlier re-output of one block before a removal spending the last", async () => {
    const current = behind();
    const [first, second] = current.finalizedHeaders;
    const recorded = reOutput(second!.queueOutRef, second!.headerHash, 150);
    const updated = reOutput(
      `${recorded.txHash}#0`,
      second!.headerHash,
      150,
      170_001n,
    );
    // The removal sorts first by hash and is reached through its anchor
    // while the walk still follows the target's original node.
    const removal = grind(
      (fee) =>
        lastFraudulent(
          first!.queueOutRef,
          first!.headerHash,
          `${updated.txHash}#0`,
          second!.headerHash,
          150,
          fee,
        ),
      ({ txHash }) => txHash < recorded.txHash && txHash < updated.txHash,
    );
    const spends = [
      [second!.queueOutRef, recorded],
      [`${recorded.txHash}#0`, updated],
      [first!.queueOutRef, removal],
      [`${updated.txHash}#0`, removal],
    ] as const;
    expect(await released(chain(spends), current)).toEqual([
      {
        headerHash: second!.headerHash,
        kind: "RemoveFraudulentBlockHeader",
        transactionHash: removal.txHash,
      },
    ]);
    // Without the middle re-output the removal never becomes applicable,
    // and the walk ends without proving it.
    await expect(
      released(chain(spends.filter(([, raw]) => raw !== updated)), current),
    ).resolves.toEqual([]);
  });
});
