import type { LedgerAt } from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import { type LucidEvolution, toUnit, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { PublishedTransactionSubmissionError } from "./published-block-actor.is-unanswered-submission.js";
import type { Published } from "./published-deposit-trace.published-deposit-trace-checkpoint.js";

/**
 * Append after the state queue's current tail instead of emptying the queue.
 * The tail's ledger comes from independently replayed state and is checked
 * against the tail's committed root.
 */
export type PublishedDepositTraceLiveTail = { ledgerAt: LedgerAt };

/**
 * The output a block whose parent is `headerHash` is committed on: the
 * parent's own node while it is the queue's tail, or the root once the
 * parent merged and nothing follows it. A commit on an empty queue spends
 * the root as its anchor (onchain/aiken/validators/state-queue.ak:1157-1191)
 * and continues the root's confirmed state (state-queue.ak:124-138), which a
 * merge sets to the merged block's (state-queue.ak:401-409); so a block
 * built on its parent commits unchanged once that parent merges. `node` is
 * the output holding `headerHash`'s node, if any. Undefined when the parent
 * was removed or a newer block follows it.
 */
export const appendPointAfter = async (
  headerHash: string,
  ends: { node: UTxO | undefined; root(): Promise<UTxO> },
): Promise<UTxO | undefined> => {
  if (ends.node !== undefined) {
    const node = await Effect.runPromise(
      SDK.getLinkedListNodeViewFromUTxO(ends.node),
    );
    return node.key !== "Empty" &&
      node.key.Key.key === headerHash &&
      node.next === "Empty"
      ? ends.node
      : undefined;
  }
  const root = await ends.root();
  const { data, link } = await Effect.runPromise(
    SDK.getConfirmedStateFromStateQueueDatum(
      await Effect.runPromise(SDK.getLinkedListNodeViewFromUTxO(root)),
    ),
  );
  return link === "Empty" && data.headerHash === headerHash ? root : undefined;
};

const outRefOf = (utxo: UTxO) =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

/**
 * The state the deposit trace extends: the confirmed state of an emptied
 * queue, or with a live tail the queue's last header and its replayed ledger.
 */
export const publishedDepositTraceQueue = ({
  contracts,
  lucid,
  liveTail,
}: {
  contracts: Published["contracts"];
  /** The wallet currently reading the chain; it may change between reads. */
  lucid: () => LucidEvolution;
  liveTail: PublishedDepositTraceLiveTail | undefined;
}) => {
  const nodeUnit = (headerHash: string) =>
    toUnit(
      contracts.stateQueue.policyId,
      SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + headerHash,
    );
  /** The queue's last output (the root when it is empty) and first header. */
  const readQueueEnds = async () => {
    const queue = await Effect.runPromise(
      SDK.fetchSortedStateQueueUTxOsProgram(lucid(), {
        stateQueueAddress: contracts.stateQueue.spendingScriptAddress,
        stateQueuePolicyId: contracts.stateQueue.policyId,
      }),
    );
    const last = queue[queue.length - 1];
    if (last === undefined) throw new Error("State queue has no root");
    return { root: queue[0]!, last, first: queue[1] };
  };
  /** The ledger after `headerHash`: empty for the confirmed state. */
  const ledgerPairs = async (
    headerHash: string,
  ): Promise<readonly SDK.DaPayloadEntry[]> => {
    if (liveTail === undefined) return [];
    const entries = await liveTail.ledgerAt(headerHash);
    if (entries === undefined)
      throw new Error(
        `Live tail ${headerHash} has no independently replayed ledger`,
      );
    return entries.map(({ outRef, output }) => [
      outRef.toString("hex"),
      output.toString("hex"),
    ]);
  };
  const readBase = async () => {
    const { root, last } = await readQueueEnds();
    const { datum } = liveTail === undefined ? root : last;
    if (datum.key === "Empty") {
      const { data } = await Effect.runPromise(
        SDK.getConfirmedStateFromStateQueueDatum(datum),
      );
      return {
        headerHash: data.headerHash,
        utxoRoot: data.utxoRoot,
        endTime: data.endTime,
        ledger: await ledgerPairs(data.headerHash),
      };
    }
    const { utxosRoot, endTime } = await Effect.runPromise(
      SDK.getHeaderFromStateQueueDatum(datum),
    );
    return {
      headerHash: datum.key.Key.key,
      utxoRoot: utxosRoot,
      endTime,
      ledger: await ledgerPairs(datum.key.Key.key),
    };
  };
  /** Where a block whose parent is `headerHash` goes now, and the first header. */
  const readAppendPointAfter = async (headerHash: string) => {
    const { root, last, first } = await readQueueEnds();
    const anchor = await appendPointAfter(headerHash, {
      node: last.datum.key === "Empty" ? undefined : last.utxo,
      root: async () => root.utxo,
    });
    return anchor === undefined
      ? undefined
      : { anchor, head: anchor === root.utxo ? undefined : first?.utxo };
  };
  /**
   * Commit `block` on its parent's append point, read again at every
   * attempt. Our node merges any attested block once it matures, whoever's
   * shift it is, so the parent can merge between the read and the commit:
   * the ledger then refuses the commit, whose anchor is spent. Only when the
   * append point has moved since, so that the refused transaction can never
   * land, is the block committed again on the new one, in a new transaction;
   * the refused bytes are never resubmitted. Any other failure is thrown.
   */
  const commitAfterParent = async (
    block: Readonly<{ header: SDK.Header; headerHash: string }>,
    submit: (anchor: UTxO, head: UTxO | undefined) => Promise<string>,
    onStage: (name: string) => void,
  ): Promise<string> => {
    const parent = block.header.prevHeaderHash;
    for (;;) {
      const point = await readAppendPointAfter(parent);
      if (point === undefined)
        throw new Error(
          `The state queue no longer ends at ${parent}, the parent of ${block.headerHash}`,
        );
      try {
        return await submit(point.anchor, point.head);
      } catch (error) {
        if (!(error instanceof PublishedTransactionSubmissionError))
          throw error;
        const moved = await readAppendPointAfter(parent);
        if (
          moved === undefined ||
          outRefOf(moved.anchor) === outRefOf(point.anchor)
        )
          throw error;
        onStage(
          `header commit ${block.headerHash} refused on spent anchor ${outRefOf(point.anchor)}; committing on ${outRefOf(moved.anchor)}`,
        );
      }
    }
  };
  return { nodeUnit, readQueueEnds, ledgerPairs, readBase, commitAfterParent };
};
