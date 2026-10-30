import type { LedgerAt } from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import { type LucidEvolution, toUnit } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type { Published } from "./published-deposit-trace.published-deposit-trace-checkpoint.js";

/**
 * Append after the state queue's current tail instead of emptying the queue.
 * The tail's ledger comes from independently replayed state and is checked
 * against the tail's committed root.
 */
export type PublishedDepositTraceLiveTail = { ledgerAt: LedgerAt };

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
  /** Where the honest predecessor is appended, and the queue's first header. */
  const readAppendPoint = async () => {
    const { root, last, first } = await readQueueEnds();
    return liveTail === undefined
      ? { anchor: root.utxo, head: undefined }
      : { anchor: last.utxo, head: first?.utxo };
  };
  return { nodeUnit, readQueueEnds, ledgerPairs, readBase, readAppendPoint };
};
