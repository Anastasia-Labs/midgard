import type { OutRefLike } from "@al-ft/midgard-core/out-ref";
import type * as SDK from "@al-ft/midgard-sdk";

import type {
  BoundHistoryChainBlock,
  EventHistorySourceBinding,
} from "./l1-event-history-source.js";
import {
  decodeEventHistoryTransition,
  type HistoryTransition,
} from "./l1-event-history-transition.js";
import type { LedgerSnapshotOutput } from "./l1-ledger-snapshot.js";

const label = (ref: OutRefLike) => `${ref.txHash}#${ref.outputIndex}`;

/** Stage exactly the tracked domain through a complete source-admitted block.
 * This carries no complete-address-capture claim. Wrappers establish either
 * complete raw address scope or complete authenticated list scope and ancestry.
 */
export const stageEventHistoryBlock = ({
  outputs,
  block,
  binding,
  histories,
  isTrackedOutput,
  resolveReference,
  slotToUnixTime,
}: {
  readonly outputs: readonly LedgerSnapshotOutput[];
  readonly block: BoundHistoryChainBlock;
  readonly binding: EventHistorySourceBinding;
  readonly histories: SDK.EventHistoryContractPair;
  readonly isTrackedOutput: (output: LedgerSnapshotOutput) => boolean;
  readonly resolveReference: (
    transactionHash: string,
    ref: OutRefLike,
  ) => LedgerSnapshotOutput | undefined;
  readonly slotToUnixTime: (slot: number) => number;
}) => {
  if (outputs.some((output) => !isTrackedOutput(output)))
    throw new Error("History staged input lies outside its tracked domain");
  const current = new Map(outputs.map((output) => [label(output), output]));
  if (current.size !== outputs.length)
    throw new Error("History capture repeats an output reference");
  const spent = new Set<string>();
  const created = new Map<string, LedgerSnapshotOutput>();
  const positions = new Map(
    block.transactions.map((tx, index) => [tx.txHash, index]),
  );
  if (positions.size !== block.transactions.length)
    throw new Error("History block repeats a transaction identity");
  const transitions: {
    transactionIndex: number;
    transition: HistoryTransition;
  }[] = [];
  for (const [transactionIndex, transaction] of block.transactions.entries()) {
    const resolve = (ref: OutRefLike) => {
      const creationIndex = positions.get(ref.txHash);
      if (
        spent.has(label(ref)) ||
        (creationIndex !== undefined && creationIndex >= transactionIndex)
      )
        throw new Error(
          "History reference is not live before its observing transaction",
        );
      const tracked = current.get(label(ref));
      if (tracked !== undefined) return tracked;
      if (creationIndex !== undefined) {
        const actual = created.get(label(ref));
        if (actual === undefined)
          throw new Error(
            "History reference was not created by its earlier transaction",
          );
        return actual;
      }
      const historical = resolveReference(transaction.txHash, ref);
      if (historical !== undefined && isTrackedOutput(historical))
        throw new Error(
          "Historical reference contradicts the complete tracked scope",
        );
      return historical;
    };
    for (const kind of ["deposit", "withdrawal"] as const) {
      const transition = decodeEventHistoryTransition({
        transaction,
        kind,
        history: histories[kind],
        binding,
        currentNodes: [...current.values()].filter(
          (output) => output.address === binding.deployments[kind].address,
        ),
        resolveReference: resolve,
        slotToUnixTime,
      });
      if (transition !== null)
        transitions.push(Object.freeze({ transactionIndex, transition }));
    }
    const inputs =
      transaction.spends === "inputs"
        ? transaction.inputs
        : transaction.collaterals;
    for (const input of inputs) {
      const ref = label(input);
      if (spent.has(ref))
        throw new Error("History block spends an input twice");
      spent.add(ref);
      current.delete(ref);
      created.delete(ref);
    }
    const outputs =
      transaction.spends === "inputs"
        ? transaction.outputs
        : transaction.collateralReturn === undefined
          ? []
          : [transaction.collateralReturn];
    for (const output of outputs) {
      created.set(label(output), output);
      if (!isTrackedOutput(output)) continue;
      if (current.has(label(output)))
        throw new Error("History block repeats a live output reference");
      current.set(label(output), output);
    }
  }
  return Object.freeze({
    outputs: Object.freeze([...current.values()]),
    transitions: Object.freeze(transitions),
  });
};
