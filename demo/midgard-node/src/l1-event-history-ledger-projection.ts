import type { OutRefLike } from "@al-ft/midgard-core/out-ref";
import type * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";

import { stageEventHistoryBlock } from "./l1-event-history-block-stage.js";
import {
  type BoundHistoryChainBlock,
  decodeBoundEventHistoryLedgerSnapshot,
  type EventHistorySourceBinding,
} from "./l1-event-history-source.js";
import type { HistoryTransition } from "./l1-event-history-transition.js";
import type {
  AcquiredLedgerSnapshot,
  LedgerSnapshotOutput,
} from "./l1-ledger-snapshot.js";

export type BoundHistoryCapture = Effect.Effect.Success<
  ReturnType<typeof decodeBoundEventHistoryLedgerSnapshot>
>;

/** Internal block staging from a complete, source-admitted raw scoped ledger.
 * It also accepts pre-initialization state; callers must establish activation
 * provenance before treating the result as a history origin.
 * Stage both lists and retention UTxOs in ledger transaction order. No caller
 * state is mutated or published until the entire block and resulting complete
 * paired snapshot have decoded. The owner must still persist the block/receipts
 * atomically under its live generation, and retain ancestry for rollback.
 */
export const projectEventHistoryLedgerBlock = async ({
  ledger,
  block,
  binding,
  histories,
  resolveReference,
  slotToUnixTime,
}: {
  readonly ledger: AcquiredLedgerSnapshot;
  readonly block: BoundHistoryChainBlock;
  readonly binding: EventHistorySourceBinding;
  readonly histories: SDK.EventHistoryContractPair;
  /** Supplies exact historical output bytes, scoped to this observing tx and
   * branch. Current captured/tracked outputs take precedence over archives. */
  readonly resolveReference: (
    transactionHash: string,
    ref: OutRefLike,
  ) => LedgerSnapshotOutput | undefined;
  readonly slotToUnixTime: (slot: number) => number;
}): Promise<
  Readonly<{
    capture: BoundHistoryCapture;
    transitions: readonly Readonly<{
      transactionIndex: number;
      transition: HistoryTransition;
    }>[];
  }>
> => {
  if (
    block.parent !== ledger.point.id ||
    block.point.slot <= ledger.point.slot ||
    block.point.id === ledger.point.id
  )
    throw new Error("History projection does not extend its bound capture");
  const scope = new Set(ledger.addresses);
  const staged = stageEventHistoryBlock({
    outputs: ledger.outputs,
    block,
    binding,
    histories,
    isTrackedOutput: (output) => scope.has(output.address),
    resolveReference,
    slotToUnixTime,
  });
  const capture = await Effect.runPromise(
    decodeBoundEventHistoryLedgerSnapshot(
      Object.freeze({
        point: Object.freeze({ slot: block.point.slot, id: block.point.id }),
        addresses: Object.freeze([...ledger.addresses]),
        outputs: staged.outputs,
      }),
      binding,
    ),
  );
  return Object.freeze({ capture, transitions: staged.transitions });
};
