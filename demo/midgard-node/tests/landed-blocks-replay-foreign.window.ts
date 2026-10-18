/**
 * The foreign replayer bench's block windows (N3): a block's window placed
 * by its earliest admission slot `T(B)`, the view's clock moved so its
 * horizon reaches the block's end, and a replay at the store's view. The
 * bench's slot configuration maps slot `s` to `s * 1000` ms.
 */
import type { FactStore, OutRef } from "@al-ft/midgard-l1-follower";
import type { SimTx } from "@al-ft/midgard-l1-follower/testing";
import * as SDK from "@al-ft/midgard-sdk";

import type { ReplayInput } from "../src/landed-blocks/replay.js";
import { unixTimeToSlotForConfig } from "../src/lucid-time.js";
import { rebind } from "./foreign-block-import.fixture.js";
import { ELSEWHERE } from "./helpers/l1-events-chain.js";
import {
  clock,
  emptyBlock,
  idKey,
  inNode,
  inputFor,
  naming,
  nonceRef,
  replayerFor,
  retain,
  retainedRow,
} from "./landed-blocks-replay-foreign.fixture.js";

export const SLOT_MS = 1_000n;

/** The slot holding `unixTimeMs` in the bench's slot configuration, as the replay maps it. */
export const benchSlotOf = (unixTimeMs: number): number =>
  unixTimeToSlotForConfig(unixTimeMs, {
    zeroTime: 0,
    zeroSlot: 0,
    slotLength: Number(SLOT_MS),
  });

/** The window start whose earliest admission slot `T(B)` is `slot`. */
export const startFor = (slot: number) =>
  BigInt(slot) * SLOT_MS +
  BigInt(SDK.EVENT_WAIT_DURATION_MS) +
  SDK.MAX_EVENT_VALIDITY_RANGE_LENGTH_MS;

/** `payload` over the window `(start, start + span]`, committed at its end. */
export const windowed = (
  payload: SDK.DaPayload,
  start: bigint,
  span = SLOT_MS,
) =>
  rebind({
    ...payload,
    block_body: {
      ...payload.block_body,
      header: {
        ...payload.block_body.header,
        startTime: start,
        endTime: start + span,
        blockSlot: (start + span) / SLOT_MS,
      },
    },
  });

/** Sets the view's clock so its horizon is `by` ms past the block's end. */
export const reach = async (
  store: FactStore,
  payload: SDK.DaPayload,
  by = 0,
) => {
  const view = (await store.currentView())!;
  clock.offsetMs =
    Number(payload.block_body.header.endTime) -
    view.point.slot * 1_000 -
    SDK.EVENT_WAIT_DURATION_MS +
    1 +
    by;
};

export const originSlot = async (store: FactStore) =>
  (await store.cursor())!.origin.slot;

/** Retains `payload` and replays it at the view, its node output entered at `enteredSlot` (default: the view's slot). */
export const replayAt = async (
  store: FactStore,
  payload: SDK.DaPayload,
  enteredSlot?: number | null,
) => {
  await retain(await retainedRow(payload));
  const input: ReplayInput = await inputFor(store, payload);
  return inNode(
    replayerFor(store)(
      enteredSlot === undefined ? input : { ...input, enteredSlot },
    ),
  );
};

/** A block over the window starting at `start` naming a forced order no one admitted. */
export const strangerOrderBlock = async (start: bigint) =>
  windowed(
    await naming(await emptyBlock(), {
      forced_transactions: [[idKey(nonceRef()), "00"]],
    }),
    start,
  );

/** The tx that spends the order output at `outRef`. */
export const spendTx = (outRef: OutRef, nonce: number): SimTx => ({
  inputs: [outRef],
  outputs: [{ address: ELSEWHERE, lovelace: 3_000_000n }],
  nonce,
});
