/**
 * An L1 access over an emulator whose observed ledger tip the test sets.
 *
 * The emulator plays the chain and the wall clock. The access reports the
 * ledger tip a node would observe, `tipAt` of the emulator's slot: the last
 * block's slot, which stands still between blocks. The submit check and
 * every builder that judges a lower bound read that tip. Its slot clock
 * follows the wall clock, as a live node's does between blocks.
 */
import {
  SUBMIT_SLOT_LENGTH_MS,
  SUBMIT_SLOT_VALIDITY_BUFFER,
} from "@al-ft/midgard-core/ogmios-slot";
import type { Emulator, LucidEvolution } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import { openL1Access } from "../../src/l1-access.js";
import { inspectSignedTxValidityInterval } from "../../src/transactions/utils.js";

export type ObservedSubmission = {
  readonly observedTipSlot: number;
  readonly invalidBeforeSlot?: number;
};

/**
 * Attaches the access to `emulator` (so to every client over it) and
 * records, for every transaction the emulator is sent, the tip observed at
 * that moment and the transaction's lower bound.
 */
export const observeEmulatorTip = (
  lucid: LucidEvolution,
  emulator: Emulator,
  tipAt: (wallSlot: number) => number,
): readonly ObservedSubmission[] => {
  const wallSlot = () => lucid.unixTimeToSlot(emulator.now());
  const viewPoint = () =>
    Promise.resolve({
      slot: wallSlot(),
      id: `emulator:${wallSlot().toString()}`,
    });
  const slotConfig = lucid.config().slotConfig;
  if (slotConfig === undefined) throw new Error("Expected a slot config");
  openL1Access(
    {
      kind: "node",
      provider: emulator,
      endpoint: "test",
      slotConfig: () => Promise.resolve(slotConfig),
      tipSlot: () => Promise.resolve(wallSlot()),
      viewPoint,
      synchronizedViewPoint: viewPoint,
      submitSlotSnapshot: () =>
        Promise.resolve({
          source: "l1_node_tip",
          currentSlot: wallSlot(),
          ledgerTipSlot: tipAt(wallSlot()),
          observedAtMs: emulator.now(),
          slotLengthMs: slotConfig.slotLength,
        }),
      close: () => Promise.resolve(),
    },
    {
      slotLengthMs: slotConfig.slotLength,
      monotonicNowMs: () => emulator.now(),
    },
  );
  const submissions: ObservedSubmission[] = [];
  const submitTx = emulator.submitTx.bind(emulator);
  emulator.submitTx = (tx: string) => {
    const { invalidBeforeSlot } = inspectSignedTxValidityInterval(tx);
    submissions.push({
      observedTipSlot: tipAt(wallSlot()),
      ...(invalidBeforeSlot === undefined ? {} : { invalidBeforeSlot }),
    });
    return submitTx(tx);
  };
  return submissions;
};

/**
 * Advances `emulator` until a transaction whose lower bound must lie past
 * `deadlineMs` (a strike past its inactivity threshold, a thread expiry past
 * its duration, a move refused past a deadline) is due at the observed
 * ledger tip. Submit admits a lower bound only once the tip is the submit
 * margin (`SUBMIT_SLOT_VALIDITY_BUFFER` slots) past it, and a node loop that
 * backdates its bound from the tip backdates it by that margin, so either
 * way the tip less the margin must be past the deadline: one slot past the
 * deadline is not enough. The emulator's tip is its current slot, and
 * `emulator.now()` is that slot's start.
 */
export const awaitEmulatorTipPastDeadline = (
  emulator: Emulator,
  deadlineMs: number | bigint,
): void => {
  const marginMs = SUBMIT_SLOT_VALIDITY_BUFFER * SUBMIT_SLOT_LENGTH_MS;
  while (emulator.now() - marginMs <= Number(deadlineMs)) emulator.awaitSlot(1);
};

/**
 * A ledger tip that moves only when a block lands, one every `blockSlots`
 * slots. `lastBlockSlotsAgo` places the last block that many slots before
 * the emulator's slot now; the next ones follow every `blockSlots`.
 */
export const blockSizedTip = (
  lucid: LucidEvolution,
  emulator: Emulator,
  blockSlots: number,
): {
  readonly tipAt: (wallSlot: number) => number;
  readonly lastBlockSlotsAgo: (slots: number) => void;
} => {
  let origin = lucid.unixTimeToSlot(emulator.now());
  return {
    tipAt: (wallSlot) => wallSlot - ((wallSlot - origin) % blockSlots),
    lastBlockSlotsAgo: (slots) => {
      origin = lucid.unixTimeToSlot(emulator.now()) - slots;
    },
  };
};

/**
 * Expects the latest submission's lower bound reached, by the submit margin,
 * by the tip observed when it was sent: the ledger admits it there.
 */
export const expectLatestAdmissibleAtTip = (
  submissions: readonly ObservedSubmission[],
): void => {
  const submitted = submissions.at(-1);
  expect(submitted?.invalidBeforeSlot).toBeDefined();
  expect(submitted!.observedTipSlot).toBeGreaterThanOrEqual(
    submitted!.invalidBeforeSlot! + SUBMIT_SLOT_VALIDITY_BUFFER,
  );
};
