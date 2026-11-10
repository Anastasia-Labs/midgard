/**
 * An L1 access over an emulator whose observed ledger tip the test sets.
 *
 * The emulator plays the chain and the wall clock. The access reports the
 * ledger tip a node would observe, `tipAt` of the emulator's slot: the last
 * block's slot, which stands still between blocks. The submit check and
 * every builder that judges a lower bound read that tip. Its slot clock
 * follows the wall clock, as a live node's does between blocks.
 */
import type { Emulator, LucidEvolution } from "@lucid-evolution/lucid";

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
