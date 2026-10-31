/** The emulator ledger under the staging tests: whether it holds an output
 * unspent, a provider that does not yet show some outputs the ledger holds,
 * and the ledger dropping a transaction it took, as a rollback within
 * release depth would. */
import type { Emulator, LucidEvolution, UTxO } from "@lucid-evolution/lucid";
import { vi } from "vitest";

import type { Scene } from "./validation-trace-dispute-open-scene.js";

const keyOf = ({ txHash, outputIndex }: Pick<UTxO, "txHash" | "outputIndex">) =>
  `${txHash}${outputIndex.toString()}`;

const outRefOf = ({
  txHash,
  outputIndex,
}: Pick<UTxO, "txHash" | "outputIndex">) =>
  `${txHash}#${outputIndex.toString()}`;

/** Whether the ledger holds `utxo` unspent. */
export const onLedger = (
  { h }: Pick<Scene, "h">,
  utxo: Pick<UTxO, "txHash" | "outputIndex">,
): boolean => {
  const entry = h.emulator.ledger[keyOf(utxo)];
  return entry !== undefined && !entry.spent;
};

/** Keeps the outputs `outRefs` out of what `lucid` reads by out-ref, as a
 * provider behind the ledger; returns the end of it. */
export const hideFromProvider = (
  lucid: LucidEvolution,
  ...outRefs: readonly string[]
): (() => void) => {
  const hidden = new Set(outRefs);
  const read = lucid.utxosByOutRef.bind(lucid);
  const spy = vi
    .spyOn(lucid, "utxosByOutRef")
    .mockImplementation(async (refs) =>
      (await read(refs)).filter((utxo) => !hidden.has(outRefOf(utxo))),
    );
  return () => spy.mockRestore();
};

/** Drops the transaction `txHash`, which spent `spent` and paid one output,
 * from the ledger of `emulator`: `spent` is unspent again and the
 * transaction is unknown. */
export const rollBack = (
  emulator: Emulator,
  txHash: string,
  spent: UTxO,
): void => {
  const ledger = { ...emulator.ledger };
  delete ledger[keyOf({ txHash, outputIndex: 0 })];
  ledger[keyOf(spent)] = { utxo: spent, spent: false };
  emulator.ledger = ledger;
  delete emulator.transactionHistory[txHash];
};
