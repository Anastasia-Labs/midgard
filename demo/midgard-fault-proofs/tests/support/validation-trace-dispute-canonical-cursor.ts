import {
  type Emulator,
  getAddressDetails,
  type LucidEvolution,
} from "@lucid-evolution/lucid";
import { vi } from "vitest";

/** Match Kupo and the recorded raw authority: mempool reservation is not spend inclusion. */
export const useCanonicalValidationDisputeCursorReads = (
  lucid: LucidEvolution,
  emulator: Emulator,
) => {
  const unit = vi
    .spyOn(lucid, "utxoByUnit")
    .mockImplementation(async (unit) => {
      const found = Object.values(emulator.ledger)
        .map(({ utxo }) => utxo)
        .filter((utxo) => (utxo.assets[unit] ?? 0n) > 0n);
      if (found.length > 1)
        throw new Error("Unit needs to be an NFT or only held by one address.");
      if (found.length === 0) throw new Error("Unit not found.");
      return found[0]!;
    });
  const address = vi.spyOn(lucid, "utxosAt").mockImplementation(async (query) =>
    Object.values(emulator.ledger)
      .map(({ utxo }) => utxo)
      .filter((utxo) =>
        typeof query === "string"
          ? utxo.address === query
          : getAddressDetails(utxo.address).paymentCredential?.hash ===
            query.hash,
      ),
  );
  return () => {
    unit.mockRestore();
    address.mockRestore();
  };
};
