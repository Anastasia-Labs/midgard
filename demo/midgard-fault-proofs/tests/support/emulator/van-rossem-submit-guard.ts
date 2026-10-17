/**
 * Vitest setup file: judges every transaction any emulator in this suite is
 * asked to accept against the Van Rossem limits before the emulator sees it,
 * so no lifecycle or publication test can forget the check. A publication
 * (zero execution units) is also held to the publication target.
 *
 * The emulator itself checks neither bound: transaction size is only refused
 * by the Lucid builder, and execution units not at all.
 *
 * The traced-refusal run is exempt: its validators carry verbose traces, so
 * its transactions are larger and costlier by design, and the plain run is
 * the one that proves every transaction fits.
 */
import { layerMethod } from "@al-ft/midgard-test-support/method-layer";
import { CML, Emulator } from "@lucid-evolution/lucid";

import {
  assertFitsVanRossem,
  type VanRossemCost,
} from "../../../src/proof-fit/van-rossem.js";
import { TRACED_REFUSALS } from "./traced-refusals.js";

const signedCost = (
  transactionCbor: string,
): VanRossemCost & { readonly txHash: string } => {
  const transaction = CML.Transaction.from_cbor_hex(transactionCbor);
  const body = transaction.body();
  const witnessSet = transaction.witness_set();
  const redeemers = witnessSet.redeemers();
  const flat = redeemers?.to_flat_format();
  try {
    let memoryUnits = 0n;
    let cpuUnits = 0n;
    for (let index = 0; index < (flat?.len() ?? 0); index += 1) {
      const redeemer = flat!.get(index);
      const exUnits = redeemer.ex_units();
      memoryUnits += exUnits.mem();
      cpuUnits += exUnits.steps();
      exUnits.free();
      redeemer.free();
    }
    const hash = CML.hash_transaction(body);
    const txHash = hash.to_hex();
    hash.free();
    return {
      txHash,
      signedBytes: transactionCbor.length / 2,
      memoryUnits,
      cpuUnits,
    };
  } finally {
    flat?.free();
    redeemers?.free();
    witnessSet.free();
    body.free();
    transaction.free();
  }
};

if (!TRACED_REFUSALS) {
  layerMethod(
    Emulator.prototype,
    "submitTx",
    (next) =>
      async function (this: Emulator, transactionCbor: string) {
        const { txHash, ...cost } = signedCost(transactionCbor);
        assertFitsVanRossem(cost, `emulator submission ${txHash}`);
        return next.call(this, transactionCbor);
      },
  );
}
