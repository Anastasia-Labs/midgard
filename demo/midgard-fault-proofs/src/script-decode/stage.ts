/**
 * Staging a script's bytes outside a workflow (the CLI and direct record
 * paths): each chunk is found by content at the staging address
 * (`workflow/own-outputs.ts`), and the first one missing is staged.
 */
import {
  type LucidEvolution,
  paymentCredentialOf,
  type UTxO,
} from "@lucid-evolution/lucid";

import { proofMaterialsAddress } from "../funding/proof-materials-address.js";
import { findCarriage } from "../workflow/own-outputs.js";
import {
  buildScriptDecodeStageTx,
  scriptDecodeStagingDatums,
  submitScriptDecodeTx,
} from "./submit.js";

/**
 * Stages `scriptBytes` in order across UTxOs at `address` (by default the
 * proof-materials address of the selected wallet's key, which that key
 * spends back), one transaction each. A chunk already staged there, by
 * whoever paid it, is reused.
 */
export const stageScriptDecodeBytes = async ({
  lucid,
  scriptBytes,
  address,
}: {
  readonly lucid: LucidEvolution;
  readonly scriptBytes: Uint8Array;
  readonly address?: string;
}): Promise<UTxO[]> => {
  const target =
    address ??
    proofMaterialsAddress(
      lucid.config().network ?? "Custom",
      paymentCredentialOf(await lucid.wallet().address()).hash,
    );
  const datums = scriptDecodeStagingDatums(scriptBytes);
  for (;;) {
    const found = await findCarriage({ lucid, address: target, datums });
    const missing = found.findIndex((utxo) => utxo === undefined);
    if (missing < 0) return found as UTxO[];
    await submitScriptDecodeTx(
      lucid,
      await buildScriptDecodeStageTx({
        lucid,
        datum: datums[missing]!,
        address: target,
      }),
    );
  }
};
