/**
 * The proof that a block's predecessor ledger does not hold an input the
 * block names, as the no-reference-input and non-existent-input submissions
 * carry it.
 *
 * The `utxos_root` is a raw PHAS/MPF root (unlike the counted
 * `transactions_root`), so the exclusion proof verifies directly against
 * `prev_utxos_root` on-chain via `plutarch_pexcludes_raw`.
 */
import { asLucidSchema } from "@al-ft/midgard-core/lucid-data";
import { Proof } from "@al-ft/midgard-sdk/core";
import { Data } from "@lucid-evolution/lucid";

import { readingLedger } from "./event-ledger.ledger.js";
import { requireLedgerOpening } from "./ledger-at.js";
import type { Ledger } from "./ledger-store/index.js";
import { transitionTraceError } from "./transition-trace/errors.js";
import { sdkProofFromCbor } from "./transition-trace/phas.js";

/**
 * The `Proof` Data CBOR hex of `key`'s absence from `ledger`, the ledger a
 * block with `prev_utxos_root` `utxosRoot` starts from (the genesis ledger
 * when omitted). Refuses a ledger opening another root and a key the ledger
 * holds. A ledger the store can no longer read is this process's own fault
 * (`readingLedger`).
 */
export const predecessorNonMembershipProof = async ({
  ledger,
  utxosRoot,
  key,
  label,
}: {
  readonly ledger: Ledger | undefined;
  readonly utxosRoot: string;
  readonly key: Buffer;
  readonly label: string;
}): Promise<string> => {
  const opened = requireLedgerOpening({ ledger, utxosRoot, label });
  const proof = await readingLedger(label, async () => {
    const read = (await opened.read([key])).get(key.toString("hex"))!;
    if (read.output !== undefined)
      throw transitionTraceError(
        "missingWitnessData",
        `Cannot build a PHAS non-membership proof for present key ${key.toString("hex")}.`,
      );
    return await read.proof();
  });
  return Data.to(sdkProofFromCbor(proof), asLucidSchema(Proof));
};
