import { decodeMidgardSpendInputItem } from "@al-ft/midgard-core/codec";
import { Constr } from "@lucid-evolution/lucid";

export const sortTxOutRefHexes = (outRefHexes: Iterable<string>): string[] =>
  [...outRefHexes].sort();

/**
 * The input hex is a ledger out-ref key, i.e. §5.3's fixed-index item, so it is
 * decoded by that twin rather than by CML: `19 0000` is deliberately non-minimal
 * and is not a shape a Cardano decoder should be asked to rule on.
 */
export const txOutRefData = (outRefHex: string): Constr<unknown> => {
  const decoded = decodeMidgardSpendInputItem(Buffer.from(outRefHex, "hex"));
  return new Constr(0, [
    Buffer.from(decoded.txId).toString("hex"),
    BigInt(decoded.outputIndex),
  ]);
};
