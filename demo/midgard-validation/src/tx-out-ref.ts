import { CML, Constr } from "@lucid-evolution/lucid";

export const sortTxOutRefHexes = (outRefHexes: Iterable<string>): string[] =>
  [...outRefHexes].sort();

export const txOutRefData = (outRefHex: string): Constr<unknown> => {
  const input = CML.TransactionInput.from_cbor_bytes(
    Buffer.from(outRefHex, "hex"),
  );
  return new Constr(0, [
    input.transaction_id().to_hex(),
    BigInt(input.index()),
  ]);
};
