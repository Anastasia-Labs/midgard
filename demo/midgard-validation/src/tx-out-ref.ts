import { CML, Constr } from "@lucid-evolution/lucid";

export type DecodedTxOutRef = {
  readonly txHashHex: string;
  readonly outputIndex: bigint;
};

export const decodeTxOutRefHex = (outRefHex: string): DecodedTxOutRef => {
  const input = CML.TransactionInput.from_cbor_bytes(
    Buffer.from(outRefHex, "hex"),
  );
  return {
    txHashHex: input.transaction_id().to_hex(),
    outputIndex: BigInt(input.index()),
  };
};

export const compareTxOutRefHex = (left: string, right: string): number => {
  const leftRef = decodeTxOutRefHex(left);
  const rightRef = decodeTxOutRefHex(right);
  if (leftRef.txHashHex !== rightRef.txHashHex) {
    return leftRef.txHashHex < rightRef.txHashHex ? -1 : 1;
  }
  return leftRef.outputIndex < rightRef.outputIndex
    ? -1
    : leftRef.outputIndex > rightRef.outputIndex
      ? 1
      : 0;
};

export const txOutRefData = (outRefHex: string): Constr<unknown> => {
  const ref = decodeTxOutRefHex(outRefHex);
  return new Constr(0, [ref.txHashHex, ref.outputIndex]);
};
