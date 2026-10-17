import { CML } from "@lucid-evolution/lucid";

export const tokenCreationBody = (
  output: CML.TransactionOutput,
  unit: string,
) => {
  const outputs = CML.TransactionOutputList.new();
  outputs.add(output);
  const body = CML.TransactionBody.new(
    CML.TransactionInputList.new(),
    outputs,
    0n,
  );
  const mint = CML.Mint.new();
  mint.set(
    CML.ScriptHash.from_hex(unit.slice(0, 56)),
    CML.AssetName.from_hex(unit.slice(56)),
    1n,
  );
  body.set_mint(mint);
  return body;
};
