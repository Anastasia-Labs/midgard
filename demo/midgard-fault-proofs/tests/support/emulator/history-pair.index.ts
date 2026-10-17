import { type UTxO } from "@lucid-evolution/lucid";

export const same = (a: UTxO, b: UTxO) =>
  a.txHash === b.txHash && a.outputIndex === b.outputIndex;

export const index = (inputs: readonly UTxO[], target: UTxO) => {
  const sorted = [...inputs].sort(
    (a, b) => a.txHash.localeCompare(b.txHash) || a.outputIndex - b.outputIndex,
  );
  const result = sorted.findIndex((u) => same(u, target));
  if (result < 0) throw new Error("Missing transaction input");
  return BigInt(result);
};
