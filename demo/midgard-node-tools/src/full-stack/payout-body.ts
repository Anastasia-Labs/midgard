import { CML, valueToAssets } from "@lucid-evolution/lucid";

export function equalAssets(
  left: Record<string, string | bigint>,
  right: Record<string, string | bigint>,
) {
  const normalize = (value: Record<string, string | bigint>) =>
    Object.entries(value)
      .filter(([, amount]) => BigInt(amount) !== 0n)
      .sort(([a], [b]) => (a < b ? -1 : a > b ? 1 : 0))
      .map(([unit, amount]) => `${unit}:${BigInt(amount)}`)
      .join("|");
  return normalize(left) === normalize(right);
}
export function verifyPayoutBody(
  cbor: string,
  address: string,
  assets: Record<string, string | bigint>,
) {
  const transaction = CML.Transaction.from_cbor_hex(cbor);
  const outputs = transaction.body().outputs();
  const matches: number[] = [];
  for (let index = 0; index < outputs.len(); index++) {
    const output = outputs.get(index);
    if (
      output.address().to_bech32() === address &&
      equalAssets(valueToAssets(output.amount()), assets)
    )
      matches.push(index);
  }
  if (matches.length !== 1)
    throw new Error(
      "Payout must contain exactly one output with the exact destination and value",
    );
  return { outputIndex: matches[0]! };
}
