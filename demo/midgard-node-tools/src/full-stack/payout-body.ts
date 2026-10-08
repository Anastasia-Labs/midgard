import { CML, valueToAssets } from "@lucid-evolution/lucid";
import JSONBig from "json-bigint";
import { decodeLedgerSnapshotOutput } from "midgard-node/l1-ledger-snapshot";

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

export type SettlementAttempt = {
  phase: string;
  status: string;
  txHash: string;
  signedCbor: string;
};
export type SettlementObservation = {
  jobs: { phase: string }[] | null;
  attempts: SettlementAttempt[] | null;
};
/** The single confirmed conclusion of a complete job; undefined while the job is still settling. */
export function payoutConclusion(status: SettlementObservation) {
  const conclusions =
    status.attempts?.filter(
      (attempt) =>
        attempt.phase === "conclude" && attempt.status === "confirmed",
    ) ?? [];
  if (conclusions.length > 1)
    throw new Error(
      "More than one confirmed payout transaction for the same withdrawal",
    );
  if (
    status.jobs?.length !== 1 ||
    status.jobs[0]!.phase !== "complete" ||
    conclusions.length !== 1
  )
    return undefined;
  return conclusions[0]!;
}
/** The payout output the signed conclusion creates; the body must hash to the recorded transaction id. */
export function payoutOutRef(
  attempt: SettlementAttempt,
  address: string,
  assets: Record<string, string | bigint>,
) {
  const body = CML.Transaction.from_cbor_hex(attempt.signedCbor).body();
  if (CML.hash_transaction(body).to_hex() !== attempt.txHash)
    throw new Error("Payout transaction does not hash to its recorded id");
  return {
    txHash: attempt.txHash,
    ...verifyPayoutBody(attempt.signedCbor, address, assets),
  };
}
const json = JSONBig({ useNativeBigInt: true, strict: true });
/**
 * The payout output once the local node's ledger holds it; undefined while it
 * does not. `frame` is the Ogmios `queryLedgerState/utxo` answer for exactly
 * this output reference. Its presence proves inclusion; its content is the
 * hash-bound signed body, so the frame is checked only for the reference.
 */
export function includedPayout(
  outRef: { txHash: string; outputIndex: number },
  address: string,
  frame: string,
) {
  const response = json.parse(frame) as { result?: unknown };
  if (!Array.isArray(response?.result))
    throw new Error("Ogmios payout query has no UTxO result");
  if (response.result.length === 0) return undefined;
  const rows = response.result.map((value) =>
    decodeLedgerSnapshotOutput(value, new Set([address])),
  );
  if (
    rows.length !== 1 ||
    rows[0]!.txHash !== outRef.txHash ||
    rows[0]!.outputIndex !== outRef.outputIndex
  )
    throw new Error("Ogmios answered another output than the payout");
  return { outputIndex: outRef.outputIndex };
}
