import { decodeLedgerUtxos } from "@al-ft/midgard-l1-follower";
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
/**
 * The single conclusion of a complete job; undefined while the job is still
 * settling. The node completes a job once its concluding attempt landed cd
 * deep (its outcome is the intent journal's, derived, and only `final` past
 * k is stored), and journals no other attempt for it unless one is proven
 * expired; so the conclusion is its one attempt that is not expired.
 */
export function payoutConclusion(status: SettlementObservation) {
  const conclusions =
    status.attempts?.filter(
      (attempt) => attempt.phase === "conclude" && attempt.status !== "expired",
    ) ?? [];
  if (conclusions.length > 1)
    throw new Error(
      "More than one unexpired payout transaction for the same withdrawal",
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
/**
 * The payout output once the local node's ledger holds it; undefined while it
 * does not. `answer` is the ledger's `utxo_by_txin` answer for exactly this
 * output reference. Its presence proves inclusion; its content is the
 * hash-bound signed body, so the answer is checked only for the reference.
 */
export function includedPayout(
  outRef: { txHash: string; outputIndex: number },
  answer: Uint8Array,
) {
  const rows = decodeLedgerUtxos(answer);
  if (rows.length === 0) return undefined;
  if (
    rows.length !== 1 ||
    rows[0]!.outRef.txHash.toString("hex") !== outRef.txHash ||
    rows[0]!.outRef.index !== outRef.outputIndex
  )
    throw new Error("The ledger answered another output than the payout");
  return { outputIndex: outRef.outputIndex };
}
