import { decodeLedgerUtxos } from "@al-ft/midgard-l1-follower";
import { eventKeyOfId } from "@al-ft/midgard-l1-follower/events";
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
function exactPayoutOutput(
  outputs: CML.TransactionOutputList,
  address: string,
  assets: Record<string, string | bigint>,
) {
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
/** One output that held a withdrawal's payout token, from the follower's L1 facts. */
export type PayoutTokenOutput = {
  /** The transaction that created it. */
  txHash: string;
  /** The transaction that spent it; null while it is unspent. */
  spentTx: string | null;
  /** The spending transaction's body as L1 carried it; null when not retained. */
  spenderBody: string | null;
};
export type SettlementObservation = {
  jobs: { phase: string }[] | null;
  attempts: SettlementAttempt[] | null;
  /** A withdrawal's payout token outputs; absent for a deposit. */
  payoutToken?: PayoutTokenOutput[] | null;
};
/** The transaction that concluded a payout on L1, whoever sent it. */
export type PayoutConclusion = { txHash: string; bodyCbor: string };

/**
 * The follower's rows for every output that held the withdrawal's payout
 * token (the payout policy and `eventKeyOfId(eventId)`, the unit the node's
 * `settledDepth` reads), each with the body of the transaction that spent
 * it. Every operator's follower holds these L1 facts, whoever settled. Both
 * inputs are checked hex, so they are safe in the query text.
 */
export function payoutTokenQuery(payoutPolicyId: string, eventId: string) {
  if (!/^[0-9a-f]{56}$/.test(payoutPolicyId))
    throw new Error("Noncanonical payout policy ID");
  if (!/^(?:[0-9a-f]{2})+$/.test(eventId))
    throw new Error("Noncanonical event ID");
  const assetName = eventKeyOfId(Buffer.from(eventId, "hex")).toString("hex");
  return `SELECT json_agg(json_build_object('txHash', encode(o.tx_hash, 'hex'), 'spentTx', encode(o.spent_tx, 'hex'), 'spenderBody', encode(t.body_cbor, 'hex'))) AS outputs FROM l1_outputs o JOIN l1_output_assets a ON a.tx_hash = o.tx_hash AND a.output_index = o.output_index LEFT JOIN l1_txs t ON t.tx_hash = o.spent_tx WHERE a.policy_id = decode('${payoutPolicyId}', 'hex') AND a.asset_name = decode('${assetName}', 'hex')`;
}

/**
 * The transaction that spent the last output holding the payout token and
 * created no other: the one that burned it. Undefined while an output still
 * holds the token, or while the follower has no such spend.
 */
function burnOnL1(outputs: readonly PayoutTokenOutput[]) {
  if (outputs.some((output) => output.spentTx === null)) return undefined;
  const created = new Set(outputs.map((output) => output.txHash));
  const burns = new Map<string, string | null>();
  for (const { spentTx, spenderBody } of outputs)
    if (!created.has(spentTx!)) burns.set(spentTx!, spenderBody);
  if (burns.size > 1)
    throw new Error("More than one transaction burned the same payout token");
  const [burn] = burns;
  return burn === undefined || burn[1] === null
    ? undefined
    : { txHash: burn[0], bodyCbor: burn[1] };
}
/**
 * The payout's conclusion, judged from L1; undefined while it is still
 * settling. Any operator may send any settlement phase, so the conclusion is
 * the transaction that burned the payout token, whoever sent it. The local
 * job's `complete` phase is only the depth signal: the node completes it once
 * a conclusion is cd deep, its own or another operator's. The local journal
 * holds at most one unexpired conclusion of its own; more is refused.
 */
export function payoutConclusion(
  status: SettlementObservation,
): PayoutConclusion | undefined {
  const local =
    status.attempts?.filter(
      (attempt) => attempt.phase === "conclude" && attempt.status !== "expired",
    ) ?? [];
  if (local.length > 1)
    throw new Error(
      "More than one unexpired payout transaction for the same withdrawal",
    );
  if (status.jobs?.length !== 1 || status.jobs[0]!.phase !== "complete")
    return undefined;
  return burnOnL1(status.payoutToken ?? []);
}
/** The payout output the conclusion creates; its body must hash to its transaction id. */
export function payoutOutRef(
  conclusion: PayoutConclusion,
  address: string,
  assets: Record<string, string | bigint>,
) {
  const body = CML.TransactionBody.from_cbor_hex(conclusion.bodyCbor);
  if (CML.hash_transaction(body).to_hex() !== conclusion.txHash)
    throw new Error("Payout transaction does not hash to its recorded id");
  return {
    txHash: conclusion.txHash,
    ...exactPayoutOutput(body.outputs(), address, assets),
  };
}
/**
 * The payout output once the local node's ledger holds it; undefined while it
 * does not. `answer` is the ledger's `utxo_by_txin` answer for exactly this
 * output reference. Its presence proves inclusion; its content is the
 * hash-bound body, so the answer is checked only for the reference.
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
