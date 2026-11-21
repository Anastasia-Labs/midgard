import { CML, walletFromSeed } from "@lucid-evolution/lucid";
import { entropyToMnemonic } from "bip39";

import type {
  PayoutConclusion,
  PayoutTokenOutput,
  SettlementAttempt,
} from "../src/full-stack/payout-body.js";

export const recipient = walletFromSeed(entropyToMnemonic("11".repeat(16)), {
  network: "Preprod",
}).address;
export const stranger = walletFromSeed(entropyToMnemonic("22".repeat(16)), {
  network: "Preprod",
}).address;
export const PAYOUT = 10_000_000n;
export const payoutAssets = { lovelace: String(PAYOUT) };

/** A Cardano transaction body with these outputs, as L1 carries it. */
export function payoutBody(
  outputs: readonly { address: string; lovelace: bigint }[],
): PayoutConclusion {
  const list = CML.TransactionOutputList.new();
  for (const { address, lovelace } of outputs)
    list.add(
      CML.TransactionOutput.new(
        CML.Address.from_bech32(address),
        CML.Value.from_coin(lovelace),
      ),
    );
  const body = CML.TransactionBody.new(
    CML.TransactionInputList.new(),
    list,
    1n,
  );
  return {
    txHash: CML.hash_transaction(body).to_hex(),
    bodyCbor: body.to_cbor_hex(),
  };
}
/** The exact payout to the recipient, with change to another address. */
export const exactPayout = payoutBody([
  { address: recipient, lovelace: PAYOUT },
  { address: stranger, lovelace: 3_000_000n },
]);

export const INITIALIZE_TX = "65".repeat(32);
export const FUND_TX = "1f".repeat(32);
/**
 * The payout token's outputs as a follower holds them: the initialized
 * payout output, spent by the funding, and the funded one, spent by
 * `conclusion`, or unspent while no conclusion landed.
 */
export const payoutToken = (
  conclusion: PayoutConclusion | null,
): PayoutTokenOutput[] => [
  { txHash: INITIALIZE_TX, spentTx: FUND_TX, spenderBody: "a0" },
  {
    txHash: FUND_TX,
    spentTx: conclusion?.txHash ?? null,
    spenderBody: conclusion?.bodyCbor ?? null,
  },
];
/**
 * The local journal of an operator that lost the initialize race and sent
 * the funding, while another operator sent the conclusion.
 */
export const fundedOnly: SettlementAttempt[] = [
  {
    phase: "initialize",
    status: "expired",
    txHash: "47".repeat(32),
    signedCbor: "",
  },
  { phase: "fund", status: "pending", txHash: FUND_TX, signedCbor: "" },
];
