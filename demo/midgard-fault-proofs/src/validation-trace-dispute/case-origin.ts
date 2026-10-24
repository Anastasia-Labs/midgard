/** Where a case and its bond were created: the VerifySource transaction the
 * case token's authenticated L1 history holds (the same history thread
 * discovery reads), whoever ran it. The case policy mints the case token
 * once, in that transaction, beside the bond and its marker, so the case and
 * bond a close spends are read from that transaction's outputs by those
 * tokens, never from what an address holds. */
import {
  MIDGARD_CONSENSUS_LIMITS,
  MIDGARD_VALIDATION_CASE_OPEN_WINDOW_MS,
  MIDGARD_VALIDATION_DISPUTE_MAX_DURATION_MS,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { CML, coreToTxOutput, Data } from "@lucid-evolution/lucid";

import { WorkflowActionChangedError } from "../workflow/action-changed.js";
import { addMintedAssets } from "../workflow/funding-reservation-permit.ledger-accounting.js";
import type { ValidationTraceCaseTransaction } from "./case-history.js";

/**
 * The case and bond out-refs of the one transaction of `transactions` (the
 * history of `caseUnit`) that minted the case token: the output carrying
 * the token, and the output carrying the case's bond marker.
 */
export const validationTraceCaseOrigin = ({
  transactions,
  caseUnit,
}: {
  readonly transactions: readonly ValidationTraceCaseTransaction[];
  readonly caseUnit: string;
}): Readonly<{ caseOutRef: string; bondOutRef: string }> => {
  const marker =
    caseUnit.slice(0, 56) +
    SDK.validationCaseBondMarkerName(caseUnit.slice(56));
  const minting = transactions.filter(({ bodyCbor }) => {
    const minted = new Map<string, bigint>();
    addMintedAssets(minted, CML.TransactionBody.from_cbor_hex(bodyCbor).mint());
    return minted.get(caseUnit) === 1n;
  });
  if (minting.length !== 1)
    throw new WorkflowActionChangedError(
      `validation-trace case ${caseUnit} has no single VerifySource in its L1 history`,
    );
  const [origin] = minting;
  const outputs = CML.TransactionBody.from_cbor_hex(origin!.bodyCbor).outputs();
  const carrying = (unit: string): string => {
    for (let index = 0; index < outputs.len(); index += 1)
      if (coreToTxOutput(outputs.get(index)).assets[unit] === 1n)
        return `${origin!.txHash}#${index.toString()}`;
    throw new WorkflowActionChangedError(
      `validation-trace case ${caseUnit}'s VerifySource carries no ${unit}`,
    );
  };
  return Object.freeze({
    caseOutRef: carrying(caseUnit),
    bondOutRef: carrying(marker),
  });
};

/** P: what is left of maturity after the open window and the longest case. */
const POST_CASE_ALLOWANCE_MS =
  MIDGARD_CONSENSUS_LIMITS.blockMaturityMs -
  MIDGARD_VALIDATION_CASE_OPEN_WINDOW_MS -
  MIDGARD_VALIDATION_DISPUTE_MAX_DURATION_MS;

/** When the bond a defense of a case started at `startedAt` holds unlocks,
 * at the earliest: `started_at + D_case + P` (validation-case-v1
 * `defense_bond_unlock_time`). The operator's bond on our case unlocks then
 * too. */
export const validationTraceCaseBondUnlockMs = (startedAt: bigint): number =>
  Number(startedAt) +
  MIDGARD_VALIDATION_DISPUTE_MAX_DURATION_MS +
  POST_CASE_ALLOWANCE_MS;

/** The `started_at` of the case `caseUnit`, by the case output of the one
 * transaction of `transactions` (its history) that minted the case token. */
export const validationTraceCaseStartedAt = (input: {
  readonly transactions: readonly ValidationTraceCaseTransaction[];
  readonly caseUnit: string;
}): bigint => {
  const outRef = validationTraceCaseOrigin(input).caseOutRef;
  const [txHash, index] = outRef.split("#");
  const origin = input.transactions.find(
    (transaction) => transaction.txHash === txHash,
  )!;
  const output = coreToTxOutput(
    CML.TransactionBody.from_cbor_hex(origin.bodyCbor)
      .outputs()
      .get(Number(index)),
  );
  if (typeof output.datum !== "string")
    throw new Error(`validation-trace case ${input.caseUnit} has no datum`);
  return Data.from(output.datum, SDK.Case).started_at;
};
