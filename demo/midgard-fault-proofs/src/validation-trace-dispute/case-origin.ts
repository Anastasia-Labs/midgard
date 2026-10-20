/** Where a case and its bond were created: the VerifySource transaction the
 * case token's authenticated L1 history holds (the same history thread
 * discovery reads), whoever ran it. The case policy mints the case token
 * once, in that transaction, beside the bond and its marker, so the case and
 * bond a close spends are read from that transaction's outputs by those
 * tokens, never from what an address holds. */
import * as SDK from "@al-ft/midgard-sdk";
import { CML, coreToTxOutput } from "@lucid-evolution/lucid";

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
