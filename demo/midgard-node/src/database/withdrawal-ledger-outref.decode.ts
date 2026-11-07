import { outRefToCbor } from "@al-ft/lucid-midgard";
import * as SDK from "@al-ft/midgard-sdk";
import { Data as LucidData } from "@lucid-evolution/lucid";

/** Existing SDK.OutputReference to canonical field-0/1 ledger-key conversion. */
export const decodeWithdrawalLedgerOutRef = (raw: Buffer): Buffer => {
  const outRef = LucidData.from(raw.toString("hex"), SDK.OutputReference);
  return outRefToCbor({
    txHash: outRef.transactionId,
    outputIndex: Number(outRef.outputIndex),
  });
};

export const withdrawalLedgerOutRefColumns = (raw: Buffer) => {
  try {
    return {
      l2_ledger_outref: decodeWithdrawalLedgerOutRef(raw),
      l2_ledger_outref_state: "decoded",
    };
  } catch {
    return { l2_ledger_outref: null, l2_ledger_outref_state: "undecodable" };
  }
};
