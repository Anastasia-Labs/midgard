import { outRefLabel } from "@al-ft/midgard-core/out-ref";
import { Data, type UTxO } from "@lucid-evolution/lucid";

import type { EventHistoryRetirementWitness } from "../user-events/history.js";
import * as SDK from "./primitives.js";

export type AbsorbDepositLayout = {
  readonly depositInputIndex: bigint;
  readonly reserveOutputIndex: bigint;
  readonly hubRefInputIndex: bigint;
  readonly settlementRefInputIndex: bigint;
  readonly burnRedeemerIndex: bigint;
  readonly witness: EventHistoryRetirementWitness;
  readonly retirementWithdrawalRedeemerIndex: bigint;
  readonly listWithdrawalRedeemerIndex: bigint;
};

export type InitializePayoutLayout = {
  readonly withdrawalInputIndex: bigint;
  readonly payoutOutputIndex: bigint;
  readonly hubRefInputIndex: bigint;
  readonly settlementRefInputIndex: bigint;
  readonly withdrawalBurnRedeemerIndex: bigint;
  readonly payoutMintRedeemerIndex: bigint;
  readonly witness: EventHistoryRetirementWitness;
  readonly retirementWithdrawalRedeemerIndex: bigint;
  readonly listWithdrawalRedeemerIndex: bigint;
};

export type AddReserveFundsLayout = {
  readonly payoutInputIndex: bigint;
  readonly reserveInputIndex: bigint;
  readonly payoutOutputIndex: bigint;
  readonly reserveChangeOutputIndex: bigint | null;
  readonly payoutSpendRedeemerIndex: bigint;
  readonly reserveSpendRedeemerIndex: bigint;
  readonly hubRefInputIndex: bigint;
};

export type ConcludePayoutLayout = {
  readonly payoutInputIndex: bigint;
  readonly l1OutputIndex: bigint;
  readonly payoutSpendRedeemerIndex: bigint;
  readonly burnRedeemerIndex: bigint;
  readonly hubRefInputIndex: bigint;
};

export type RefundWithdrawalLayout = {
  readonly withdrawalInputIndex: bigint;
  readonly refundOutputIndex: bigint;
  readonly hubRefInputIndex: bigint;
  readonly settlementRefInputIndex: bigint;
  readonly burnRedeemerIndex: bigint;
  readonly witness: EventHistoryRetirementWitness;
  readonly retirementWithdrawalRedeemerIndex: bigint;
  readonly listWithdrawalRedeemerIndex: bigint;
};

export const settlementDatumFromInput = (
  settlementRefInput: UTxO,
): SDK.SettlementDatum => {
  if (settlementRefInput.datum == null) {
    throw new Error(
      `Settlement reference input ${outRefLabel(settlementRefInput)} has no inline datum`,
    );
  }
  return Data.from(
    settlementRefInput.datum,
    SDK.SettlementDatum,
  ) as SDK.SettlementDatum;
};
