import * as SDK from "@al-ft/midgard-sdk";
import { CML, Data } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import {
  findRedeemerDataCbor,
  getRedeemerPointersInContextOrder,
} from "./helpers/redeemer-inspection.js";
import {
  decodeRedeemer,
  mintPointer,
} from "./reserve-payout-builders.submit-with-wallet.js";

type RetirementLayout = {
  readonly witness: SDK.EventHistoryRetirementWitness;
  readonly hubRefInputIndex: bigint;
  readonly retirementWithdrawalRedeemerIndex: bigint;
  readonly listWithdrawalRedeemerIndex: bigint;
};

export const expectRetirementLayout = (
  built: SDK.BuiltReservePayoutTx<RetirementLayout>,
) => {
  const tx = built.tx.toTransaction();
  const pointers = getRedeemerPointersInContextOrder(tx);
  const retirement = decodeRedeemer<SDK.EventHistoryRetirementArgs>(
    tx,
    pointers[Number(built.layout.retirementWithdrawalRedeemerIndex)]!,
    SDK.EventHistoryRetirementArgs,
  );
  const observe = decodeRedeemer<SDK.EventHistoryObserve>(
    tx,
    pointers[Number(built.layout.listWithdrawalRedeemerIndex)]!,
    SDK.EventHistoryObserve,
  );
  expect(retirement).toEqual({
    hub_reference_index: built.layout.hubRefInputIndex,
    witness: built.layout.witness,
  });
  expect(observe).toEqual({
    Apply: {
      hub_reference_index: built.layout.hubRefInputIndex,
      operation: SDK.eventHistoryRetirementOperation(built.layout.witness),
    },
  });
  const spendCbor = findRedeemerDataCbor(tx, {
    tag: CML.RedeemerTag.Spend,
    index: built.layout.witness.order_input_index,
  });
  expect(Data.from(spendCbor!)).toBe(built.layout.witness.order_input_index);
  expect(tx.body().certs()?.len() ?? 0).toBe(0);
  const claims = [
    built.layout.witness.predecessor_output_index,
    built.layout.witness.funds_output_index,
    built.layout.witness.structural_refund_output_index,
  ].filter((index) => index !== null);
  expect(new Set(claims).size).toBe(claims.length);
};

export const expectAbsorbRedeemerLayout = (
  built: SDK.BuiltReservePayoutTx<
    RetirementLayout & { depositInputIndex: bigint; reserveOutputIndex: bigint }
  >,
) => {
  expectRetirementLayout(built);
  expect(built.layout.witness.order_input_index).toBe(
    built.layout.depositInputIndex,
  );
  expect(built.layout.witness.funds_output_index).toBe(
    built.layout.reserveOutputIndex,
  );
  expect(built.layout.witness.purpose).toBe("AbsorbDeposit");
};

export const expectInitializeRedeemerLayout = (
  built: SDK.BuiltReservePayoutTx<
    RetirementLayout & {
      withdrawalInputIndex: bigint;
      payoutOutputIndex: bigint;
    }
  >,
  contracts: SDK.MidgardValidators,
) => {
  expectRetirementLayout(built);
  expect(built.layout.witness.order_input_index).toBe(
    built.layout.withdrawalInputIndex,
  );
  expect(built.layout.witness.funds_output_index).toBe(
    built.layout.payoutOutputIndex,
  );
  expect(built.layout.witness.purpose).toBe("InitializeWithdrawalPayout");
  const payoutMint = decodeRedeemer<SDK.PayoutMintRedeemer>(
    built.tx.toTransaction(),
    mintPointer(
      [contracts.withdrawal.policyId, contracts.payout.policyId],
      contracts.payout.policyId,
    ),
    SDK.PayoutMintRedeemer,
  );
  if (!("MintPayout" in payoutMint)) throw new Error("Expected payout mint");
  expect(payoutMint.MintPayout.withdrawal_input_index).toBe(
    built.layout.withdrawalInputIndex,
  );
  expect(payoutMint.MintPayout.retirement_withdraw_redeemer_index).toBe(
    built.layout.retirementWithdrawalRedeemerIndex,
  );
  expect(payoutMint.MintPayout.hub_ref_input_index).toBe(
    built.layout.hubRefInputIndex,
  );
};

export const expectAddFundsRedeemerLayout = (
  built: SDK.BuiltReservePayoutTx<{
    readonly payoutInputIndex: bigint;
    readonly reserveInputIndex: bigint;
    readonly payoutOutputIndex: bigint;
    readonly reserveChangeOutputIndex: bigint | null;
    readonly payoutSpendRedeemerIndex: bigint;
    readonly reserveSpendRedeemerIndex: bigint;
    readonly hubRefInputIndex: bigint;
  }>,
): void => {
  const tx = built.tx.toTransaction();
  const payoutSpend = decodeRedeemer<SDK.PayoutSpendRedeemer>(
    tx,
    { tag: CML.RedeemerTag.Spend, index: built.layout.payoutInputIndex },
    SDK.PayoutSpendRedeemer,
  );
  if (!("AddFunds" in payoutSpend)) {
    throw new Error("Expected AddFunds payout redeemer");
  }
  expect(payoutSpend.AddFunds.payout_input_index).toBe(
    built.layout.payoutInputIndex,
  );
  expect(payoutSpend.AddFunds.payout_output_index).toBe(
    built.layout.payoutOutputIndex,
  );
  expect(payoutSpend.AddFunds.reserve_input_index).toBe(
    built.layout.reserveInputIndex,
  );
  expect(payoutSpend.AddFunds.reserve_change_output_index).toBe(
    built.layout.reserveChangeOutputIndex,
  );
  expect(payoutSpend.AddFunds.reserve_spend_redeemer_index).toBe(
    built.layout.reserveSpendRedeemerIndex,
  );
  expect(payoutSpend.AddFunds.payout_spend_redeemer_index).toBe(
    built.layout.payoutSpendRedeemerIndex,
  );
  expect(payoutSpend.AddFunds.hub_ref_input_index).toBe(
    built.layout.hubRefInputIndex,
  );

  const reserveSpend = decodeRedeemer<any>(
    tx,
    { tag: CML.RedeemerTag.Spend, index: built.layout.reserveInputIndex },
    SDK.ReserveSpendRedeemer,
  );
  const reserveSpendBody = reserveSpend.Spend ?? reserveSpend;
  expect(reserveSpendBody.reserve_input_index).toBe(
    built.layout.reserveInputIndex,
  );
  expect(reserveSpendBody.payout_input_index).toBe(
    built.layout.payoutInputIndex,
  );
  expect(reserveSpendBody.payout_spend_redeemer_index).toBe(
    built.layout.payoutSpendRedeemerIndex,
  );
  expect(reserveSpendBody.hub_ref_input_index).toBe(
    built.layout.hubRefInputIndex,
  );
};

export const expectConcludeRedeemerLayout = (
  built: SDK.BuiltReservePayoutTx<{
    readonly payoutInputIndex: bigint;
    readonly l1OutputIndex: bigint;
    readonly payoutSpendRedeemerIndex: bigint;
    readonly burnRedeemerIndex: bigint;
    readonly hubRefInputIndex: bigint;
  }>,
): void => {
  const tx = built.tx.toTransaction();
  const payoutSpend = decodeRedeemer<SDK.PayoutSpendRedeemer>(
    tx,
    { tag: CML.RedeemerTag.Spend, index: built.layout.payoutInputIndex },
    SDK.PayoutSpendRedeemer,
  );
  if (!("ConcludeWithdrawal" in payoutSpend)) {
    throw new Error("Expected ConcludeWithdrawal payout redeemer");
  }
  expect(payoutSpend.ConcludeWithdrawal.payout_input_index).toBe(
    built.layout.payoutInputIndex,
  );
  expect(payoutSpend.ConcludeWithdrawal.l1_output_index).toBe(
    built.layout.l1OutputIndex,
  );
  expect(payoutSpend.ConcludeWithdrawal.burn_redeemer_index).toBe(
    built.layout.burnRedeemerIndex,
  );
  expect(payoutSpend.ConcludeWithdrawal.hub_ref_input_index).toBe(
    built.layout.hubRefInputIndex,
  );
};

export const expectRefundRedeemerLayout = (
  built: SDK.BuiltReservePayoutTx<
    RetirementLayout & {
      withdrawalInputIndex: bigint;
      refundOutputIndex: bigint;
    }
  >,
  validityOverride: SDK.WithdrawalValidity,
) => {
  expectRetirementLayout(built);
  expect(built.layout.witness.order_input_index).toBe(
    built.layout.withdrawalInputIndex,
  );
  expect(built.layout.witness.funds_output_index).toBe(
    built.layout.refundOutputIndex,
  );
  expect(built.layout.witness.purpose).toEqual({
    RefundInvalidWithdrawal: { validity: validityOverride },
  });
};
