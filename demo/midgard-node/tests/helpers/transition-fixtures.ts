import * as SDK from "@al-ft/midgard-sdk";

export const h32 = (byte: number): string =>
  byte.toString(16).padStart(2, "0").repeat(32);

export const h28 = (byte: number): string =>
  byte.toString(16).padStart(2, "0").repeat(28);

export const outputReference = (byte: number): SDK.OutputReference => ({
  transactionId: h32(byte),
  outputIndex: 0n,
});

export const withdrawalEventKey = (byte: number): SDK.EventKey => ({
  WithdrawalEventKey: { withdrawal_id: outputReference(byte) },
});

export const forcedTransactionEventKey = (byte: number): SDK.EventKey => ({
  ForcedTransactionEventKey: { tx_order_id: outputReference(byte) },
});

export const l2TransactionEventKey = (byte: number): SDK.EventKey => ({
  L2TransactionEventKey: { tx_id: h32(byte) },
});

export const depositEventKey = (byte: number): SDK.EventKey => ({
  DepositEventKey: { deposit_id: outputReference(byte) },
});
