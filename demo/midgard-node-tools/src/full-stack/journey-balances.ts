/** Exact L2 postconditions of the wallet journey. A mismatch fails; it is never retried. */
export function assertDepositCredit(
  before: bigint,
  after: bigint,
  deposit: bigint,
) {
  if (after !== before + deposit)
    throw new Error("Deposit did not credit the exact L2 balance");
}

export function assertTransferDeltas(transfer: {
  amount: bigint;
  fee: bigint;
  senderBefore: bigint;
  senderAfter: bigint;
  recipientBefore: bigint;
  recipientAfter: bigint;
  /** Lovelace of each recipient output the transfer transaction created. */
  received: readonly bigint[];
}) {
  if (
    transfer.senderAfter !==
    transfer.senderBefore - transfer.amount - transfer.fee
  )
    throw new Error("L2 transfer balance or fee does not match");
  if (transfer.recipientAfter !== transfer.recipientBefore + transfer.amount)
    throw new Error("Recipient L2 balance does not match the transfer");
  if (
    transfer.received.length !== 1 ||
    transfer.received[0] !== transfer.amount
  )
    throw new Error("Recipient did not receive the exact transfer");
}

export function assertWithdrawalDebit(
  before: bigint,
  after: bigint,
  withdrawn: bigint,
) {
  if (after !== before - withdrawn)
    throw new Error("Withdrawal did not debit the exact recipient L2 balance");
}

/** Fee and change headroom the user's L1 wallet must hold above each deposit. */
export const DEPOSIT_FEE_HEADROOM_LOVELACE = 5_000_000n;
export function assertDepositFunding(available: bigint, deposit: bigint) {
  if (available < deposit + DEPOSIT_FEE_HEADROOM_LOVELACE)
    throw new Error(
      `User wallet holds ${available} lovelace; the deposit needs ${deposit + DEPOSIT_FEE_HEADROOM_LOVELACE} including fee headroom`,
    );
}
