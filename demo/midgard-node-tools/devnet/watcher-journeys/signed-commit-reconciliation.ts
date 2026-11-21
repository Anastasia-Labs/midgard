import { CML } from "@lucid-evolution/lucid";

import {
  type SignedTransactionRecovery,
  SignedTransactionRecoveryUnavailableError,
} from "./signed-transaction-recovery.js";

/** Exact signed header commitment bytes persisted before submission. */
export type SignedCommitAttempt = { txHash: string; signedCbor: string };

export type SignedCommitDisposition =
  | { kind: "included"; txHash: string }
  | { kind: "retired"; reason: string };

export type SignedCommitReconciliationPorts = {
  attempt: SignedCommitAttempt;
  /** The local node's classification of the exact recorded bytes. */
  readRecovery(
    attempt: SignedCommitAttempt,
  ): Promise<SignedTransactionRecovery>;
  /** Delay between receipt observations without requiring a new block. */
  pollDelay(): Promise<void>;
  /** Broadcast the exact recorded bytes; resolves to the accepted hash. */
  resubmit(signedCbor: string): Promise<string>;
  onStage?(name: string): void;
};

/** Only proven expiry or invalidation permits replacement of signed bytes. */
export const reconcileSignedCommit = async ({
  attempt,
  readRecovery,
  pollDelay,
  resubmit,
  onStage = () => {},
}: SignedCommitReconciliationPorts): Promise<SignedCommitDisposition> => {
  const transaction = CML.Transaction.from_cbor_hex(attempt.signedCbor);
  try {
    if (
      !transaction.is_valid() ||
      CML.hash_transaction(transaction.body()).to_hex() !== attempt.txHash ||
      transaction.to_cbor_hex() !== attempt.signedCbor
    )
      throw new Error(
        "Recorded header transaction bytes changed their identity",
      );
  } finally {
    transaction.free();
  }
  let rebroadcastAttempted = false;
  for (;;) {
    let observed: SignedTransactionRecovery;
    try {
      observed = await readRecovery(attempt);
    } catch (cause) {
      // An unanswered read settles neither inclusion nor retirement. Keep the
      // attempt and read again; identity errors still escape instead of
      // becoming permission to rebuild.
      if (!(cause instanceof SignedTransactionRecoveryUnavailableError))
        throw cause;
      onStage(`header recovery ${attempt.txHash} awaits the local node`);
      await pollDelay();
      continue;
    }
    if (
      observed.transactionHash !== attempt.txHash ||
      observed.signedTransactionCborHex !== attempt.signedCbor
    )
      throw new Error(
        "Header recovery changed the recorded transaction identity",
      );
    switch (observed.status) {
      case "included":
        return { kind: "included", txHash: attempt.txHash };
      // Whichever lands wins, and a replacement spends the same state-queue
      // input, so it cannot double-commit.
      case "expired":
      case "invalidated":
        return { kind: "retired", reason: observed.reason };
      case "rebroadcast":
        if (!rebroadcastAttempted) {
          // A failed RPC is ambiguous too: preserve the bytes and re-observe.
          rebroadcastAttempted = true;
          let submitted: string | undefined;
          try {
            submitted = await resubmit(attempt.signedCbor);
          } catch (cause) {
            onStage(
              `header rebroadcast ${attempt.txHash} unresolved: ${String(cause)}`,
            );
          }
          if (submitted !== undefined && submitted !== attempt.txHash)
            throw new Error(
              "Resubmitted header differs from its recorded hash",
            );
        }
        break;
      case "pending":
        break;
    }
    await pollDelay();
  }
};
