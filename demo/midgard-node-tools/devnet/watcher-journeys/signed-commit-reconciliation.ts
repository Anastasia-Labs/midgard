import {
  LocalKupmiosCheckpointChangedError,
  type readAdmittedLocalKupmiosSignedTransactionRecovery,
} from "@al-ft/midgard-fault-proofs";
import { CML } from "@lucid-evolution/lucid";

type SignedTransactionRecoveryObservation = Awaited<
  ReturnType<typeof readAdmittedLocalKupmiosSignedTransactionRecovery>
>;

/** Exact signed header commitment bytes persisted before submission. */
export type SignedCommitAttempt = { txHash: string; signedCbor: string };

export type SignedCommitDisposition =
  | { kind: "included"; txHash: string }
  | { kind: "retired"; reason: string };

export type SignedCommitReconciliationPorts = {
  attempt: SignedCommitAttempt;
  /** Concrete local recovery binds exact bytes to canonical/release-final evidence. */
  readRecovery(
    attempt: SignedCommitAttempt,
  ): Promise<SignedTransactionRecoveryObservation>;
  /** Delay between receipt observations without requiring a new block. */
  pollDelay(): Promise<void>;
  /** Broadcast the exact recorded bytes; resolves to the accepted hash. */
  resubmit(signedCbor: string): Promise<string>;
  onStage?(name: string): void;
};

/** Only authenticated expiry or invalidation permits replacement of signed bytes. */
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
    let observed: SignedTransactionRecoveryObservation;
    try {
      observed = await readRecovery(attempt);
    } catch (cause) {
      // A moving capture boundary settles neither inclusion nor retirement.
      // Keep the attempt and capture again; identity/authentication errors
      // still escape instead of becoming permission to rebuild.
      if (!(cause instanceof LocalKupmiosCheckpointChangedError)) throw cause;
      onStage(`header recovery ${attempt.txHash} awaits a stable checkpoint`);
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
      case "expired":
      case "invalidated":
        return { kind: "retired", reason: observed.reason };
      case "conflict":
        throw new Error(
          `Header transaction recovery conflict: ${observed.reason}`,
        );
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
      case "unknown":
        break;
    }
    await pollDelay();
  }
};
