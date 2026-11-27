import type { MidgardAddressWitness } from "@al-ft/midgard-core";
import {
  missingSignatureFieldWalkCheckpoint,
  missingSignatureVkeyHash,
  verifyAddressWitness,
} from "@al-ft/midgard-sdk/fraud-proof";

/** Witnesses one scan transaction visits at most; mirrors the shared scan
 * validator's `witness_scan_batch_size`. */
export const SIGNER_WITNESS_SCAN_BATCH = 32;

/** The cumulative signer state once the scan has passed `cursor` witnesses. */
export type SignerWitnessScanCheckpoint = Readonly<{
  cursor: number;
  signerPresent: boolean;
}>;

export type SignerWitnessScan = Readonly<{
  signerPresent: boolean;
  checkpoints: readonly SignerWitnessScanCheckpoint[];
}>;

/**
 * The field-7 signer scan the spend-input and protected-output signer-missing
 * families share: the signer is present once a witness whose key hashes to
 * the payment credential also signs the transaction id. The scan checkpoints
 * its cumulative state at every batch boundary and at the end, or at the
 * initial position of an empty field: the cursors a scan turn ends at.
 */
export const scanSignerWitnesses = ({
  transactionId,
  witnesses,
  paymentCredentialHex,
}: {
  readonly transactionId: string;
  readonly witnesses: readonly MidgardAddressWitness[];
  readonly paymentCredentialHex: string;
}): SignerWitnessScan => {
  const checkpoints: SignerWitnessScanCheckpoint[] = [];
  let signerPresent = false;
  witnesses.forEach((witness, index) => {
    const verificationKey = Buffer.from(witness.verificationKey).toString(
      "hex",
    );
    if (
      !signerPresent &&
      missingSignatureVkeyHash(verificationKey) === paymentCredentialHex &&
      verifyAddressWitness({
        txId: transactionId,
        witness: {
          verification_key: verificationKey,
          signature: Buffer.from(witness.signature).toString("hex"),
        },
      })
    )
      signerPresent = true;
    if (
      (index + 1) % SIGNER_WITNESS_SCAN_BATCH === 0 ||
      index + 1 === witnesses.length
    )
      checkpoints.push(Object.freeze({ cursor: index + 1, signerPresent }));
  });
  if (witnesses.length === 0)
    checkpoints.push(Object.freeze({ cursor: 0, signerPresent: false }));
  return Object.freeze({
    signerPresent,
    checkpoints: Object.freeze(checkpoints),
  });
};

export type SignerWitnessScanTurn = Readonly<{
  /** `null` on the opening turn; the resumed checkpoint's bytes otherwise. */
  checkpointCbor: string | null;
  nextCursor: number;
  signerPresent: boolean;
  /** The turn writes the verdict: a signer was found or the field ended. */
  terminal: boolean;
  /** The checkpoint a continuing turn writes; absent on a terminal turn. */
  nextCheckpointHash: string | undefined;
}>;

/**
 * One scan turn: an empty `checkpointHash` is the opening turn at cursor 0;
 * any other resolves to the only batch-schedule cursor that commits to it.
 * The turn reads the scan's cumulative signer state where it ends, and is
 * terminal when the signer is present there or the field is exhausted.
 */
export const planSignerWitnessScanTurn = ({
  checkpoints,
  txId,
  itemCount,
  totalLength,
  checkpointHash,
}: {
  readonly checkpoints: readonly SignerWitnessScanCheckpoint[];
  readonly txId: string;
  readonly itemCount: number;
  readonly totalLength: number;
  readonly checkpointHash: string;
}): SignerWitnessScanTurn => {
  const at = (nextItemIndex: number) =>
    missingSignatureFieldWalkCheckpoint({
      txId,
      itemCount,
      totalLength,
      nextItemIndex,
    });
  let cursor: number | undefined;
  let checkpointCbor: string | null = null;
  if (checkpointHash === "") cursor = 0;
  for (
    let candidate = SIGNER_WITNESS_SCAN_BATCH;
    cursor === undefined && candidate < itemCount;
    candidate += SIGNER_WITNESS_SCAN_BATCH
  ) {
    const checkpoint = at(candidate);
    if (checkpoint.checkpointHash === checkpointHash) {
      cursor = candidate;
      checkpointCbor = checkpoint.checkpointCbor;
    }
  }
  if (cursor === undefined)
    throw new Error(
      `signer witness scan: checkpoint is not on the deterministic ${SIGNER_WITNESS_SCAN_BATCH.toString()}-item schedule`,
    );
  const nextCursor = Math.min(cursor + SIGNER_WITNESS_SCAN_BATCH, itemCount);
  const reached = checkpoints.find((entry) => entry.cursor === nextCursor);
  if (reached === undefined)
    throw new Error(
      `signer witness scan: scan has no checkpoint at cursor ${nextCursor.toString()}`,
    );
  const terminal = reached.signerPresent || nextCursor === itemCount;
  return Object.freeze({
    checkpointCbor,
    nextCursor,
    signerPresent: reached.signerPresent,
    terminal,
    nextCheckpointHash: terminal ? undefined : at(nextCursor).checkpointHash,
  });
};
