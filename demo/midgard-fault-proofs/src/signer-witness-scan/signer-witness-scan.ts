import type { MidgardAddressWitness } from "@al-ft/midgard-core";
import {
  type MissingSignatureFieldWalkCheckpoint,
  missingSignatureFieldWalkCheckpoint,
  missingSignatureVkeyHash,
  verifyAddressWitness,
} from "@al-ft/midgard-sdk";

/** The cumulative signer state once the scan has passed `cursor` witnesses. */
export type SignerWitnessScanCheckpoint = Readonly<{
  cursor: number;
  signerPresent: boolean;
}>;

export type SignerWitnessScan = Readonly<{
  validSignerHashes: readonly string[];
  signerPresent: boolean;
  checkpoints: readonly SignerWitnessScanCheckpoint[];
}>;

/**
 * The field-7 signer scan the spend-input and protected-output signer-missing
 * families share: a witness counts only when its signature verifies over the
 * transaction id, and the signer is present once a counted key hashes to the
 * payment credential. The scan checkpoints its cumulative state at every
 * `batch` boundary and at the end, or at the initial position of an empty
 * field — the cursors step 04 resumes from.
 */
export const scanSignerWitnesses = ({
  transactionId,
  witnesses,
  paymentCredentialHex,
  batch,
}: {
  readonly transactionId: string;
  readonly witnesses: readonly MidgardAddressWitness[];
  readonly paymentCredentialHex: string;
  readonly batch: number;
}): SignerWitnessScan => {
  const validSignerHashes: string[] = [];
  const checkpoints: SignerWitnessScanCheckpoint[] = [];
  let signerPresent = false;
  witnesses.forEach((witness, index) => {
    const verificationKey = Buffer.from(witness.verificationKey).toString(
      "hex",
    );
    if (
      verifyAddressWitness({
        txId: transactionId,
        witness: {
          verification_key: verificationKey,
          signature: Buffer.from(witness.signature).toString("hex"),
        },
      })
    ) {
      const hash = missingSignatureVkeyHash(verificationKey);
      validSignerHashes.push(hash);
      if (hash === paymentCredentialHex) signerPresent = true;
    }
    if ((index + 1) % batch === 0 || index + 1 === witnesses.length)
      checkpoints.push(Object.freeze({ cursor: index + 1, signerPresent }));
  });
  if (witnesses.length === 0)
    checkpoints.push(Object.freeze({ cursor: 0, signerPresent: false }));
  return Object.freeze({
    validSignerHashes: Object.freeze(validSignerHashes),
    signerPresent,
    checkpoints: Object.freeze(checkpoints),
  });
};

/**
 * One step-04 turn of the shared scan: resolves the thread-carried checkpoint
 * to the only cursor of the `batch` schedule that commits to it, and reads
 * the scan's cumulative signer state where the turn ends. `exhausted` means
 * the turn reaches the end of the field; when the turn closes the thread is
 * each family's own rule.
 */
export const planSignerWitnessScanTurn = ({
  checkpoints,
  txId,
  itemCount,
  totalLength,
  checkpointHash,
  batch,
}: {
  readonly checkpoints: readonly SignerWitnessScanCheckpoint[];
  readonly txId: string;
  readonly itemCount: number;
  readonly totalLength: number;
  readonly checkpointHash: string;
  readonly batch: number;
}): Readonly<{
  checkpoint: MissingSignatureFieldWalkCheckpoint;
  nextCursor: number;
  exhausted: boolean;
  signerPresent: boolean;
  nextCheckpoint: MissingSignatureFieldWalkCheckpoint | undefined;
}> => {
  const at = (nextItemIndex: number) =>
    missingSignatureFieldWalkCheckpoint({
      txId,
      itemCount,
      totalLength,
      nextItemIndex,
    });
  let checkpoint: MissingSignatureFieldWalkCheckpoint | undefined;
  for (
    let cursor = 0;
    checkpoint === undefined && (cursor === 0 || cursor < itemCount);
    cursor += batch
  ) {
    const candidate = at(cursor);
    if (candidate.checkpointHash === checkpointHash) checkpoint = candidate;
  }
  if (checkpoint === undefined)
    throw new Error(
      `signer witness scan: checkpoint is not on the deterministic ${batch.toString()}-item schedule`,
    );
  const nextCursor = Math.min(checkpoint.nextItemIndex + batch, itemCount);
  const reached = checkpoints.find(({ cursor }) => cursor === nextCursor);
  if (reached === undefined)
    throw new Error(
      `signer witness scan: scan has no checkpoint at cursor ${nextCursor.toString()}`,
    );
  const exhausted = nextCursor === itemCount;
  return {
    checkpoint,
    nextCursor,
    exhausted,
    signerPresent: reached.signerPresent,
    nextCheckpoint: exhausted ? undefined : at(nextCursor),
  };
};
