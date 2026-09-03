import {
  decodeMidgardAddressBytes,
  decodeMidgardAddressWitnessItem,
  decodeMidgardFieldPreimage,
  decodeMidgardTxOutput,
  deriveMidgardNativeTxFaultEvidenceMaterial,
  selectMidgardFieldCarriageTier,
} from "@al-ft/midgard-core";
import {
  acceptedVerdictSubject,
  forcedVerdictSubject,
  missingSignatureVkeyHash,
  PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE,
  PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION,
  terminalVerdictContradiction,
  type VerdictSubject,
  verdictSubjectIsCanonical,
  verifyAddressWitness,
} from "@al-ft/midgard-sdk";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence-v1.js";

export const PROTECTED_OUTPUT_SIGNER_MISSING_CATEGORY =
  "protectedOutputSignerMissing" as const;
export const PROTECTED_OUTPUT_SIGNER_MISSING_ID = "0000002b" as const;
export const PROTECTED_OUTPUT_SIGNER_SCAN_BATCH = 32;
export const PROTECTED_OUTPUT_SIGNER_MAX_WITNESSES = 318;

const fail = (message: string): never => {
  throw new Error(`${PROTECTED_OUTPUT_SIGNER_MISSING_CATEGORY}: ${message}`);
};

export type ProtectedOutputSignerMissingFinding = Readonly<{
  subject: VerdictSubject;
  outputIndex: number;
}>;

export type ProtectedOutputSignerMissingEvidence =
  ProtectedOutputSignerMissingFinding &
    Readonly<{
      canonicalTransactionCborHex: string;
      outputCborHex: string;
      paymentCredentialHex: string;
      witnessSetHashHex: string;
      addressWitnessFieldPreimageHex: string;
      validSignerHashes: readonly string[];
      signerPresent: boolean;
      outputCarriage: "Inline" | "RawUtxo" | "Certified";
      witnessCarriage: "Inline" | "RawUtxo" | "Certified";
      checkpoints: readonly Readonly<{
        cursor: number;
        signerPresent: boolean;
      }>[];
    }>;

const exactForcedOutputIndex = (subject: VerdictSubject): number => {
  const reason = subject.rejection_reason;
  if (
    reason === null ||
    typeof reason === "string" ||
    !("ProtectedOutputSignerMissing" in reason)
  ) {
    return fail("forced subject has the wrong typed rejection reason");
  }
  const outputIndex = Number(reason.ProtectedOutputSignerMissing.output_index);
  if (!Number.isSafeInteger(outputIndex) || outputIndex < 0)
    return fail("forced reason output coordinate is invalid");
  return outputIndex;
};

export const classifyProtectedOutputSignerMissingFinding = ({
  subject,
  outputIndex,
}: ProtectedOutputSignerMissingFinding): void => {
  if (!verdictSubjectIsCanonical(subject))
    return fail("subject is not canonical");
  if (!Number.isSafeInteger(outputIndex) || outputIndex < 0)
    return fail("output coordinate is invalid");
  if (subject.direction === PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION) {
    if (exactForcedOutputIndex(subject) !== outputIndex)
      return fail("reason output coordinate was substituted");
  } else if (
    subject.direction !== PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE ||
    subject.rejection_reason !== null
  ) {
    return fail("subject polarity is invalid");
  }
};

export const prepareProtectedOutputSignerMissingEvidence = ({
  subject,
  outputIndex,
  canonicalTransactionCbor,
}: ProtectedOutputSignerMissingFinding & {
  readonly canonicalTransactionCbor: Uint8Array;
}): ProtectedOutputSignerMissingEvidence => {
  classifyProtectedOutputSignerMissingFinding({ subject, outputIndex });
  const material = deriveMidgardNativeTxFaultEvidenceMaterial(
    canonicalTransactionCbor,
  );
  if (material.transactionId.toString("hex") !== subject.transaction_id)
    return fail("transaction identity was substituted");
  const outputItems = decodeMidgardFieldPreimage(material.fieldPreimages[2]!);
  const outputCbor = outputItems[outputIndex];
  if (outputCbor === undefined)
    return fail("output coordinate is out of range");
  const output = decodeMidgardTxOutput(outputCbor);
  const address = decodeMidgardAddressBytes(output.address);
  if (!address.protected) return fail("selected output is not protected");
  if (address.paymentCredential.kind !== "PubKey")
    return fail("selected protected output does not use a key credential");
  const paymentCredentialHex = address.paymentCredential.hash.toString("hex");
  const witnessItems = decodeMidgardFieldPreimage(material.fieldPreimages[7]!);
  if (witnessItems.length > PROTECTED_OUTPUT_SIGNER_MAX_WITNESSES)
    return fail("address-witness frontier exceeds the canonical maximum");
  let signerPresent = false;
  const validSignerHashes: string[] = [];
  const checkpoints: { cursor: number; signerPresent: boolean }[] = [];
  witnessItems.forEach((item, index) => {
    const witness = decodeMidgardAddressWitnessItem(item);
    const verificationKey = Buffer.from(witness.verificationKey).toString(
      "hex",
    );
    const valid = verifyAddressWitness({
      txId: subject.transaction_id,
      witness: {
        verification_key: verificationKey,
        signature: Buffer.from(witness.signature).toString("hex"),
      },
    });
    if (valid) {
      const hash = missingSignatureVkeyHash(verificationKey);
      validSignerHashes.push(hash);
      if (hash === paymentCredentialHex) signerPresent = true;
    }
    if (
      (index + 1) % PROTECTED_OUTPUT_SIGNER_SCAN_BATCH === 0 ||
      index + 1 === witnessItems.length
    ) {
      checkpoints.push({ cursor: index + 1, signerPresent });
    }
  });
  if (witnessItems.length === 0)
    checkpoints.push({ cursor: 0, signerPresent: false });
  const evidence = Object.freeze({
    subject,
    outputIndex,
    canonicalTransactionCborHex: Buffer.from(canonicalTransactionCbor).toString(
      "hex",
    ),
    outputCborHex: Buffer.from(outputCbor).toString("hex"),
    paymentCredentialHex,
    witnessSetHashHex: Buffer.from(
      material.compact.transactionWitnessSetHash,
    ).toString("hex"),
    addressWitnessFieldPreimageHex: material.fieldPreimages[7]!.toString("hex"),
    validSignerHashes: Object.freeze(validSignerHashes),
    signerPresent,
    outputCarriage: selectMidgardFieldCarriageTier(
      material.fieldPreimages[2]!.length,
    ),
    witnessCarriage: selectMidgardFieldCarriageTier(
      material.fieldPreimages[7]!.length,
    ),
    checkpoints: Object.freeze(
      checkpoints.map((checkpoint) => Object.freeze(checkpoint)),
    ),
  });
  if (!protectedOutputSignerMissingEvidenceCloses(evidence))
    return fail("authenticated signer state agrees with the operator verdict");
  return evidence;
};

export const protectedOutputSignerMissingEvidenceCloses = (
  evidence: ProtectedOutputSignerMissingEvidence,
): boolean =>
  terminalVerdictContradiction(evidence.subject, !evidence.signerPresent);

/** Exhaustive accepted plus exact forced-reason replay over authenticated DA. */
export const detectProtectedOutputSignerMissingCompleteReplay = (
  block: CanonicalBlockEvidence,
): readonly ProtectedOutputSignerMissingEvidence[] => {
  const detections: ProtectedOutputSignerMissingEvidence[] = [];
  const inspect = (
    subject: VerdictSubject,
    transaction: Uint8Array,
    outputIndex: number,
  ): void => {
    try {
      const evidence = prepareProtectedOutputSignerMissingEvidence({
        subject,
        outputIndex,
        canonicalTransactionCbor: transaction,
      });
      if (protectedOutputSignerMissingEvidenceCloses(evidence))
        detections.push(evidence);
    } catch (cause) {
      if (
        cause instanceof Error &&
        (cause.message.endsWith("selected output is not protected") ||
          cause.message.endsWith(
            "selected protected output does not use a key credential",
          ) ||
          cause.message.endsWith("output coordinate is out of range") ||
          cause.message.endsWith(
            "authenticated signer state agrees with the operator verdict",
          ))
      )
        return;
      throw cause;
    }
  };
  block.transactions.forEach((transaction) => {
    const bytes = Buffer.from(transaction.txCbor, "hex");
    const material = deriveMidgardNativeTxFaultEvidenceMaterial(bytes);
    const subject = acceptedVerdictSubject(
      material.transactionId.toString("hex"),
    );
    decodeMidgardFieldPreimage(material.fieldPreimages[2]!).forEach(
      (_output, outputIndex) => inspect(subject, bytes, outputIndex),
    );
  });
  block.reconstruction.forcedTransactions.forEach((forced) => {
    if (forced.value.verdict === "ForcedTxValid") return;
    const reason = forced.value.verdict.ForcedTxInvalid.reason;
    if (
      typeof reason === "string" ||
      !("ProtectedOutputSignerMissing" in reason)
    )
      return;
    const outputIndex = Number(
      reason.ProtectedOutputSignerMissing.output_index,
    );
    inspect(
      forcedVerdictSubject({
        transactionId: forced.value.tx_id,
        sourceKey: forced.key,
        rejectionReason: reason,
      }),
      forced.fullTransactionCbor,
      outputIndex,
    );
  });
  return Object.freeze(detections);
};
