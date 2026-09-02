import {
  buildMidgardBoundedItemV1,
  decodeMidgardAddressBytes,
  decodeMidgardAddressWitnessFieldPreimageV1,
  decodeMidgardInputFieldPreimageV1,
  decodeMidgardLedgerOutputCommitmentV1,
  decodeMidgardTxOutput,
  deriveMidgardNativeTxFaultEvidenceMaterialV1,
  selectMidgardFieldCarriageTierV1,
} from "@al-ft/midgard-core";
import {
  acceptedVerdictSubjectV1,
  forcedVerdictSubjectV1,
  missingSignatureVkeyHashV1,
  PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE_V1,
  PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION_V1,
  terminalVerdictContradictionV1,
  verdictSubjectIsCanonicalV1,
  type VerdictSubjectV1,
  verifyAddressWitness,
} from "@al-ft/midgard-sdk";

import type { CanonicalBlockEvidenceV1 } from "../evidence/canonical-block-evidence-v1.js";
import type {
  AuthenticatedPriorLedgerOutputV1,
  ResolvedOutputPriorLedgerReplayV1,
} from "../resolved-output-non-canonical/resolved-output-non-canonical-v1.js";

export const SPEND_INPUT_SIGNER_MISSING_CATEGORY_V1 =
  "spendInputSignerMissing" as const;
export const SPEND_INPUT_SIGNER_MISSING_ID_V1 = "00000027" as const;
export const SPEND_INPUT_SIGNER_SCAN_BATCH_V1 = 16;
export const SPEND_INPUT_SIGNER_MAX_WITNESSES_V1 = 318;

const fail = (message: string): never => {
  throw new Error(`${SPEND_INPUT_SIGNER_MISSING_CATEGORY_V1}: ${message}`);
};

const nonNegativeIndex = (value: number, label: string): number => {
  if (!Number.isSafeInteger(value) || value < 0)
    return fail(`${label} is invalid`);
  return value;
};

export type SpendInputSignerMissingFindingV1 = Readonly<{
  subject: VerdictSubjectV1;
  inputIndex: number;
}>;

export type SpendInputSignerMissingEvidenceV1 =
  SpendInputSignerMissingFindingV1 &
    Readonly<{
      canonicalTransactionCborHex: string;
      inputFieldPreimageHex: string;
      addressWitnessFieldPreimageHex: string;
      witnessSetHashHex: string;
      resolved: AuthenticatedPriorLedgerOutputV1;
      paymentCredentialHex: string;
      validSignerHashes: readonly string[];
      signerMissing: boolean;
      inputCarriage: "Inline" | "RawUtxo" | "Certified";
      witnessCarriage: "Inline" | "RawUtxo" | "Certified";
      checkpoints: readonly Readonly<{
        cursor: number;
        signerPresent: boolean;
      }>[];
    }>;

const exactForcedInputIndex = (subject: VerdictSubjectV1): number => {
  const reason = subject.rejection_reason;
  if (
    reason === null ||
    typeof reason === "string" ||
    !("SpendInputSignerMissing" in reason)
  )
    return fail("forced subject has the wrong typed rejection reason");
  return nonNegativeIndex(
    Number(reason.SpendInputSignerMissing.input_index),
    "forced reason input coordinate",
  );
};

export const classifySpendInputSignerMissingFindingV1 = ({
  subject,
  inputIndex,
}: SpendInputSignerMissingFindingV1): void => {
  if (!verdictSubjectIsCanonicalV1(subject))
    return fail("subject is not canonical");
  nonNegativeIndex(inputIndex, "input coordinate");
  if (subject.direction === PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION_V1) {
    if (exactForcedInputIndex(subject) !== inputIndex)
      return fail("reason input coordinate was substituted");
  } else if (
    subject.direction !== PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE_V1 ||
    subject.rejection_reason !== null
  ) {
    return fail("subject polarity is invalid");
  }
};

export const prepareSpendInputSignerMissingEvidenceV1 = ({
  subject,
  inputIndex,
  canonicalTransactionCbor,
  resolved,
}: SpendInputSignerMissingFindingV1 & {
  readonly canonicalTransactionCbor: Uint8Array;
  readonly resolved: AuthenticatedPriorLedgerOutputV1;
}): SpendInputSignerMissingEvidenceV1 => {
  classifySpendInputSignerMissingFindingV1({ subject, inputIndex });
  const material = deriveMidgardNativeTxFaultEvidenceMaterialV1(
    canonicalTransactionCbor,
  );
  if (material.transactionId.toString("hex") !== subject.transaction_id)
    return fail("transaction identity was substituted");
  const selected = decodeMidgardInputFieldPreimageV1(
    material.fieldPreimages[0]!,
  )[inputIndex];
  if (selected === undefined) return fail("input coordinate is out of range");
  if (
    Buffer.from(selected.txId).toString("hex") !== resolved.transactionId ||
    selected.outputIndex !== resolved.outputIndex
  )
    return fail("resolved out-ref differs from the authenticated spend item");
  const descriptor = decodeMidgardLedgerOutputCommitmentV1(
    Buffer.from(resolved.descriptorCborHex, "hex"),
  );
  const outputCbor = Buffer.from(resolved.outputCborHex, "hex");
  const item = buildMidgardBoundedItemV1({
    fieldIndex: 2,
    itemIndex: resolved.outputIndex,
    bytes: outputCbor,
  });
  if (
    descriptor.outputIndex !== resolved.outputIndex ||
    descriptor.totalLength !== outputCbor.length ||
    !descriptor.itemCommitment.equals(item.commitment)
  )
    return fail("prior-ledger descriptor does not bind the resolved output");
  const output = decodeMidgardTxOutput(outputCbor);
  const address = decodeMidgardAddressBytes(output.address);
  if (address.paymentCredential.kind !== "PubKey")
    return fail("selected spend input is not key locked");
  const paymentCredentialHex = address.paymentCredential.hash.toString("hex");
  const witnessPreimage = material.fieldPreimages[7]!;
  const witnesses = decodeMidgardAddressWitnessFieldPreimageV1(witnessPreimage);
  if (witnesses.length > SPEND_INPUT_SIGNER_MAX_WITNESSES_V1)
    return fail("address-witness frontier exceeds the canonical maximum");
  const validSignerHashes: string[] = [];
  const checkpoints: { cursor: number; signerPresent: boolean }[] = [];
  let signerPresent = false;
  witnesses.forEach((witness, witnessIndex) => {
    const verificationKey = Buffer.from(witness.verificationKey).toString(
      "hex",
    );
    if (
      verifyAddressWitness({
        txId: subject.transaction_id,
        witness: {
          verification_key: verificationKey,
          signature: Buffer.from(witness.signature).toString("hex"),
        },
      })
    ) {
      const hash = missingSignatureVkeyHashV1(verificationKey);
      validSignerHashes.push(hash);
      if (hash === paymentCredentialHex) signerPresent = true;
    }
    if (
      (witnessIndex + 1) % SPEND_INPUT_SIGNER_SCAN_BATCH_V1 === 0 ||
      witnessIndex + 1 === witnesses.length
    )
      checkpoints.push({ cursor: witnessIndex + 1, signerPresent });
  });
  if (witnesses.length === 0)
    checkpoints.push({ cursor: 0, signerPresent: false });
  const evidence = Object.freeze({
    subject,
    inputIndex,
    canonicalTransactionCborHex: Buffer.from(canonicalTransactionCbor).toString(
      "hex",
    ),
    inputFieldPreimageHex: material.fieldPreimages[0]!.toString("hex"),
    addressWitnessFieldPreimageHex: witnessPreimage.toString("hex"),
    witnessSetHashHex:
      material.compact.transactionWitnessSetHash.toString("hex"),
    resolved,
    paymentCredentialHex,
    validSignerHashes: Object.freeze(validSignerHashes),
    signerMissing: !signerPresent,
    inputCarriage: selectMidgardFieldCarriageTierV1(
      material.fieldPreimages[0]!.length,
    ),
    witnessCarriage: selectMidgardFieldCarriageTierV1(witnessPreimage.length),
    checkpoints: Object.freeze(
      checkpoints.map((value) => Object.freeze(value)),
    ),
  });
  if (!spendInputSignerMissingEvidenceClosesV1(evidence))
    return fail("authenticated signer state agrees with the operator verdict");
  return evidence;
};

export const spendInputSignerMissingEvidenceClosesV1 = (
  evidence: Pick<
    SpendInputSignerMissingEvidenceV1,
    "subject" | "signerMissing"
  >,
): boolean =>
  terminalVerdictContradictionV1(evidence.subject, evidence.signerMissing);

export const spendInputSignerMissingEvidenceIdentityV1 = (
  evidence: SpendInputSignerMissingEvidenceV1,
): string =>
  [
    evidence.subject.transaction_id,
    evidence.subject.direction.toString(),
    evidence.inputIndex.toString(),
    evidence.resolved.priorRoot,
    evidence.resolved.transactionId,
    evidence.resolved.outputIndex.toString(),
    evidence.paymentCredentialHex,
    evidence.witnessSetHashHex,
  ].join(":");

const outRefKey = (transactionId: string, outputIndex: number): string =>
  `${transactionId}#${outputIndex.toString()}`;

/** Package-owned exhaustive route over every accepted spend coordinate and
 * every forced leaf carrying exactly this typed reason. */
export const detectSpendInputSignerMissingCompleteReplayV1 = ({
  block,
  priorLedger,
}: {
  readonly block: CanonicalBlockEvidenceV1;
  readonly priorLedger: ResolvedOutputPriorLedgerReplayV1;
}): readonly SpendInputSignerMissingEvidenceV1[] => {
  if (priorLedger.priorRoot !== block.header.prevUtxosRoot)
    return fail("predecessor replay root differs from authenticated header");
  const detections: SpendInputSignerMissingEvidenceV1[] = [];
  const inspect = (
    bytes: Uint8Array,
    subject: VerdictSubjectV1,
    inputIndex: number,
  ): void => {
    const material = deriveMidgardNativeTxFaultEvidenceMaterialV1(bytes);
    const selected = decodeMidgardInputFieldPreimageV1(
      material.fieldPreimages[0]!,
    )[inputIndex];
    if (selected === undefined)
      return fail("forced reason input coordinate is absent");
    const transactionId = Buffer.from(selected.txId).toString("hex");
    const resolved = priorLedger.outputs.get(
      outRefKey(transactionId, selected.outputIndex),
    );
    if (resolved === undefined)
      return fail("complete predecessor replay omitted a resolved spend input");
    try {
      detections.push(
        prepareSpendInputSignerMissingEvidenceV1({
          subject,
          inputIndex,
          canonicalTransactionCbor: bytes,
          resolved: { ...resolved, priorRoot: priorLedger.priorRoot },
        }),
      );
    } catch (cause) {
      if (
        cause instanceof Error &&
        cause.message.endsWith(
          "authenticated signer state agrees with the operator verdict",
        )
      )
        return;
      throw cause;
    }
  };
  block.transactions.forEach((transaction) => {
    const bytes = Buffer.from(transaction.txCbor, "hex");
    const material = deriveMidgardNativeTxFaultEvidenceMaterialV1(bytes);
    const subject = acceptedVerdictSubjectV1(
      material.transactionId.toString("hex"),
    );
    decodeMidgardInputFieldPreimageV1(material.fieldPreimages[0]!).forEach(
      (_input, inputIndex) => inspect(bytes, subject, inputIndex),
    );
  });
  block.reconstruction.forcedTransactions.forEach((transaction) => {
    if (transaction.value.verdict === "ForcedTxValid") return;
    const reason = transaction.value.verdict.ForcedTxInvalid.reason;
    if (typeof reason === "string" || !("SpendInputSignerMissing" in reason))
      return;
    inspect(
      transaction.fullTransactionCbor,
      forcedVerdictSubjectV1({
        transactionId: transaction.value.tx_id,
        sourceKey: transaction.key,
        rejectionReason: reason,
      }),
      nonNegativeIndex(
        Number(reason.SpendInputSignerMissing.input_index),
        "forced reason input coordinate",
      ),
    );
  });
  return Object.freeze(detections);
};

/** Production admission is intentionally singular: the authenticated header
 * decision identifies one concrete family invocation, never caller-selected
 * evidence from an otherwise valid block. */
export const deriveSpendInputSignerMissingEvidenceFromCompleteReplayV1 = (
  input: Parameters<typeof detectSpendInputSignerMissingCompleteReplayV1>[0],
): SpendInputSignerMissingEvidenceV1 => {
  const detections = detectSpendInputSignerMissingCompleteReplayV1(input);
  if (detections.length !== 1)
    return fail(
      `authenticated complete replay produced ${detections.length.toString()} detections; exactly one is required`,
    );
  return detections[0]!;
};
