import {
  buildMidgardBoundedItem,
  decodeMidgardAddressBytes,
  decodeMidgardAddressWitnessFieldPreimage,
  decodeMidgardInputFieldPreimage,
  decodeMidgardLedgerOutputCommitment,
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

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import type {
  AuthenticatedPriorLedgerOutput,
  ResolvedOutputPriorLedgerReplay,
} from "../resolved-output-non-canonical/resolved-output-non-canonical.js";

export const SPEND_INPUT_SIGNER_MISSING_CATEGORY =
  "spendInputSignerMissing" as const;
export const SPEND_INPUT_SIGNER_MISSING_ID = "00000027" as const;
export const SPEND_INPUT_SIGNER_SCAN_BATCH = 16;
export const SPEND_INPUT_SIGNER_MAX_WITNESSES = 318;

const fail = (message: string): never => {
  throw new Error(`${SPEND_INPUT_SIGNER_MISSING_CATEGORY}: ${message}`);
};

const nonNegativeIndex = (value: number, label: string): number => {
  if (!Number.isSafeInteger(value) || value < 0)
    return fail(`${label} is invalid`);
  return value;
};

export type SpendInputSignerMissingFinding = Readonly<{
  subject: VerdictSubject;
  inputIndex: number;
}>;

export type SpendInputSignerMissingEvidence = SpendInputSignerMissingFinding &
  Readonly<{
    canonicalTransactionCborHex: string;
    inputFieldPreimageHex: string;
    addressWitnessFieldPreimageHex: string;
    witnessSetHashHex: string;
    resolved: AuthenticatedPriorLedgerOutput;
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

const exactForcedInputIndex = (subject: VerdictSubject): number => {
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

export const classifySpendInputSignerMissingFinding = ({
  subject,
  inputIndex,
}: SpendInputSignerMissingFinding): void => {
  if (!verdictSubjectIsCanonical(subject))
    return fail("subject is not canonical");
  nonNegativeIndex(inputIndex, "input coordinate");
  if (subject.direction === PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION) {
    if (exactForcedInputIndex(subject) !== inputIndex)
      return fail("reason input coordinate was substituted");
  } else if (
    subject.direction !== PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE ||
    subject.rejection_reason !== null
  ) {
    return fail("subject polarity is invalid");
  }
};

export const prepareSpendInputSignerMissingEvidence = ({
  subject,
  inputIndex,
  canonicalTransactionCbor,
  resolved,
}: SpendInputSignerMissingFinding & {
  readonly canonicalTransactionCbor: Uint8Array;
  readonly resolved: AuthenticatedPriorLedgerOutput;
}): SpendInputSignerMissingEvidence => {
  classifySpendInputSignerMissingFinding({ subject, inputIndex });
  const material = deriveMidgardNativeTxFaultEvidenceMaterial(
    canonicalTransactionCbor,
  );
  if (material.transactionId.toString("hex") !== subject.transaction_id)
    return fail("transaction identity was substituted");
  const selected = decodeMidgardInputFieldPreimage(material.fieldPreimages[0]!)[
    inputIndex
  ];
  if (selected === undefined) return fail("input coordinate is out of range");
  if (
    Buffer.from(selected.txId).toString("hex") !== resolved.transactionId ||
    selected.outputIndex !== resolved.outputIndex
  )
    return fail("resolved out-ref differs from the authenticated spend item");
  const descriptor = decodeMidgardLedgerOutputCommitment(
    Buffer.from(resolved.descriptorCborHex, "hex"),
  );
  const outputCbor = Buffer.from(resolved.outputCborHex, "hex");
  const item = buildMidgardBoundedItem({
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
  const witnesses = decodeMidgardAddressWitnessFieldPreimage(witnessPreimage);
  if (witnesses.length > SPEND_INPUT_SIGNER_MAX_WITNESSES)
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
      const hash = missingSignatureVkeyHash(verificationKey);
      validSignerHashes.push(hash);
      if (hash === paymentCredentialHex) signerPresent = true;
    }
    if (
      (witnessIndex + 1) % SPEND_INPUT_SIGNER_SCAN_BATCH === 0 ||
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
    inputCarriage: selectMidgardFieldCarriageTier(
      material.fieldPreimages[0]!.length,
    ),
    witnessCarriage: selectMidgardFieldCarriageTier(witnessPreimage.length),
    checkpoints: Object.freeze(
      checkpoints.map((value) => Object.freeze(value)),
    ),
  });
  if (!spendInputSignerMissingEvidenceCloses(evidence))
    return fail("authenticated signer state agrees with the operator verdict");
  return evidence;
};

export const spendInputSignerMissingEvidenceCloses = (
  evidence: Pick<SpendInputSignerMissingEvidence, "subject" | "signerMissing">,
): boolean =>
  terminalVerdictContradiction(evidence.subject, evidence.signerMissing);

export const spendInputSignerMissingEvidenceIdentity = (
  evidence: SpendInputSignerMissingEvidence,
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
export const detectSpendInputSignerMissingCompleteReplay = ({
  block,
  priorLedger,
}: {
  readonly block: CanonicalBlockEvidence;
  readonly priorLedger: ResolvedOutputPriorLedgerReplay;
}): readonly SpendInputSignerMissingEvidence[] => {
  if (priorLedger.priorRoot !== block.header.prevUtxosRoot)
    return fail("predecessor replay root differs from authenticated header");
  const detections: SpendInputSignerMissingEvidence[] = [];
  const inspect = (
    bytes: Uint8Array,
    subject: VerdictSubject,
    inputIndex: number,
  ): void => {
    const material = deriveMidgardNativeTxFaultEvidenceMaterial(bytes);
    const selected = decodeMidgardInputFieldPreimage(
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
        prepareSpendInputSignerMissingEvidence({
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
    const material = deriveMidgardNativeTxFaultEvidenceMaterial(bytes);
    const subject = acceptedVerdictSubject(
      material.transactionId.toString("hex"),
    );
    decodeMidgardInputFieldPreimage(material.fieldPreimages[0]!).forEach(
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
      forcedVerdictSubject({
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
export const deriveSpendInputSignerMissingEvidenceFromCompleteReplay = (
  input: Parameters<typeof detectSpendInputSignerMissingCompleteReplay>[0],
): SpendInputSignerMissingEvidence => {
  const detections = detectSpendInputSignerMissingCompleteReplay(input);
  if (detections.length !== 1)
    return fail(
      `authenticated complete replay produced ${detections.length.toString()} detections; exactly one is required`,
    );
  return detections[0]!;
};
