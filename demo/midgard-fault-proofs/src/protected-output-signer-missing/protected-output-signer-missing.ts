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

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";

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

/**
 * Which physical route step 02 takes. Canonical validation consults the
 * signer frontier only for a protected pub-key output at a position its
 * cursor visits (`protected_output_authorization`); every other coordinate
 * is authorized with no signer, so a forced rejection citing it is wrong
 * without any witness and step 02 closes at step 05 directly.
 */
export type ProtectedOutputSignerMissingRoute =
  | "witness_scan"
  | "unprotected_output"
  | "script_credential"
  | "coordinate_out_of_range";

export type ProtectedOutputSignerMissingEvidence =
  ProtectedOutputSignerMissingFinding &
    Readonly<{
      canonicalTransactionCborHex: string;
      route: ProtectedOutputSignerMissingRoute;
      /** `true` exactly on the witness-scan route. */
      signerRequired: boolean;
      /** Absent only when the coordinate is past the field-2 count. */
      outputCborHex?: string;
      /** Present only on the witness-scan route. */
      paymentCredentialHex?: string;
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

/** Narrows scan-route evidence to the fields the scan steps consume. */
export const requireProtectedOutputSignerScanEvidence = (
  evidence: ProtectedOutputSignerMissingEvidence,
): Readonly<{ outputCborHex: string; paymentCredentialHex: string }> => {
  if (
    evidence.route !== "witness_scan" ||
    evidence.outputCborHex === undefined ||
    evidence.paymentCredentialHex === undefined
  )
    return fail("evidence does not take the witness-scan route");
  return {
    outputCborHex: evidence.outputCborHex,
    paymentCredentialHex: evidence.paymentCredentialHex,
  };
};

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

/**
 * Classifies the bound coordinate the way step 02 does on chain. A direct
 * route contradicts only a wrongful forced rejection; an accepted subject at
 * such a coordinate has no fault, and the preparer refuses it.
 */
const classifyRoute = (
  subject: VerdictSubject,
  outputCbor: Uint8Array | undefined,
): ProtectedOutputSignerMissingRoute => {
  const direct = (
    route: Exclude<ProtectedOutputSignerMissingRoute, "witness_scan">,
    reason: string,
  ): ProtectedOutputSignerMissingRoute =>
    subject.direction === PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION
      ? route
      : fail(reason);
  if (outputCbor === undefined)
    return direct(
      "coordinate_out_of_range",
      "output coordinate is out of range",
    );
  const address = decodeMidgardAddressBytes(
    decodeMidgardTxOutput(outputCbor).address,
  );
  if (!address.protected)
    return direct("unprotected_output", "selected output is not protected");
  if (address.paymentCredential.kind !== "PubKey")
    return direct(
      "script_credential",
      "selected protected output does not use a key credential",
    );
  return "witness_scan";
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
  const route = classifyRoute(subject, outputCbor);
  const witnessItems = decodeMidgardFieldPreimage(material.fieldPreimages[7]!);
  if (witnessItems.length > PROTECTED_OUTPUT_SIGNER_MAX_WITNESSES)
    return fail("address-witness frontier exceeds the canonical maximum");
  const common = {
    subject,
    outputIndex,
    route,
    signerRequired: route === "witness_scan",
    canonicalTransactionCborHex: Buffer.from(canonicalTransactionCbor).toString(
      "hex",
    ),
    ...(outputCbor === undefined
      ? {}
      : { outputCborHex: Buffer.from(outputCbor).toString("hex") }),
    witnessSetHashHex: Buffer.from(
      material.compact.transactionWitnessSetHash,
    ).toString("hex"),
    addressWitnessFieldPreimageHex: material.fieldPreimages[7]!.toString("hex"),
    outputCarriage: selectMidgardFieldCarriageTier(
      material.fieldPreimages[2]!.length,
    ),
    witnessCarriage: selectMidgardFieldCarriageTier(
      material.fieldPreimages[7]!.length,
    ),
  } as const;
  if (route !== "witness_scan") {
    // No signer is required, so no scan runs: the verdict step 02 writes is
    // `signer_required = False, signer_present = False` by construction.
    const evidence: ProtectedOutputSignerMissingEvidence = Object.freeze({
      ...common,
      validSignerHashes: Object.freeze([]),
      signerPresent: false,
      checkpoints: Object.freeze([]),
    });
    if (!protectedOutputSignerMissingEvidenceCloses(evidence))
      return fail(
        "authenticated signer state agrees with the operator verdict",
      );
    return evidence;
  }
  const address = decodeMidgardAddressBytes(
    decodeMidgardTxOutput(outputCbor!).address,
  );
  if (address.paymentCredential.kind !== "PubKey")
    return fail("selected protected output does not use a key credential");
  const paymentCredentialHex = address.paymentCredential.hash.toString("hex");
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
  const evidence: ProtectedOutputSignerMissingEvidence = Object.freeze({
    ...common,
    paymentCredentialHex,
    validSignerHashes: Object.freeze(validSignerHashes),
    signerPresent,
    checkpoints: Object.freeze(
      checkpoints.map((checkpoint) => Object.freeze(checkpoint)),
    ),
  });
  if (!protectedOutputSignerMissingEvidenceCloses(evidence))
    return fail("authenticated signer state agrees with the operator verdict");
  return evidence;
};

/** Mirrors `rule.terminal_v1`: the decisive fault is a required, absent signer. */
export const protectedOutputSignerMissingEvidenceCloses = (
  evidence: Pick<
    ProtectedOutputSignerMissingEvidence,
    "subject" | "signerRequired" | "signerPresent"
  >,
): boolean =>
  terminalVerdictContradiction(
    evidence.subject,
    evidence.signerRequired && !evidence.signerPresent,
  );

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
      // An accepted coordinate that needs no signer is not a fault; the
      // forced direction never reaches these refusals because the direct
      // route classifies such a coordinate instead.
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
