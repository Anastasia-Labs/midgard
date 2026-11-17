import {
  decodeMidgardAddressBytes,
  decodeMidgardAddressWitnessItem,
  decodeMidgardFieldPreimage,
  decodeMidgardTxOutput,
  deriveMidgardNativeTxFaultEvidenceMaterial,
  selectMidgardFieldCarriageTier,
} from "@al-ft/midgard-core";
import { deriveMidgardForcedTxFaultEvidenceMaterial } from "@al-ft/midgard-core/codec/forced";
import {
  acceptedVerdictSubject,
  forcedVerdictSubject,
  PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE,
  PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION,
  type VerdictSubject,
  verdictSubjectIsCanonical,
} from "@al-ft/midgard-sdk";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { claimedItem } from "../evidence/claimed-coordinate.js";
import {
  type SignerWitnessScanEvidence,
  signerWitnessVerdictCloses,
} from "../signer-witness-scan/evidence.js";
import { scanSignerWitnesses } from "../signer-witness-scan/signer-witness-scan.js";

export const PROTECTED_OUTPUT_SIGNER_MISSING_CATEGORY =
  "protectedOutputSignerMissing" as const;
export const PROTECTED_OUTPUT_SIGNER_MISSING_ID = "0000002b" as const;
export const PROTECTED_OUTPUT_SIGNER_MAX_WITNESSES = 318;

const fail = (message: string): never => {
  throw new Error(`${PROTECTED_OUTPUT_SIGNER_MISSING_CATEGORY}: ${message}`);
};

export type ProtectedOutputSignerMissingFinding = Readonly<{
  subject: VerdictSubject;
  /** The claimed position, unbounded like the on-chain coordinate. */
  outputIndex: bigint;
}>;

/**
 * Which physical route step 02 takes. Canonical validation consults the
 * signer frontier only for a protected pub-key output at a position its
 * cursor visits (`protected_output_authorization`); every other coordinate
 * is authorized with no signer, so a forced rejection citing it is wrong
 * without any witness and step 02 writes the terminal verdict directly.
 */
export type ProtectedOutputSignerMissingRoute =
  | "witness_scan"
  | "unprotected_output"
  | "script_credential"
  | "coordinate_out_of_range";

export type ProtectedOutputSignerMissingEvidence =
  ProtectedOutputSignerMissingFinding &
    SignerWitnessScanEvidence &
    Readonly<{
      canonicalTransactionCborHex: string;
      route: ProtectedOutputSignerMissingRoute;
      /**
       * Absent only when the coordinate is negative or past the field-2
       * count.
       */
      outputCborHex?: string;
      outputCarriage: "Inline" | "RawUtxo" | "Certified";
      witnessCarriage: "Inline" | "RawUtxo" | "Certified";
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

const exactForcedOutputIndex = (subject: VerdictSubject): bigint => {
  const reason = subject.rejection_reason;
  if (
    reason === null ||
    typeof reason === "string" ||
    !("ProtectedOutputSignerMissing" in reason)
  ) {
    return fail("forced subject has the wrong typed rejection reason");
  }
  return reason.ProtectedOutputSignerMissing.output_index;
};

export const classifyProtectedOutputSignerMissingFinding = ({
  subject,
  outputIndex,
}: ProtectedOutputSignerMissingFinding): void => {
  if (!verdictSubjectIsCanonical(subject))
    return fail("subject is not canonical");
  // A negative coordinate names no output: only a forced rejection can cite
  // one, and step 02 answers it on the out-of-range route.
  if (
    typeof outputIndex !== "bigint" ||
    (outputIndex < 0n &&
      subject.direction !== PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION)
  )
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
  const material = (
    subject.source_kind === 1n
      ? deriveMidgardForcedTxFaultEvidenceMaterial
      : deriveMidgardNativeTxFaultEvidenceMaterial
  )(canonicalTransactionCbor);
  if (material.transactionId.toString("hex") !== subject.transaction_id)
    return fail("transaction identity was substituted");
  const outputItems = decodeMidgardFieldPreimage(material.fieldPreimages[2]!);
  const outputCbor = claimedItem(outputItems, outputIndex);
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
      signerPresent: false,
      checkpoints: Object.freeze([]),
    });
    if (!signerWitnessVerdictCloses(evidence))
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
  const scan = scanSignerWitnesses({
    transactionId: subject.transaction_id,
    witnesses: witnessItems.map(decodeMidgardAddressWitnessItem),
    paymentCredentialHex,
  });
  const evidence: ProtectedOutputSignerMissingEvidence = Object.freeze({
    ...common,
    paymentCredentialHex,
    signerPresent: scan.signerPresent,
    checkpoints: scan.checkpoints,
  });
  if (!signerWitnessVerdictCloses(evidence))
    return fail("authenticated signer state agrees with the operator verdict");
  return evidence;
};

/** Exhaustive accepted plus exact forced-reason replay over authenticated DA. */
export const detectProtectedOutputSignerMissingCompleteReplay = (
  block: CanonicalBlockEvidence,
): readonly ProtectedOutputSignerMissingEvidence[] => {
  const detections: ProtectedOutputSignerMissingEvidence[] = [];
  const inspect = (
    subject: VerdictSubject,
    transaction: Uint8Array,
    outputIndex: bigint,
  ): void => {
    try {
      const evidence = prepareProtectedOutputSignerMissingEvidence({
        subject,
        outputIndex,
        canonicalTransactionCbor: transaction,
      });
      if (signerWitnessVerdictCloses(evidence)) detections.push(evidence);
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
      (_output, outputIndex) => inspect(subject, bytes, BigInt(outputIndex)),
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
    inspect(
      forcedVerdictSubject({
        transactionId: forced.value.tx_id,
        sourceKey: forced.key,
        rejectionReason: reason,
      }),
      forced.fullTransactionCbor,
      reason.ProtectedOutputSignerMissing.output_index,
    );
  });
  return Object.freeze(detections);
};
