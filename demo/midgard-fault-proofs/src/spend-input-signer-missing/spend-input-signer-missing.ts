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

/**
 * Which physical route step 02 takes. Canonical validation consults the
 * signer frontier only for a resolved pub-key spend input at a position its
 * cursor visits (`input_signer_authorization`); a script credential is
 * authorized with no signer and a position past the field-0 count is never
 * evaluated, so a forced rejection citing either is wrong without any
 * witness and step 02 closes at step 05 directly.
 */
export type SpendInputSignerMissingRoute =
  | "witness_scan"
  | "script_credential"
  | "coordinate_out_of_range";

export type SpendInputSignerMissingEvidence = SpendInputSignerMissingFinding &
  Readonly<{
    canonicalTransactionCborHex: string;
    inputFieldPreimageHex: string;
    addressWitnessFieldPreimageHex: string;
    witnessSetHashHex: string;
    /** The header's committed `prev_utxos_root` the thread binds. */
    priorRoot: string;
    route: SpendInputSignerMissingRoute;
    /** `true` exactly on the witness-scan route. */
    signerRequired: boolean;
    /** Absent only when the coordinate is past the field-0 count. */
    resolved?: AuthenticatedPriorLedgerOutput;
    /** Present only on the witness-scan route. */
    paymentCredentialHex?: string;
    validSignerHashes: readonly string[];
    signerMissing: boolean;
    inputCarriage: "Inline" | "RawUtxo" | "Certified";
    witnessCarriage: "Inline" | "RawUtxo" | "Certified";
    checkpoints: readonly Readonly<{
      cursor: number;
      signerPresent: boolean;
    }>[];
  }>;

/** Narrows in-range evidence to the resolved prior-ledger output step 02 proves. */
export const requireSpendInputSignerResolvedOutput = (
  evidence: SpendInputSignerMissingEvidence,
): AuthenticatedPriorLedgerOutput => {
  if (evidence.resolved === undefined)
    return fail("evidence carries no resolved prior-ledger output");
  return evidence.resolved;
};

/** Narrows scan-route evidence to the credential the scan steps consume. */
export const requireSpendInputSignerScanEvidence = (
  evidence: SpendInputSignerMissingEvidence,
): Readonly<{
  resolved: AuthenticatedPriorLedgerOutput;
  paymentCredentialHex: string;
}> => {
  if (
    evidence.route !== "witness_scan" ||
    evidence.paymentCredentialHex === undefined
  )
    return fail("evidence does not take the witness-scan route");
  return {
    resolved: requireSpendInputSignerResolvedOutput(evidence),
    paymentCredentialHex: evidence.paymentCredentialHex,
  };
};

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

/** A direct route contradicts only a wrongful forced rejection. */
const directRoute = (
  subject: VerdictSubject,
  route: Exclude<SpendInputSignerMissingRoute, "witness_scan">,
  reason: string,
): SpendInputSignerMissingRoute =>
  subject.direction === PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION
    ? route
    : fail(reason);

export const prepareSpendInputSignerMissingEvidence = ({
  subject,
  inputIndex,
  canonicalTransactionCbor,
  resolved,
  priorRoot = resolved?.priorRoot,
}: SpendInputSignerMissingFinding & {
  readonly canonicalTransactionCbor: Uint8Array;
  /** Required for an in-range coordinate; absent past the field-0 count. */
  readonly resolved?: AuthenticatedPriorLedgerOutput;
  /** Defaults to `resolved.priorRoot`; required when `resolved` is absent. */
  readonly priorRoot?: string;
}): SpendInputSignerMissingEvidence => {
  classifySpendInputSignerMissingFinding({ subject, inputIndex });
  if (priorRoot === undefined) return fail("prior-ledger root is required");
  if (resolved !== undefined && resolved.priorRoot !== priorRoot)
    return fail("resolved prior-ledger output names another root");
  const material = deriveMidgardNativeTxFaultEvidenceMaterial(
    canonicalTransactionCbor,
  );
  if (material.transactionId.toString("hex") !== subject.transaction_id)
    return fail("transaction identity was substituted");
  const witnessPreimage = material.fieldPreimages[7]!;
  // The aggregate field bound is this family's adjacent consensus bound on
  // every route: a transaction past it is never a subject.
  const witnesses = decodeMidgardAddressWitnessFieldPreimage(witnessPreimage);
  if (witnesses.length > SPEND_INPUT_SIGNER_MAX_WITNESSES)
    return fail("address-witness frontier exceeds the canonical maximum");
  const common = {
    subject,
    inputIndex,
    canonicalTransactionCborHex: Buffer.from(canonicalTransactionCbor).toString(
      "hex",
    ),
    inputFieldPreimageHex: material.fieldPreimages[0]!.toString("hex"),
    addressWitnessFieldPreimageHex: witnessPreimage.toString("hex"),
    witnessSetHashHex:
      material.compact.transactionWitnessSetHash.toString("hex"),
    priorRoot,
    inputCarriage: selectMidgardFieldCarriageTier(
      material.fieldPreimages[0]!.length,
    ),
    witnessCarriage: selectMidgardFieldCarriageTier(witnessPreimage.length),
  } as const;
  const direct = (
    route: Exclude<SpendInputSignerMissingRoute, "witness_scan">,
  ): SpendInputSignerMissingEvidence => {
    // No signer is required, so no scan runs: the verdict step 02 writes is
    // `signer_required = False, signer_missing = False` by construction.
    const evidence: SpendInputSignerMissingEvidence = Object.freeze({
      ...common,
      route,
      signerRequired: false,
      ...(resolved === undefined ? {} : { resolved }),
      validSignerHashes: Object.freeze([]),
      signerMissing: false,
      checkpoints: Object.freeze([]),
    });
    if (!spendInputSignerMissingEvidenceCloses(evidence))
      return fail(
        "authenticated signer state agrees with the operator verdict",
      );
    return evidence;
  };
  const selected = decodeMidgardInputFieldPreimage(material.fieldPreimages[0]!)[
    inputIndex
  ];
  if (selected === undefined) {
    directRoute(
      subject,
      "coordinate_out_of_range",
      "input coordinate is out of range",
    );
    return direct("coordinate_out_of_range");
  }
  if (resolved === undefined)
    return fail("resolved prior-ledger output is required in range");
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
  if (address.paymentCredential.kind !== "PubKey") {
    directRoute(
      subject,
      "script_credential",
      "selected spend input is not key locked",
    );
    return direct("script_credential");
  }
  const paymentCredentialHex = address.paymentCredential.hash.toString("hex");
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
  const evidence: SpendInputSignerMissingEvidence = Object.freeze({
    ...common,
    route: "witness_scan",
    signerRequired: true,
    resolved,
    paymentCredentialHex,
    validSignerHashes: Object.freeze(validSignerHashes),
    signerMissing: !signerPresent,
    checkpoints: Object.freeze(
      checkpoints.map((value) => Object.freeze(value)),
    ),
  });
  if (!spendInputSignerMissingEvidenceCloses(evidence))
    return fail("authenticated signer state agrees with the operator verdict");
  return evidence;
};

/** Mirrors `rule.terminal_v1`: the decisive fault is a required, missing signer. */
export const spendInputSignerMissingEvidenceCloses = (
  evidence: Pick<
    SpendInputSignerMissingEvidence,
    "subject" | "signerRequired" | "signerMissing"
  >,
): boolean =>
  terminalVerdictContradiction(
    evidence.subject,
    evidence.signerRequired && evidence.signerMissing,
  );

export const spendInputSignerMissingEvidenceIdentity = (
  evidence: SpendInputSignerMissingEvidence,
): string =>
  [
    evidence.subject.transaction_id,
    evidence.subject.direction.toString(),
    evidence.inputIndex.toString(),
    evidence.route,
    evidence.priorRoot,
    evidence.resolved?.transactionId ?? "",
    evidence.resolved?.outputIndex.toString() ?? "",
    evidence.paymentCredentialHex ?? "",
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
    let resolved: AuthenticatedPriorLedgerOutput | undefined;
    if (selected !== undefined) {
      const transactionId = Buffer.from(selected.txId).toString("hex");
      const replayed = priorLedger.outputs.get(
        outRefKey(transactionId, selected.outputIndex),
      );
      // An in-range input the prior ledger does not hold is `InputNotFound`
      // territory: this family carries no non-membership proof.
      if (replayed === undefined)
        return fail(
          "complete predecessor replay omitted a resolved spend input",
        );
      resolved = { ...replayed, priorRoot: priorLedger.priorRoot };
    }
    try {
      detections.push(
        prepareSpendInputSignerMissingEvidence({
          subject,
          inputIndex,
          canonicalTransactionCbor: bytes,
          ...(resolved === undefined ? {} : { resolved }),
          priorRoot: priorLedger.priorRoot,
        }),
      );
    } catch (cause) {
      // An accepted script-locked input needs no signer and is not a fault;
      // the forced direction never reaches that refusal because the direct
      // route classifies such a coordinate instead.
      if (
        cause instanceof Error &&
        (cause.message.endsWith(
          "authenticated signer state agrees with the operator verdict",
        ) ||
          cause.message.endsWith("selected spend input is not key locked"))
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
