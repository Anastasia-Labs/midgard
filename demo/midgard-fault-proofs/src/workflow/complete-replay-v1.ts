import { createHash } from "node:crypto";

import {
  decodeMidgardAddressBytes,
  decodeMidgardAddressWitnessFieldPreimageV1,
  decodeMidgardFieldPreimageV1,
  decodeMidgardLedgerOutputCommitmentV1,
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  decodeMidgardSpendInputItemV1,
  decodeMidgardTxOutput,
  decodeMidgardVersionedScript,
  deriveMidgardNativeTxProofSourceV1,
  MIDGARD_POSIX_TIME_NONE,
  verifyMidgardNativeScript,
} from "@al-ft/midgard-core";
import {
  type AuthenticatedStateQueueHeaderObservationV1,
  CANONICAL_DECODABILITY_VIOLATION_ID_V1,
  canonicalDecodabilityEvidenceFromCommittedFieldV1,
  COMMITTED_FIELD_SHAPE_VIOLATION_ID_V1,
  committedWithdrawalKeyBytesV1,
  decodeAddressWitnessPreimage,
  DOUBLE_WITHDRAW_VIOLATION_ID_V1,
  EMPTY_MERKLE_TREE_ROOT,
  type EvidenceProvenanceV1,
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  type FraudProofCatalogueCategoryName,
  INPUT_NO_IDX_VIOLATION_ID_V1,
  INVALID_SIGNATURE_VIOLATION_ID_V1,
  invalidRangeViolationReason,
  isPayableWithdrawalLeafV1,
  isWithdrawnInputViolationV1,
  MIN_ADA_VIOLATION_ID_V1,
  MIN_FEE_VIOLATION_ID_V1,
  minimumFeeFromProofSourceV1,
  MISSING_NATIVE_SCRIPT_TX_VIOLATION_ID_V1,
  MISSING_SIGNATURE_VIOLATION_ID_V1,
  missingNativeScriptIsAbsentV1,
  missingSignatureVkeyHashV1,
  NATIVE_SCRIPT_INVALID_VIOLATION_ID_V1,
  nativeTxBodyHasZeroInputViolation,
  normalizeNativeTxValidityRange,
  REFERENCE_INPUT_NO_IDX_VIOLATION_ID_V1,
  verifyAddressWitness,
  WITHDRAWN_INPUT_VIOLATION_ID_V1,
  WITHDRAWN_REFERENCE_INPUT_VIOLATION_ID_V1,
} from "@al-ft/midgard-sdk";
import {
  buildCanonicalMidgardLedgerEntryOutputMaterialV1,
  buildCanonicalMidgardLedgerOutputMaterialV1,
  MIDGARD_COINS_PER_UTXO_BYTE_V1,
  outputMeetsMinAdaV1,
} from "@al-ft/midgard-validation";

import { classifyCommittedFieldShapeFieldsV1 } from "../committed-field-shape/prepare-committed-field-shape-v1.js";
import {
  canonicalBlockEvidenceFromVerifiedPayloadV1,
  type CanonicalBlockEvidenceV1,
} from "../evidence/canonical-block-evidence-v1.js";
import {
  INPUT_SET_UNIQUENESS_VIOLATION_ID_V1,
  scanInputSetUniquenessV1,
} from "../input-set-uniqueness/scan-v1.js";
import { ledgerKeyBytesHex } from "../ne-submit-step-03.js";
import { findNetworkIdFaultsV1 } from "../network-id/evidence-v1.js";
import { decodeTransactionMaterial } from "../prepare-double-spend.js";
import { detectInputNoIdxViolationsFromTransactionsV1 } from "../prepare-input-no-idx.js";
import { detectReferenceInputNoIdxViolationsFromTransactionsV1 } from "../prepare-reference-input-no-idx.js";
import {
  type CanonicalViolationDetectionV1,
  DOUBLE_SPEND_VIOLATION_ID_V1,
  NETWORK_ID_VIOLATION_ID_V1,
} from "./classification-v1.js";
import {
  type ProductionFabricatedDepositEvidenceAuthorityV1,
  requireProductionFabricatedDepositEvidenceAuthorityV1,
} from "./production-fabricated-deposit-evidence-v1.js";
import {
  type ProductionFabricatedWithdrawalEvidenceAuthorityV1,
  requireProductionFabricatedWithdrawalEvidenceAuthorityV1,
} from "./production-fabricated-withdrawal-evidence-v1.js";
import {
  detectMinAdaUtxoFromHistoricalCorpusV1,
  detectMissingNativeScriptUtxoFromHistoricalCorpusV1,
  type ProductionHistoricalNativeScriptCorpusV1,
} from "./production-historical-native-script-corpus-v1.js";

export const COMPLETE_CANONICAL_REPLAY_V1 =
  "midgard-complete-canonical-replay-v1" as const;
export const COMPLETE_CANONICAL_REPLAY_PREDECESSOR_V1 =
  "midgard-complete-canonical-replay-predecessor-v1" as const;

export type CompleteCanonicalReplayPredecessorV1 = Readonly<{
  schemaVersion: typeof COMPLETE_CANONICAL_REPLAY_PREDECESSOR_V1;
  challengedHeaderHash: string;
  headerHash: string;
  payloadEnvelopeSha256: string;
  payloadSha256: string;
}>;

export type CompleteCanonicalReplayContextV1 = Readonly<{
  /**
   * Exact public-DA/L1-authenticated predecessor. Required by ledger-relative
   * detectors unless the current header commits the empty genesis ledger.
   */
  predecessor?: CompleteCanonicalReplayPredecessorV1;
}>;

export type CompleteCanonicalReplayContextIdentityV1 = Readonly<{
  predecessorHeaderHash: string;
  predecessorPayloadEnvelopeSha256: string;
  predecessorPayloadSha256: string;
}>;

export type CompleteCanonicalReplayDecisionV1 = {
  readonly replayVersion: typeof COMPLETE_CANONICAL_REPLAY_V1;
  readonly launchScope: readonly FraudProofCatalogueCategoryName[];
  readonly headerHash: string;
  readonly payloadEnvelopeSha256: string;
  readonly payloadSha256: string;
  readonly context: CompleteCanonicalReplayContextIdentityV1 | null;
  readonly detections: readonly CanonicalViolationDetectionV1[];
};

export interface CompleteCanonicalReplayV1 {
  readonly replayVersion: typeof COMPLETE_CANONICAL_REPLAY_V1;
  readonly launchScope: readonly FraudProofCatalogueCategoryName[];
  replay(
    evidence: CanonicalBlockEvidenceV1,
    context?: CompleteCanonicalReplayContextV1,
  ): Promise<CompleteCanonicalReplayDecisionV1>;
}

const admittedReplayers = new WeakSet<object>();
const admittedDecisions = new WeakSet<object>();
const predecessorEvidenceByAuthority = new WeakMap<
  object,
  CanonicalBlockEvidenceV1
>();

const predecessorRecord = (
  value: unknown,
  label: string,
): Readonly<Record<string, unknown>> => {
  if (
    typeof value !== "object" ||
    value === null ||
    Array.isArray(value) ||
    Object.getPrototypeOf(value) !== Object.prototype ||
    Reflect.ownKeys(value).length !== Object.keys(value).length
  ) {
    throw new Error(`${label} must be a plain string-keyed object`);
  }
  return value as Readonly<Record<string, unknown>>;
};

/**
 * Re-admits exact untrusted predecessor bytes through the canonical L1/DA
 * evidence constructor and returns a non-revivable replay authority.
 */
export const admitCompleteCanonicalReplayPredecessorV1 = async ({
  value,
  currentEvidence,
  minimumConfirmationDepth,
}: {
  readonly value: unknown;
  readonly currentEvidence: CanonicalBlockEvidenceV1;
  readonly minimumConfirmationDepth: number;
}): Promise<CompleteCanonicalReplayPredecessorV1> => {
  const parsed = predecessorRecord(value, "raw predecessor context");
  if (
    Object.keys(parsed).sort().join(",") !==
    "daProvenance,observation,payloadEnvelopeCborHex"
  ) {
    throw new Error("raw predecessor context has missing or unknown fields");
  }
  if (
    typeof parsed.payloadEnvelopeCborHex !== "string" ||
    !/^(?:[0-9a-f]{2})+$/u.test(parsed.payloadEnvelopeCborHex)
  ) {
    throw new Error(
      "raw predecessor payload envelope must be canonical lowercase byte hex",
    );
  }
  const predecessor = await canonicalBlockEvidenceFromVerifiedPayloadV1({
    observation:
      parsed.observation as AuthenticatedStateQueueHeaderObservationV1,
    payloadEnvelopeCbor: Buffer.from(parsed.payloadEnvelopeCborHex, "hex"),
    daProvenance: parsed.daProvenance as EvidenceProvenanceV1,
    minimumConfirmationDepth,
  });
  if (
    currentEvidence.header.prevHeaderHash !== predecessor.headerHash ||
    currentEvidence.header.prevUtxosRoot !== predecessor.header.utxosRoot
  ) {
    throw new Error(
      "raw predecessor does not match the challenged header's prev_header_hash and prev_utxos_root",
    );
  }
  const authority: CompleteCanonicalReplayPredecessorV1 = Object.freeze({
    schemaVersion: COMPLETE_CANONICAL_REPLAY_PREDECESSOR_V1,
    challengedHeaderHash: currentEvidence.headerHash,
    headerHash: predecessor.headerHash,
    payloadEnvelopeSha256: predecessor.payloadEnvelopeSha256,
    payloadSha256: predecessor.payloadSha256,
  });
  predecessorEvidenceByAuthority.set(authority, predecessor);
  return authority;
};

const requireReplayPredecessorEvidenceV1 = ({
  evidence,
  context,
}: {
  readonly evidence: CanonicalBlockEvidenceV1;
  readonly context: CompleteCanonicalReplayContextV1 | undefined;
}): CanonicalBlockEvidenceV1 | undefined => {
  if (context?.predecessor === undefined) return undefined;
  const predecessor = predecessorEvidenceByAuthority.get(context.predecessor);
  if (
    predecessor === undefined ||
    context.predecessor.schemaVersion !==
      COMPLETE_CANONICAL_REPLAY_PREDECESSOR_V1 ||
    context.predecessor.challengedHeaderHash !== evidence.headerHash
  ) {
    throw new Error(
      "complete replay predecessor was not admitted for this challenged header",
    );
  }
  return predecessor;
};

/**
 * Returns only the exact predecessor evidence retained behind an admitted
 * replay authority. Production family preparers use this instead of accepting
 * caller-supplied predecessor payloads or reconstructed ledger roots.
 */
export const completeCanonicalReplayPredecessorEvidenceV1 = ({
  evidence,
  context,
}: {
  readonly evidence: CanonicalBlockEvidenceV1;
  readonly context: CompleteCanonicalReplayContextV1 | undefined;
}): CanonicalBlockEvidenceV1 | undefined =>
  requireReplayPredecessorEvidenceV1({ evidence, context });

type CanonicalReplayJsonV1 =
  | null
  | string
  | readonly CanonicalReplayJsonV1[]
  | { readonly [key: string]: CanonicalReplayJsonV1 };

const canonicalizeReplayJsonV1 = (
  value: CanonicalReplayJsonV1,
): CanonicalReplayJsonV1 => {
  if (Array.isArray(value)) return value.map(canonicalizeReplayJsonV1);
  if (typeof value !== "object" || value === null) return value;
  return Object.freeze(
    Object.fromEntries(
      Object.entries(value)
        .sort(([left], [right]) => (left < right ? -1 : left > right ? 1 : 0))
        .map(([key, child]) => [key, canonicalizeReplayJsonV1(child)]),
    ),
  );
};

const replayDecisionJsonV1 = (
  replay: CompleteCanonicalReplayDecisionV1,
): CanonicalReplayJsonV1 => ({
  replayVersion: replay.replayVersion,
  launchScope: replay.launchScope,
  headerHash: replay.headerHash,
  payloadEnvelopeSha256: replay.payloadEnvelopeSha256,
  payloadSha256: replay.payloadSha256,
  context: replay.context,
  detections: replay.detections.map((detection) => ({
    detectionId: detection.detectionId,
    headerHash: detection.headerHash,
    violationId: detection.violationId,
    position: detection.position.toString(),
    diagnostic: detection.diagnostic ?? null,
  })),
});

const outputReferenceKey = (input: {
  readonly transactionId: string;
  readonly outputIndex: bigint;
}): string => `${input.transactionId}#${input.outputIndex.toString()}`;

const detectDoubleSpends = async (
  evidence: CanonicalBlockEvidenceV1,
): Promise<readonly CanonicalViolationDetectionV1[]> => {
  const transactions = await Promise.all(
    evidence.transactions.map(decodeTransactionMaterial),
  );
  const firstSpendByInput = new Map<
    string,
    { readonly transactionIndex: number; readonly transactionId: string }
  >();
  const detections: CanonicalViolationDetectionV1[] = [];
  for (const [transactionIndex, transaction] of transactions.entries()) {
    const seenInTransaction = new Set<string>();
    for (const [inputIndex, input] of transaction.inputs.entries()) {
      const inputKey = outputReferenceKey(input);
      if (seenInTransaction.has(inputKey)) continue;
      seenInTransaction.add(inputKey);
      const first = firstSpendByInput.get(inputKey);
      if (first === undefined) {
        firstSpendByInput.set(inputKey, {
          transactionIndex,
          transactionId: transaction.nodeTxId,
        });
        continue;
      }
      if (first.transactionId === transaction.nodeTxId) continue;
      detections.push({
        detectionId: [
          DOUBLE_SPEND_VIOLATION_ID_V1,
          first.transactionIndex.toString(),
          transactionIndex.toString(),
          inputIndex.toString(),
          inputKey,
        ].join(":"),
        headerHash: evidence.headerHash,
        violationId: DOUBLE_SPEND_VIOLATION_ID_V1,
        position: BigInt(transactionIndex),
        diagnostic: `transactions ${first.transactionId} and ${transaction.nodeTxId} spend ${inputKey}`,
      });
    }
  }
  return detections;
};

const PREDECESSOR_CONTEXT_UNAVAILABLE_VIOLATION_ID_V1 =
  "authenticated-predecessor-context-unavailable" as const;

const predecessorLedgerKeysV1 = ({
  evidence,
  context,
}: {
  readonly evidence: CanonicalBlockEvidenceV1;
  readonly context: CompleteCanonicalReplayContextV1 | undefined;
}): ReadonlySet<string> | null => {
  if (evidence.header.prevUtxosRoot === EMPTY_MERKLE_TREE_ROOT) {
    if (context?.predecessor !== undefined) {
      throw new Error(
        "genesis-ledger replay received a predecessor that the challenged header does not commit",
      );
    }
    return new Set<string>();
  }
  const predecessor = requireReplayPredecessorEvidenceV1({
    evidence,
    context,
  });
  if (predecessor === undefined) return null;
  if (
    evidence.header.prevHeaderHash !== predecessor.headerHash ||
    evidence.header.prevUtxosRoot !== predecessor.header.utxosRoot
  ) {
    throw new Error(
      "canonical replay predecessor differs from the challenged prev_header_hash or prev_utxos_root",
    );
  }
  return new Set(
    predecessor.reconstruction.utxos.map((entry) =>
      Buffer.from(entry.key).toString("hex"),
    ),
  );
};

const detectLedgerRelativeMissingInputsV1 = async ({
  evidence,
  context,
  kind,
}: {
  readonly evidence: CanonicalBlockEvidenceV1;
  readonly context: CompleteCanonicalReplayContextV1 | undefined;
  readonly kind: "spend" | "reference";
}): Promise<readonly CanonicalViolationDetectionV1[]> => {
  const transactions = await Promise.all(
    evidence.transactions.map(decodeTransactionMaterial),
  );
  const currentTransactionIds = new Set(
    transactions.map((transaction) => transaction.nodeTxId),
  );
  const predecessorLedger = predecessorLedgerKeysV1({ evidence, context });
  const violationId =
    kind === "spend" ? "non-existent-input" : "no-reference-input";
  const detections: CanonicalViolationDetectionV1[] = [];
  for (const [transactionIndex, transaction] of transactions.entries()) {
    if (transaction.nativeTxCompact.validity_code !== 0n) continue;
    const inputs =
      kind === "spend" ? transaction.inputs : transaction.referenceInputs;
    for (const [inputIndex, input] of inputs.entries()) {
      if (currentTransactionIds.has(input.transactionId)) continue;
      const inputKey = ledgerKeyBytesHex({
        tx_id: input.transactionId,
        output_index: input.outputIndex,
      });
      if (predecessorLedger === null) {
        detections.push({
          detectionId: `${PREDECESSOR_CONTEXT_UNAVAILABLE_VIOLATION_ID_V1}:${kind}:${transactionIndex.toString()}:${inputIndex.toString()}:${transaction.nodeTxId}:${inputKey}`,
          headerHash: evidence.headerHash,
          violationId: PREDECESSOR_CONTEXT_UNAVAILABLE_VIOLATION_ID_V1,
          position: BigInt(transactionIndex),
          diagnostic: `${kind} input ${inputIndex.toString()} requires the exact authenticated predecessor ledger before classification`,
        });
      } else if (!predecessorLedger.has(inputKey)) {
        detections.push({
          detectionId: `${violationId}:${transactionIndex.toString()}:${inputIndex.toString()}:${transaction.nodeTxId}:${inputKey}`,
          headerHash: evidence.headerHash,
          violationId,
          position: BigInt(transactionIndex),
          diagnostic: `accepted transaction ${transaction.nodeTxId} ${kind} input ${inputIndex.toString()} is absent from both the current transaction set and predecessor ledger`,
        });
      }
    }
  }
  return detections;
};

const detectNetworkIds = (
  evidence: CanonicalBlockEvidenceV1,
): readonly CanonicalViolationDetectionV1[] =>
  evidence.transactions.flatMap((transaction, transactionIndex) =>
    findNetworkIdFaultsV1({
      evidence: {
        source: "retained-da",
        evidenceSourceId: evidence.provenance.da.sourceId,
        nativeTxCanonicalCbor: transaction.txCbor,
      },
      expectedNetworkId: evidence.header.expectedNetworkId,
    }).map((fault) => ({
      detectionId:
        fault.kind === "transaction-network"
          ? `${NETWORK_ID_VIOLATION_ID_V1}:${transactionIndex.toString()}:transaction`
          : `${NETWORK_ID_VIOLATION_ID_V1}:${transactionIndex.toString()}:output:${fault.outputIndex.toString()}`,
      headerHash: evidence.headerHash,
      violationId: NETWORK_ID_VIOLATION_ID_V1,
      position: BigInt(transactionIndex),
      diagnostic:
        fault.kind === "transaction-network"
          ? `transaction ${transaction.nodeTxId} carries the wrong explicit network id`
          : `transaction ${transaction.nodeTxId} output ${fault.outputIndex.toString()} carries the wrong network id`,
    })),
  );

const detectInvalidRanges = async (
  evidence: CanonicalBlockEvidenceV1,
): Promise<readonly CanonicalViolationDetectionV1[]> => {
  const transactions = await Promise.all(
    evidence.transactions.map(decodeTransactionMaterial),
  );
  return transactions.flatMap((transaction, transactionIndex) => {
    const normalizedRange = normalizeNativeTxValidityRange(
      transaction.nativeTxCompact.body,
    );
    const reason = invalidRangeViolationReason({
      blockSlot: evidence.header.blockSlot,
      normalizedRange,
    });
    return reason === null
      ? []
      : [
          {
            detectionId: `invalid-range:${transactionIndex.toString()}:${transaction.nodeTxId}:${reason}`,
            headerHash: evidence.headerHash,
            violationId: "invalid-range",
            position: BigInt(transactionIndex),
            diagnostic: `transaction ${transaction.nodeTxId} excludes the committed block slot: ${reason}`,
          },
        ];
  });
};

const detectZeroInputs = async (
  evidence: CanonicalBlockEvidenceV1,
): Promise<readonly CanonicalViolationDetectionV1[]> => {
  const transactions = await Promise.all(
    evidence.transactions.map(decodeTransactionMaterial),
  );
  return transactions.flatMap((transaction, transactionIndex) =>
    nativeTxBodyHasZeroInputViolation({
      txBody: transaction.nativeTxCompact.body,
    })
      ? [
          {
            detectionId: `zero-input:${transactionIndex.toString()}:${transaction.nodeTxId}`,
            headerHash: evidence.headerHash,
            violationId: "zero-input",
            position: BigInt(transactionIndex),
            diagnostic: `transaction ${transaction.nodeTxId} has no spending inputs`,
          },
        ]
      : [],
  );
};

const detectL2TxMistags = async (
  evidence: CanonicalBlockEvidenceV1,
): Promise<readonly CanonicalViolationDetectionV1[]> => {
  const transactions = await Promise.all(
    evidence.transactions.map(decodeTransactionMaterial),
  );
  return transactions.flatMap((transaction, transactionIndex) =>
    transaction.nativeTxCompact.validity_code === 1n
      ? [
          {
            detectionId: `l2-tx-mistag:${transactionIndex.toString()}:${transaction.nodeTxId}:1`,
            headerHash: evidence.headerHash,
            violationId: "l2-tx-mistag",
            position: BigInt(transactionIndex),
            diagnostic: `normal transactions-root leaf ${transaction.nodeTxId} is mistagged with validity code 1`,
          },
        ]
      : [],
  );
};

const detectMinFees = async (
  evidence: CanonicalBlockEvidenceV1,
): Promise<readonly CanonicalViolationDetectionV1[]> => {
  const transactions = await Promise.all(
    evidence.transactions.map(decodeTransactionMaterial),
  );
  return transactions.flatMap((transaction, transactionIndex) => {
    const fee = transaction.nativeTx.body.fee;
    const { minimumFee } = minimumFeeFromProofSourceV1({
      source: deriveMidgardNativeTxProofSourceV1(transaction.nativeTx),
      minFeeA: evidence.header.minFeeA,
      minFeeB: evidence.header.minFeeB,
    });
    return fee < minimumFee
      ? [
          {
            detectionId: `${MIN_FEE_VIOLATION_ID_V1}:${transactionIndex.toString()}:${transaction.nodeTxId}:${fee.toString()}:${minimumFee.toString()}`,
            headerHash: evidence.headerHash,
            violationId: MIN_FEE_VIOLATION_ID_V1,
            position: BigInt(transactionIndex),
            diagnostic: `transaction ${transaction.nodeTxId} pays ${fee.toString()} below exact minimum ${minimumFee.toString()}`,
          },
        ]
      : [];
  });
};

const detectCommittedFieldShape = (
  evidence: CanonicalBlockEvidenceV1,
): readonly CanonicalViolationDetectionV1[] =>
  evidence.transactions.flatMap((transaction, transactionIndex) => {
    const canonical = decodeMidgardNativeTxFullV1FromCanonicalCbor(
      Buffer.from(transaction.txCbor, "hex"),
    );
    return classifyCommittedFieldShapeFieldsV1(canonical)
      .filter(({ evidence: fieldEvidence }) => fieldEvidence.isViolation)
      .map(({ fieldIndex, evidence: fieldEvidence }) => {
        if (fieldEvidence.badTxId !== transaction.nodeTxId) {
          throw new Error(
            "committed-field-shape replay transaction id differs from canonical evidence",
          );
        }
        return {
          detectionId: `${COMMITTED_FIELD_SHAPE_VIOLATION_ID_V1}:${transactionIndex.toString()}:${transaction.nodeTxId}:${fieldIndex.toString()}`,
          headerHash: evidence.headerHash,
          violationId: COMMITTED_FIELD_SHAPE_VIOLATION_ID_V1,
          position: BigInt(transactionIndex),
          diagnostic: `transaction ${transaction.nodeTxId} committed malformed field ${fieldIndex.toString()}`,
        };
      });
  });

const detectCanonicalDecodability = (
  evidence: CanonicalBlockEvidenceV1,
): readonly CanonicalViolationDetectionV1[] =>
  evidence.transactions.flatMap((transaction, transactionIndex) => {
    const canonical = decodeMidgardNativeTxFullV1FromCanonicalCbor(
      Buffer.from(transaction.txCbor, "hex"),
    );
    return classifyCommittedFieldShapeFieldsV1(canonical).flatMap(
      ({ fieldIndex, preimage }) => {
        const fieldEvidence = canonicalDecodabilityEvidenceFromCommittedFieldV1(
          {
            badTxId: transaction.nodeTxId,
            fieldIndex,
            committedPreimage: preimage,
          },
        );
        return fieldEvidence.isViolation
          ? [
              {
                detectionId: `${CANONICAL_DECODABILITY_VIOLATION_ID_V1}:${transactionIndex.toString()}:${transaction.nodeTxId}:${fieldIndex.toString()}:${fieldEvidence.verdict.toString()}`,
                headerHash: evidence.headerHash,
                violationId: CANONICAL_DECODABILITY_VIOLATION_ID_V1,
                position: BigInt(transactionIndex),
                diagnostic: `transaction ${transaction.nodeTxId} committed non-canonical field ${fieldIndex.toString()} with verdict ${fieldEvidence.verdict.toString()}`,
              },
            ]
          : [];
      },
    );
  });

const canonicalSignerHashes = (
  preimageCbor: Uint8Array,
  label: string,
): readonly string[] =>
  decodeMidgardNativeByteListPreimage(preimageCbor, label).map(
    (bytes, index) => {
      if (bytes.length !== 28) {
        throw new Error(
          `${label}[${index.toString()}] is ${bytes.length.toString()} bytes, expected 28`,
        );
      }
      return Buffer.from(bytes).toString("hex");
    },
  );

/**
 * Complete Phase-A required-signer scan over every normal transaction leaf.
 *
 * The normal `transactions_root` contains the transactions the operator
 * accepted into the block. Forced transactions have their own root and their
 * adjudicated validity is handled by the forced/validation-trace surface. A
 * required key is present only when field 7 contains that exact verification
 * key hash; signature validity itself belongs to `invalidSignature`.
 */
const detectMissingSignatures = async (
  evidence: CanonicalBlockEvidenceV1,
): Promise<readonly CanonicalViolationDetectionV1[]> => {
  const transactions = await Promise.all(
    evidence.transactions.map(decodeTransactionMaterial),
  );
  return transactions.flatMap((transaction, transactionIndex) => {
    const requiredSignerHashes = canonicalSignerHashes(
      transaction.nativeTx.body.requiredSignersPreimageCbor,
      `transaction ${transaction.nodeTxId} required_signers`,
    );
    const witnessSignerHashes = new Set(
      decodeAddressWitnessPreimage(
        transaction.nativeTx.witnessSet.addrTxWitsPreimageCbor,
      ).map((witness) => missingSignatureVkeyHashV1(witness.verification_key)),
    );
    return requiredSignerHashes.flatMap((requiredSignerHash, signerIndex) =>
      witnessSignerHashes.has(requiredSignerHash)
        ? []
        : [
            {
              detectionId: `${MISSING_SIGNATURE_VIOLATION_ID_V1}:${transactionIndex.toString()}:${signerIndex.toString()}:${transaction.nodeTxId}:${requiredSignerHash}`,
              headerHash: evidence.headerHash,
              violationId: MISSING_SIGNATURE_VIOLATION_ID_V1,
              position: BigInt(transactionIndex),
              diagnostic: `accepted transaction ${transaction.nodeTxId} is missing required signer ${requiredSignerHash} at ordinal ${signerIndex.toString()}`,
            },
          ],
    );
  });
};

/**
 * Complete same-block accepted-input scan for a missing script witness.
 *
 * The proof family authenticates both the spending and producing transaction
 * through the accused header's transactions root, so only same-block
 * producers are executable by this family. The native-script preimage is
 * deliberately not accepted here: the production transaction port resolves
 * it from authenticated finalized L1 history after this detector identifies
 * the exact credential hash.
 */
const detectMissingNativeScriptTransactions = async (
  evidence: CanonicalBlockEvidenceV1,
): Promise<readonly CanonicalViolationDetectionV1[]> => {
  const transactions = await Promise.all(
    evidence.transactions.map(decodeTransactionMaterial),
  );
  const producerById = new Map(
    transactions.map((transaction, transactionIndex) => [
      transaction.nodeTxId,
      { transaction, transactionIndex },
    ]),
  );
  return transactions.flatMap((transaction, transactionIndex) => {
    if (transaction.nativeTxCompact.validity_code !== 0n) return [];
    const scriptWitnessItems = decodeMidgardNativeByteListPreimage(
      transaction.nativeTx.witnessSet.scriptTxWitsPreimageCbor,
      `transaction ${transaction.nodeTxId} script witnesses`,
    );
    return transaction.inputs.flatMap((input, inputIndex) => {
      const producer = producerById.get(input.transactionId);
      if (
        producer === undefined ||
        producer.transaction.nativeTxCompact.validity_code !== 0n
      ) {
        return [];
      }
      const outputItems = decodeMidgardNativeByteListPreimage(
        producer.transaction.nativeTx.body.outputsPreimageCbor,
        `transaction ${producer.transaction.nodeTxId} outputs`,
      );
      if (
        input.outputIndex < 0n ||
        input.outputIndex > BigInt(Number.MAX_SAFE_INTEGER)
      ) {
        return [];
      }
      const outputItem = outputItems[Number(input.outputIndex)];
      if (outputItem === undefined) return [];
      const credential = decodeMidgardAddressBytes(
        decodeMidgardTxOutput(outputItem).address,
      ).paymentCredential;
      if (credential.kind !== "Script") return [];
      const expectedScriptHash = credential.hash.toString("hex");
      if (
        !missingNativeScriptIsAbsentV1({
          scriptTxWitsItems: scriptWitnessItems,
          expectedMissingScriptHash: expectedScriptHash,
        })
      ) {
        return [];
      }
      return [
        {
          detectionId: `${MISSING_NATIVE_SCRIPT_TX_VIOLATION_ID_V1}:${transactionIndex.toString()}:${inputIndex.toString()}:${producer.transactionIndex.toString()}:${input.outputIndex.toString()}:${transaction.nodeTxId}:${producer.transaction.nodeTxId}:${expectedScriptHash}`,
          headerHash: evidence.headerHash,
          violationId: MISSING_NATIVE_SCRIPT_TX_VIOLATION_ID_V1,
          position: BigInt(transactionIndex),
          diagnostic: `accepted transaction ${transaction.nodeTxId} input ${inputIndex.toString()} spends same-block script output ${producer.transaction.nodeTxId}#${input.outputIndex.toString()} without witness ${expectedScriptHash}`,
        },
      ];
    });
  });
};

/** Complete positional scan of every committed address witness. */
const detectInvalidSignatures = async (
  evidence: CanonicalBlockEvidenceV1,
): Promise<readonly CanonicalViolationDetectionV1[]> => {
  const transactions = await Promise.all(
    evidence.transactions.map(decodeTransactionMaterial),
  );
  return transactions.flatMap((transaction, transactionIndex) =>
    decodeAddressWitnessPreimage(
      transaction.nativeTx.witnessSet.addrTxWitsPreimageCbor,
    ).flatMap((witness, witnessIndex) =>
      verifyAddressWitness({ txId: transaction.nodeTxId, witness })
        ? []
        : [
            {
              detectionId: `${INVALID_SIGNATURE_VIOLATION_ID_V1}:${transactionIndex.toString()}:${witnessIndex.toString()}:${transaction.nodeTxId}:${witness.verification_key}`,
              headerHash: evidence.headerHash,
              violationId: INVALID_SIGNATURE_VIOLATION_ID_V1,
              position: BigInt(transactionIndex),
              diagnostic: `transaction ${transaction.nodeTxId} carries invalid address witness ${witnessIndex.toString()} for verification key ${witness.verification_key}`,
            },
          ],
    ),
  );
};

/** Complete evaluation of every well-formed native witness in accepted txs. */
const detectNativeScriptInvalid = async (
  evidence: CanonicalBlockEvidenceV1,
): Promise<readonly CanonicalViolationDetectionV1[]> => {
  const transactions = await Promise.all(
    evidence.transactions.map(decodeTransactionMaterial),
  );
  return transactions.flatMap((transaction, transactionIndex) => {
    if (transaction.nativeTxCompact.validity_code !== 0n) return [];
    const signers = new Set(
      decodeMidgardAddressWitnessFieldPreimageV1(
        transaction.nativeTx.witnessSet.addrTxWitsPreimageCbor,
      ).map((witness) =>
        missingSignatureVkeyHashV1(
          Buffer.from(witness.verificationKey).toString("hex"),
        ),
      ),
    );
    const start = transaction.nativeTx.body.validityIntervalStart;
    const end = transaction.nativeTx.body.validityIntervalEnd;
    return decodeMidgardFieldPreimageV1(
      transaction.nativeTx.witnessSet.scriptTxWitsPreimageCbor,
    ).flatMap((item, scriptIndex) => {
      const script = decodeMidgardVersionedScript(item);
      if (
        script.language !== "NativeCardano" ||
        verifyMidgardNativeScript(script.nativeScript, {
          validityIntervalStart:
            start === MIDGARD_POSIX_TIME_NONE ? undefined : start,
          validityIntervalEnd:
            end === MIDGARD_POSIX_TIME_NONE ? undefined : end,
          witnessSigners: signers,
        })
      ) {
        return [];
      }
      return [
        {
          detectionId: `${NATIVE_SCRIPT_INVALID_VIOLATION_ID_V1}:${transaction.nodeTxId}:${scriptIndex.toString()}`,
          headerHash: evidence.headerHash,
          violationId: NATIVE_SCRIPT_INVALID_VIOLATION_ID_V1,
          position: BigInt(transactionIndex),
          diagnostic: `accepted transaction ${transaction.nodeTxId} carries false native witness ${scriptIndex.toString()}`,
        },
      ];
    });
  });
};

const descriptorIsBelowMinAda = (descriptorCbor: Uint8Array): boolean => {
  const descriptor = decodeMidgardLedgerOutputCommitmentV1(descriptorCbor);
  return !outputMeetsMinAdaV1(
    MIDGARD_COINS_PER_UTXO_BYTE_V1,
    BigInt(descriptor.totalLength),
    descriptor.lovelace,
  );
};

/** Complete MIN-ADA-TX and introducing-transition MIN-ADA-UTXO scan. */
const detectMinAda = async (
  evidence: CanonicalBlockEvidenceV1,
  context: CompleteCanonicalReplayContextV1 | undefined,
): Promise<readonly CanonicalViolationDetectionV1[]> => {
  const transactions = await Promise.all(
    evidence.transactions.map(decodeTransactionMaterial),
  );
  const txDetections = transactions.flatMap((transaction, transactionIndex) => {
    if (transaction.nativeTxCompact.validity_code !== 0n) return [];
    return decodeMidgardFieldPreimageV1(
      transaction.nativeTx.body.outputsPreimageCbor,
    ).flatMap((output, outputIndex) => {
      const descriptor = buildCanonicalMidgardLedgerOutputMaterialV1({
        outputIndex,
        outputCbor: output,
      }).descriptorCbor;
      return descriptorIsBelowMinAda(descriptor)
        ? [
            {
              detectionId: `${MIN_ADA_VIOLATION_ID_V1}:tx:${transaction.nodeTxId}:${outputIndex.toString()}`,
              headerHash: evidence.headerHash,
              violationId: MIN_ADA_VIOLATION_ID_V1,
              position: BigInt(transactionIndex),
              diagnostic: `accepted transaction ${transaction.nodeTxId} output ${outputIndex.toString()} is below the exact min-Ada floor`,
            },
          ]
        : [];
    });
  });
  const predecessor = requireReplayPredecessorEvidenceV1({
    evidence,
    context,
  });
  if (
    predecessor === undefined &&
    evidence.header.prevUtxosRoot !== EMPTY_MERKLE_TREE_ROOT
  ) {
    return [
      ...txDetections,
      {
        detectionId: `${PREDECESSOR_CONTEXT_UNAVAILABLE_VIOLATION_ID_V1}:min-ada-utxo:${evidence.headerHash}`,
        headerHash: evidence.headerHash,
        violationId: PREDECESSOR_CONTEXT_UNAVAILABLE_VIOLATION_ID_V1,
        position: BigInt(transactions.length),
        diagnostic:
          "MIN-ADA-UTXO classification requires the exact authenticated predecessor ledger",
      },
    ];
  }
  const predecessorKeys = new Set(
    (predecessor?.reconstruction.utxos ?? []).map((entry) =>
      Buffer.from(entry.key).toString("hex"),
    ),
  );
  const utxoDetections = evidence.reconstruction.utxos.flatMap(
    (entry, index) => {
      const key = Buffer.from(entry.key).toString("hex");
      if (predecessorKeys.has(key)) return [];
      const material = buildCanonicalMidgardLedgerEntryOutputMaterialV1({
        outRef: entry.key,
        outputCbor: entry.value,
      });
      if (!descriptorIsBelowMinAda(material.descriptorCbor)) return [];
      const outRef = decodeMidgardSpendInputItemV1(entry.key);
      const transactionId = Buffer.from(outRef.txId).toString("hex");
      return [
        {
          detectionId: `${MIN_ADA_VIOLATION_ID_V1}:utxo:${transactionId}:${outRef.outputIndex.toString()}`,
          headerHash: evidence.headerHash,
          violationId: MIN_ADA_VIOLATION_ID_V1,
          position: BigInt(transactions.length + index),
          diagnostic: `post-state UTxO ${transactionId}#${outRef.outputIndex.toString()} was introduced below the exact min-Ada floor`,
        },
      ];
    },
  );
  return [...txDetections, ...utxoDetections];
};

const detectInputNoIdxViolations = async (
  evidence: CanonicalBlockEvidenceV1,
): Promise<readonly CanonicalViolationDetectionV1[]> =>
  (
    await detectInputNoIdxViolationsFromTransactionsV1(evidence.transactions)
  ).map((detection) => ({
    detectionId: `${INPUT_NO_IDX_VIOLATION_ID_V1}:${detection.badTxIndex.toString()}:${detection.badInputsIndex.toString()}:${detection.badTxId}:${detection.producingTxId}:${detection.badInputOutputIndex.toString()}:${detection.producingTxOutputCount.toString()}`,
    headerHash: evidence.headerHash,
    violationId: INPUT_NO_IDX_VIOLATION_ID_V1,
    position: BigInt(detection.badTxIndex),
    diagnostic: `transaction ${detection.badTxId} input ${detection.badInputsIndex.toString()} names output ${detection.badInputOutputIndex.toString()} beyond same-block producer ${detection.producingTxId}'s ${detection.producingTxOutputCount.toString()} outputs`,
  }));

const detectInputSetUniqueness = async (
  evidence: CanonicalBlockEvidenceV1,
): Promise<readonly CanonicalViolationDetectionV1[]> => {
  const transactions = await Promise.all(
    evidence.transactions.map(decodeTransactionMaterial),
  );
  return transactions.flatMap((transaction, transactionIndex) => {
    if (transaction.nativeTxCompact.validity_code !== 0n) return [];
    const spendInputItemCbors = decodeMidgardNativeByteListPreimage(
      transaction.nativeTx.body.spendInputsPreimageCbor,
      `transaction ${transaction.nodeTxId} spend inputs`,
    ).map((item) => Buffer.from(item).toString("hex"));
    const referenceInputItemCbors = decodeMidgardNativeByteListPreimage(
      transaction.nativeTx.body.referenceInputsPreimageCbor,
      `transaction ${transaction.nodeTxId} reference inputs`,
    ).map((item) => Buffer.from(item).toString("hex"));
    const [claim] = scanInputSetUniquenessV1({
      spendInputItemCbors,
      referenceInputItemCbors,
    });
    if (claim === undefined) return [];
    const identity =
      claim.kind === "spendReferenceOverlap"
        ? `${claim.kind}:${claim.spendIndex.toString()}:${claim.referenceIndex.toString()}`
        : `${claim.kind}:${claim.firstIndex.toString()}:${claim.secondIndex.toString()}`;
    return [
      {
        detectionId: `${INPUT_SET_UNIQUENESS_VIOLATION_ID_V1}:${transactionIndex.toString()}:${transaction.nodeTxId}:${identity}`,
        headerHash: evidence.headerHash,
        violationId: INPUT_SET_UNIQUENESS_VIOLATION_ID_V1,
        position: BigInt(transactionIndex),
        diagnostic: `accepted transaction ${transaction.nodeTxId} violates input-set uniqueness via ${identity}`,
      },
    ];
  });
};

/**
 * Complete same-block scan of every accepted spend input against every
 * authenticated withdrawal leaf. Both roots are reconstructed from the one
 * retained-DA payload before this detector runs.
 */
const detectWithdrawnInputs = async (
  evidence: CanonicalBlockEvidenceV1,
): Promise<readonly CanonicalViolationDetectionV1[]> => {
  const transactions = await Promise.all(
    evidence.transactions.map(decodeTransactionMaterial),
  );
  return transactions.flatMap((transaction, transactionIndex) => {
    if (transaction.nativeTxCompact.validity_code !== 0n) return [];
    return transaction.inputs.flatMap((input, inputIndex) =>
      evidence.reconstruction.withdrawals.flatMap(
        (withdrawal, withdrawalIndex) => {
          if (
            !isWithdrawnInputViolationV1({
              input: {
                tx_id: input.transactionId,
                output_index: input.outputIndex,
              },
              withdrawal: withdrawal.value,
            })
          ) {
            return [];
          }
          return [
            {
              detectionId: `${WITHDRAWN_INPUT_VIOLATION_ID_V1}:${transactionIndex.toString()}:${inputIndex.toString()}:${withdrawalIndex.toString()}:${transaction.nodeTxId}:${committedWithdrawalKeyBytesV1(withdrawal.key)}`,
              headerHash: evidence.headerHash,
              violationId: WITHDRAWN_INPUT_VIOLATION_ID_V1,
              position: BigInt(transactionIndex),
              diagnostic: `accepted transaction ${transaction.nodeTxId} spend input ${inputIndex.toString()} consumes the valid withdrawal leaf at ordinal ${withdrawalIndex.toString()}`,
            },
          ];
        },
      ),
    );
  });
};

/** Complete same-block accepted reference-input/withdrawal intersection. */
const detectWithdrawnReferenceInputs = async (
  evidence: CanonicalBlockEvidenceV1,
): Promise<readonly CanonicalViolationDetectionV1[]> => {
  const transactions = await Promise.all(
    evidence.transactions.map(decodeTransactionMaterial),
  );
  return transactions.flatMap((transaction, transactionIndex) => {
    if (transaction.nativeTxCompact.validity_code !== 0n) return [];
    return transaction.referenceInputs.flatMap((input, inputIndex) =>
      evidence.reconstruction.withdrawals.flatMap(
        (withdrawal, withdrawalIndex) => {
          const outRef = withdrawal.value.body.l2_outref;
          if (
            withdrawal.value.validity !== "WithdrawalIsValid" ||
            input.transactionId !== outRef.transactionId ||
            input.outputIndex !== outRef.outputIndex
          ) {
            return [];
          }
          return [
            {
              detectionId: `${WITHDRAWN_REFERENCE_INPUT_VIOLATION_ID_V1}:${transactionIndex.toString()}:${inputIndex.toString()}:${withdrawalIndex.toString()}:${transaction.nodeTxId}:${committedWithdrawalKeyBytesV1(withdrawal.key)}`,
              headerHash: evidence.headerHash,
              violationId: WITHDRAWN_REFERENCE_INPUT_VIOLATION_ID_V1,
              position: BigInt(transactionIndex),
              diagnostic: `accepted transaction ${transaction.nodeTxId} reference input ${inputIndex.toString()} names the valid withdrawal leaf at ordinal ${withdrawalIndex.toString()}`,
            },
          ];
        },
      ),
    );
  });
};

const detectReferenceInputNoIdxViolations = (
  evidence: CanonicalBlockEvidenceV1,
): readonly CanonicalViolationDetectionV1[] =>
  detectReferenceInputNoIdxViolationsFromTransactionsV1(
    evidence.transactions,
  ).map((detection) => ({
    detectionId: `${REFERENCE_INPUT_NO_IDX_VIOLATION_ID_V1}:${detection.badTxIndex.toString()}:${detection.badReferenceInputIndex.toString()}:${detection.badTxId}:${detection.producingTxId}:${detection.badReferenceInputOutputIndex.toString()}:${detection.producingTxOutputCount.toString()}`,
    headerHash: evidence.headerHash,
    violationId: REFERENCE_INPUT_NO_IDX_VIOLATION_ID_V1,
    position: BigInt(detection.badTxIndex),
    diagnostic: `transaction ${detection.badTxId} reference input ${detection.badReferenceInputIndex.toString()} names output ${detection.badReferenceInputOutputIndex.toString()} beyond same-block producer ${detection.producingTxId}'s ${detection.producingTxOutputCount.toString()} outputs`,
  }));

const sameOutputReference = (
  left: {
    readonly transactionId: string;
    readonly outputIndex: bigint;
  },
  right: {
    readonly transactionId: string;
    readonly outputIndex: bigint;
  },
): boolean =>
  left.transactionId.toLowerCase() === right.transactionId.toLowerCase() &&
  left.outputIndex === right.outputIndex;

/**
 * Complete same-block scan for two distinct payable withdrawal leaves which
 * drain the same L2 output. The reconstruction has already re-admitted every
 * leaf from public DA against the L1-committed counted withdrawals root.
 */
const detectDoubleWithdraws = (
  evidence: CanonicalBlockEvidenceV1,
): readonly CanonicalViolationDetectionV1[] => {
  const withdrawals = evidence.reconstruction.withdrawals;
  const detections: CanonicalViolationDetectionV1[] = [];
  for (let firstIndex = 0; firstIndex < withdrawals.length; firstIndex += 1) {
    const first = withdrawals[firstIndex]!;
    if (!isPayableWithdrawalLeafV1(first.value)) continue;
    for (
      let secondIndex = firstIndex + 1;
      secondIndex < withdrawals.length;
      secondIndex += 1
    ) {
      const second = withdrawals[secondIndex]!;
      if (
        !isPayableWithdrawalLeafV1(second.value) ||
        sameOutputReference(first.key, second.key) ||
        !sameOutputReference(
          first.value.body.l2_outref,
          second.value.body.l2_outref,
        )
      ) {
        continue;
      }
      const firstKey = committedWithdrawalKeyBytesV1(first.key);
      const secondKey = committedWithdrawalKeyBytesV1(second.key);
      detections.push({
        detectionId: `${DOUBLE_WITHDRAW_VIOLATION_ID_V1}:${firstIndex.toString()}:${secondIndex.toString()}:${firstKey}:${secondKey}`,
        headerHash: evidence.headerHash,
        violationId: DOUBLE_WITHDRAW_VIOLATION_ID_V1,
        position: BigInt(secondIndex),
        diagnostic: `withdrawal leaves ${firstIndex.toString()} and ${secondIndex.toString()} are both payable for the same L2 output`,
      });
    }
  }
  return detections;
};

const completeReplayer = (
  launchScope: readonly FraudProofCatalogueCategoryName[],
  replay: (
    evidence: CanonicalBlockEvidenceV1,
    context: CompleteCanonicalReplayContextV1 | undefined,
  ) => Promise<readonly CanonicalViolationDetectionV1[]>,
): CompleteCanonicalReplayV1 => {
  const frozenScope = Object.freeze([...launchScope]);
  const replayer: CompleteCanonicalReplayV1 = Object.freeze({
    replayVersion: COMPLETE_CANONICAL_REPLAY_V1,
    launchScope: frozenScope,
    replay: async (
      evidence: CanonicalBlockEvidenceV1,
      context?: CompleteCanonicalReplayContextV1,
    ) => {
      const predecessor = requireReplayPredecessorEvidenceV1({
        evidence,
        context,
      });
      const contextIdentity =
        predecessor === undefined
          ? null
          : Object.freeze({
              predecessorHeaderHash: predecessor.headerHash,
              predecessorPayloadEnvelopeSha256:
                predecessor.payloadEnvelopeSha256,
              predecessorPayloadSha256: predecessor.payloadSha256,
            });
      const detections = Object.freeze(
        (await replay(evidence, context)).map((detection) =>
          Object.freeze({ ...detection }),
        ),
      );
      const decision: CompleteCanonicalReplayDecisionV1 = Object.freeze({
        replayVersion: COMPLETE_CANONICAL_REPLAY_V1,
        launchScope: frozenScope,
        headerHash: evidence.headerHash,
        payloadEnvelopeSha256: evidence.payloadEnvelopeSha256,
        payloadSha256: evidence.payloadSha256,
        context: contextIdentity,
        detections,
      });
      admittedDecisions.add(decision);
      return decision;
    },
  });
  admittedReplayers.add(replayer);
  return replayer;
};

/** Runtime admission for application-installed replay bundles. */
export const requireCompleteCanonicalReplayBundleV1 = (
  replayer: CompleteCanonicalReplayV1,
): readonly FraudProofCatalogueCategoryName[] => {
  if (
    !admittedReplayers.has(replayer) ||
    replayer.replayVersion !== COMPLETE_CANONICAL_REPLAY_V1
  ) {
    throw new Error(
      "production workflow requires a closed canonical replay bundle",
    );
  }
  return replayer.launchScope;
};

/** Complete replay for the constrained double-spend family surface. */
export const DOUBLE_SPEND_COMPLETE_CANONICAL_REPLAY_V1 = completeReplayer(
  ["doubleSpend"],
  detectDoubleSpends,
);

/** Complete accepted spend-input scan against current and predecessor state. */
export const NON_EXISTENT_INPUT_COMPLETE_CANONICAL_REPLAY_V1 = completeReplayer(
  ["nonExistentInput"],
  async (evidence, context) =>
    await detectLedgerRelativeMissingInputsV1({
      evidence,
      context,
      kind: "spend",
    }),
);

/** Complete replay for every transaction/output covered by the Q35 family. */
export const NETWORK_ID_COMPLETE_CANONICAL_REPLAY_V1 = completeReplayer(
  ["networkId"],
  async (evidence) => detectNetworkIds(evidence),
);

/** Complete replay for the two-step invalid-range family. */
export const INVALID_RANGE_COMPLETE_CANONICAL_REPLAY_V1 = completeReplayer(
  ["invalidRange"],
  detectInvalidRanges,
);

/** Complete replay for the two-step zero-input family. */
export const ZERO_INPUT_COMPLETE_CANONICAL_REPLAY_V1 = completeReplayer(
  ["zeroInput"],
  detectZeroInputs,
);

/** Complete accepted reference-input scan against current and predecessor state. */
export const NO_REFERENCE_INPUT_COMPLETE_CANONICAL_REPLAY_V1 = completeReplayer(
  ["noReferenceInput"],
  async (evidence, context) =>
    await detectLedgerRelativeMissingInputsV1({
      evidence,
      context,
      kind: "reference",
    }),
);

/**
 * Complete Q44 absence decision after canonical reconstruction succeeds. A
 * malformed source leaf is routed before this replay by the authenticated raw
 * source-leaf branch; reaching canonical evidence proves every source leaf was
 * canonical and key/body-id bound, so the closed detector result is empty.
 */
export const DA_HASH_PREIMAGE_COMPLETE_CANONICAL_REPLAY_V1 = completeReplayer(
  ["daHashPreimage"],
  async () => [],
);

/** Complete replay for all nine committed native-transaction field slots. */
export const COMMITTED_FIELD_SHAPE_COMPLETE_CANONICAL_REPLAY_V1 =
  completeReplayer(["committedFieldShape"], async (evidence) =>
    detectCommittedFieldShape(evidence),
  );

/** Complete total-envelope scan over all nine fields of every transaction. */
export const CANONICAL_DECODABILITY_COMPLETE_CANONICAL_REPLAY_V1 =
  completeReplayer(["canonicalDecodability"], async (evidence) =>
    detectCanonicalDecodability(evidence),
  );

/** Complete required-signer scan of every committed accepted transaction. */
export const MISSING_SIGNATURE_COMPLETE_CANONICAL_REPLAY_V1 = completeReplayer(
  ["missingSignature"],
  detectMissingSignatures,
);

/** Complete same-block missing-script-witness scan for every accepted input. */
export const MISSING_NATIVE_SCRIPT_TX_COMPLETE_CANONICAL_REPLAY_V1 =
  completeReplayer(
    ["missingNativeScriptTx"],
    detectMissingNativeScriptTransactions,
  );

/**
 * Q33 is history-relative: a script credential alone cannot prove that the
 * preimage is native. This factory admits only the complete retained-history
 * capability derived for the exact challenged block.
 */
export const createMissingNativeScriptUtxoCompleteCanonicalReplayV1 = (
  corpus: ProductionHistoricalNativeScriptCorpusV1,
): CompleteCanonicalReplayV1 =>
  completeReplayer(
    ["missingNativeScriptUtxo"],
    async (evidence) =>
      await detectMissingNativeScriptUtxoFromHistoricalCorpusV1({
        evidence,
        corpus,
      }),
  );

/** Complete Ed25519 verification of every committed address witness. */
export const INVALID_SIGNATURE_COMPLETE_CANONICAL_REPLAY_V1 = completeReplayer(
  ["invalidSignature"],
  detectInvalidSignatures,
);

/**
 * Complete committed-deposit scan with each candidate classified against the
 * concrete public L1 authority. The returned replayer is opaque-admitted like
 * every fixed replay bundle; structural evidence-authority copies are refused.
 */
export const createFabricatedDepositCompleteCanonicalReplayV1 = ({
  authority,
  owner,
}: {
  readonly authority: ProductionFabricatedDepositEvidenceAuthorityV1;
  readonly owner: string;
}): CompleteCanonicalReplayV1 => {
  const admitted =
    requireProductionFabricatedDepositEvidenceAuthorityV1(authority);
  return completeReplayer(["fabricatedDeposit"], async (evidence) =>
    (await admitted.detect(evidence, owner)).map(({ detection }) => detection),
  );
};

/** Complete committed-withdrawal scan against the concrete public L1 authority. */
export const createFabricatedWithdrawalCompleteCanonicalReplayV1 = ({
  authority,
  owner,
}: {
  readonly authority: ProductionFabricatedWithdrawalEvidenceAuthorityV1;
  readonly owner: string;
}): CompleteCanonicalReplayV1 => {
  const admitted =
    requireProductionFabricatedWithdrawalEvidenceAuthorityV1(authority);
  return completeReplayer(["fabricatedWithdrawal"], async (evidence) =>
    (await admitted.detect(evidence, owner)).map(({ detection }) => detection),
  );
};

/** Complete evaluation of all accepted native script witnesses. */
export const NATIVE_SCRIPT_INVALID_COMPLETE_CANONICAL_REPLAY_V1 =
  completeReplayer(["nativeScriptInvalid"], detectNativeScriptInvalid);

/** Complete transaction-output and introducing post-state min-Ada scan. */
export const MIN_ADA_COMPLETE_CANONICAL_REPLAY_V1 = completeReplayer(
  ["minAda"],
  detectMinAda,
);

/** Complete Q27 replay backed by the same checkpointed predecessor authority as Q33. */
export const createMinAdaCompleteCanonicalReplayFromHistoricalCorpusV1 = (
  corpus: ProductionHistoricalNativeScriptCorpusV1,
): CompleteCanonicalReplayV1 =>
  completeReplayer(["minAda"], async (evidence) => [
    ...(await detectMinAda(evidence, undefined)).filter(
      (detection) => detection.violationId === MIN_ADA_VIOLATION_ID_V1,
    ),
    ...detectMinAdaUtxoFromHistoricalCorpusV1({ evidence, corpus }),
  ]);

/** Complete same-block input/producer output-count scan. */
export const INPUT_NO_IDX_COMPLETE_CANONICAL_REPLAY_V1 = completeReplayer(
  ["nonExistentInputNoIndex"],
  detectInputNoIdxViolations,
);

/** Complete same-block reference-input/producer output-count scan. */
export const REFERENCE_INPUT_NO_IDX_COMPLETE_CANONICAL_REPLAY_V1 =
  completeReplayer(["referenceInputNoIdx"], async (evidence) =>
    detectReferenceInputNoIdxViolations(evidence),
  );

/** Complete scan of every committed withdrawal pair in the accused block. */
export const DOUBLE_WITHDRAW_COMPLETE_CANONICAL_REPLAY_V1 = completeReplayer(
  ["doubleWithdraw"],
  async (evidence) => detectDoubleWithdraws(evidence),
);

/** Complete scan of every normal transactions-root leaf for code-1 mistags. */
export const L2_TX_MISTAG_COMPLETE_CANONICAL_REPLAY_V1 = completeReplayer(
  ["l2TxMistag"],
  detectL2TxMistags,
);

/** Complete exact-size/header-schedule fee scan of every transaction leaf. */
export const MIN_FEE_COMPLETE_CANONICAL_REPLAY_V1 = completeReplayer(
  ["minFee"],
  detectMinFees,
);

/** Complete input-set scan for every accepted transaction leaf. */
export const INPUT_SET_UNIQUENESS_COMPLETE_CANONICAL_REPLAY_V1 =
  completeReplayer(["inputSetUniqueness"], detectInputSetUniqueness);

/** Complete accepted-spend/withdrawal intersection scan for one block. */
export const WITHDRAWN_INPUT_COMPLETE_CANONICAL_REPLAY_V1 = completeReplayer(
  ["withdrawnInput"],
  detectWithdrawnInputs,
);

/** Complete accepted-reference/withdrawal intersection scan for one block. */
export const WITHDRAWN_REFERENCE_INPUT_COMPLETE_CANONICAL_REPLAY_V1 =
  completeReplayer(["withdrawnReferenceInput"], detectWithdrawnReferenceInputs);

/** Closed union used once both family adapters are launch-scope complete. */
export const DOUBLE_SPEND_NETWORK_ID_COMPLETE_CANONICAL_REPLAY_V1 =
  completeReplayer(["doubleSpend", "networkId"], async (evidence) => [
    ...(await detectDoubleSpends(evidence)),
    ...detectNetworkIds(evidence),
  ]);

/**
 * Builds one closed replay bundle from already-admitted complete family
 * replayers. The application installs this object, rather than a list of
 * detector callbacks, so no caller can omit a family or inject a partial scan
 * after deployment composition. Categories must be disjoint and in the
 * append-only catalogue order.
 */
export const createCompleteCanonicalReplayUnionV1 = (
  members: readonly CompleteCanonicalReplayV1[],
): CompleteCanonicalReplayV1 => {
  if (members.length === 0) {
    throw new Error("complete replay union must contain at least one member");
  }
  const categories: FraudProofCatalogueCategoryName[] = [];
  const seen = new Set<FraudProofCatalogueCategoryName>();
  for (const member of members) {
    const memberScope = requireCompleteCanonicalReplayBundleV1(member);
    for (const category of memberScope) {
      if (seen.has(category)) {
        throw new Error(`complete replay union duplicates ${category}`);
      }
      seen.add(category);
      categories.push(category);
    }
  }
  const canonical = [...categories].sort(
    (left, right) =>
      FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.indexOf(left) -
      FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.indexOf(right),
  );
  if (canonical.some((category, index) => category !== categories[index])) {
    throw new Error(
      "complete replay union members are not in canonical catalogue order",
    );
  }
  return completeReplayer(
    Object.freeze(categories),
    async (evidence, context) => {
      const detections: CanonicalViolationDetectionV1[] = [];
      for (const member of members) {
        const decision = await member.replay(evidence, context);
        detections.push(
          ...requireCompleteCanonicalReplayDecisionV1({
            evidence,
            replayer: member,
            decision,
            ...(context === undefined ? {} : { context }),
          }),
        );
      }
      return detections;
    },
  );
};

/** Rejects caller-authored or partial-detector decisions at runtime. */
export const requireCompleteCanonicalReplayDecisionV1 = ({
  evidence,
  replayer,
  decision,
  context,
}: {
  readonly evidence: CanonicalBlockEvidenceV1;
  readonly replayer: CompleteCanonicalReplayV1;
  readonly decision: CompleteCanonicalReplayDecisionV1;
  readonly context?: CompleteCanonicalReplayContextV1;
}): readonly CanonicalViolationDetectionV1[] => {
  requireCompleteCanonicalReplayBundleV1(replayer);
  if (!admittedDecisions.has(decision)) {
    throw new Error(
      "canonical replay decision was not produced by the closed replay bundle",
    );
  }
  const predecessor = requireReplayPredecessorEvidenceV1({
    evidence,
    context,
  });
  const expectedContext =
    predecessor === undefined
      ? null
      : {
          predecessorHeaderHash: predecessor.headerHash,
          predecessorPayloadEnvelopeSha256: predecessor.payloadEnvelopeSha256,
          predecessorPayloadSha256: predecessor.payloadSha256,
        };
  if (
    decision.replayVersion !== COMPLETE_CANONICAL_REPLAY_V1 ||
    decision.launchScope !== replayer.launchScope ||
    decision.headerHash !== evidence.headerHash ||
    decision.payloadEnvelopeSha256 !== evidence.payloadEnvelopeSha256 ||
    decision.payloadSha256 !== evidence.payloadSha256 ||
    JSON.stringify(decision.context) !== JSON.stringify(expectedContext)
  ) {
    throw new Error(
      "canonical replay decision does not bind its bundle and fetched evidence",
    );
  }
  return decision.detections;
};

/**
 * Digest of a module-admitted complete replay decision. The admission check is
 * part of this operation so a structural/caller-authored decision can never
 * mint the digest used by production capture artifacts.
 */
export const completeCanonicalReplayDecisionDigestV1 = ({
  evidence,
  replayer,
  decision,
  context,
}: {
  readonly evidence: CanonicalBlockEvidenceV1;
  readonly replayer: CompleteCanonicalReplayV1;
  readonly decision: CompleteCanonicalReplayDecisionV1;
  readonly context?: CompleteCanonicalReplayContextV1;
}): string => {
  requireCompleteCanonicalReplayDecisionV1({
    evidence,
    replayer,
    decision,
    ...(context === undefined ? {} : { context }),
  });
  return createHash("sha256")
    .update(
      JSON.stringify(canonicalizeReplayJsonV1(replayDecisionJsonV1(decision))),
    )
    .digest("hex");
};
