import { Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  assertMidgardCekProgramMaterialBundleV1,
  decodeMidgardCekProgramMaterialDaEntryV1,
  encodeMidgardCekProgramEnvelopeV1,
  type MidgardCekProgramEnvelopeV1,
} from "@al-ft/midgard-core/cek-proof";
import {
  computeMidgardNativeTxIdV1,
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  deriveMidgardNativeTxProofSourceV1,
} from "@al-ft/midgard-core/codec";
import { readCborBytes, readCborInteger } from "@al-ft/midgard-core/codec/cbor";
import {
  MIDGARD_CONSENSUS_LIMITS_V1,
  MIDGARD_PROTOCOL_V1_VERSION,
  MIDGARD_TRANSITION_STEP_V1_SCHEMA_VERSION,
} from "@al-ft/midgard-core/consensus-profile-v1";
import { validateMidgardConsensusV1TxCbor } from "@al-ft/midgard-core/consensus-validation-v1";
import {
  type DaPayloadEnvelopeTimingStage,
  unwrapDaPayloadV1,
} from "@al-ft/midgard-core/da-payload-envelope";
import { DA_TRANSPORT_LIMITS_V1 } from "@al-ft/midgard-core/da-transport";
import {
  collectMidgardV1AttachedProgramEnvelopes,
  collectMidgardV1ReferencedProgramEnvelopes,
} from "@al-ft/midgard-core/script-proof";
import { decodeMidgardValidationTraceDescriptorV1 } from "@al-ft/midgard-core/validation-trace";
import * as SDK from "@al-ft/midgard-sdk";
import { buildCanonicalMidgardLedgerEntryOutputMaterialV1 } from "@al-ft/midgard-validation";
import { Data as LucidData } from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";
import { sha256 } from "@noble/hashes/sha2.js";
import { Effect } from "effect";

import type {
  PayloadCountSet,
  PayloadRootSet,
  ValidationSummary,
} from "../domain.js";
import { bytesToHex, hexToBytes, normalizeHex } from "../utils/hex.js";

type DataSchema = Parameters<typeof LucidData.Nullable>[0];

export type PayloadVerificationOptions = {
  readonly payloadSchemaVersion: 1;
  readonly stateQueueOutRef: string;
  readonly timing?: DaPayloadVerificationTimingOptions;
};

export type DaPayloadVerificationTimingStage =
  | DaPayloadEnvelopeTimingStage
  | "stored_hash"
  | "inner_decode"
  | "payload_structure_validation"
  | "semantic_validation";

export type DaPayloadVerificationTimingOptions = {
  readonly monotonicNow?: () => number;
  readonly onStageTiming?: (
    stage: DaPayloadVerificationTimingStage,
    durationMs: number,
  ) => void;
};

export type DaPayloadRootSetV1 = PayloadRootSet & {
  readonly validationTracesRoot: string;
};

export type DaPayloadCountSetV1 = PayloadCountSet & {
  readonly validationTraceCount: bigint;
};

export type VerifiedDaPayloadV1 = {
  readonly payload: SDK.DaPayloadV1;
  readonly storedPayloadCbor: Buffer;
  readonly innerPayloadCbor: Buffer;
  readonly payloadSha256: string;
  readonly roots: DaPayloadRootSetV1;
  readonly counts: DaPayloadCountSetV1;
  readonly validation: Omit<
    ValidationSummary,
    "rootSummary" | "countSummary"
  > & {
    readonly rootSummary: DaPayloadRootSetV1;
    readonly countSummary: DaPayloadCountSetV1;
  };
};

export class DaPayloadValidationError extends Error {
  readonly code:
    | "malformed_da"
    | "non_canonical"
    | "wrong_version"
    | "duplicate_key"
    | "unsorted_key"
    | "header_hash_mismatch"
    | "header_mismatch"
    | "malformed_transaction"
    | "malformed_trace"
    | "unsupported_feature"
    | "consensus_bound"
    | "version_mismatch"
    | "root_mismatch"
    | "count_mismatch"
    | "coverage_mismatch";

  constructor(
    code: DaPayloadValidationError["code"],
    message: string,
    options?: ErrorOptions,
  ) {
    super(message, options);
    this.name = "DaPayloadValidationError";
    this.code = code;
  }
}

export const decodeDaPayloadV1Strict = (
  payloadCbor: Uint8Array,
  timing: DaPayloadVerificationTimingOptions = {},
): SDK.DaPayloadV1 => {
  if (payloadCbor.length > MIDGARD_CONSENSUS_LIMITS_V1.maxDaPayloadBytes) {
    throw new DaPayloadValidationError(
      "consensus_bound",
      `canonical DA payload bytes ${payloadCbor.length.toString()} exceed V1 maximum ${MIDGARD_CONSENSUS_LIMITS_V1.maxDaPayloadBytes.toString()}`,
    );
  }
  const payloadBuffer = Buffer.isBuffer(payloadCbor)
    ? payloadCbor
    : Buffer.from(payloadCbor);
  let payload: SDK.DaPayloadV1;
  const decodeStartedAt = readMonotonicNow(timing);
  try {
    payload = SDK.decodeDaPayloadV1(payloadBuffer);
  } catch (cause) {
    throw new DaPayloadValidationError(
      cause instanceof SDK.DaPayloadV1NonCanonicalError
        ? "non_canonical"
        : "malformed_da",
      cause instanceof SDK.DaPayloadV1NonCanonicalError
        ? "payload CBOR was not canonical for DaPayloadV1"
        : "failed to decode DaPayloadV1 canonical CBOR",
      { cause },
    );
  } finally {
    recordTiming(timing, "inner_decode", decodeStartedAt);
  }

  const validationStartedAt = readMonotonicNow(timing);
  try {
    if (payload.version !== SDK.DA_PAYLOAD_V1_VERSION) {
      throw new DaPayloadValidationError(
        "wrong_version",
        `expected DaPayloadV1 version ${SDK.DA_PAYLOAD_V1_VERSION.toString()}, got ${payload.version.toString()}`,
      );
    }
    const body = payload.block_body;
    normalizeHex(body.header_hash, {
      fieldName: "payload header_hash",
      byteLength: 28,
    });
    const embeddedHeaderHash = hashBlockHeaderCborV1(body.header);
    if (embeddedHeaderHash !== body.header_hash) {
      throw new DaPayloadValidationError(
        "header_hash_mismatch",
        `embedded V1 header hash ${embeddedHeaderHash} does not match payload header_hash ${body.header_hash}`,
      );
    }
    validateEntries("utxos", body.utxos);
    validateEntries("withdrawals", body.withdrawals);
    validateEntries("forced_transactions", body.forced_transactions);
    validateEntries("transactions", body.transactions);
    validateEntries("transaction_preimages", body.transaction_preimages);
    validateEntries(
      "forced_transaction_preimages",
      body.forced_transaction_preimages,
    );
    validateEntries("cek_program_material", body.cek_program_material);
    validateEntries("deposits", body.deposits);
    validateEntries("transition_trace", body.transition_trace);
    validateEntries("event_to_step", body.event_to_step);
    validateEntries("validation_traces", body.validation_traces);
    validateDaPayloadCountsV1(body.counts);
    validateDaPayloadConsensusV1(body);
    validateProofTraceCoverageV1(payload);
    return payload;
  } finally {
    recordTiming(timing, "payload_structure_validation", validationStartedAt);
  }
};

const computeDaPayloadRootsForForcedDomain = async (
  payload: SDK.DaPayloadV1,
): Promise<PayloadRootSet> => {
  const body = payload.block_body;
  const transactionValues: Buffer[] = [];
  const utxoDescriptorValues: Buffer[] = [];
  const utxoKeys: Buffer[] = [];
  for (const [outRefHex, outputHex] of body.utxos) {
    try {
      const outRef = hexToBytes(outRefHex, "utxos key");
      const outputCbor = hexToBytes(outputHex, "utxos value");
      utxoKeys.push(outRef);
      utxoDescriptorValues.push(
        buildCanonicalMidgardLedgerEntryOutputMaterialV1({
          outRef,
          outputCbor,
        }).descriptorCbor,
      );
    } catch (cause) {
      throw new DaPayloadValidationError(
        "malformed_da",
        "failed to project a full V1 UTxO to its exact canonical descriptor",
        { cause },
      );
    }
  }
  for (const [, value] of body.transactions) {
    try {
      transactionValues.push(hexToBytes(value, "tx value"));
    } catch (cause) {
      throw new DaPayloadValidationError(
        "malformed_transaction",
        "failed to project full transaction CBOR to compact root value",
        { cause },
      );
    }
  }
  const [
    utxosRoot,
    withdrawalsRoot,
    forcedTransactionsRoot,
    transactionsRoot,
    depositsRoot,
    transitionTraceRoot,
    eventToStepRoot,
  ] = await Promise.all([
    keyValuePhasRootWithValues(utxoKeys, utxoDescriptorValues),
    countedRoot(SDK.ROOT_DOMAINS.withdrawals, body.withdrawals),
    countedRoot(
      SDK.ROOT_DOMAINS.forcedTransactionsV1,
      body.forced_transactions,
    ),
    countedRootWithValues(
      SDK.ROOT_DOMAINS.transactionsV1,
      body.transactions.map(([key]) => hexToBytes(key, "tx key")),
      transactionValues,
    ),
    countedRoot(SDK.ROOT_DOMAINS.deposits, body.deposits),
    countedRoot(SDK.ROOT_DOMAINS.transitionTrace, body.transition_trace),
    countedRoot(SDK.ROOT_DOMAINS.eventToStep, body.event_to_step),
  ]);
  return {
    utxosRoot,
    withdrawalsRoot,
    forcedTransactionsRoot,
    transactionsRoot,
    depositsRoot,
    transitionTraceRoot,
    eventToStepRoot,
  };
};

export const computeDaPayloadV1Roots = async (
  payload: SDK.DaPayloadV1,
): Promise<DaPayloadRootSetV1> => {
  const proofRoots = await computeDaPayloadRootsForForcedDomain(payload);
  return {
    ...proofRoots,
    validationTracesRoot: await countedRoot(
      SDK.ROOT_DOMAINS.validationTraces,
      payload.block_body.validation_traces,
    ),
  };
};

export const verifyDaPayloadV1AgainstHeader = async (
  storedPayloadCbor: Uint8Array,
  expectedHeaderHash: string,
  header: SDK.HeaderV1,
  options: PayloadVerificationOptions,
): Promise<VerifiedDaPayloadV1> => {
  if (options.payloadSchemaVersion !== Number(SDK.DA_PAYLOAD_V1_VERSION)) {
    throw new DaPayloadValidationError(
      "wrong_version",
      `expected DA payload schema version ${SDK.DA_PAYLOAD_V1_VERSION.toString()}, got ${String(options.payloadSchemaVersion)}`,
    );
  }
  const storedPayloadBuffer = Buffer.from(storedPayloadCbor);
  const hashStartedAt = readMonotonicNow(options.timing);
  const payloadSha256 = bytesToHex(sha256(storedPayloadBuffer));
  recordTiming(options.timing, "stored_hash", hashStartedAt);
  let payloadBuffer: Buffer;
  try {
    payloadBuffer = (
      await unwrapDaPayloadV1(storedPayloadBuffer, {
        maxPayloadBytes: DA_TRANSPORT_LIMITS_V1.maxPayloadBytes,
        timing: options.timing,
      })
    ).innerBytes;
  } catch (cause) {
    throw new DaPayloadValidationError(
      "malformed_da",
      "failed to unwrap versioned DA payload bytes",
      { cause },
    );
  }
  const normalizedHeaderHash = normalizeHex(expectedHeaderHash, {
    fieldName: "expected header hash",
    byteLength: 28,
  });
  const payload = decodeDaPayloadV1Strict(payloadBuffer, options.timing);
  const semanticStartedAt = readMonotonicNow(options.timing);
  try {
    if (payload.block_body.header_hash !== normalizedHeaderHash) {
      throw new DaPayloadValidationError(
        "header_hash_mismatch",
        `payload header_hash ${payload.block_body.header_hash} does not match L1 header hash ${normalizedHeaderHash}`,
      );
    }
    if (hashBlockHeaderCborV1(header) !== normalizedHeaderHash) {
      throw new DaPayloadValidationError(
        "header_hash_mismatch",
        `L1 V1 header body does not hash to expected header_hash ${normalizedHeaderHash}`,
      );
    }
    if (
      headerCborHexV1(payload.block_body.header) !== headerCborHexV1(header)
    ) {
      throw new DaPayloadValidationError(
        "header_mismatch",
        "payload embedded V1 header does not match the L1 header",
      );
    }
    if (header.protocolVersion !== BigInt(MIDGARD_PROTOCOL_V1_VERSION)) {
      throw new DaPayloadValidationError(
        "version_mismatch",
        `V1 header protocol_version must equal ${MIDGARD_PROTOCOL_V1_VERSION.toString()}, got ${header.protocolVersion.toString()}`,
      );
    }
    const roots = await computeDaPayloadV1Roots(payload);
    const counts = payload.block_body.counts;
    const rootMismatchFields = daPayloadRootMismatchesV1(header, roots);
    if (rootMismatchFields.length > 0) {
      throw new DaPayloadValidationError(
        "root_mismatch",
        `V1 DA payload roots do not match L1 header: ${rootMismatchFields.join(",")}`,
      );
    }
    const countMismatchFields = daPayloadCountMismatchesV1(
      daPayloadHeaderCountsV1(header),
      counts,
    );
    if (countMismatchFields.length > 0) {
      throw new DaPayloadValidationError(
        "count_mismatch",
        `V1 DA payload counts do not match L1 header: ${countMismatchFields.join(",")}`,
      );
    }
    return {
      payload,
      storedPayloadCbor: storedPayloadBuffer,
      innerPayloadCbor: payloadBuffer,
      payloadSha256,
      roots,
      counts,
      validation: {
        payloadVersion: Number(payload.version),
        rootsMatch: true,
        stateQueueOutRef: options.stateQueueOutRef,
        headerHash: normalizedHeaderHash,
        rootSummary: roots,
        countSummary: counts,
        l1Header: {
          startTime: header.startTime.toString(),
          endTime: header.endTime.toString(),
          operatorVkey: header.operatorVkey,
          prevHeaderHash: header.prevHeaderHash,
          protocolVersion: header.protocolVersion.toString(),
        },
      },
    };
  } finally {
    recordTiming(options.timing, "semantic_validation", semanticStartedAt);
  }
};

export const daPayloadSha256 = (payloadCbor: Uint8Array): string =>
  bytesToHex(sha256(payloadCbor));

const readMonotonicNow = (
  timing: DaPayloadVerificationTimingOptions | undefined,
): number | undefined => {
  try {
    return (timing?.monotonicNow ?? (() => performance.now()))();
  } catch {
    return undefined;
  }
};

const recordTiming = (
  timing: DaPayloadVerificationTimingOptions | undefined,
  stage: DaPayloadVerificationTimingStage,
  startedAt: number | undefined,
): void => {
  if (startedAt === undefined) return;
  const completedAt = readMonotonicNow(timing);
  if (completedAt === undefined) return;
  try {
    timing?.onStageTiming?.(stage, completedAt - startedAt);
  } catch {
    // Observability must not change committee validation semantics.
  }
};

const validateEntries = (
  fieldName: string,
  entries: readonly SDK.DaPayloadEntry[],
): void => {
  let previousKey: string | undefined;
  for (const [index, [key, value]] of entries.entries()) {
    const normalizedKey = normalizeHex(key, {
      fieldName: `${fieldName}[${index.toString()}].key`,
    });
    normalizeHex(value, {
      fieldName: `${fieldName}[${index.toString()}].value`,
    });
    if (previousKey !== undefined) {
      if (normalizedKey === previousKey) {
        throw new DaPayloadValidationError(
          "duplicate_key",
          `${fieldName} contains duplicate key ${normalizedKey}`,
        );
      }
      if (normalizedKey < previousKey) {
        throw new DaPayloadValidationError(
          "unsorted_key",
          `${fieldName} keys must be sorted ascending`,
        );
      }
    }
    previousKey = normalizedKey;
  }
};

const validateCountsV1 = (counts: SDK.DaPayloadCountsV1): void => {
  const fields = [
    ["withdrawal_count", counts.withdrawalCount],
    ["forced_transaction_count", counts.forcedTransactionCount],
    ["l2_transaction_count", counts.l2TransactionCount],
    ["deposit_count", counts.depositCount],
    ["total_event_count", counts.totalEventCount],
    ["transition_step_count", counts.transitionStepCount],
  ] as const;
  for (const [field, count] of fields) {
    if (count < 0n) {
      throw new DaPayloadValidationError(
        "count_mismatch",
        `${field} must be non-negative`,
      );
    }
  }
  const expectedTotal =
    counts.withdrawalCount +
    counts.forcedTransactionCount +
    counts.l2TransactionCount +
    counts.depositCount;
  if (counts.totalEventCount !== expectedTotal) {
    throw new DaPayloadValidationError(
      "count_mismatch",
      `total_event_count ${counts.totalEventCount.toString()} does not match source counts ${expectedTotal.toString()}`,
    );
  }
  if (counts.transitionStepCount !== counts.totalEventCount) {
    throw new DaPayloadValidationError(
      "count_mismatch",
      "transition_step_count must equal total_event_count",
    );
  }
};

const validateDaPayloadCountsV1 = (counts: SDK.DaPayloadCountsV1): void => {
  validateCountsV1(counts);
  if (counts.validationTraceCount < 0n) {
    throw new DaPayloadValidationError(
      "count_mismatch",
      "validation_trace_count must be non-negative",
    );
  }
  const expectedValidationTraces =
    counts.forcedTransactionCount + counts.l2TransactionCount;
  if (counts.validationTraceCount !== expectedValidationTraces) {
    throw new DaPayloadValidationError(
      "count_mismatch",
      `validation_trace_count ${counts.validationTraceCount.toString()} must equal forced_transaction_count + l2_transaction_count ${expectedValidationTraces.toString()}`,
    );
  }
};

const collectProofProgramEnvelopes = (
  tx: ReturnType<typeof decodeMidgardNativeTxFullV1FromCanonicalCbor>,
  fieldName: string,
  target: Map<string, MidgardCekProgramEnvelopeV1>,
  resolvedOutputsByOutRef?: ReadonlyMap<string, Uint8Array>,
): void => {
  try {
    const envelopes = [...collectMidgardV1AttachedProgramEnvelopes(tx)];
    if (resolvedOutputsByOutRef !== undefined) {
      envelopes.push(
        ...collectMidgardV1ReferencedProgramEnvelopes(
          tx,
          resolvedOutputsByOutRef,
        ),
      );
    }
    for (const envelope of envelopes) {
      target.set(
        encodeMidgardCekProgramEnvelopeV1(envelope).toString("hex"),
        envelope,
      );
    }
  } catch (cause) {
    throw new DaPayloadValidationError(
      "malformed_transaction",
      `${fieldName} has malformed V1 program envelopes`,
      { cause },
    );
  }
};

const validateDaPayloadConsensusV1 = (body: SDK.DaPayloadBodyV1): void => {
  if (body.header.protocolVersion !== BigInt(MIDGARD_PROTOCOL_V1_VERSION)) {
    throw new DaPayloadValidationError(
      "version_mismatch",
      `embedded V1 header protocol_version must equal ${MIDGARD_PROTOCOL_V1_VERSION.toString()}, got ${body.header.protocolVersion.toString()}`,
    );
  }

  const limits = MIDGARD_CONSENSUS_LIMITS_V1;
  const countBounds = [
    [
      "withdrawal_count",
      body.counts.withdrawalCount,
      limits.maxWithdrawalCount,
    ],
    [
      "forced_transaction_count",
      body.counts.forcedTransactionCount,
      limits.maxForcedTransactionCount,
    ],
    [
      "l2_transaction_count",
      body.counts.l2TransactionCount,
      limits.maxL2TransactionCount,
    ],
    ["deposit_count", body.counts.depositCount, limits.maxDepositCount],
    [
      "total_event_count",
      body.counts.totalEventCount,
      limits.maxTotalEventCount,
    ],
    [
      "transition_step_count",
      body.counts.transitionStepCount,
      limits.maxTransitionStepCount,
    ],
    [
      "validation_trace_count",
      body.counts.validationTraceCount,
      limits.maxValidationTraceCount,
    ],
  ] as const;
  for (const [field, value, maximum] of countBounds) {
    if (value > BigInt(maximum)) {
      throw new DaPayloadValidationError(
        "consensus_bound",
        `${field} ${value.toString()} exceeds V1 maximum ${maximum.toString()}`,
      );
    }
  }

  if (body.transactions.length !== body.transaction_preimages.length) {
    throw new DaPayloadValidationError(
      "coverage_mismatch",
      "every committed normal transaction source must have exactly one canonical transaction preimage",
    );
  }
  if (
    body.forced_transactions.length !== body.forced_transaction_preimages.length
  ) {
    throw new DaPayloadValidationError(
      "coverage_mismatch",
      "every committed forced transaction source must have exactly one canonical transaction preimage",
    );
  }
  const transactionPreimages = new Map(body.transaction_preimages);
  const forcedTransactionPreimages = new Map(body.forced_transaction_preimages);
  const resolvedOutputsByOutRef = new Map(
    body.utxos.map(([outRefHex, outputHex]) => [
      normalizeHex(outRefHex, { fieldName: "utxos.key" }),
      hexToBytes(outputHex, "utxos.value"),
    ]),
  );

  let canonicalTransactionBytes = 0;
  let ledgerOperationCount = body.deposits.length;
  const programEnvelopes = new Map<string, MidgardCekProgramEnvelopeV1>();
  const validateFullTransaction = (
    txCbor: Buffer,
    fieldName: string,
  ): ReturnType<typeof decodeMidgardNativeTxFullV1FromCanonicalCbor> => {
    canonicalTransactionBytes += txCbor.length;
    let tx;
    try {
      tx = decodeMidgardNativeTxFullV1FromCanonicalCbor(txCbor);
    } catch (cause) {
      throw new DaPayloadValidationError(
        "malformed_transaction",
        `${fieldName} is not a canonical full Midgard transaction`,
        { cause },
      );
    }
    const violation = validateMidgardConsensusV1TxCbor(txCbor);
    if (violation !== null) {
      throw new DaPayloadValidationError(
        violation.code === "E_TX_SIZE" ||
        violation.code === "E_FIELD_PREIMAGE_SIZE" ||
        violation.code === "E_LEDGER_OUTPUT_SIZE"
          ? "consensus_bound"
          : "unsupported_feature",
        `${fieldName} violates proof consensus profile: ${violation.code} ${violation.featureId} ${violation.detail}`,
      );
    }
    collectProofProgramEnvelopes(
      tx,
      fieldName,
      programEnvelopes,
      resolvedOutputsByOutRef,
    );
    return tx;
  };
  const countLedgerOperations = (
    tx: ReturnType<typeof decodeMidgardNativeTxFullV1FromCanonicalCbor>,
    fieldName: string,
  ): void => {
    ledgerOperationCount +=
      decodeMidgardNativeByteListPreimage(
        tx.body.spendInputsPreimageCbor,
        `${fieldName}.spend_inputs`,
      ).length +
      decodeMidgardNativeByteListPreimage(
        tx.body.outputsPreimageCbor,
        `${fieldName}.outputs`,
      ).length;
  };
  const assertSourceBinding = (
    source: SDK.L2TransactionSourceV1,
    tx: ReturnType<typeof decodeMidgardNativeTxFullV1FromCanonicalCbor>,
    fieldName: string,
  ): string => {
    const decodedTxId = computeMidgardNativeTxIdV1(tx).toString("hex");
    const committedTxId = normalizeHex(source.tx_id, {
      fieldName: `${fieldName}.tx_id`,
      byteLength: 32,
    });
    if (committedTxId !== decodedTxId) {
      throw new DaPayloadValidationError(
        "malformed_transaction",
        `${fieldName}.tx_id ${committedTxId} does not match decoded transaction id ${decodedTxId}`,
      );
    }
    const derived = deriveMidgardNativeTxProofSourceV1(tx);
    const compactCbor = normalizeHex(source.source.compact_cbor, {
      fieldName: `${fieldName}.source.compact_cbor`,
    });
    const witnessSetCompactCbor = normalizeHex(
      source.source.witness_set_compact_cbor,
      {
        fieldName: `${fieldName}.source.witness_set_compact_cbor`,
      },
    );
    const fieldPreimageLengthsCbor = normalizeHex(
      source.source.field_preimage_lengths_cbor,
      {
        fieldName: `${fieldName}.source.field_preimage_lengths_cbor`,
      },
    );
    if (
      compactCbor !== derived.compactCbor.toString("hex") ||
      witnessSetCompactCbor !== derived.witnessSetCompactCbor.toString("hex") ||
      fieldPreimageLengthsCbor !==
        derived.fieldPreimageLengthsCbor.toString("hex")
    ) {
      throw new DaPayloadValidationError(
        "malformed_transaction",
        `${fieldName}.source does not match the canonical transaction field commitments`,
      );
    }
    // No `transaction_commitment` to check: the committed source carries the
    // proof-source triple and nothing derived from it, so the three equalities
    // above are the whole of the binding. The retired field was
    // `computeMidgardNativeTxProofCommitmentV1(derived)` by construction, and a
    // check of a value against its own derivation could only ever pass.
    return decodedTxId;
  };

  for (const [index, [keyHex, valueHex]] of body.transactions.entries()) {
    const fieldName = `transactions[${index.toString()}]`;
    const committedTxId = normalizeHex(keyHex, {
      fieldName: `${fieldName}.key`,
      byteLength: 32,
    });
    const preimageHex = transactionPreimages.get(keyHex);
    if (preimageHex === undefined) {
      throw new DaPayloadValidationError(
        "coverage_mismatch",
        `${fieldName} has no same-key transaction_preimages entry`,
      );
    }
    const source = decodeCanonicalData<SDK.L2TransactionSourceV1>(
      valueHex,
      SDK.L2TransactionSourceV1Schema as never,
      `${fieldName}.value`,
    );
    const tx = validateFullTransaction(
      hexToBytes(preimageHex, `transaction_preimages[${index.toString()}]`),
      `transaction_preimages[${index.toString()}]`,
    );
    if (assertSourceBinding(source, tx, fieldName) !== committedTxId) {
      throw new DaPayloadValidationError(
        "malformed_transaction",
        `${fieldName}.key does not match the committed transaction source`,
      );
    }
    countLedgerOperations(tx, fieldName);
  }

  for (const [
    index,
    [keyHex, valueHex],
  ] of body.forced_transactions.entries()) {
    const fieldName = `forced_transactions[${index.toString()}]`;
    const preimageHex = forcedTransactionPreimages.get(keyHex);
    if (preimageHex === undefined) {
      throw new DaPayloadValidationError(
        "coverage_mismatch",
        `${fieldName} has no same-key forced_transaction_preimages entry`,
      );
    }
    const forced = decodeCanonicalData<SDK.ForcedInclusionTxV1>(
      valueHex,
      SDK.ForcedInclusionTxV1Schema as never,
      `${fieldName}.value`,
    );
    const tx = validateFullTransaction(
      hexToBytes(
        preimageHex,
        `forced_transaction_preimages[${index.toString()}]`,
      ),
      `forced_transaction_preimages[${index.toString()}]`,
    );
    assertSourceBinding(forced, tx, fieldName);
    if (forced.operator_validity === "TxIsValid") {
      countLedgerOperations(tx, fieldName);
    }
  }

  if (canonicalTransactionBytes > limits.maxCanonicalTransactionBytesPerBlock) {
    throw new DaPayloadValidationError(
      "consensus_bound",
      `canonical transaction bytes ${canonicalTransactionBytes.toString()} exceed V1 maximum ${limits.maxCanonicalTransactionBytesPerBlock.toString()}`,
    );
  }
  if (ledgerOperationCount > limits.maxLedgerOperationCount) {
    throw new DaPayloadValidationError(
      "consensus_bound",
      `ledger operations ${ledgerOperationCount.toString()} exceed V1 maximum ${limits.maxLedgerOperationCount.toString()}`,
    );
  }
  try {
    const material = body.cek_program_material.map(([rootHex, valueHex]) =>
      decodeMidgardCekProgramMaterialDaEntryV1(
        hexToBytes(rootHex, "cek_program_material.root"),
        hexToBytes(valueHex, "cek_program_material.value"),
      ),
    );
    assertMidgardCekProgramMaterialBundleV1(
      [...programEnvelopes.values()],
      material,
    );
  } catch (cause) {
    throw new DaPayloadValidationError(
      "coverage_mismatch",
      "CEK program material does not exactly cover every inline and newly referenced V1 program",
      { cause },
    );
  }
};

const decodeCanonicalData = <A>(
  hex: string,
  schema: DataSchema,
  fieldName: string,
): A => {
  const normalized = normalizeHex(hex, { fieldName });
  try {
    const value = LucidData.from(normalized, schema as never) as A;
    const recoded = LucidData.to(value as never, schema as never);
    if (recoded !== normalized) {
      throw new Error(`${fieldName} is not canonical for its schema`);
    }
    return value;
  } catch (cause) {
    throw new DaPayloadValidationError(
      "malformed_trace",
      `failed to decode ${fieldName}`,
      { cause },
    );
  }
};

const dataHex = <A>(value: A, schema: DataSchema): string =>
  LucidData.to(value as never, schema as never);

const L2_EVENT_KEY_PREFIX = "d87b9f5820";
const L2_EVENT_KEY_SUFFIX = "ff";
const L2_PHASE_CBOR = Buffer.from("d87b80", "hex");

const l2EventKeyFingerprintFromTxId = (txId: string): string =>
  `${L2_EVENT_KEY_PREFIX}${txId}${L2_EVENT_KEY_SUFFIX}`;

const parseCanonicalL2EventKey = (
  keyHex: string,
):
  | { readonly fingerprint: string; readonly phase: SDK.TransitionPhase }
  | undefined =>
  keyHex.length === 76 &&
  keyHex.startsWith(L2_EVENT_KEY_PREFIX) &&
  keyHex.endsWith(L2_EVENT_KEY_SUFFIX)
    ? { fingerprint: keyHex, phase: "L2Transaction" }
    : undefined;

const bufferStartsWith = (
  bytes: Buffer,
  offset: number,
  expected: Uint8Array,
): boolean =>
  offset + expected.length <= bytes.length &&
  expected.every((value, index) => bytes[offset + index] === value);

const parseCanonicalInteger = (bytes: Buffer): bigint | undefined => {
  try {
    const decoded = readCborInteger(bytes, 0, "transition integer");
    return decoded.nextOffset === bytes.length ? decoded.value : undefined;
  } catch {
    return undefined;
  }
};

const parseCanonicalL2TransitionStep = (
  keyHex: string,
  valueHex: string,
):
  | {
      readonly stepIndex: bigint;
      readonly schemaVersion: bigint;
      readonly eventKey: string;
      readonly phase: SDK.TransitionPhase;
    }
  | undefined => {
  const key = parseCanonicalInteger(Buffer.from(keyHex, "hex"));
  if (key === undefined) return undefined;
  try {
    const bytes = Buffer.from(valueHex, "hex");
    if (!bufferStartsWith(bytes, 0, Buffer.from("d8799f", "hex"))) {
      return undefined;
    }
    let offset = 3;
    const schema = readCborInteger(bytes, offset, "transition schema_version");
    offset = schema.nextOffset;
    const step = readCborInteger(bytes, offset, "transition step_index");
    offset = step.nextOffset;
    const eventStart = offset;
    if (!bufferStartsWith(bytes, offset, Buffer.from("d87b9f", "hex"))) {
      return undefined;
    }
    offset += 3;
    const txId = readCborBytes(bytes, offset, "transition event tx_id");
    if (txId.value.length !== 32) return undefined;
    offset = txId.nextOffset;
    if (bytes[offset] !== 0xff) return undefined;
    offset += 1;
    const eventKey = bytes.toString("hex", eventStart, offset);
    if (parseCanonicalL2EventKey(eventKey) === undefined) return undefined;
    if (!bufferStartsWith(bytes, offset, L2_PHASE_CBOR)) return undefined;
    offset += L2_PHASE_CBOR.length;
    const preRoot = readCborBytes(bytes, offset, "transition pre root");
    if (preRoot.value.length !== 32) return undefined;
    offset = preRoot.nextOffset;
    const postRoot = readCborBytes(bytes, offset, "transition post root");
    if (postRoot.value.length !== 32) return undefined;
    offset = postRoot.nextOffset;
    if (bytes[offset] !== 0xff || offset + 1 !== bytes.length) {
      return undefined;
    }
    if (key !== step.value) return undefined;
    return {
      stepIndex: step.value,
      schemaVersion: schema.value,
      eventKey,
      phase: "L2Transaction",
    };
  } catch {
    return undefined;
  }
};

const parseCanonicalL2EventToStep = (
  keyHex: string,
  valueHex: string,
):
  | {
      readonly eventKey: string;
      readonly stepIndex: bigint;
      readonly phase: SDK.TransitionPhase;
    }
  | undefined => {
  const event = parseCanonicalL2EventKey(keyHex);
  if (event === undefined) return undefined;
  try {
    const bytes = Buffer.from(valueHex, "hex");
    if (!bufferStartsWith(bytes, 0, Buffer.from("d8799f", "hex"))) {
      return undefined;
    }
    const step = readCborInteger(bytes, 3, "event_to_step step_index");
    if (!bufferStartsWith(bytes, step.nextOffset, L2_PHASE_CBOR)) {
      return undefined;
    }
    const end = step.nextOffset + L2_PHASE_CBOR.length;
    if (bytes[end] !== 0xff || end + 1 !== bytes.length) return undefined;
    return {
      eventKey: event.fingerprint,
      stepIndex: step.value,
      phase: event.phase,
    };
  } catch {
    return undefined;
  }
};

const headerCborHexV1 = (header: SDK.HeaderV1): string =>
  dataHex(header, SDK.HeaderV1 as never);

const hashBlockHeaderCborV1 = (header: SDK.HeaderV1): string =>
  bytesToHex(
    blake2b(Buffer.from(headerCborHexV1(header), "hex"), { dkLen: 28 }),
  );

const eventKeyFingerprint = (eventKey: SDK.EventKey): string =>
  dataHex(eventKey, SDK.EventKeySchema);

const eventPhase = (eventKey: SDK.EventKey): SDK.TransitionPhase => {
  if ("WithdrawalEventKey" in eventKey) {
    return "Withdrawal";
  }
  if ("ForcedTransactionEventKey" in eventKey) {
    return "ForcedTransaction";
  }
  if ("L2TransactionEventKey" in eventKey) {
    return "L2Transaction";
  }
  return "Deposit";
};

const sourceEventFingerprints = (body: SDK.DaPayloadBodyV1): Set<string> => {
  const fingerprints = new Set<string>();
  const add = (fingerprint: string, fieldName: string) => {
    if (fingerprints.has(fingerprint)) {
      throw new DaPayloadValidationError(
        "coverage_mismatch",
        `duplicate source event key derived from ${fieldName}`,
      );
    }
    fingerprints.add(fingerprint);
  };
  for (const [index, [key]] of body.withdrawals.entries()) {
    const withdrawalId = decodeCanonicalData<SDK.OutputReference>(
      key,
      SDK.OutputReference as never,
      `withdrawals[${index.toString()}].key`,
    );
    add(
      eventKeyFingerprint({
        WithdrawalEventKey: { withdrawal_id: withdrawalId },
      }),
      `withdrawals[${index.toString()}]`,
    );
  }
  for (const [index, [key]] of body.forced_transactions.entries()) {
    const txOrderId = decodeCanonicalData<SDK.OutputReference>(
      key,
      SDK.OutputReference as never,
      `forced_transactions[${index.toString()}].key`,
    );
    add(
      eventKeyFingerprint({
        ForcedTransactionEventKey: { tx_order_id: txOrderId },
      }),
      `forced_transactions[${index.toString()}]`,
    );
  }
  for (const [index, [key]] of body.transactions.entries()) {
    const txId = normalizeHex(key, {
      fieldName: `transactions[${index.toString()}].key`,
      byteLength: 32,
    });
    add(
      l2EventKeyFingerprintFromTxId(txId),
      `transactions[${index.toString()}]`,
    );
  }
  for (const [index, [key]] of body.deposits.entries()) {
    const depositId = decodeCanonicalData<SDK.OutputReference>(
      key,
      SDK.OutputReference as never,
      `deposits[${index.toString()}].key`,
    );
    add(
      eventKeyFingerprint({ DepositEventKey: { deposit_id: depositId } }),
      `deposits[${index.toString()}]`,
    );
  }
  return fingerprints;
};

const validateTraceCoverage = (payload: SDK.DaPayloadV1): void => {
  const body = payload.block_body;
  const counts = body.counts;
  const expectedTransitionStepSchemaVersion =
    MIDGARD_TRANSITION_STEP_V1_SCHEMA_VERSION;
  const memberCounts: PayloadCountSet = {
    withdrawalCount: BigInt(body.withdrawals.length),
    forcedTransactionCount: BigInt(body.forced_transactions.length),
    l2TransactionCount: BigInt(body.transactions.length),
    depositCount: BigInt(body.deposits.length),
    totalEventCount:
      BigInt(body.withdrawals.length) +
      BigInt(body.forced_transactions.length) +
      BigInt(body.transactions.length) +
      BigInt(body.deposits.length),
    transitionStepCount: BigInt(body.transition_trace.length),
  };
  const countMismatchFields = countMismatches(counts, memberCounts);
  if (countMismatchFields.length > 0) {
    throw new DaPayloadValidationError(
      "count_mismatch",
      `payload counts do not match payload member arrays: ${countMismatchFields.join(",")}`,
    );
  }
  if (BigInt(body.event_to_step.length) !== counts.totalEventCount) {
    throw new DaPayloadValidationError(
      "count_mismatch",
      "event_to_step member count must equal total_event_count",
    );
  }

  const sourceEvents = sourceEventFingerprints(body);
  if (BigInt(sourceEvents.size) !== counts.totalEventCount) {
    throw new DaPayloadValidationError(
      "coverage_mismatch",
      "source event key set size does not match total_event_count",
    );
  }

  const traceByIndex = new Map<
    bigint,
    { readonly eventKey: string; readonly phase: SDK.TransitionPhase }
  >();
  for (const [index, [keyHex, valueHex]] of body.transition_trace.entries()) {
    const fast = parseCanonicalL2TransitionStep(keyHex, valueHex);
    const step =
      fast === undefined
        ? decodeCanonicalData<SDK.TransitionStep>(
            valueHex,
            SDK.TransitionStepSchema as never,
            `transition_trace[${index.toString()}].value`,
          )
        : ({
            schema_version: fast.schemaVersion,
            step_index: fast.stepIndex,
            event_key: {
              L2TransactionEventKey: {
                tx_id: fast.eventKey.slice(
                  L2_EVENT_KEY_PREFIX.length,
                  -L2_EVENT_KEY_SUFFIX.length,
                ),
              },
            },
            phase: fast.phase,
            pre_utxos_root: "00".repeat(32),
            post_utxos_root: "00".repeat(32),
          } satisfies SDK.TransitionStep);
    const key =
      fast?.stepIndex ??
      decodeCanonicalData<bigint>(
        keyHex,
        LucidData.Integer() as never,
        `transition_trace[${index.toString()}].key`,
      );
    if (step.schema_version !== BigInt(expectedTransitionStepSchemaVersion)) {
      throw new DaPayloadValidationError(
        "version_mismatch",
        `transition step schema_version must equal ${expectedTransitionStepSchemaVersion.toString()}, got ${step.schema_version.toString()}`,
      );
    }
    if (key !== step.step_index) {
      throw new DaPayloadValidationError(
        "coverage_mismatch",
        "transition trace key must equal step_index",
      );
    }
    if (step.phase !== eventPhase(step.event_key)) {
      throw new DaPayloadValidationError(
        "coverage_mismatch",
        "transition step phase does not match event key variant",
      );
    }
    if (traceByIndex.has(step.step_index)) {
      throw new DaPayloadValidationError(
        "duplicate_key",
        `duplicate transition step_index ${step.step_index.toString()}`,
      );
    }
    traceByIndex.set(step.step_index, {
      eventKey: fast?.eventKey ?? eventKeyFingerprint(step.event_key),
      phase: step.phase,
    });
  }
  for (let index = 0n; index < counts.transitionStepCount; index += 1n) {
    if (!traceByIndex.has(index)) {
      throw new DaPayloadValidationError(
        "coverage_mismatch",
        `transition trace is missing dense step_index ${index.toString()}`,
      );
    }
  }

  const eventToStep = new Map<string, SDK.EventToStepValue>();
  for (const [index, [keyHex, valueHex]] of body.event_to_step.entries()) {
    const fast = parseCanonicalL2EventToStep(keyHex, valueHex);
    const eventKey =
      fast === undefined
        ? decodeCanonicalData<SDK.EventKey>(
            keyHex,
            SDK.EventKeySchema as never,
            `event_to_step[${index.toString()}].key`,
          )
        : ({
            L2TransactionEventKey: {
              tx_id: fast.eventKey.slice(
                L2_EVENT_KEY_PREFIX.length,
                -L2_EVENT_KEY_SUFFIX.length,
              ),
            },
          } satisfies SDK.EventKey);
    const value =
      fast === undefined
        ? decodeCanonicalData<SDK.EventToStepValue>(
            valueHex,
            SDK.EventToStepValueSchema as never,
            `event_to_step[${index.toString()}].value`,
          )
        : { step_index: fast.stepIndex, phase: fast.phase };
    if (value.step_index < 0n) {
      throw new DaPayloadValidationError(
        "coverage_mismatch",
        "event_to_step step_index must be non-negative",
      );
    }
    if (value.phase !== eventPhase(eventKey)) {
      throw new DaPayloadValidationError(
        "coverage_mismatch",
        "event_to_step phase does not match event key variant",
      );
    }
    const fingerprint = fast?.eventKey ?? eventKeyFingerprint(eventKey);
    if (eventToStep.has(fingerprint)) {
      throw new DaPayloadValidationError(
        "duplicate_key",
        `duplicate event_to_step event key ${fingerprint}`,
      );
    }
    eventToStep.set(fingerprint, value);
  }

  for (const sourceEvent of sourceEvents) {
    const mapped = eventToStep.get(sourceEvent);
    if (mapped === undefined) {
      throw new DaPayloadValidationError(
        "coverage_mismatch",
        "event_to_step omits a committed source event",
      );
    }
    const trace = traceByIndex.get(mapped.step_index);
    if (trace === undefined) {
      throw new DaPayloadValidationError(
        "coverage_mismatch",
        "event_to_step points to a missing transition step",
      );
    }
    if (trace.eventKey !== sourceEvent || trace.phase !== mapped.phase) {
      throw new DaPayloadValidationError(
        "coverage_mismatch",
        "event_to_step does not point back to the matching transition trace event",
      );
    }
  }
};

const validateProofTraceCoverageV1 = (payload: SDK.DaPayloadV1): void => {
  validateTraceCoverage(payload);
  const body = payload.block_body;
  if (
    BigInt(body.validation_traces.length) !== body.counts.validationTraceCount
  ) {
    throw new DaPayloadValidationError(
      "count_mismatch",
      "validation_traces member count must equal validation_trace_count",
    );
  }

  const expectedVerdicts = new Map<string, "accepted" | "rejected">();
  for (const [index, [key]] of body.transactions.entries()) {
    const txId = normalizeHex(key, {
      fieldName: `transactions[${index.toString()}].key`,
      byteLength: 32,
    });
    expectedVerdicts.set(l2EventKeyFingerprintFromTxId(txId), "accepted");
  }
  for (const [index, [key, value]] of body.forced_transactions.entries()) {
    const txOrderId = decodeCanonicalData<SDK.OutputReference>(
      key,
      SDK.OutputReference as never,
      `forced_transactions[${index.toString()}].key`,
    );
    const forced = decodeCanonicalData<SDK.ForcedInclusionTxV1>(
      value,
      SDK.ForcedInclusionTxV1Schema as never,
      `forced_transactions[${index.toString()}].value`,
    );
    expectedVerdicts.set(
      eventKeyFingerprint({
        ForcedTransactionEventKey: { tx_order_id: txOrderId },
      }),
      forced.operator_validity === "TxIsValid" ? "accepted" : "rejected",
    );
  }

  const observed = new Set<string>();
  for (const [index, [keyHex, valueHex]] of body.validation_traces.entries()) {
    const eventKey = decodeCanonicalData<SDK.EventKey>(
      keyHex,
      SDK.EventKeySchema as never,
      `validation_traces[${index.toString()}].key`,
    );
    if (
      !("L2TransactionEventKey" in eventKey) &&
      !("ForcedTransactionEventKey" in eventKey)
    ) {
      throw new DaPayloadValidationError(
        "coverage_mismatch",
        "validation trace keys must identify an L2 or forced transaction",
      );
    }
    const fingerprint = eventKeyFingerprint(eventKey);
    if (observed.has(fingerprint)) {
      throw new DaPayloadValidationError(
        "duplicate_key",
        `duplicate validation trace event key ${fingerprint}`,
      );
    }
    const expectedVerdict = expectedVerdicts.get(fingerprint);
    if (expectedVerdict === undefined) {
      throw new DaPayloadValidationError(
        "coverage_mismatch",
        "validation trace does not correspond to a committed transaction source",
      );
    }
    let descriptor;
    try {
      descriptor = decodeMidgardValidationTraceDescriptorV1(
        hexToBytes(valueHex, `validation_traces[${index.toString()}].value`),
      );
    } catch (cause) {
      throw new DaPayloadValidationError(
        "malformed_trace",
        `validation_traces[${index.toString()}].value is not a canonical bounded descriptor`,
        { cause },
      );
    }
    if (descriptor.verdict !== expectedVerdict) {
      throw new DaPayloadValidationError(
        "coverage_mismatch",
        `validation trace verdict ${descriptor.verdict} does not match committed operator verdict ${expectedVerdict}`,
      );
    }
    observed.add(fingerprint);
  }

  for (const fingerprint of expectedVerdicts.keys()) {
    if (!observed.has(fingerprint)) {
      throw new DaPayloadValidationError(
        "coverage_mismatch",
        "validation_traces omits a committed transaction source",
      );
    }
  }
};

const keyValuePhasRootWithValues = async (
  keys: readonly Buffer[],
  values: readonly Buffer[],
): Promise<string> => {
  if (keys.length !== values.length) {
    throw new Error(
      `cannot build PHAS root for ${keys.length.toString()} keys and ${values.length.toString()} values`,
    );
  }
  if (keys.length === 0) {
    return SDK.EMPTY_MERKLE_TREE_ROOT;
  }
  const trie = await Trie.fromList(
    keys.map((key, index) => ({
      key: Buffer.from(key),
      value: Buffer.from(values[index]!),
    })),
  );
  return Buffer.from(trie.hash).toString("hex");
};

const countedRoot = async (
  domain: SDK.RootDomain,
  entries: readonly SDK.DaPayloadEntry[],
): Promise<string> =>
  countedRootWithValues(
    domain,
    entries.map(([key]) => hexToBytes(key, "entry key")),
    entries.map(([, value]) => hexToBytes(value, "entry value")),
  );

const countedRootWithValues = async (
  domain: SDK.RootDomain,
  keys: readonly Buffer[],
  values: readonly Buffer[],
): Promise<string> => {
  const phasRoot = await keyValuePhasRootWithValues(keys, values);
  return Effect.runPromise(
    SDK.commitCountedRootProgram({
      domain,
      phasRoot,
      count: BigInt(keys.length),
    }),
  );
};

const rootMismatchesV1 = (
  header: SDK.HeaderV1,
  roots: PayloadRootSet,
): readonly string[] =>
  [
    header.utxosRoot === roots.utxosRoot ? undefined : "utxos_root",
    header.withdrawalsRoot === roots.withdrawalsRoot
      ? undefined
      : "withdrawals_root",
    header.forcedTransactionsRoot === roots.forcedTransactionsRoot
      ? undefined
      : "forced_transactions_root",
    header.transactionsRoot === roots.transactionsRoot
      ? undefined
      : "transactions_root",
    header.depositsRoot === roots.depositsRoot ? undefined : "deposits_root",
    header.transitionTraceRoot === roots.transitionTraceRoot
      ? undefined
      : "transition_trace_root",
    header.eventToStepRoot === roots.eventToStepRoot
      ? undefined
      : "event_to_step_root",
  ].filter((field): field is string => field !== undefined);

const daPayloadRootMismatchesV1 = (
  header: SDK.HeaderV1,
  roots: DaPayloadRootSetV1,
): readonly string[] => [
  ...rootMismatchesV1(header, roots),
  ...(header.validationTracesRoot === roots.validationTracesRoot
    ? []
    : ["validation_traces_root"]),
];

const headerCountsV1 = (header: SDK.HeaderV1): PayloadCountSet => ({
  withdrawalCount: header.withdrawalCount,
  forcedTransactionCount: header.forcedTransactionCount,
  l2TransactionCount: header.l2TransactionCount,
  depositCount: header.depositCount,
  totalEventCount: header.totalEventCount,
  transitionStepCount: header.transitionStepCount,
});

const daPayloadHeaderCountsV1 = (
  header: SDK.HeaderV1,
): DaPayloadCountSetV1 => ({
  ...headerCountsV1(header),
  validationTraceCount: header.validationTraceCount,
});

const countMismatches = (
  expected: PayloadCountSet,
  actual: PayloadCountSet,
): readonly string[] =>
  [
    expected.withdrawalCount === actual.withdrawalCount
      ? undefined
      : "withdrawal_count",
    expected.forcedTransactionCount === actual.forcedTransactionCount
      ? undefined
      : "forced_transaction_count",
    expected.l2TransactionCount === actual.l2TransactionCount
      ? undefined
      : "l2_transaction_count",
    expected.depositCount === actual.depositCount ? undefined : "deposit_count",
    expected.totalEventCount === actual.totalEventCount
      ? undefined
      : "total_event_count",
    expected.transitionStepCount === actual.transitionStepCount
      ? undefined
      : "transition_step_count",
  ].filter((field): field is string => field !== undefined);

const daPayloadCountMismatchesV1 = (
  expected: DaPayloadCountSetV1,
  actual: DaPayloadCountSetV1,
): readonly string[] => [
  ...countMismatches(expected, actual),
  ...(expected.validationTraceCount === actual.validationTraceCount
    ? []
    : ["validation_trace_count"]),
];
