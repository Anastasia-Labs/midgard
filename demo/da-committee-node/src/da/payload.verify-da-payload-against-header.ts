import { MIDGARD_PROTOCOL_VERSION } from "@al-ft/midgard-core/consensus-profile";
import { unwrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { DA_TRANSPORT_LIMITS } from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";
import { buildCanonicalMidgardLedgerEntryOutputMaterial } from "@al-ft/midgard-validation";
import { sha256 } from "@noble/hashes/sha2.js";

import type { PayloadCountSet, PayloadRootSet } from "../domain.js";
import { bytesToHex, hexToBytes, normalizeHex } from "../utils/hex.js";
import {
  type DaPayloadCountSet,
  type DaPayloadRootSet,
  DaPayloadValidationError,
  type PayloadVerificationOptions,
  readMonotonicNow,
  recordTiming,
  type VerifiedDaPayload,
} from "./payload.da-payload-validation-error.js";
import {
  countMismatches,
  hashBlockHeaderCbor,
  headerCborHex,
} from "./payload.source-event-fingerprints.js";
import {
  countedRoot,
  countedRootWithValues,
  decodeDaPayloadStrict,
  keyValuePhasRootWithValues,
} from "./payload.validate-proof-trace-coverage.js";

const computeDaPayloadRootsForForcedDomain = async (
  payload: SDK.DaPayload,
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
        buildCanonicalMidgardLedgerEntryOutputMaterial({
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

export const computeDaPayloadRoots = async (
  payload: SDK.DaPayload,
): Promise<DaPayloadRootSet> => {
  const proofRoots = await computeDaPayloadRootsForForcedDomain(payload);
  return {
    ...proofRoots,
    validationTracesRoot: await countedRoot(
      SDK.ROOT_DOMAINS.validationTraces,
      payload.block_body.validation_traces,
    ),
  };
};

export const verifyDaPayloadAgainstHeader = async (
  storedPayloadCbor: Uint8Array,
  expectedHeaderHash: string,
  header: SDK.Header,
  options: PayloadVerificationOptions,
): Promise<VerifiedDaPayload> => {
  if (options.payloadSchemaVersion !== Number(SDK.DA_PAYLOAD_VERSION)) {
    throw new DaPayloadValidationError(
      "wrong_version",
      `expected DA payload schema version ${SDK.DA_PAYLOAD_VERSION.toString()}, got ${String(options.payloadSchemaVersion)}`,
    );
  }
  const storedPayloadBuffer = Buffer.from(storedPayloadCbor);
  const hashStartedAt = readMonotonicNow(options.timing);
  const payloadSha256 = bytesToHex(sha256(storedPayloadBuffer));
  recordTiming(options.timing, "stored_hash", hashStartedAt);
  let payloadBuffer: Buffer;
  try {
    payloadBuffer = (
      await unwrapDaPayload(storedPayloadBuffer, {
        maxPayloadBytes: DA_TRANSPORT_LIMITS.maxPayloadBytes,
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
  const payload = decodeDaPayloadStrict(payloadBuffer, options.timing);
  const semanticStartedAt = readMonotonicNow(options.timing);
  try {
    if (payload.block_body.header_hash !== normalizedHeaderHash) {
      throw new DaPayloadValidationError(
        "header_hash_mismatch",
        `payload header_hash ${payload.block_body.header_hash} does not match L1 header hash ${normalizedHeaderHash}`,
      );
    }
    if (hashBlockHeaderCbor(header) !== normalizedHeaderHash) {
      throw new DaPayloadValidationError(
        "header_hash_mismatch",
        `L1 V1 header body does not hash to expected header_hash ${normalizedHeaderHash}`,
      );
    }
    if (headerCborHex(payload.block_body.header) !== headerCborHex(header)) {
      throw new DaPayloadValidationError(
        "header_mismatch",
        "payload embedded V1 header does not match the L1 header",
      );
    }
    if (header.protocolVersion !== BigInt(MIDGARD_PROTOCOL_VERSION)) {
      throw new DaPayloadValidationError(
        "version_mismatch",
        `V1 header protocol_version must equal ${MIDGARD_PROTOCOL_VERSION.toString()}, got ${header.protocolVersion.toString()}`,
      );
    }
    const roots = await computeDaPayloadRoots(payload);
    const counts = payload.block_body.counts;
    const rootMismatchFields = daPayloadRootMismatches(header, roots);
    if (rootMismatchFields.length > 0) {
      throw new DaPayloadValidationError(
        "root_mismatch",
        `V1 DA payload roots do not match L1 header: ${rootMismatchFields.join(",")}`,
      );
    }
    const countMismatchFields = daPayloadCountMismatches(
      daPayloadHeaderCounts(header),
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

const rootMismatches = (
  header: SDK.Header,
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

const daPayloadRootMismatches = (
  header: SDK.Header,
  roots: DaPayloadRootSet,
): readonly string[] => [
  ...rootMismatches(header, roots),
  ...(header.validationTracesRoot === roots.validationTracesRoot
    ? []
    : ["validation_traces_root"]),
];

const headerCounts = (header: SDK.Header): PayloadCountSet => ({
  withdrawalCount: header.withdrawalCount,
  forcedTransactionCount: header.forcedTransactionCount,
  l2TransactionCount: header.l2TransactionCount,
  depositCount: header.depositCount,
  totalEventCount: header.totalEventCount,
  transitionStepCount: header.transitionStepCount,
});

const daPayloadHeaderCounts = (header: SDK.Header): DaPayloadCountSet => ({
  ...headerCounts(header),
  validationTraceCount: header.validationTraceCount,
});

const daPayloadCountMismatches = (
  expected: DaPayloadCountSet,
  actual: DaPayloadCountSet,
): readonly string[] => [
  ...countMismatches(expected, actual),
  ...(expected.validationTraceCount === actual.validationTraceCount
    ? []
    : ["validation_trace_count"]),
];
