import { Trie } from "@aiken-lang/merkle-patricia-forestry";
import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core/consensus-profile";
import { type MidgardValidationTraceDescriptor } from "@al-ft/midgard-core/validation-trace";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";

import { hexToBytes, normalizeHex } from "../utils/hex.js";
import {
  DaPayloadValidationError,
  type DaPayloadVerificationTimingOptions,
  decodeCanonicalData,
  readMonotonicNow,
  recordTiming,
  validateDaPayloadCounts,
  validateEntries,
} from "./payload.da-payload-validation-error.js";
import {
  eventKeyFingerprint,
  hashBlockHeaderCbor,
  l2EventKeyFingerprintFromTxId,
} from "./payload.source-event-fingerprints.js";
import { validateDaPayloadConsensus } from "./payload.validate-da-payload-consensus.js";
import { validateRetainedValidationWitnesses } from "./payload.validate-retained-validation-witnesses.js";
import {
  decodeCommittedValidationTraceDescriptor,
  retainedTransactionPreimages,
  validateTraceCoverage,
} from "./payload.validate-trace-coverage.js";

export const decodeDaPayloadStrict = (
  payloadCbor: Uint8Array,
  timing: DaPayloadVerificationTimingOptions = {},
): SDK.DaPayload => {
  if (payloadCbor.length > MIDGARD_CONSENSUS_LIMITS.maxDaPayloadBytes) {
    throw new DaPayloadValidationError(
      "consensus_bound",
      `canonical DA payload bytes ${payloadCbor.length.toString()} exceed V1 maximum ${MIDGARD_CONSENSUS_LIMITS.maxDaPayloadBytes.toString()}`,
    );
  }
  const payloadBuffer = Buffer.isBuffer(payloadCbor)
    ? payloadCbor
    : Buffer.from(payloadCbor);
  let payload: SDK.DaPayload;
  const decodeStartedAt = readMonotonicNow(timing);
  try {
    payload = SDK.decodeDaPayload(payloadBuffer);
  } catch (cause) {
    throw new DaPayloadValidationError(
      cause instanceof SDK.DaPayloadNonCanonicalError
        ? "non_canonical"
        : "malformed_da",
      cause instanceof SDK.DaPayloadNonCanonicalError
        ? "payload CBOR was not canonical for DaPayloadV1"
        : "failed to decode DaPayloadV1 canonical CBOR",
      { cause },
    );
  } finally {
    recordTiming(timing, "inner_decode", decodeStartedAt);
  }

  const validationStartedAt = readMonotonicNow(timing);
  try {
    if (payload.version !== SDK.DA_PAYLOAD_VERSION) {
      throw new DaPayloadValidationError(
        "wrong_version",
        `expected DaPayloadV1 version ${SDK.DA_PAYLOAD_VERSION.toString()}, got ${payload.version.toString()}`,
      );
    }
    const body = payload.block_body;
    normalizeHex(body.header_hash, {
      fieldName: "payload header_hash",
      byteLength: 28,
    });
    const embeddedHeaderHash = hashBlockHeaderCbor(body.header);
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
    validateEntries(
      "validation_trace_witnesses",
      body.validation_trace_witnesses,
    );
    validateDaPayloadCounts(body.counts);
    validateDaPayloadConsensus(body);
    validateProofTraceCoverage(payload);
    return payload;
  } finally {
    recordTiming(timing, "payload_structure_validation", validationStartedAt);
  }
};

const validateProofTraceCoverage = (payload: SDK.DaPayload): void => {
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
      forced.verdict === "ForcedTxValid" ? "accepted" : "rejected",
    );
  }

  const observed = new Set<string>();
  const descriptors = new Map<
    string,
    {
      readonly keyCbor: Buffer;
      readonly descriptor: MidgardValidationTraceDescriptor;
    }
  >();
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
    const descriptor = decodeCommittedValidationTraceDescriptor(
      valueHex,
      `validation_traces[${index.toString()}].value`,
    );
    if (descriptor.verdict !== expectedVerdict) {
      throw new DaPayloadValidationError(
        "coverage_mismatch",
        `validation trace verdict ${descriptor.verdict} does not match committed operator verdict ${expectedVerdict}`,
      );
    }
    observed.add(fingerprint);
    descriptors.set(fingerprint, {
      keyCbor: Buffer.from(keyHex, "hex"),
      descriptor,
    });
  }

  for (const fingerprint of expectedVerdicts.keys()) {
    if (!observed.has(fingerprint)) {
      throw new DaPayloadValidationError(
        "coverage_mismatch",
        "validation_traces omits a committed transaction source",
      );
    }
  }
  validateRetainedValidationWitnesses(
    body.validation_trace_witnesses,
    descriptors,
    retainedTransactionPreimages(payload),
  );
};

export const keyValuePhasRootWithValues = async (
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

export const countedRoot = async (
  domain: SDK.RootDomain,
  entries: readonly SDK.DaPayloadEntry[],
): Promise<string> =>
  countedRootWithValues(
    domain,
    entries.map(([key]) => hexToBytes(key, "entry key")),
    entries.map(([, value]) => hexToBytes(value, "entry value")),
  );

export const countedRootWithValues = async (
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
