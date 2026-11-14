import { readCborBytes, readCborInteger } from "@al-ft/midgard-core/codec/cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { blake2b } from "@noble/hashes/blake2.js";

import type { PayloadCountSet } from "../domain.js";
import { bytesToHex, normalizeHex } from "../utils/hex.js";
import {
  DaPayloadValidationError,
  decodeCanonicalData,
} from "./payload.da-payload-validation-error.js";
import { dataHex } from "./payload.validate-da-payload-consensus.js";

export const L2_EVENT_KEY_PREFIX = "d87b9f5820";

export const L2_EVENT_KEY_SUFFIX = "ff";

export const L2_PHASE_CBOR = Buffer.from("d87b80", "hex");

export const l2EventKeyFingerprintFromTxId = (txId: string): string =>
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

export const parseCanonicalL2TransitionStep = (
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

export const parseCanonicalL2EventToStep = (
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

export const headerCborHex = (header: SDK.Header): string =>
  dataHex(header, SDK.Header as never);

export const hashBlockHeaderCbor = (header: SDK.Header): string =>
  bytesToHex(blake2b(Buffer.from(headerCborHex(header), "hex"), { dkLen: 28 }));

export const eventKeyFingerprint = (eventKey: SDK.EventKey): string =>
  dataHex(eventKey, SDK.EventKeySchema);

export const eventPhase = (eventKey: SDK.EventKey): SDK.TransitionPhase => {
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

export const sourceEventFingerprints = (
  body: SDK.DaPayloadBody,
): Set<string> => {
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

export const countMismatches = (
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
