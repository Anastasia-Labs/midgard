import { MIDGARD_TRANSITION_STEP_SCHEMA_VERSION } from "@al-ft/midgard-core/consensus-profile";
import {
  decodeMidgardValidationTraceDescriptor,
  encodeMidgardValidationTraceDescriptor,
  type MidgardValidationTraceDescriptor,
} from "@al-ft/midgard-core/validation-trace";
import * as SDK from "@al-ft/midgard-sdk";
import { Data as LucidData } from "@lucid-evolution/lucid";

import type { PayloadCountSet } from "../domain.js";
import {
  DaPayloadValidationError,
  decodeCanonicalData,
} from "./payload.da-payload-validation-error.js";
import {
  countMismatches,
  eventKeyFingerprint,
  eventPhase,
  L2_EVENT_KEY_PREFIX,
  L2_EVENT_KEY_SUFFIX,
  parseCanonicalL2EventToStep,
  parseCanonicalL2TransitionStep,
  sourceEventFingerprints,
} from "./payload.source-event-fingerprints.js";

/**
 * The committed `event_to_step` map, keyed by the canonical event-key
 * fingerprint. Rejects a negative step index, a phase that does not match
 * the event-key variant, and a repeated event key.
 */
export const parseEventToStep = (
  body: SDK.DaPayloadBody,
): ReadonlyMap<string, SDK.EventToStepValue> => {
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
  return eventToStep;
};

export const validateTraceCoverage = (payload: SDK.DaPayload): void => {
  const body = payload.block_body;
  const counts = body.counts;
  const expectedTransitionStepSchemaVersion =
    MIDGARD_TRANSITION_STEP_SCHEMA_VERSION;
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

  const eventToStep = parseEventToStep(body);

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

/**
 * Decodes one committed `validation_traces` value. The committed leaf is the
 * canonical Plutus Data form of `ValidationTraceDescriptorV1` (the bytes the
 * on-chain root membership check serialises), not the core codec's plain CBOR
 * array. The frozen V1 bounds (versions, step-count cap, terminal verdict,
 * verdict/rejection binding) are enforced by round-tripping through the core
 * codec.
 */
export const decodeCommittedValidationTraceDescriptor = (
  valueHex: string,
  fieldName: string,
): MidgardValidationTraceDescriptor => {
  const data = decodeCanonicalData<SDK.ValidationTraceDescriptor>(
    valueHex,
    SDK.ValidationTraceDescriptorSchema as never,
    fieldName,
  );
  try {
    return decodeMidgardValidationTraceDescriptor(
      encodeMidgardValidationTraceDescriptor(
        SDK.validationTraceDescriptorCoreFromData(data),
      ),
    );
  } catch (cause) {
    throw new DaPayloadValidationError(
      "malformed_trace",
      `${fieldName} is not a canonical bounded descriptor`,
      { cause },
    );
  }
};

export const retainedTransactionPreimages = (
  payload: SDK.DaPayload,
): ReadonlyMap<string, Buffer> => {
  const result = new Map<string, Buffer>();
  for (const [txId, txCbor] of payload.block_body.transaction_preimages) {
    result.set(
      eventKeyFingerprint({ L2TransactionEventKey: { tx_id: txId } }),
      Buffer.from(txCbor, "hex"),
    );
  }
  for (const [outRefCbor, txCbor] of payload.block_body
    .forced_transaction_preimages) {
    const outRef = decodeCanonicalData<SDK.OutputReference>(
      outRefCbor,
      SDK.OutputReference as never,
      "forced_transaction_preimages key",
    );
    result.set(
      eventKeyFingerprint({
        ForcedTransactionEventKey: { tx_order_id: outRef },
      }),
      Buffer.from(txCbor, "hex"),
    );
  }
  return result;
};

export const PHASE_FROM_DATA = {
  CanonicalDecode: "canonicalDecode",
  CompactBinding: "compactBinding",
  StaticLedgerRules: "staticLedgerRules",
  InputSets: "inputSets",
  Signatures: "signatures",
  PhaseANativeScripts: "phaseANativeScripts",
  PhaseAScriptPreconditions: "phaseAScriptPreconditions",
  ResolveInputs: "resolveInputs",
  ScriptSources: "scriptSources",
  NativeScripts: "nativeScripts",
  ScriptIntegrity: "scriptIntegrity",
  Cek: "cek",
  ValueAndMint: "valueAndMint",
  LedgerDelta: "ledgerDelta",
  Terminal: "terminal",
} as const;

export const retainedSafeNumber = (value: bigint, label: string): number => {
  const number = Number(value);
  if (!Number.isSafeInteger(number) || number < 0) {
    throw new DaPayloadValidationError(
      "consensus_bound",
      `${label} must be a non-negative safe integer`,
    );
  }
  return number;
};
