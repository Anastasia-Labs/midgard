import {
  buildMidgardBoundedItem,
  commitMidgardValidationMerkleFrontier,
} from "@al-ft/midgard-core";
import { encodeCbor } from "@al-ft/midgard-core/codec/cbor";
import {
  hashMidgardInlineScriptSourceLeaf,
  hashMidgardReferenceScriptSourceLeaf,
  hashMidgardScriptExecutionLeaf,
  hashMidgardScriptPurposeLeaf,
} from "@al-ft/midgard-core/script-proof";
import {
  hashMidgardValidationContext,
  hashMidgardValidationEventKey,
  hashMidgardValidationMachineState,
  hashMidgardValidationWorkWitness,
  MidgardValidationPhase,
  type MidgardValidationTraceDescriptor,
  verifyMidgardValidationTraceProof,
} from "@al-ft/midgard-core/validation-trace";
import * as SDK from "@al-ft/midgard-sdk";
import { projectMidgardRawEnvelopeForPhaseAV1 } from "@al-ft/midgard-validation";
import { Data as LucidData } from "@lucid-evolution/lucid";

import { DaPayloadValidationError } from "./payload.da-payload-validation-error.js";
import { eventKeyFingerprint } from "./payload.source-event-fingerprints.js";
import {
  nativeControlRoots,
  requireRetainedMembership,
  retainedFrontier,
  retainedWitnessSlot,
  stateFromRetainedData,
} from "./payload.state-from-retained-data.js";
import { retainedSafeNumber } from "./payload.validate-trace-coverage.js";

export const validateRetainedValidationWitnesses = (
  entries: readonly SDK.DaPayloadEntry[],
  descriptors: ReadonlyMap<
    string,
    {
      readonly keyCbor: Buffer;
      readonly descriptor: MidgardValidationTraceDescriptor;
    }
  >,
  rawTransactions: ReadonlyMap<string, Buffer>,
): void => {
  const coordinates = new Set<string>();
  for (const [index, [keyHex, valueHex]] of entries.entries()) {
    let key: SDK.RetainedValidationWitnessKey;
    let value: SDK.RetainedValidationWitness;
    try {
      key = SDK.decodeRetainedValidationWitnessKey(Buffer.from(keyHex, "hex"));
      value = SDK.decodeRetainedValidationWitness(Buffer.from(valueHex, "hex"));
    } catch (cause) {
      throw new DaPayloadValidationError(
        "malformed_trace",
        `validation_trace_witnesses[${index.toString()}] is not canonical`,
        { cause },
      );
    }
    const fingerprint = eventKeyFingerprint(key.event_key);
    const descriptor = descriptors.get(fingerprint);
    if (descriptor === undefined) {
      throw new DaPayloadValidationError(
        "coverage_mismatch",
        "validation trace witness is orphaned from validation_traces",
      );
    }
    const canonicalEventKey = Buffer.from(
      LucidData.to(key.event_key as never, SDK.EventKeySchema as never),
      "hex",
    );
    if (!canonicalEventKey.equals(descriptor.keyCbor)) {
      throw new DaPayloadValidationError(
        "coverage_mismatch",
        "validation trace witness event key differs from its descriptor key",
      );
    }
    const coordinate = `${fingerprint}:${key.execution_index.toString()}`;
    if (coordinates.has(coordinate)) {
      throw new DaPayloadValidationError(
        "duplicate_key",
        `duplicate retained validation witness ${coordinate}`,
      );
    }
    coordinates.add(coordinate);
    const auxiliary = value.auxiliary;
    const native =
      typeof auxiliary === "object" &&
      "NativeExecutionDescriptorWitness" in auxiliary
        ? auxiliary.NativeExecutionDescriptorWitness
        : null;
    const slot = retainedWitnessSlot(
      key.execution_index,
      BigInt(descriptor.descriptor.stepCount),
    );
    if (slot === null) {
      throw new DaPayloadValidationError(
        "coverage_mismatch",
        "retained validation witness coordinate is outside its descriptor's retained domain",
      );
    }
    if (slot.kind === "nativeAlias" && native === null) {
      throw new DaPayloadValidationError(
        "coverage_mismatch",
        "retained non-negative validation coordinates are reserved for NativeScripts execution aliases",
      );
    }
    if (
      native !== null &&
      ((slot.kind === "nativeAlias" &&
        native.execution_index !== key.execution_index) ||
        (native.language_tag !== 0n &&
          native.language_tag !== 3n &&
          native.language_tag !== 128n) ||
        (native.origin_kind !== 0n && native.origin_kind !== 1n))
    ) {
      throw new DaPayloadValidationError(
        "coverage_mismatch",
        "retained validation witness execution index differs from its key",
      );
    }
    if (native !== null && native.origin_kind === 0n) {
      if (
        !Buffer.from(native.source_key, "hex").equals(
          encodeCbor(native.source_index),
        )
      ) {
        throw new DaPayloadValidationError(
          "coverage_mismatch",
          "inline retained validation witness has a non-canonical source key",
        );
      }
      const rawTransaction = rawTransactions.get(fingerprint);
      if (rawTransaction === undefined) {
        throw new DaPayloadValidationError(
          "coverage_mismatch",
          "inline retained validation witness has no canonical transaction preimage",
        );
      }
      const projection = projectMidgardRawEnvelopeForPhaseAV1(
        rawTransaction,
        "ForcedTransactionEventKey" in key.event_key ? "forced" : "normal",
      );
      const sourceIndex = retainedSafeNumber(
        native.source_index,
        "source index",
      );
      const rawScript = projection.scriptWitnesses[sourceIndex];
      if (rawScript === undefined) {
        throw new DaPayloadValidationError(
          "coverage_mismatch",
          "inline retained validation witness source index is absent from raw field 6",
        );
      }
      const bounded = buildMidgardBoundedItem({
        fieldIndex: 6,
        itemIndex: sourceIndex,
        bytes: rawScript.versionedItemBytes,
      });
      if (
        BigInt(rawScript.languageTag) !== native.language_tag ||
        !rawScript.hash.equals(Buffer.from(native.script_hash, "hex")) ||
        BigInt(rawScript.versionedItemBytes.length) !==
          native.script_total_length ||
        !bounded.commitment.equals(
          Buffer.from(native.script_item_commitment, "hex"),
        )
      ) {
        throw new DaPayloadValidationError(
          "coverage_mismatch",
          "inline retained validation witness differs from exact raw field-6 item",
        );
      }
    }
    const state = stateFromRetainedData(value.machine_state);
    const proof = SDK.validationTraceProofCoreFromData(value.trace_proof);
    const expectedStateIndex =
      slot.kind === "state"
        ? slot.stateIndex
        : slot.kind === "initial"
          ? 0n
          : slot.kind === "terminal"
            ? BigInt(descriptor.descriptor.stepCount)
            : value.trace_proof.state_index;
    const expectedEndpointHash =
      slot.kind === "initial"
        ? descriptor.descriptor.initialStateHash
        : slot.kind === "terminal"
          ? descriptor.descriptor.terminalStateHash
          : proof.stateHash;
    if (
      value.trace_proof.state_index !== expectedStateIndex ||
      !proof.stateHash.equals(expectedEndpointHash) ||
      (native !== null && state.phase !== "nativeScripts") ||
      value.program_counter !== value.machine_state.program_counter ||
      state.programCounter !==
        retainedSafeNumber(value.program_counter, "program counter") ||
      !hashMidgardValidationMachineState(state).equals(proof.stateHash) ||
      !verifyMidgardValidationTraceProof({
        descriptor: descriptor.descriptor,
        proof,
      })
    ) {
      throw new DaPayloadValidationError(
        "coverage_mismatch",
        "retained validation witness state/proof does not open the committed descriptor",
      );
    }
    const expectedEventHash = hashMidgardValidationEventKey(canonicalEventKey);
    if (!state.eventKeyHash.equals(expectedEventHash)) {
      throw new DaPayloadValidationError(
        "coverage_mismatch",
        "retained validation witness state is bound to another event key",
      );
    }
    const witnessCbor = Buffer.from(value.witness_cbor, "hex");
    if (slot.kind === "initial") {
      // The initial endpoint opens the exact validation-context bytes under
      // the reserved phase marker -1 instead of a work witness.
      if (
        value.phase !== -1n ||
        !hashMidgardValidationContext(witnessCbor).equals(
          state.validationContextHash,
        )
      ) {
        throw new DaPayloadValidationError(
          "coverage_mismatch",
          "retained validation context differs from the initial operator state",
        );
      }
    } else if (
      value.phase !== BigInt(MidgardValidationPhase[state.phase]) ||
      !state.workRoot.equals(
        hashMidgardValidationWorkWitness({
          phase: state.phase,
          programCounter: state.programCounter,
          witnessCbor,
        }),
      )
    ) {
      throw new DaPayloadValidationError(
        "coverage_mismatch",
        "retained validation witness bytes do not match state.work_root",
      );
    }
    // Every retained record is authenticated above against the committed
    // descriptor, its event key and its own phase's work root; family
    // reconstruction performs the phase-specific semantic checks. Native
    // execution descriptors keep the additional eager checks below.
    if (native === null) continue;
    const roots = nativeControlRoots(Buffer.from(value.witness_cbor, "hex"));
    const purposeKind = retainedSafeNumber(native.purpose_kind, "purpose kind");
    if (purposeKind > 3) {
      throw new DaPayloadValidationError(
        "malformed_trace",
        "retained validation witness purpose kind is unsupported",
      );
    }
    const purposeLeaf = hashMidgardScriptPurposeLeaf({
      purposeKind: purposeKind as 0 | 1 | 2 | 3,
      purposeIndex: native.purpose_index,
      scriptHash: Buffer.from(native.script_hash, "hex"),
      subject: Buffer.from(native.subject, "hex"),
    });
    const sourceLeaf =
      native.origin_kind === 0n
        ? hashMidgardInlineScriptSourceLeaf({
            sourceIndex: native.source_index,
            scriptLanguageTag: Number(native.language_tag) as 0 | 3 | 128,
            scriptHash: Buffer.from(native.script_hash, "hex"),
            scriptTotalLength: retainedSafeNumber(
              native.script_total_length,
              "script length",
            ),
            itemCommitment: Buffer.from(native.script_item_commitment, "hex"),
          })
        : hashMidgardReferenceScriptSourceLeaf({
            sourceKey: Buffer.from(native.source_key, "hex"),
            scriptLanguageTag: Number(native.language_tag) as 0 | 3 | 128,
            scriptHash: Buffer.from(native.script_hash, "hex"),
            scriptTotalLength: retainedSafeNumber(
              native.script_total_length,
              "script length",
            ),
            itemCommitment: Buffer.from(native.script_item_commitment, "hex"),
          });
    const executionLeaf = hashMidgardScriptExecutionLeaf({
      languageTag: Number(native.language_tag) as 0 | 3 | 128,
      purposeLeaf,
      sourceLeaf,
      redeemerLeaf: Buffer.from(native.redeemer_leaf, "hex"),
    });
    requireRetainedMembership(
      roots.purpose,
      native.purpose_index,
      purposeLeaf,
      native.purpose_siblings,
      "purpose",
    );
    requireRetainedMembership(
      roots.source,
      native.source_index,
      sourceLeaf,
      native.source_siblings,
      "source",
    );
    requireRetainedMembership(
      roots.execution,
      native.execution_index,
      executionLeaf,
      native.execution_siblings,
      "execution",
    );
    let signerFrontierValid = native.language_tag !== 0n;
    if (native.language_tag === 0n) {
      try {
        signerFrontierValid = commitMidgardValidationMerkleFrontier(
          retainedFrontier(roots.signerCount, native.signer_peaks),
        ).equals(roots.signerCommitment);
      } catch {
        signerFrontierValid = false;
      }
    }
    if (!signerFrontierValid) {
      throw new DaPayloadValidationError(
        "coverage_mismatch",
        "retained validation signer frontier is invalid",
      );
    }
  }
};
