import {
  hashMidgardInlineScriptSourceLeaf,
  hashMidgardReferenceScriptSourceLeaf,
  hashMidgardScriptPurposeLeaf,
  hashMidgardValidationEventKey,
  hashMidgardValidationMachineState,
  hashMidgardValidationWorkWitness,
  verifyMidgardValidationMerkleMembership,
  verifyMidgardValidationTraceProof,
} from "@al-ft/midgard-core";
import {
  decodeRetainedValidationWitness,
  decodeRetainedValidationWitnessKey,
  type EventKey,
  EventKeySchema,
  Proof,
  ROOT_DOMAINS,
  validationTraceDescriptorCoreFromData,
  ValidationTraceDescriptorSchema,
  validationTraceProofCoreFromData,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  buildCountedRoot,
  keyValuePhasProof,
} from "../transition-trace/phas.js";
import {
  coreFrontier,
  decodeUnusedScriptWitnessDirectionControl,
  type EncodedEntry,
  exactNumber,
  machineState,
  retainedScriptSourcesStage,
  type RetainedUnusedScriptPurpose,
  type RetainedUnusedScriptSource,
} from "./retained-stage-twelve.decode-unused-script-witness-direction-control.js";

/** Strict, callback-free reconstruction of the complete stage-12 universe. */
export const buildUnusedScriptWitnessDirectionControlFromRetainedDa = async ({
  eventKey,
  transactionId,
  direction,
  scriptIndex,
  authenticatedValidationTraceEntries,
  retainedValidationWitnessEntries,
  expectedValidationTracesRoot,
}: {
  eventKey: EventKey;
  transactionId: string;
  direction: 0n | 1n;
  scriptIndex: number;
  authenticatedValidationTraceEntries: readonly EncodedEntry[];
  retainedValidationWitnessEntries: readonly EncodedEntry[];
  expectedValidationTracesRoot: string;
}) => {
  const eventKeyCbor = Buffer.from(
    Data.to(eventKey as never, EventKeySchema),
    "hex",
  );
  const descriptorEntries = authenticatedValidationTraceEntries.map(
    ({ key, value }) => ({ key: Buffer.from(key), value: Buffer.from(value) }),
  );
  const descriptorMatches = descriptorEntries.filter(({ key }) =>
    key.equals(eventKeyCbor),
  );
  if (descriptorMatches.length !== 1)
    throw new Error(
      "unusedScriptWitness validation descriptor is absent or duplicated",
    );
  const descriptorData = Data.from(
    descriptorMatches[0]!.value.toString("hex"),
    ValidationTraceDescriptorSchema,
  ) as unknown as import("@al-ft/midgard-sdk").ValidationTraceDescriptor;
  const descriptor = validationTraceDescriptorCoreFromData(descriptorData);
  const eventEntries = retainedValidationWitnessEntries
    .map((entry) => ({
      key: decodeRetainedValidationWitnessKey(entry.key),
      retained: decodeRetainedValidationWitness(entry.value),
    }))
    .filter(({ key }) =>
      Buffer.from(
        Data.to(key.event_key as never, EventKeySchema),
        "hex",
      ).equals(eventKeyCbor),
    )
    // Trace order comes from the authenticated trace position, not from the
    // retained key's coordinate label.
    .sort((left, right) =>
      left.retained.trace_proof.state_index <
      right.retained.trace_proof.state_index
        ? -1
        : 1,
    );
  const validated = eventEntries
    .filter(
      ({ retained }) =>
        retained.phase === 8n &&
        retained.machine_state.phase === "ScriptSources",
    )
    .map(({ key, retained }) => {
      const state = machineState(retained.machine_state);
      const proof = validationTraceProofCoreFromData(retained.trace_proof);
      if (
        retained.program_counter !== retained.machine_state.program_counter ||
        state.transactionId.toString("hex") !== transactionId ||
        !state.eventKeyHash.equals(
          hashMidgardValidationEventKey(eventKeyCbor),
        ) ||
        !hashMidgardValidationMachineState(state).equals(proof.stateHash) ||
        !verifyMidgardValidationTraceProof({ descriptor, proof }) ||
        !state.workRoot.equals(
          hashMidgardValidationWorkWitness({
            phase: "scriptSources",
            programCounter: state.programCounter,
            witnessCbor: Buffer.from(retained.witness_cbor, "hex"),
          }),
        )
      )
        throw new Error(
          "unusedScriptWitness retained state/proof/work witness is invalid",
        );
      return { key, retained };
    });
  const terminals = validated.flatMap(({ retained }) => {
    try {
      const control = decodeUnusedScriptWitnessDirectionControl(
        Buffer.from(retained.witness_cbor, "hex"),
      );
      const auxiliary = retained.auxiliary;
      const exactDirectionState =
        direction === 0n
          ? control.stage === 11n &&
            control.discovery.source_cursor === BigInt(scriptIndex) &&
            typeof auxiliary === "object" &&
            "ScriptSourceScanWitness" in auxiliary &&
            auxiliary.ScriptSourceScanWitness.source_index ===
              BigInt(scriptIndex)
          : control.stage === 12n && auxiliary === "NoAuxiliaryWitness";
      return exactDirectionState ? [{ retained, control }] : [];
    } catch {
      return [];
    }
  });
  if (terminals.length !== 1)
    throw new Error(
      "unusedScriptWitness exact direction-specific ScriptSources state is absent or duplicated",
    );
  const { retained, control } = terminals[0]!;
  if (
    control.source_total_count !== control.source_count ||
    control.redeemer_total_count !== control.redeemer_count ||
    control.discovery.purpose_cursor !== control.purpose_count ||
    (direction === 0n
      ? control.discovery.source_cursor !== BigInt(scriptIndex)
      : control.discovery.source_cursor !== control.source_count) ||
    control.discovery.execution_count !== control.purpose_count ||
    control.discovery.current_purpose_kind !== -1n ||
    control.discovery.current_purpose_index !== -1n ||
    control.discovery.current_script_hash !== "" ||
    control.discovery.current_subject !== "" ||
    control.discovery.matched_source_index !== -1n ||
    control.discovery.matched_language_tag !== -1n ||
    control.discovery.matched_source_leaf !== ""
  )
    throw new Error(
      "unusedScriptWitness terminal ScriptSources frontier is incomplete",
    );

  const sourceCandidates = validated.flatMap(({ retained: item }) => {
    const auxiliary = item.auxiliary;
    return typeof auxiliary === "object" &&
      "ScriptSourceScanWitness" in auxiliary
      ? [auxiliary.ScriptSourceScanWitness]
      : [];
  });
  const sourceCount = exactNumber(control.source_count, "source count");
  const retainedSourceCount = direction === 0n ? scriptIndex + 1 : sourceCount;
  if (retainedSourceCount > sourceCount)
    throw new Error("unusedScriptWitness target source coordinate changed");
  const sources = Array.from(
    { length: retainedSourceCount },
    (_, sourceIndex) => {
      const matches = sourceCandidates.filter(
        (source) => source.source_index === BigInt(sourceIndex),
      );
      // The purpose discovery and the stage-11 audit both retain a scan
      // witness for every source; they must agree field for field.
      const unique = new Map(
        matches.map((value) => [
          JSON.stringify(value, (_, field: unknown) =>
            typeof field === "bigint" ? field.toString() : field,
          ),
          value,
        ]),
      );
      if (unique.size !== 1)
        throw new Error(
          "unusedScriptWitness retained source frontier is incomplete or ambiguous",
        );
      const source = [...unique.values()][0]!;
      const originKind = exactNumber(source.origin_kind, "source origin");
      const languageTag = exactNumber(
        source.script_language_tag,
        "language tag",
      );
      if (
        (originKind !== 0 && originKind !== 1) ||
        (languageTag !== 0 && languageTag !== 3 && languageTag !== 128)
      )
        throw new Error(
          "unusedScriptWitness retained source descriptor changed",
        );
      const leaf =
        originKind === 0
          ? hashMidgardInlineScriptSourceLeaf({
              sourceIndex: BigInt(sourceIndex),
              scriptLanguageTag: languageTag,
              scriptHash: Buffer.from(source.script_hash, "hex"),
              scriptTotalLength: exactNumber(
                source.script_total_length,
                "source length",
              ),
              itemCommitment: Buffer.from(source.script_item_commitment, "hex"),
            })
          : hashMidgardReferenceScriptSourceLeaf({
              sourceKey: Buffer.from(source.source_key, "hex"),
              scriptLanguageTag: languageTag,
              scriptHash: Buffer.from(source.script_hash, "hex"),
              scriptTotalLength: exactNumber(
                source.script_total_length,
                "source length",
              ),
              itemCommitment: Buffer.from(source.script_item_commitment, "hex"),
            });
      if (
        !verifyMidgardValidationMerkleMembership({
          frontier: coreFrontier(control.source_count, control.source_peaks),
          leafIndex: sourceIndex,
          leafHash: leaf,
          siblings: source.siblings.map((value) => Buffer.from(value, "hex")),
        })
      )
        throw new Error(
          "unusedScriptWitness retained source membership is invalid",
        );
      return {
        sourceIndex,
        originKind,
        sourceKeyHex: source.source_key,
        languageTag,
        scriptHashHex: source.script_hash,
        scriptTotalLength: exactNumber(
          source.script_total_length,
          "source length",
        ),
        itemCommitmentHex: source.script_item_commitment,
        siblings: source.siblings,
      } as RetainedUnusedScriptSource;
    },
  );

  // Only the stage-8 purpose discovery opens the complete purpose frontier;
  // the earlier receive-purpose scan retains same-kind witnesses over the
  // receive-source frontier and must not be mistaken for purpose leaves.
  const purposeCandidates = validated.flatMap(({ retained: item }) => {
    const auxiliary = item.auxiliary;
    return typeof auxiliary === "object" &&
      "ScriptPurposeScanWitness" in auxiliary &&
      retainedScriptSourcesStage(Buffer.from(item.witness_cbor, "hex")) === 8n
      ? [auxiliary.ScriptPurposeScanWitness]
      : [];
  });
  const purposeCount = exactNumber(control.purpose_count, "purpose count");
  if (purposeCandidates.length !== purposeCount)
    throw new Error(
      "unusedScriptWitness retained purpose frontier is incomplete or duplicated",
    );
  const purposes = purposeCandidates.map((purpose, frontierIndex) => {
    const purposeKind = exactNumber(purpose.purpose_kind, "purpose kind");
    if (
      purposeKind !== 0 &&
      purposeKind !== 1 &&
      purposeKind !== 2 &&
      purposeKind !== 3
    )
      throw new Error("unusedScriptWitness retained purpose kind changed");
    const purposeIndex = exactNumber(purpose.purpose_index, "purpose index");
    const leaf = hashMidgardScriptPurposeLeaf({
      purposeKind,
      purposeIndex: BigInt(purposeIndex),
      scriptHash: Buffer.from(purpose.script_hash, "hex"),
      subject: Buffer.from(purpose.subject, "hex"),
    });
    if (
      !verifyMidgardValidationMerkleMembership({
        frontier: coreFrontier(control.purpose_count, control.purpose_peaks),
        leafIndex: frontierIndex,
        leafHash: leaf,
        siblings: purpose.siblings.map((value) => Buffer.from(value, "hex")),
      })
    )
      throw new Error(
        "unusedScriptWitness retained purpose membership is invalid",
      );
    return {
      frontierIndex,
      purposeKind,
      purposeIndex,
      scriptHashHex: purpose.script_hash,
      purposeSubjectHex: purpose.subject,
      siblings: purpose.siblings,
    } as RetainedUnusedScriptPurpose;
  });

  const root = await buildCountedRoot(
    ROOT_DOMAINS.validationTraces,
    descriptorEntries,
  );
  if (root.root !== expectedValidationTracesRoot)
    throw new Error("unusedScriptWitness retained validation root changed");
  const membership = await keyValuePhasProof(
    { root: root.phasRoot, count: root.count, entries: root.entries },
    eventKeyCbor,
    descriptorMatches[0]!.value,
  );
  return Object.freeze({
    traceMembership: {
      domain: root.domain,
      root: root.root,
      phas_root: root.phasRoot,
      count: root.count,
      key: eventKey,
      value: descriptorData,
      proof: Data.from(Data.to(membership, Proof), Proof),
    },
    machineState: retained.machine_state,
    traceProof: retained.trace_proof,
    control,
    witnessCbor: retained.witness_cbor,
    sources: Object.freeze(sources),
    purposes: Object.freeze(purposes),
  });
};
