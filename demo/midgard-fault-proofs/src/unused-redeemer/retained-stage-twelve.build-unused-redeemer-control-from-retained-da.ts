import {
  buildMidgardValidationMerkleMembership,
  hashMidgardInlineScriptSourceLeaf,
  hashMidgardRedeemerItemLeaf,
  hashMidgardReferenceScriptSourceLeaf,
  hashMidgardScriptExecutionLeaf,
  hashMidgardScriptPurposeLeaf,
  hashMidgardValidationEventKey,
  hashMidgardValidationMachineState,
  hashMidgardValidationWorkWitness,
  MidgardValidationPhase,
  verifyMidgardValidationMerkleMembership,
  verifyMidgardValidationTraceProof,
} from "@al-ft/midgard-core";
import {
  decodeRetainedValidationWitness,
  decodeRetainedValidationWitnessKey,
  encodeRetainedValidationWitness,
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
  type Control,
  coreFrontier,
  decodeUnusedRedeemerDirectionControl,
  type EncodedEntry,
  exactNumber,
  machineState,
  type RetainedLegacyUnusedRedeemerPurpose,
  type RetainedLegacyUnusedRedeemerSource,
} from "./retained-stage-twelve.decode-unused-redeemer-direction-control.js";

/** Strict, callback-free reconstruction of the complete stage-12 universe. */
export const buildUnusedRedeemerControlFromRetainedDa = async ({
  eventKey,
  transactionId,
  redeemerIndex,
  authenticatedValidationTraceEntries,
  retainedValidationWitnessEntries,
  expectedValidationTracesRoot,
}: {
  eventKey: EventKey;
  transactionId: string;
  redeemerIndex: number;
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
      "unusedRedeemer validation descriptor is absent or duplicated",
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
    .sort((left, right) =>
      left.key.execution_index < right.key.execution_index ? -1 : 1,
    );
  const validatedAll = eventEntries
    .filter(
      ({ retained }) =>
        (retained.phase === 8n &&
          retained.machine_state.phase === "ScriptSources") ||
        (retained.phase === 9n &&
          retained.machine_state.phase === "NativeScripts"),
    )
    .map(({ key, retained }) => {
      const state = machineState(retained.machine_state);
      const proof = validationTraceProofCoreFromData(retained.trace_proof);
      if (
        retained.program_counter !== retained.machine_state.program_counter ||
        retained.program_counter !== retained.trace_proof.state_index ||
        retained.trace_proof.state_index >= BigInt(descriptor.stepCount) ||
        retained.phase !== BigInt(MidgardValidationPhase[state.phase]) ||
        state.transactionId.toString("hex") !== transactionId ||
        !state.eventKeyHash.equals(
          hashMidgardValidationEventKey(eventKeyCbor),
        ) ||
        !hashMidgardValidationMachineState(state).equals(proof.stateHash) ||
        !verifyMidgardValidationTraceProof({ descriptor, proof }) ||
        !state.workRoot.equals(
          hashMidgardValidationWorkWitness({
            phase: state.phase,
            programCounter: state.programCounter,
            witnessCbor: Buffer.from(retained.witness_cbor, "hex"),
          }),
        )
      )
        throw new Error(
          "unusedRedeemer retained state/proof/work witness is invalid",
        );
      return { key, retained, state };
    });
  const validated = validatedAll.filter(
    ({ retained, state }) =>
      retained.phase === 8n && state.phase === "scriptSources",
  );
  const terminals = validated.flatMap(({ retained }) => {
    try {
      const control = decodeUnusedRedeemerDirectionControl(
        Buffer.from(retained.witness_cbor, "hex"),
      );
      const auxiliary = retained.auxiliary;
      const exactDirectionState =
        typeof auxiliary === "object" &&
        "RedeemerItemStepWitness" in auxiliary &&
        auxiliary.RedeemerItemStepWitness.control.item_index ===
          BigInt(redeemerIndex) &&
        auxiliary.RedeemerItemStepWitness.control.stage === 0n &&
        control.discovery.redeemer_cursor === BigInt(redeemerIndex) &&
        control.stage === 12n;
      return exactDirectionState ? [{ retained, control }] : [];
    } catch {
      return [];
    }
  });
  if (terminals.length !== 1)
    throw new Error(
      "unusedRedeemer exact direction-specific ScriptSources state is absent or duplicated",
    );
  const { retained, control } = terminals[0]!;
  const selectedBit =
    (control.discovery.used_redeemer_bitmap >> BigInt(redeemerIndex)) & 1n;
  if (
    control.source_total_count !== control.source_count ||
    control.redeemer_total_count !== control.redeemer_count ||
    control.discovery.execution_count !== control.purpose_count ||
    control.discovery.redeemer_item_control_hash === ""
  )
    throw new Error(
      `unusedRedeemer terminal ScriptSources frontier is incomplete: ${JSON.stringify({ sourceTotal: control.source_total_count.toString(), sourceCount: control.source_count.toString(), redeemerTotal: control.redeemer_total_count.toString(), redeemerCount: control.redeemer_count.toString(), executionCount: control.discovery.execution_count.toString(), purposeCount: control.purpose_count.toString(), itemHash: control.discovery.redeemer_item_control_hash })}`,
    );

  const sourceCandidates = validated.flatMap(({ retained: item }) => {
    const auxiliary = item.auxiliary;
    return typeof auxiliary === "object" &&
      "ScriptSourceScanWitness" in auxiliary
      ? [auxiliary.ScriptSourceScanWitness]
      : [];
  });
  const sourceCount = exactNumber(control.source_count, "source count");
  const retainedSourceCount = sourceCount;
  const sources = Array.from(
    { length: retainedSourceCount },
    (_, sourceIndex) => {
      const matches = sourceCandidates.filter(
        (source) => source.source_index === BigInt(sourceIndex),
      );
      const unique = new Map(
        matches.map((value) => [
          JSON.stringify(value, (_key, item: unknown) =>
            typeof item === "bigint" ? item.toString() : item,
          ),
          value,
        ]),
      );
      if (unique.size !== 1)
        throw new Error(
          "unusedRedeemer retained source frontier is incomplete or ambiguous",
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
        throw new Error("unusedRedeemer retained source descriptor changed");
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
        throw new Error("unusedRedeemer retained source membership is invalid");
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
      } as RetainedLegacyUnusedRedeemerSource;
    },
  );

  const purposeCandidates = validated.flatMap(({ retained: item }) => {
    const auxiliary = item.auxiliary;
    return typeof auxiliary === "object" &&
      "ScriptPurposeScanWitness" in auxiliary
      ? [auxiliary.ScriptPurposeScanWitness]
      : [];
  });
  const purposeCount = exactNumber(control.purpose_count, "purpose count");
  if (purposeCandidates.length !== purposeCount)
    throw new Error(
      "unusedRedeemer retained purpose frontier is incomplete or duplicated",
    );
  const purposes = purposeCandidates.map((purpose, frontierIndex) => {
    const purposeKind = exactNumber(purpose.purpose_kind, "purpose kind");
    if (
      purposeKind !== 0 &&
      purposeKind !== 1 &&
      purposeKind !== 2 &&
      purposeKind !== 3
    )
      throw new Error("unusedRedeemer retained purpose kind changed");
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
      throw new Error("unusedRedeemer retained purpose membership is invalid");
    return {
      frontierIndex,
      purposeKind,
      purposeIndex,
      scriptHashHex: purpose.script_hash,
      purposeSubjectHex: purpose.subject,
      siblings: purpose.siblings,
    } as RetainedLegacyUnusedRedeemerPurpose;
  });

  const beginMatches = validated.filter(
    ({ retained: candidate }) =>
      candidate.program_counter === retained.program_counter - 1n &&
      typeof candidate.auxiliary === "object" &&
      "RedeemerScanBeginWitness" in candidate.auxiliary &&
      candidate.auxiliary.RedeemerScanBeginWitness.item_index ===
        BigInt(redeemerIndex),
  );
  if (beginMatches.length !== 1)
    throw new Error("unusedRedeemer retained begin witness changed");
  const beginAuxiliary = beginMatches[0]!.retained.auxiliary;
  if (
    !(
      typeof beginAuxiliary === "object" &&
      "RedeemerScanBeginWitness" in beginAuxiliary
    )
  )
    throw new Error("unusedRedeemer retained begin witness changed");
  const itemSteps = validated.flatMap(({ retained: item }) => {
    const auxiliary = item.auxiliary;
    if (
      !(typeof auxiliary === "object" && "RedeemerItemStepWitness" in auxiliary)
    )
      return [];
    if (
      auxiliary.RedeemerItemStepWitness.control.item_index !==
      BigInt(redeemerIndex)
    )
      return [];
    const allowed =
      item.program_counter === retained.program_counter ||
      item.program_counter === retained.program_counter + 1n;
    return allowed ? [auxiliary.RedeemerItemStepWitness] : [];
  });
  const nativeStates = new Map<bigint, string>();
  const retainedNativeExecutions = validatedAll.flatMap(
    ({ retained: item, state }) => {
      const auxiliary = item.auxiliary;
      if (
        state.phase !== "nativeScripts" ||
        item.phase !== 9n ||
        typeof auxiliary !== "object" ||
        !("NativeExecutionDescriptorWitness" in auxiliary)
      )
        return [];
      const exact = encodeRetainedValidationWitness(item).toString("hex");
      const previous = nativeStates.get(item.trace_proof.state_index);
      if (previous !== undefined) {
        if (previous !== exact)
          throw new Error("unusedRedeemer retained native aliases disagree");
        return [];
      }
      nativeStates.set(item.trace_proof.state_index, exact);
      return [auxiliary.NativeExecutionDescriptorWitness];
    },
  );
  const reconstructedExecutions = validated.flatMap(({ retained: item }) => {
    const auxiliary = item.auxiliary;
    if (
      !(
        typeof auxiliary === "object" && "RedeemerItemStepWitness" in auxiliary
      ) ||
      auxiliary.RedeemerItemStepWitness.control.stage !== 1n
    )
      return [];
    let selectionControl: Control;
    try {
      selectionControl = decodeUnusedRedeemerDirectionControl(
        Buffer.from(item.witness_cbor, "hex"),
      );
    } catch {
      return [];
    }
    if (selectionControl.stage !== 10n) return [];
    const discovery = selectionControl.discovery;
    const languageTag = exactNumber(
      discovery.matched_language_tag,
      "language tag",
    );
    if (languageTag !== 0 && languageTag !== 3 && languageTag !== 128)
      throw new Error("unusedRedeemer selected language tag changed");
    const itemControl = auxiliary.RedeemerItemStepWitness.control;
    // A purpose scans earlier redeemer coordinates before it reaches its own.
    // Only the exact selected pointer contributes an execution leaf.
    const purposeTag = [0n, 1n, 3n, 6n][Number(discovery.current_purpose_kind)];
    if (
      purposeTag === undefined ||
      itemControl.purpose_tag !== purposeTag ||
      itemControl.pointer_index !== discovery.current_purpose_index
    )
      return [];
    const purpose = purposes.find(
      (candidate) =>
        candidate.purposeKind === Number(discovery.current_purpose_kind) &&
        candidate.purposeIndex === Number(discovery.current_purpose_index),
    );
    if (purpose === undefined)
      throw new Error("unusedRedeemer selected purpose witness is absent");
    const redeemerLeaf = hashMidgardRedeemerItemLeaf({
      redeemerIndex: exactNumber(itemControl.item_index, "item index"),
      itemCommitment: Buffer.from(itemControl.item_commitment, "hex"),
    });
    const purposeLeaf = hashMidgardScriptPurposeLeaf({
      purposeKind: purpose.purposeKind,
      purposeIndex: BigInt(purpose.purposeIndex),
      scriptHash: Buffer.from(purpose.scriptHashHex, "hex"),
      subject: Buffer.from(purpose.purposeSubjectHex, "hex"),
    });
    return [
      {
        execution_index: discovery.execution_count,
        purpose_kind: discovery.current_purpose_kind,
        purpose_index: discovery.current_purpose_index,
        script_hash: purpose.scriptHashHex,
        subject: purpose.purposeSubjectHex,
        purpose_siblings: purpose.siblings,
        language_tag: discovery.matched_language_tag,
        source_leaf: discovery.matched_source_leaf,
        redeemer_leaf: redeemerLeaf.toString("hex"),
        execution_siblings: [] as string[],
        purposeLeaf,
        executionLeaf: hashMidgardScriptExecutionLeaf({
          languageTag,
          purposeLeaf,
          sourceLeaf: Buffer.from(discovery.matched_source_leaf, "hex"),
          redeemerLeaf,
        }),
      },
    ];
  });
  const executions =
    retainedNativeExecutions.length > 0
      ? retainedNativeExecutions
      : reconstructedExecutions.map((execution, index, all) => ({
          ...execution,
          execution_siblings: buildMidgardValidationMerkleMembership(
            all.map(({ executionLeaf }) => executionLeaf),
            index,
          ).siblings.map((sibling) => sibling.toString("hex")),
        }));

  const root = await buildCountedRoot(
    ROOT_DOMAINS.validationTraces,
    descriptorEntries,
  );
  if (root.root !== expectedValidationTracesRoot)
    throw new Error("unusedRedeemer retained validation root changed");
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
    selectedBit,
    witnessCbor: retained.witness_cbor,
    begin: beginAuxiliary.RedeemerScanBeginWitness,
    itemSteps: Object.freeze(itemSteps),
    executions: Object.freeze(executions),
    sources: Object.freeze(sources),
    purposes: Object.freeze(purposes),
  });
};
