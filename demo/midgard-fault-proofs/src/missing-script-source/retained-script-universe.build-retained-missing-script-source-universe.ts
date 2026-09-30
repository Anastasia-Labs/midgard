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
import type { ExecutionSourceDescriptor } from "./family.js";
import {
  auxiliaryObject,
  type EncodedEntry,
  exactNumber,
  fail,
  parseRetainedScriptSourcesStageNineControl,
  type PurposeScanWitness,
  type RetainedMissingScriptSourceUniverse,
  retainedScriptSourcesStage,
  sameEvent,
  type SourceScanWitness,
  stateFromData,
} from "./retained-script-universe.parse-retained-script-sources-stage-nine-control.js";
import type { ExecutionSourceAuthenticationData } from "./submit-step-02.js";

/** Reconstructs the exact stage-9 missing-source universe from public retained DA. */
export const buildRetainedMissingScriptSourceUniverse = async ({
  eventKey,
  purposeKind,
  purposeIndex,
  authenticatedValidationTraceEntries,
  retainedValidationWitnessEntries,
  expectedValidationTracesRoot,
  expectedPresence = false,
}: {
  eventKey: EventKey;
  purposeKind: 0 | 1 | 2 | 3;
  purposeIndex: number;
  authenticatedValidationTraceEntries: readonly EncodedEntry[];
  retainedValidationWitnessEntries: readonly EncodedEntry[];
  expectedValidationTracesRoot: string;
  expectedPresence?: boolean;
}): Promise<RetainedMissingScriptSourceUniverse> => {
  const retained = retainedValidationWitnessEntries
    .map((entry) => ({
      key: decodeRetainedValidationWitnessKey(entry.key),
      witness: decodeRetainedValidationWitness(entry.value),
    }))
    .filter(({ key }) => sameEvent(key.event_key, eventKey))
    .sort((a, b) => Number(a.key.execution_index - b.key.execution_index));
  const terminalCandidates = retained.flatMap((entry) => {
    if (entry.witness.phase !== 8n) return [];
    try {
      const control = parseRetainedScriptSourcesStageNineControl(
        entry.witness.witness_cbor,
      );
      const scannedSource = auxiliaryObject<SourceScanWitness>(
        entry.witness,
        "ScriptSourceScanWitness",
      );
      const hasExpectedTerminal = expectedPresence
        ? scannedSource !== null &&
          scannedSource.source_index === control.discovery.sourceCursor &&
          scannedSource.script_hash === control.discovery.scriptHash
        : entry.witness.auxiliary === "NoAuxiliaryWitness" &&
          control.discovery.sourceCursor === control.sourceCount;
      return control.discovery.purposeKind === BigInt(purposeKind) &&
        control.discovery.purposeIndex === BigInt(purposeIndex) &&
        control.discovery.matchedSourceIndex === -1n &&
        hasExpectedTerminal
        ? [{ ...entry, control }]
        : [];
    } catch {
      return [];
    }
  });
  if (terminalCandidates.length !== 1)
    return fail("exact terminal purpose scan is absent or duplicated");
  const terminal = terminalCandidates[0]!;
  // The discovery loop opens every purpose with a stage-8 purpose scan. The
  // receive-source scan (stage 7) also retains purpose-scan witnesses for
  // receive candidates under the same (kind, index) coordinate; they are
  // not the discovery's opening and never select the universe.
  const purposeCandidates = retained.filter(({ key, witness }) => {
    if (key.execution_index >= terminal.key.execution_index) return false;
    const purpose = auxiliaryObject<PurposeScanWitness>(
      witness,
      "ScriptPurposeScanWitness",
    );
    return (
      purpose?.purpose_kind === BigInt(purposeKind) &&
      purpose?.purpose_index === BigInt(purposeIndex) &&
      retainedScriptSourcesStage(witness.witness_cbor) === 8n
    );
  });
  if (purposeCandidates.length !== 1)
    return fail("exact purpose witness is absent or duplicated");
  const purposeEntry = purposeCandidates[0]!;
  const purposeWitness = auxiliaryObject<PurposeScanWitness>(
    purposeEntry.witness,
    "ScriptPurposeScanWitness",
  )!;
  if (
    purposeWitness.script_hash !== terminal.control.discovery.scriptHash ||
    purposeWitness.subject !== terminal.control.discovery.subject
  )
    return fail("purpose witness differs from terminal discovery control");
  const purposeLeaf = hashMidgardScriptPurposeLeaf({
    purposeKind,
    purposeIndex: BigInt(purposeIndex),
    scriptHash: Buffer.from(purposeWitness.script_hash, "hex"),
    subject: Buffer.from(purposeWitness.subject, "hex"),
  });
  const purposeMembership = {
    frontier: {
      count: exactNumber(terminal.control.purposeCount, "purpose count"),
      peaks: terminal.control.purposePeaks.map(({ height, hash }) => ({
        height: exactNumber(height, "peak height"),
        hash: Buffer.from(hash, "hex"),
      })),
    },
    leafIndex: exactNumber(
      terminal.control.discovery.purposeCursor,
      "purpose cursor",
    ),
    leafHash: purposeLeaf,
    siblings: (purposeWitness.siblings as string[]).map((value) =>
      Buffer.from(value, "hex"),
    ),
  };
  if (!verifyMidgardValidationMerkleMembership(purposeMembership))
    return fail("purpose frontier membership is invalid");
  const sourceRows = retained
    .filter(
      ({ key }) =>
        key.execution_index > purposeEntry.key.execution_index &&
        (expectedPresence
          ? key.execution_index <= terminal.key.execution_index
          : key.execution_index < terminal.key.execution_index),
    )
    .flatMap(({ witness }) => {
      const source = auxiliaryObject<SourceScanWitness>(
        witness,
        "ScriptSourceScanWitness",
      );
      return source === null ? [] : [source];
    });
  const scanLimit = expectedPresence
    ? exactNumber(terminal.control.discovery.sourceCursor, "source cursor") + 1
    : exactNumber(terminal.control.sourceCount, "source count");
  if (sourceRows.length !== scanLimit)
    return fail("source witness frontier is incomplete");
  let transactionSourceCount = sourceRows.length;
  let sawReference = false;
  const sourceFrontier = {
    count: exactNumber(terminal.control.sourceCount, "source count"),
    peaks: terminal.control.sourcePeaks.map(({ height, hash }) => ({
      height: exactNumber(height, "peak height"),
      hash: Buffer.from(hash, "hex"),
    })),
  };
  const sources = sourceRows.map((source, index): ExecutionSourceDescriptor => {
    const originKind = Number(source.origin_kind) as 0 | 1;
    if (originKind !== 0 && originKind !== 1)
      return fail("source origin kind changed");
    if (originKind === 1) {
      sawReference = true;
      transactionSourceCount = Math.min(transactionSourceCount, index);
    } else if (sawReference)
      return fail("inline source follows a reference source");
    if (source.source_index !== BigInt(index))
      return fail("source witnesses are not consensus ordered");
    const leaf =
      originKind === 0
        ? hashMidgardInlineScriptSourceLeaf({
            sourceIndex: BigInt(index),
            scriptLanguageTag: Number(source.script_language_tag) as
              | 0
              | 3
              | 128,
            scriptHash: Buffer.from(source.script_hash, "hex"),
            scriptTotalLength: Number(source.script_total_length),
            itemCommitment: Buffer.from(source.script_item_commitment, "hex"),
          })
        : hashMidgardReferenceScriptSourceLeaf({
            sourceKey: Buffer.from(source.source_key, "hex"),
            scriptLanguageTag: Number(source.script_language_tag) as
              | 0
              | 3
              | 128,
            scriptHash: Buffer.from(source.script_hash, "hex"),
            scriptTotalLength: Number(source.script_total_length),
            itemCommitment: Buffer.from(source.script_item_commitment, "hex"),
          });
    const sourceMembership = {
      frontier: sourceFrontier,
      leafIndex: index,
      leafHash: leaf,
      siblings: (source.siblings as string[]).map((value) =>
        Buffer.from(value, "hex"),
      ),
    };
    if (!verifyMidgardValidationMerkleMembership(sourceMembership))
      return fail(`source ${index.toString()} membership is invalid`);
    return {
      sourceIndex: index,
      originKind,
      sourceKeyHex: source.source_key,
      languageTag: Number(source.script_language_tag) as 0 | 3 | 128,
      scriptHashHex: source.script_hash,
      scriptItemHex: "",
      scriptTotalLength: Number(source.script_total_length),
      scriptItemCommitmentHex: source.script_item_commitment,
      purposeKind,
      purposeIndex,
      purposeSubjectHex: purposeWitness.subject,
      redeemerLeafHex: "",
      purposeMembership,
      sourceMembership,
      executionMembership: purposeMembership,
    };
  });
  const eventKeyCbor = Buffer.from(
    Data.to(eventKey as never, EventKeySchema),
    "hex",
  );
  const descriptorEntries = authenticatedValidationTraceEntries.map(
    ({ key, value }) => ({ key: Buffer.from(key), value: Buffer.from(value) }),
  );
  const descriptorMatch = descriptorEntries.filter(({ key }) =>
    key.equals(eventKeyCbor),
  );
  if (descriptorMatch.length !== 1)
    return fail("validation descriptor is absent or duplicated");
  const descriptorData = Data.from(
    descriptorMatch[0]!.value.toString("hex"),
    ValidationTraceDescriptorSchema,
  ) as unknown as import("@al-ft/midgard-sdk").ValidationTraceDescriptor;
  const descriptor = validationTraceDescriptorCoreFromData(
    descriptorData as never,
  );
  const state = stateFromData(terminal.witness.machine_state);
  const traceProof = validationTraceProofCoreFromData(
    terminal.witness.trace_proof,
  );
  if (
    terminal.witness.program_counter !==
      terminal.witness.machine_state.program_counter ||
    state.programCounter !==
      exactNumber(
        terminal.witness.program_counter,
        "retained program counter",
      ) ||
    !hashMidgardValidationMachineState(state).equals(traceProof.stateHash) ||
    !verifyMidgardValidationTraceProof({ descriptor, proof: traceProof }) ||
    !state.eventKeyHash.equals(hashMidgardValidationEventKey(eventKeyCbor)) ||
    !state.workRoot.equals(
      hashMidgardValidationWorkWitness({
        phase: "scriptSources",
        programCounter: state.programCounter,
        witnessCbor: Buffer.from(terminal.witness.witness_cbor, "hex"),
      }),
    )
  )
    return fail("terminal validation state/proof/work witness is invalid");
  const root = await buildCountedRoot(
    ROOT_DOMAINS.validationTraces,
    descriptorEntries,
  );
  if (root.root !== expectedValidationTracesRoot)
    return fail("validation trace root changed");
  const proof = await keyValuePhasProof(
    { root: root.phasRoot, count: root.count, entries: root.entries },
    eventKeyCbor,
    descriptorMatch[0]!.value,
  );
  const first = sources[0];
  const authentication = {
    trace_membership: {
      domain: root.domain,
      root: root.root,
      phas_root: root.phasRoot,
      count: root.count,
      key: eventKey,
      value: descriptorData,
      proof: Data.from(Data.to(proof, Proof), Proof),
    },
    machine_state: terminal.witness.machine_state,
    trace_proof: terminal.witness.trace_proof,
    control: terminal.control.control,
    control_data: terminal.control.controlData,
    absolute_purpose_index: BigInt(purposeMembership.leafIndex),
    required_script_hash: purposeWitness.script_hash,
    purpose_kind: BigInt(purposeKind),
    purpose_index: BigInt(purposeIndex),
    script_hash: purposeWitness.script_hash,
    purpose_subject: purposeWitness.subject,
    purpose_siblings: purposeWitness.siblings,
    source_index: BigInt(first?.sourceIndex ?? 0),
    origin_kind: BigInt(first?.originKind ?? 0),
    source_key: first?.sourceKeyHex ?? "",
    language_tag: BigInt(first?.languageTag ?? 0),
    total_length: BigInt(first?.scriptTotalLength ?? 0),
    item_commitment: first?.scriptItemCommitmentHex ?? "",
    source_siblings:
      first?.sourceMembership.siblings.map((value) =>
        Buffer.from(value).toString("hex"),
      ) ?? [],
    redeemer_leaf: "",
    execution_siblings: [],
  } satisfies ExecutionSourceAuthenticationData;
  return Object.freeze({
    authentication,
    purpose: Object.freeze({
      absoluteIndex: purposeMembership.leafIndex,
      purposeKind,
      purposeIndex,
      requiredScriptHashHex: purposeWitness.script_hash,
      subjectHex: purposeWitness.subject,
      membership: purposeMembership,
    }),
    sources: Object.freeze(sources),
    transactionSourceCount,
  });
};
