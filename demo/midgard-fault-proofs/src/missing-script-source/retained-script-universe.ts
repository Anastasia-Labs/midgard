import {
  hashMidgardInlineScriptSourceLeaf,
  hashMidgardReferenceScriptSourceLeaf,
  hashMidgardScriptPurposeLeaf,
  hashMidgardValidationEventKey,
  hashMidgardValidationMachineState,
  hashMidgardValidationWorkWitness,
  type MidgardValidationMachineState,
  verifyMidgardValidationMerkleMembership,
  verifyMidgardValidationTraceProof,
} from "@al-ft/midgard-core";
import { decodeSingleCbor } from "@al-ft/midgard-core/codec/cbor";
import {
  decodeRetainedValidationWitness,
  decodeRetainedValidationWitnessKey,
  type EventKey,
  EventKeySchema,
  Proof,
  type RetainedValidationWitness,
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
import { ScriptSourcesControlSchema } from "./schemas.js";
import type { ExecutionSourceAuthenticationData } from "./submit-step-02.js";

type EncodedEntry = Readonly<{ key: Uint8Array; value: Uint8Array }>;
type Peak = Readonly<{ height: bigint; hash: string }>;

const fail = (message: string): never => {
  throw new Error(`missingScriptSource retained universe: ${message}`);
};
const integer = (value: unknown, label: string): bigint => {
  if (typeof value !== "bigint" && typeof value !== "number")
    return fail(`${label} is not an integer`);
  return BigInt(value);
};
const exactNumber = (value: bigint, label: string): number => {
  const result = Number(value);
  if (!Number.isSafeInteger(result) || result < 0)
    return fail(`${label} is outside safe natural range`);
  return result;
};
const bytes = (value: unknown, label: string): string => {
  if (!(value instanceof Uint8Array)) return fail(`${label} is not bytes`);
  return Buffer.from(value).toString("hex");
};
const list = (value: unknown, label: string): readonly unknown[] => {
  if (!Array.isArray(value)) return fail(`${label} is not a list`);
  return value;
};
const peaks = (value: unknown, label: string): readonly Peak[] =>
  list(value, label).map((entry, index) => {
    const pair = list(entry, `${label}[${index.toString()}]`);
    if (pair.length !== 2) return fail(`${label} peak shape changed`);
    return {
      height: integer(pair[0], "frontier height"),
      hash: bytes(pair[1], "frontier hash"),
    };
  });

type ParsedControl = Readonly<{
  control: Data.Static<typeof ScriptSourcesControlSchema>;
  controlData: Data;
  sourceCount: bigint;
  sourcePeaks: readonly Peak[];
  purposeCount: bigint;
  purposePeaks: readonly Peak[];
  transactionSourceCount: bigint;
  discovery: Readonly<{
    purposeCursor: bigint;
    sourceCursor: bigint;
    purposeKind: bigint;
    purposeIndex: bigint;
    scriptHash: string;
    subject: string;
    matchedSourceIndex: bigint;
  }>;
}>;

const parseStageNineControl = (witnessCbor: string): ParsedControl => {
  const value = list(
    decodeSingleCbor(Buffer.from(witnessCbor, "hex")),
    "ScriptSources control",
  );
  if (value.length !== 31 || integer(value[9], "stage") !== 9n)
    return fail("control is not canonical ScriptSources stage 9");
  const discovery = list(
    decodeSingleCbor(Buffer.from(bytes(value[30], "discovery bytes"), "hex")),
    "discovery control",
  );
  if (discovery.length !== 15) return fail("discovery control shape changed");
  const sourceCount = integer(value[10], "source count");
  const sourceCursor = integer(discovery[1], "source cursor");
  if (sourceCursor < 0n || sourceCursor > sourceCount)
    return fail("source cursor is outside the authenticated frontier");
  const sourcePeaks = peaks(value[11], "source frontier");
  const transactionSourceCount =
    exactNumber(sourceCount, "source count") === 0 ? 0n : sourceCount; // refined from the ordered source witnesses below
  const receive = list(value[24], "receive scan");
  const observer = list(value[27], "observer scan");
  const mint = list(value[28], "mint fold");
  if (receive.length !== 6 || observer.length !== 3 || mint.length !== 12)
    return fail("nested ScriptSources control shape changed");
  const frontier = (raw: unknown, label: string) =>
    peaks(raw, label).map(({ height, hash }) => ({ height, hash }));
  const control: Data.Static<typeof ScriptSourcesControlSchema> = {
    compact_cbor: bytes(value[0], "compact cbor"),
    witness_set_compact_cbor: bytes(value[1], "witness set compact cbor"),
    field_preimage_lengths_cbor: bytes(value[2], "field lengths cbor"),
    context_cbor: bytes(value[3], "context cbor"),
    resolved_input_count: integer(value[4], "resolved input count"),
    resolved_inputs_accumulator: bytes(value[5], "resolved accumulator"),
    signer_count: integer(value[6], "signer count"),
    signer_frontier_commitment: bytes(value[7], "signer commitment"),
    resolved_item_peaks: frontier(value[8], "resolved peaks"),
    stage: 9n,
    source_count: sourceCount,
    source_peaks: frontier(value[11], "source peaks"),
    redeemer_count: integer(value[12], "redeemer count"),
    redeemer_peaks: frontier(value[13], "redeemer peaks"),
    replay_cursor: integer(value[14], "replay cursor"),
    replay_accumulator: bytes(value[15], "replay accumulator"),
    replay_remaining_schedule_hash: bytes(value[16], "remaining schedule"),
    spend_index: integer(value[17], "spend index"),
    purpose_count: integer(value[18], "purpose count"),
    purpose_peaks: frontier(value[19], "purpose peaks"),
    output_cursor: integer(value[20], "output cursor"),
    output_count: integer(value[21], "output count"),
    output_peaks: frontier(value[22], "output peaks"),
    output_total_count: integer(value[23], "output total count"),
    receive_scan: {
      source_count: integer(receive[0], "receive source count"),
      source_peaks: frontier(receive[1], "receive source peaks"),
      receive_count: integer(receive[2], "receive count"),
      previous_hash: bytes(receive[3], "receive previous hash"),
      candidate_hash: bytes(receive[4], "receive candidate hash"),
      descriptor_peaks: frontier(receive[5], "receive descriptor peaks"),
    },
    source_total_count: integer(value[25], "source total count"),
    redeemer_total_count: integer(value[26], "redeemer total count"),
    observer_scan: {
      total_count: integer(observer[0], "observer total count"),
      seen: integer(observer[2], "observer seen"),
      previous_hash: bytes(observer[1], "observer previous hash"),
    },
    discovery: {
      purpose_cursor: integer(discovery[0], "purpose cursor"),
      source_cursor: sourceCursor,
      redeemer_cursor: integer(discovery[2], "redeemer cursor"),
      current_purpose_kind: integer(discovery[3], "purpose kind"),
      current_purpose_index: integer(discovery[4], "purpose index"),
      current_script_hash: bytes(discovery[5], "required script hash"),
      current_subject: bytes(discovery[6], "purpose subject"),
      matched_source_index: integer(discovery[7], "matched source index"),
      matched_language_tag: integer(discovery[8], "matched language tag"),
      matched_source_leaf: bytes(discovery[9], "matched source leaf"),
      used_inline_bitmap: integer(discovery[10], "used inline bitmap"),
      used_redeemer_bitmap: integer(discovery[11], "used redeemer bitmap"),
      redeemer_item_control_hash: bytes(discovery[12], "redeemer control hash"),
      execution_count: integer(discovery[13], "execution count"),
      execution_peaks: frontier(discovery[14], "execution peaks"),
    },
    output_proof: null,
    pending_source_cbor: "",
    mint_fold: {
      policy_count: integer(mint[0], "mint policy count"),
      policy_cursor: integer(mint[1], "mint policy cursor"),
      previous_policy: bytes(mint[2], "mint previous policy"),
      active_policy: bytes(mint[3], "mint active policy"),
      item_length: integer(mint[4], "mint item length"),
      item_commitment: bytes(mint[5], "mint item commitment"),
      item_cursor: integer(mint[6], "mint item cursor"),
      assets_remaining: integer(mint[7], "mint assets remaining"),
      policy_asset_cursor: integer(mint[8], "mint asset cursor"),
      previous_asset: bytes(mint[9], "mint previous asset"),
      asset_count: integer(mint[10], "mint asset count"),
      asset_peaks: frontier(mint[11], "mint asset peaks"),
    },
    resolution_schedule_hash: bytes(value[29], "resolution schedule"),
  };
  return {
    control,
    controlData: Data.from(
      Data.to(control as never, ScriptSourcesControlSchema as never),
    ),
    sourceCount,
    sourcePeaks,
    purposeCount: integer(value[18], "purpose count"),
    purposePeaks: peaks(value[19], "purpose frontier"),
    transactionSourceCount,
    discovery: {
      purposeCursor: integer(discovery[0], "purpose cursor"),
      sourceCursor,
      purposeKind: integer(discovery[3], "purpose kind"),
      purposeIndex: integer(discovery[4], "purpose index"),
      scriptHash: bytes(discovery[5], "required script hash"),
      subject: bytes(discovery[6], "purpose subject"),
      matchedSourceIndex: integer(discovery[7], "matched source index"),
    },
  };
};

const sameEvent = (left: EventKey, right: EventKey): boolean =>
  Data.to(left as never, EventKeySchema) ===
  Data.to(right as never, EventKeySchema);

const stateFromData = (
  state: RetainedValidationWitness["machine_state"],
): MidgardValidationMachineState => {
  const machineVersion = exactNumber(state.machine_version, "machine version");
  if (machineVersion !== 1) return fail("machine version changed");
  if (state.phase !== "ScriptSources")
    return fail("machine state is not ScriptSources");
  return {
    machineVersion,
    eventKeyHash: Buffer.from(state.event_key_hash, "hex"),
    transactionId: Buffer.from(state.transaction_id, "hex"),
    transactionCommitment: Buffer.from(state.transaction_commitment, "hex"),
    validationContextHash: Buffer.from(state.validation_context_hash, "hex"),
    sourceKind: state.source_kind === "Normal" ? "normal" : "forced",
    priorLedgerRoot: Buffer.from(state.prior_ledger_root, "hex"),
    phase: "scriptSources",
    programCounter: exactNumber(state.program_counter, "program counter"),
    workRoot: Buffer.from(state.work_root, "hex"),
    executionCpu: state.execution_cpu,
    executionMemory: state.execution_memory,
    verdict:
      state.verdict === "Pending"
        ? "pending"
        : state.verdict === "Accepted"
          ? "accepted"
          : "rejected",
    rejectionCodeHash: Buffer.from(state.rejection_code_hash, "hex"),
    ledgerDeltaRoot: Buffer.from(state.ledger_delta_root, "hex"),
  };
};

type PurposeScanWitness = Readonly<{
  purpose_kind: bigint;
  purpose_index: bigint;
  script_hash: string;
  subject: string;
  siblings: readonly string[];
}>;
type SourceScanWitness = Readonly<{
  source_index: bigint;
  origin_kind: bigint;
  source_key: string;
  script_language_tag: bigint;
  script_hash: string;
  script_total_length: bigint;
  script_item_commitment: string;
  siblings: readonly string[];
}>;

const auxiliaryObject = <T>(
  witness: RetainedValidationWitness,
  name: string,
): T | null => {
  const auxiliary = witness.auxiliary;
  return typeof auxiliary === "object" &&
    auxiliary !== null &&
    name in auxiliary
    ? ((auxiliary as unknown as Record<string, unknown>)[name] as T)
    : null;
};

export type RetainedMissingScriptSourceUniverse = Readonly<{
  authentication: ExecutionSourceAuthenticationData;
  purpose: Readonly<{
    absoluteIndex: number;
    purposeKind: 0 | 1 | 2 | 3;
    purposeIndex: number;
    requiredScriptHashHex: string;
    subjectHex: string;
    membership: ExecutionSourceDescriptor["purposeMembership"];
  }>;
  sources: readonly ExecutionSourceDescriptor[];
  transactionSourceCount: number;
}>;

/** Discovers each canonical unmatched stage-9 purpose coordinate in one pass. */
export const discoverRetainedMissingScriptSourceCoordinates = ({
  eventKey,
  retainedValidationWitnessEntries,
}: {
  eventKey: EventKey;
  retainedValidationWitnessEntries: readonly EncodedEntry[];
}): readonly Readonly<{
  purposeKind: 0 | 1 | 2 | 3;
  purposeIndex: number;
}>[] => {
  const coordinates = new Map<
    string,
    { purposeKind: 0 | 1 | 2 | 3; purposeIndex: number }
  >();
  for (const entry of retainedValidationWitnessEntries) {
    const key = decodeRetainedValidationWitnessKey(entry.key);
    if (!sameEvent(key.event_key, eventKey)) continue;
    const witness = decodeRetainedValidationWitness(entry.value);
    if (witness.phase !== 8n || witness.auxiliary !== "NoAuxiliaryWitness")
      continue;
    let control: ParsedControl;
    try {
      control = parseStageNineControl(witness.witness_cbor);
    } catch {
      continue;
    }
    if (control.discovery.matchedSourceIndex !== -1n) continue;
    const purposeKind = exactNumber(
      control.discovery.purposeKind,
      "purpose kind",
    );
    if (purposeKind > 3) return fail("purpose kind changed");
    const purposeIndex = exactNumber(
      control.discovery.purposeIndex,
      "purpose index",
    );
    const coordinate = {
      purposeKind: purposeKind as 0 | 1 | 2 | 3,
      purposeIndex,
    };
    const fingerprint = `${purposeKind.toString()}:${purposeIndex.toString()}`;
    if (coordinates.has(fingerprint))
      return fail("terminal purpose coordinate is duplicated");
    coordinates.set(fingerprint, coordinate);
  }
  return Object.freeze([...coordinates.values()]);
};

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
      const control = parseStageNineControl(entry.witness.witness_cbor);
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
  const purposeCandidates = retained.filter(({ key, witness }) => {
    if (key.execution_index >= terminal.key.execution_index) return false;
    const purpose = auxiliaryObject<PurposeScanWitness>(
      witness,
      "ScriptPurposeScanWitness",
    );
    return (
      purpose?.purpose_kind === BigInt(purposeKind) &&
      purpose?.purpose_index === BigInt(purposeIndex)
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
