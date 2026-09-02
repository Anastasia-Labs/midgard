import {
  hashMidgardInlineScriptSourceLeafV1,
  hashMidgardReferenceScriptSourceLeafV1,
  hashMidgardScriptPurposeLeafV1,
  hashMidgardValidationEventKeyV1,
  hashMidgardValidationMachineStateV1,
  hashMidgardValidationWorkWitnessV1,
  type MidgardValidationMachineStateV1,
  type MidgardValidationMerkleFrontierV1,
  verifyMidgardValidationMerkleMembershipV1,
  verifyMidgardValidationTraceProofV1,
} from "@al-ft/midgard-core";
import { decodeSingleCbor } from "@al-ft/midgard-core/codec/cbor";
import {
  decodeRetainedValidationWitnessKeyV1,
  decodeRetainedValidationWitnessV1,
  type EventKey,
  EventKeySchema,
  Proof,
  ROOT_DOMAINS,
  validationTraceDescriptorCoreFromData,
  ValidationTraceDescriptorV1Schema,
  validationTraceProofCoreFromData,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import { MissingRedeemerScriptSourcesControlV1Schema } from "../missing-redeemer/schemas-v1.js";
import {
  buildCountedRoot,
  keyValuePhasProof,
} from "../transition-trace/phas.js";

type EncodedEntry = Readonly<{ key: Uint8Array; value: Uint8Array }>;
type Control = Data.Static<typeof MissingRedeemerScriptSourcesControlV1Schema>;
type Retained = ReturnType<typeof decodeRetainedValidationWitnessV1>;

export type RetainedUnusedScriptSourceV1 = Readonly<{
  sourceIndex: number;
  originKind: 0 | 1;
  sourceKeyHex: string;
  languageTag: 0 | 3 | 128;
  scriptHashHex: string;
  scriptTotalLength: number;
  itemCommitmentHex: string;
  siblings: readonly string[];
}>;

export type RetainedUnusedScriptPurposeV1 = Readonly<{
  frontierIndex: number;
  purposeKind: 0 | 1 | 2 | 3;
  purposeIndex: number;
  scriptHashHex: string;
  purposeSubjectHex: string;
  siblings: readonly string[];
}>;

const exactNumber = (value: bigint, label: string): number => {
  const result = Number(value);
  if (!Number.isSafeInteger(result) || result < 0)
    throw new Error(`unusedScriptWitness retained ${label} is invalid`);
  return result;
};
const array = (value: unknown, label: string): readonly unknown[] => {
  if (!Array.isArray(value))
    throw new Error(`unusedScriptWitness retained ${label} is not an array`);
  return value;
};
const bytes = (value: unknown, label: string): Buffer => {
  if (!(value instanceof Uint8Array))
    throw new Error(`unusedScriptWitness retained ${label} is not bytes`);
  return Buffer.from(value);
};
const integer = (value: unknown, label: string): bigint => {
  if (typeof value !== "bigint" && typeof value !== "number")
    throw new Error(`unusedScriptWitness retained ${label} is not an integer`);
  return BigInt(value);
};
const frontier = (value: unknown, label: string) =>
  array(value, label).map((item, index) => {
    const pair = array(item, `${label}[${index.toString()}]`);
    if (pair.length !== 2)
      throw new Error(
        `unusedScriptWitness retained ${label} peak is malformed`,
      );
    return {
      height: integer(pair[0], `${label}.height`),
      hash: bytes(pair[1], `${label}.hash`).toString("hex"),
    };
  });

const decodeDiscovery = (value: unknown): Control["discovery"] => {
  const fields = array(
    decodeSingleCbor(bytes(value, "discovery cbor")),
    "discovery",
  );
  if (fields.length !== 15)
    throw new Error(
      "unusedScriptWitness retained discovery field count changed",
    );
  return {
    purpose_cursor: integer(fields[0], "purpose cursor"),
    source_cursor: integer(fields[1], "source cursor"),
    redeemer_cursor: integer(fields[2], "redeemer cursor"),
    current_purpose_kind: integer(fields[3], "purpose kind"),
    current_purpose_index: integer(fields[4], "purpose index"),
    current_script_hash: bytes(fields[5], "script hash").toString("hex"),
    current_subject: bytes(fields[6], "purpose subject").toString("hex"),
    matched_source_index: integer(fields[7], "matched source index"),
    matched_language_tag: integer(fields[8], "matched language tag"),
    matched_source_leaf: bytes(fields[9], "matched source leaf").toString(
      "hex",
    ),
    used_inline_bitmap: integer(fields[10], "used inline bitmap"),
    used_redeemer_bitmap: integer(fields[11], "used redeemer bitmap"),
    redeemer_item_control_hash: bytes(
      fields[12],
      "redeemer control hash",
    ).toString("hex"),
    execution_count: integer(fields[13], "execution count"),
    execution_peaks: frontier(fields[14], "execution peaks"),
  };
};

/** Decodes the exact consensus 31-field stage-11/12 direction seam. */
export const decodeUnusedScriptWitnessDirectionControlV1 = (
  witnessCbor: Uint8Array,
): Control => {
  const fields = array(decodeSingleCbor(witnessCbor), "stage-12 control");
  const stage = integer(fields[9], "stage");
  if (fields.length !== 31 || (stage !== 11n && stage !== 12n))
    throw new Error(
      "unusedScriptWitness retained control is not exact stage 12",
    );
  const receive = array(fields[24], "receive scan");
  const observer = array(fields[27], "observer scan");
  const mint = array(fields[28], "mint fold");
  if (receive.length !== 6 || observer.length !== 3 || mint.length !== 12)
    throw new Error(
      "unusedScriptWitness retained nested control shape changed",
    );
  const control: Control = {
    compact_cbor: bytes(fields[0], "compact cbor").toString("hex"),
    witness_set_compact_cbor: bytes(fields[1], "witness set").toString("hex"),
    field_preimage_lengths_cbor: bytes(fields[2], "field lengths").toString(
      "hex",
    ),
    context_cbor: bytes(fields[3], "context").toString("hex"),
    resolved_input_count: integer(fields[4], "resolved input count"),
    resolved_inputs_accumulator: bytes(
      fields[5],
      "resolved accumulator",
    ).toString("hex"),
    signer_count: integer(fields[6], "signer count"),
    signer_frontier_commitment: bytes(fields[7], "signer frontier").toString(
      "hex",
    ),
    resolved_item_peaks: frontier(fields[8], "resolved peaks"),
    stage,
    source_count: integer(fields[10], "source count"),
    source_peaks: frontier(fields[11], "source peaks"),
    redeemer_count: integer(fields[12], "redeemer count"),
    redeemer_peaks: frontier(fields[13], "redeemer peaks"),
    replay_cursor: integer(fields[14], "replay cursor"),
    replay_accumulator: bytes(fields[15], "replay accumulator").toString("hex"),
    replay_remaining_schedule_hash: bytes(
      fields[16],
      "remaining schedule",
    ).toString("hex"),
    spend_index: integer(fields[17], "spend index"),
    purpose_count: integer(fields[18], "purpose count"),
    purpose_peaks: frontier(fields[19], "purpose peaks"),
    output_cursor: integer(fields[20], "output cursor"),
    output_count: integer(fields[21], "output count"),
    output_peaks: frontier(fields[22], "output peaks"),
    output_total_count: integer(fields[23], "output total count"),
    receive_scan: {
      source_count: integer(receive[0], "receive source count"),
      source_peaks: frontier(receive[1], "receive source peaks"),
      receive_count: integer(receive[2], "receive count"),
      previous_hash: bytes(receive[3], "receive previous hash").toString("hex"),
      candidate_hash: bytes(receive[4], "receive candidate hash").toString(
        "hex",
      ),
      descriptor_peaks: frontier(receive[5], "receive descriptor peaks"),
    },
    source_total_count: integer(fields[25], "source total count"),
    redeemer_total_count: integer(fields[26], "redeemer total count"),
    observer_scan: {
      total_count: integer(observer[0], "observer total count"),
      previous_hash: bytes(observer[1], "observer previous hash").toString(
        "hex",
      ),
      seen: integer(observer[2], "observer seen"),
    },
    discovery: decodeDiscovery(fields[30]),
    output_proof: null,
    pending_source_cbor: "",
    mint_fold: {
      policy_count: integer(mint[0], "mint policy count"),
      policy_cursor: integer(mint[1], "mint policy cursor"),
      previous_policy: bytes(mint[2], "mint previous policy").toString("hex"),
      active_policy: bytes(mint[3], "mint active policy").toString("hex"),
      item_length: integer(mint[4], "mint item length"),
      item_commitment: bytes(mint[5], "mint item commitment").toString("hex"),
      item_cursor: integer(mint[6], "mint item cursor"),
      assets_remaining: integer(mint[7], "mint assets remaining"),
      policy_asset_cursor: integer(mint[8], "mint policy asset cursor"),
      previous_asset: bytes(mint[9], "mint previous asset").toString("hex"),
      asset_count: integer(mint[10], "mint asset count"),
      asset_peaks: frontier(mint[11], "mint asset peaks"),
    },
    resolution_schedule_hash: bytes(fields[29], "resolution schedule").toString(
      "hex",
    ),
  };
  return Data.from(
    Data.to(
      control as never,
      MissingRedeemerScriptSourcesControlV1Schema as never,
    ),
    MissingRedeemerScriptSourcesControlV1Schema as never,
  ) as Control;
};

const machineState = (
  state: Retained["machine_state"],
): MidgardValidationMachineStateV1 => {
  if (state.phase !== "ScriptSources")
    throw new Error("unusedScriptWitness retained machine phase changed");
  const version = exactNumber(state.machine_version, "machine version");
  if (version !== 1)
    throw new Error("unusedScriptWitness retained machine version changed");
  return {
    machineVersion: 1,
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
const coreFrontier = (
  count: bigint,
  peaks: readonly { height: bigint; hash: string }[],
): MidgardValidationMerkleFrontierV1 => ({
  count: exactNumber(count, "frontier count"),
  peaks: peaks.map(({ height, hash }) => ({
    height: exactNumber(height, "frontier height"),
    hash: Buffer.from(hash, "hex"),
  })),
});

/** Strict, callback-free reconstruction of the complete stage-12 universe. */
export const buildUnusedScriptWitnessDirectionControlFromRetainedDaV1 = async ({
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
    ValidationTraceDescriptorV1Schema,
  ) as unknown as import("@al-ft/midgard-sdk").ValidationTraceDescriptorV1;
  const descriptor = validationTraceDescriptorCoreFromData(descriptorData);
  const eventEntries = retainedValidationWitnessEntries
    .map((entry) => ({
      key: decodeRetainedValidationWitnessKeyV1(entry.key),
      retained: decodeRetainedValidationWitnessV1(entry.value),
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
          hashMidgardValidationEventKeyV1(eventKeyCbor),
        ) ||
        !hashMidgardValidationMachineStateV1(state).equals(proof.stateHash) ||
        !verifyMidgardValidationTraceProofV1({ descriptor, proof }) ||
        !state.workRoot.equals(
          hashMidgardValidationWorkWitnessV1({
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
      const control = decodeUnusedScriptWitnessDirectionControlV1(
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
      const unique = new Map(
        matches.map((value) => [Data.to(value as never, Data.Any()), value]),
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
          ? hashMidgardInlineScriptSourceLeafV1({
              sourceIndex: BigInt(sourceIndex),
              scriptLanguageTag: languageTag,
              scriptHash: Buffer.from(source.script_hash, "hex"),
              scriptTotalLength: exactNumber(
                source.script_total_length,
                "source length",
              ),
              itemCommitment: Buffer.from(source.script_item_commitment, "hex"),
            })
          : hashMidgardReferenceScriptSourceLeafV1({
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
        !verifyMidgardValidationMerkleMembershipV1({
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
      } as RetainedUnusedScriptSourceV1;
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
    const leaf = hashMidgardScriptPurposeLeafV1({
      purposeKind,
      purposeIndex: BigInt(purposeIndex),
      scriptHash: Buffer.from(purpose.script_hash, "hex"),
      subject: Buffer.from(purpose.subject, "hex"),
    });
    if (
      !verifyMidgardValidationMerkleMembershipV1({
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
    } as RetainedUnusedScriptPurposeV1;
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
