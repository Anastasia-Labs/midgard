import {
  hashMidgardInlineScriptSourceLeaf,
  hashMidgardReferenceScriptSourceLeaf,
  hashMidgardScriptPurposeLeaf,
  hashMidgardValidationEventKey,
  hashMidgardValidationMachineState,
  hashMidgardValidationWorkWitness,
  type MidgardValidationMachineState,
  type MidgardValidationMerkleFrontier,
  verifyMidgardValidationMerkleMembership,
  verifyMidgardValidationTraceProof,
} from "@al-ft/midgard-core";
import { decodeSingleCbor, encodeCbor } from "@al-ft/midgard-core/codec/cbor";
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
import { MissingRedeemerScriptSourcesControlSchema } from "./schemas.js";

type EncodedEntry = Readonly<{ key: Uint8Array; value: Uint8Array }>;
type Control = Data.Static<typeof MissingRedeemerScriptSourcesControlSchema>;

export type MissingRedeemerStageTenAuthentication = Readonly<{
  validationTracesRoot: string;
  validationTraceCount: bigint;
  traceMembership: Readonly<Record<string, unknown>>;
  machineState: ReturnType<
    typeof decodeRetainedValidationWitness
  >["machine_state"];
  traceProof: ReturnType<typeof decodeRetainedValidationWitness>["trace_proof"];
  control: Control;
  absolutePurposeIndex: bigint;
  purposeSiblings: readonly string[];
  sourceOriginKind: 0n | 1n;
  sourceKey: string;
  sourceLanguageTag: 3n | 128n;
  sourceScriptHash: string;
  sourceTotalLength: bigint;
  sourceItemCommitment: string;
  sourceSiblings: readonly string[];
}>;

const exactNumber = (value: bigint, label: string): number => {
  const result = Number(value);
  if (!Number.isSafeInteger(result) || result < 0)
    throw new Error(`missingRedeemer retained ${label} is invalid`);
  return result;
};
const array = (value: unknown, label: string): readonly unknown[] => {
  if (!Array.isArray(value))
    throw new Error(`missingRedeemer retained ${label} is not an array`);
  return value;
};
const bytes = (value: unknown, label: string): Buffer => {
  if (!(value instanceof Uint8Array))
    throw new Error(`missingRedeemer retained ${label} is not bytes`);
  return Buffer.from(value);
};
const integer = (value: unknown, label: string): bigint => {
  if (typeof value !== "bigint" && typeof value !== "number")
    throw new Error(`missingRedeemer retained ${label} is not an integer`);
  return BigInt(value);
};
const frontier = (value: unknown, label: string) =>
  array(value, label).map((item, index) => {
    const pair = array(item, `${label}[${index.toString()}]`);
    if (pair.length !== 2)
      throw new Error(`missingRedeemer retained ${label} peak is malformed`);
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
    throw new Error("missingRedeemer retained discovery field count changed");
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

/** Converts the exact 31-field stage-10 work witness into its Aiken datum. */
export const decodeMissingRedeemerStageTenControl = (
  witnessCbor: Uint8Array,
): Control => {
  const fields = array(decodeSingleCbor(witnessCbor), "stage-10 control");
  if (fields.length !== 31 || integer(fields[9], "stage") !== 10n)
    throw new Error("missingRedeemer retained control is not exact stage 10");
  const receive = array(fields[24], "receive scan");
  const observer = array(fields[27], "observer scan");
  const mint = array(fields[28], "mint fold");
  if (receive.length !== 6 || observer.length !== 3 || mint.length !== 12)
    throw new Error("missingRedeemer retained nested control shape changed");
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
    stage: 10n,
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
  const canonical = Data.from(
    Data.to(
      control as never,
      MissingRedeemerScriptSourcesControlSchema as never,
    ),
    MissingRedeemerScriptSourcesControlSchema as never,
  ) as Control;
  return canonical;
};

const machineState = (
  state: ReturnType<typeof decodeRetainedValidationWitness>["machine_state"],
): MidgardValidationMachineState => {
  if (state.phase !== "ScriptSources")
    throw new Error("missingRedeemer retained machine phase changed");
  const version = exactNumber(state.machine_version, "machine version");
  if (version !== 1)
    throw new Error("missingRedeemer retained machine version changed");
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
): MidgardValidationMerkleFrontier => ({
  count: exactNumber(count, "frontier count"),
  peaks: peaks.map(({ height, hash }) => ({
    height: exactNumber(height, "frontier height"),
    hash: Buffer.from(hash, "hex"),
  })),
});

/** Strict callback-free reconstruction from public retained validation DA. */
export const buildMissingRedeemerStageTenAuthenticationFromRetainedDa = async ({
  eventKey,
  transactionId,
  purposeKind,
  purposeIndex,
  authenticatedValidationTraceEntries,
  retainedValidationWitnessEntries,
  expectedValidationTracesRoot,
}: {
  eventKey: EventKey;
  transactionId: string;
  purposeKind: 0 | 1 | 2 | 3;
  purposeIndex: number;
  authenticatedValidationTraceEntries: readonly EncodedEntry[];
  retainedValidationWitnessEntries: readonly EncodedEntry[];
  expectedValidationTracesRoot: string;
}): Promise<MissingRedeemerStageTenAuthentication> => {
  const eventKeyCbor = Buffer.from(
    Data.to(eventKey as never, EventKeySchema),
    "hex",
  );
  const descriptorEntries = authenticatedValidationTraceEntries.map(
    ({ key, value }) => ({
      key: Buffer.from(key),
      value: Buffer.from(value),
    }),
  );
  const descriptorMatches = descriptorEntries.filter(({ key }) =>
    key.equals(eventKeyCbor),
  );
  if (descriptorMatches.length !== 1)
    throw new Error(
      "missingRedeemer validation descriptor is absent or duplicated",
    );
  const descriptorData = Data.from(
    descriptorMatches[0]!.value.toString("hex"),
    ValidationTraceDescriptorSchema,
  ) as unknown as import("@al-ft/midgard-sdk").ValidationTraceDescriptor;
  const descriptor = validationTraceDescriptorCoreFromData(descriptorData);
  const eventEntries = retainedValidationWitnessEntries.flatMap((entry) => {
    const key = decodeRetainedValidationWitnessKey(entry.key);
    const keyCbor = Buffer.from(
      Data.to(key.event_key as never, EventKeySchema),
      "hex",
    );
    return keyCbor.equals(eventKeyCbor)
      ? [{ key, retained: decodeRetainedValidationWitness(entry.value) }]
      : [];
  });
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
        retained.phase !== 8n ||
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
          "missingRedeemer retained state/proof/work witness is invalid",
        );
      return { key, retained };
    });
  const terminals = validated.flatMap(({ retained }) => {
    if (retained.auxiliary !== "NoAuxiliaryWitness") return [];
    try {
      const control = decodeMissingRedeemerStageTenControl(
        Buffer.from(retained.witness_cbor, "hex"),
      );
      return control.discovery.current_purpose_kind === BigInt(purposeKind) &&
        control.discovery.current_purpose_index === BigInt(purposeIndex)
        ? [{ retained, control }]
        : [];
    } catch {
      return [];
    }
  });
  if (terminals.length !== 1)
    throw new Error(
      "missingRedeemer exact terminal stage-10 state is absent or duplicated",
    );
  const { retained, control } = terminals[0]!;
  const acceptedEvent = "L2TransactionEventKey" in eventKey;
  if (
    retained.machine_state.source_kind !==
      (acceptedEvent ? "Normal" : "Forced") ||
    descriptor.verdict !== (acceptedEvent ? "accepted" : "rejected") ||
    retained.machine_state.verdict !== "Pending"
  )
    throw new Error("missingRedeemer retained direction/verdict changed");
  const discovery = control.discovery;
  if (
    discovery.matched_language_tag !== 3n &&
    discovery.matched_language_tag !== 128n
  )
    throw new Error("missingRedeemer selected source is not Plutus");
  const purposeCandidates = validated.flatMap(({ retained: item }) => {
    const auxiliary = item.auxiliary;
    if (
      !(
        typeof auxiliary === "object" && "ScriptPurposeScanWitness" in auxiliary
      )
    )
      return [];
    const purpose = auxiliary.ScriptPurposeScanWitness;
    return purpose.purpose_kind === BigInt(purposeKind) &&
      purpose.purpose_index === BigInt(purposeIndex) &&
      purpose.script_hash === discovery.current_script_hash &&
      purpose.subject === discovery.current_subject
      ? [purpose]
      : [];
  });
  const purposeUnique = new Map(
    purposeCandidates.map((value) => [
      [
        value.purpose_kind.toString(),
        value.purpose_index.toString(),
        value.script_hash,
        value.subject,
        value.siblings.join(":"),
      ].join("/"),
      value,
    ]),
  );
  if (purposeUnique.size !== 1)
    throw new Error(
      "missingRedeemer purpose membership witness is absent or ambiguous",
    );
  const purpose = [...purposeUnique.values()][0]!;
  const purposeLeaf = hashMidgardScriptPurposeLeaf({
    purposeKind,
    purposeIndex: BigInt(purposeIndex),
    scriptHash: Buffer.from(purpose.script_hash, "hex"),
    subject: Buffer.from(purpose.subject, "hex"),
  });
  if (
    !verifyMidgardValidationMerkleMembership({
      frontier: coreFrontier(control.purpose_count, control.purpose_peaks),
      leafIndex: exactNumber(
        discovery.purpose_cursor,
        "absolute purpose index",
      ),
      leafHash: purposeLeaf,
      siblings: purpose.siblings.map((value) => Buffer.from(value, "hex")),
    })
  )
    throw new Error("missingRedeemer purpose membership is invalid");
  const sourceCandidates = validated.flatMap(({ retained: item }) => {
    const auxiliary = item.auxiliary;
    if (
      !(typeof auxiliary === "object" && "ScriptSourceScanWitness" in auxiliary)
    )
      return [];
    const source = auxiliary.ScriptSourceScanWitness;
    return source.source_index === discovery.matched_source_index &&
      source.script_language_tag === discovery.matched_language_tag &&
      source.script_hash === discovery.current_script_hash
      ? [source]
      : [];
  });
  const sourceUnique = new Map(
    sourceCandidates.map((value) => [
      [
        value.source_index.toString(),
        value.origin_kind.toString(),
        value.source_key,
        value.script_language_tag.toString(),
        value.script_hash,
        value.script_total_length.toString(),
        value.script_item_commitment,
        value.siblings.join(":"),
      ].join("/"),
      value,
    ]),
  );
  if (sourceUnique.size !== 1)
    throw new Error(
      "missingRedeemer source membership witness is absent or ambiguous",
    );
  const source = [...sourceUnique.values()][0]!;
  if (source.origin_kind !== 0n && source.origin_kind !== 1n)
    throw new Error("missingRedeemer source origin changed");
  if (
    source.origin_kind === 0n &&
    source.source_key !== encodeCbor(source.source_index).toString("hex")
  )
    throw new Error("missingRedeemer inline source key/index changed");
  const sourceLeaf =
    source.origin_kind === 0n
      ? hashMidgardInlineScriptSourceLeaf({
          sourceIndex: source.source_index,
          scriptLanguageTag: Number(source.script_language_tag) as 3 | 128,
          scriptHash: Buffer.from(source.script_hash, "hex"),
          scriptTotalLength: exactNumber(
            source.script_total_length,
            "source length",
          ),
          itemCommitment: Buffer.from(source.script_item_commitment, "hex"),
        })
      : hashMidgardReferenceScriptSourceLeaf({
          sourceKey: Buffer.from(source.source_key, "hex"),
          scriptLanguageTag: Number(source.script_language_tag) as 3 | 128,
          scriptHash: Buffer.from(source.script_hash, "hex"),
          scriptTotalLength: exactNumber(
            source.script_total_length,
            "source length",
          ),
          itemCommitment: Buffer.from(source.script_item_commitment, "hex"),
        });
  if (
    sourceLeaf.toString("hex") !== discovery.matched_source_leaf ||
    !verifyMidgardValidationMerkleMembership({
      frontier: coreFrontier(control.source_count, control.source_peaks),
      leafIndex: exactNumber(source.source_index, "source index"),
      leafHash: sourceLeaf,
      siblings: source.siblings.map((value) => Buffer.from(value, "hex")),
    })
  )
    throw new Error("missingRedeemer source membership is invalid");
  const root = await buildCountedRoot(
    ROOT_DOMAINS.validationTraces,
    descriptorEntries,
  );
  if (root.root !== expectedValidationTracesRoot)
    throw new Error("missingRedeemer retained validation root changed");
  const membership = await keyValuePhasProof(
    { root: root.phasRoot, count: root.count, entries: root.entries },
    eventKeyCbor,
    descriptorMatches[0]!.value,
  );
  return Object.freeze({
    validationTracesRoot: root.root,
    validationTraceCount: root.count,
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
    absolutePurposeIndex: discovery.purpose_cursor,
    purposeSiblings: purpose.siblings,
    sourceOriginKind: source.origin_kind,
    sourceKey: source.source_key,
    sourceLanguageTag: discovery.matched_language_tag,
    sourceScriptHash: source.script_hash,
    sourceTotalLength: source.script_total_length,
    sourceItemCommitment: source.script_item_commitment,
    sourceSiblings: source.siblings,
  });
};
