import { type MidgardValidationMachineState } from "@al-ft/midgard-core";
import { decodeSingleCbor } from "@al-ft/midgard-core/codec/cbor";
import {
  type EventKey,
  EventKeySchema,
  type RetainedValidationWitness,
} from "@al-ft/midgard-sdk";
import { decodeScriptDiscoveryBitmap } from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";

import type { ExecutionSourceDescriptor } from "./family.js";
import { ScriptSourcesControlSchema } from "./schemas.js";
import type { ExecutionSourceAuthenticationData } from "./submit-step-02.js";

export type EncodedEntry = Readonly<{ key: Uint8Array; value: Uint8Array }>;

type Peak = Readonly<{ height: bigint; hash: string }>;

export const fail = (message: string): never => {
  throw new Error(`missingScriptSource retained universe: ${message}`);
};

const integer = (value: unknown, label: string): bigint => {
  if (typeof value !== "bigint" && typeof value !== "number")
    return fail(`${label} is not an integer`);
  return BigInt(value);
};

export const exactNumber = (value: bigint, label: string): number => {
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

export type ParsedControl = Readonly<{
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

/** The ScriptSources stage a retained control witness carries, if any. */
export const retainedScriptSourcesStage = (
  witnessCbor: string,
): bigint | null => {
  try {
    const value = decodeSingleCbor(Buffer.from(witnessCbor, "hex"));
    if (!Array.isArray(value) || value.length !== 31) return null;
    const stage: unknown = value[9];
    return typeof stage === "bigint" || typeof stage === "number"
      ? BigInt(stage)
      : null;
  } catch {
    return null;
  }
};

/**
 * Parses one retained stage-9 ScriptSources control into its redeemer shape
 * and the discovery facts the universe builder selects on. Exported so a
 * suite can point the family's own builders at any retained witness (a
 * prefix a lying prover might claim) without a second decoder.
 */
export const parseRetainedScriptSourcesStageNineControl = (
  witnessCbor: string,
): ParsedControl => {
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
      used_inline_bitmap: decodeScriptDiscoveryBitmap(discovery[10]),
      used_redeemer_bitmap: decodeScriptDiscoveryBitmap(discovery[11]),
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

export const sameEvent = (left: EventKey, right: EventKey): boolean =>
  Data.to(left as never, EventKeySchema) ===
  Data.to(right as never, EventKeySchema);

export const stateFromData = (
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

export type PurposeScanWitness = Readonly<{
  purpose_kind: bigint;
  purpose_index: bigint;
  script_hash: string;
  subject: string;
  siblings: readonly string[];
}>;

export type SourceScanWitness = Readonly<{
  source_index: bigint;
  origin_kind: bigint;
  source_key: string;
  script_language_tag: bigint;
  script_hash: string;
  script_total_length: bigint;
  script_item_commitment: string;
  siblings: readonly string[];
}>;

export const auxiliaryObject = <T>(
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
