import {
  type MidgardValidationMachineState,
  type MidgardValidationMerkleFrontier,
} from "@al-ft/midgard-core";
import { decodeSingleCbor } from "@al-ft/midgard-core/codec/cbor";
import { decodeRetainedValidationWitness } from "@al-ft/midgard-sdk";
import { decodeScriptDiscoveryBitmap } from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";

import { MissingRedeemerScriptSourcesControlSchema } from "../missing-redeemer/schemas.js";

export type EncodedEntry = Readonly<{ key: Uint8Array; value: Uint8Array }>;

type Control = Data.Static<typeof MissingRedeemerScriptSourcesControlSchema>;

type Retained = ReturnType<typeof decodeRetainedValidationWitness>;

export type RetainedUnusedScriptSource = Readonly<{
  sourceIndex: number;
  originKind: 0 | 1;
  sourceKeyHex: string;
  languageTag: 0 | 3 | 128;
  scriptHashHex: string;
  scriptTotalLength: number;
  itemCommitmentHex: string;
  siblings: readonly string[];
}>;

export type RetainedUnusedScriptPurpose = Readonly<{
  frontierIndex: number;
  purposeKind: 0 | 1 | 2 | 3;
  purposeIndex: number;
  scriptHashHex: string;
  purposeSubjectHex: string;
  siblings: readonly string[];
}>;

export const exactNumber = (value: bigint, label: string): number => {
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
    used_inline_bitmap: decodeScriptDiscoveryBitmap(fields[10]),
    used_redeemer_bitmap: decodeScriptDiscoveryBitmap(fields[11]),
    redeemer_item_control_hash: bytes(
      fields[12],
      "redeemer control hash",
    ).toString("hex"),
    execution_count: integer(fields[13], "execution count"),
    execution_peaks: frontier(fields[14], "execution peaks"),
  };
};

/**
 * The stage a retained 31-field ScriptSources control carries, or `null`
 * when the witness is not that control (the receive scan and the CEK
 * witnesses of the same phase have other shapes).
 */
export const retainedScriptSourcesStage = (
  witnessCbor: Uint8Array,
): bigint | null => {
  let fields: unknown;
  try {
    fields = decodeSingleCbor(witnessCbor);
  } catch {
    return null;
  }
  if (!Array.isArray(fields) || fields.length !== 31) return null;
  const stage = fields[9];
  return typeof stage === "bigint" || typeof stage === "number"
    ? BigInt(stage)
    : null;
};

/** Decodes the exact consensus 31-field stage-11/12 direction seam. */
export const decodeUnusedScriptWitnessDirectionControl = (
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
      MissingRedeemerScriptSourcesControlSchema as never,
    ),
    MissingRedeemerScriptSourcesControlSchema as never,
  ) as Control;
};

export const machineState = (
  state: Retained["machine_state"],
): MidgardValidationMachineState => {
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

export const coreFrontier = (
  count: bigint,
  peaks: readonly { height: bigint; hash: string }[],
): MidgardValidationMerkleFrontier => ({
  count: exactNumber(count, "frontier count"),
  peaks: peaks.map(({ height, hash }) => ({
    height: exactNumber(height, "frontier height"),
    hash: Buffer.from(hash, "hex"),
  })),
});
