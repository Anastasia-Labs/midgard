import {
  asArray,
  asBigInt,
  asBytes,
  decodeSingleCbor,
} from "@al-ft/midgard-core/codec/cbor";

import { type ValidationMachineWorkWitness } from "./validation-machine/index.js";

export const scriptSourcesDiscoveryCurrentPurpose = (
  witness: ValidationMachineWorkWitness,
): {
  readonly purposeKind: 0 | 1 | 2 | 3;
  readonly purposeIndex: bigint;
} => {
  const control = asArray(
    decodeSingleCbor(witness.cbor),
    "script_sources_control",
  );
  if (
    control.length !== 31 ||
    asBigInt(control[9], "script_sources_control.stage") !== 10n
  ) {
    throw new Error("script_sources_control is not at discovery stage 10");
  }
  const discovery = asArray(
    decodeSingleCbor(asBytes(control[30], "script_sources_control.discovery")),
    "script_sources_discovery",
  );
  if (discovery.length !== 15) {
    throw new Error("script_sources discovery has an invalid field count");
  }
  const purposeKind = Number(
    asBigInt(discovery[3], "script_sources_discovery.current_purpose_kind"),
  );
  if (
    purposeKind !== 0 &&
    purposeKind !== 1 &&
    purposeKind !== 2 &&
    purposeKind !== 3
  ) {
    throw new Error("script_sources discovery current purpose kind is invalid");
  }
  return {
    purposeKind,
    purposeIndex: asBigInt(
      discovery[4],
      "script_sources_discovery.current_purpose_index",
    ),
  };
};

export const scriptIntegrityStage = (
  witness: ValidationMachineWorkWitness,
): number => {
  const control = asArray(
    decodeSingleCbor(witness.cbor),
    "script_integrity_control",
  );
  if (control.length !== 2 && control.length !== 4) {
    throw new Error("script_integrity_control has an invalid field count");
  }
  const stage = Number(asBigInt(control[1], "script_integrity_control.stage"));
  if (
    !Number.isSafeInteger(stage) ||
    stage < 0 ||
    stage > 3 ||
    (stage < 2 && control.length !== 2) ||
    (stage >= 2 && control.length !== 4)
  ) {
    throw new Error("script_integrity_control stage is invalid");
  }
  return stage;
};

export const resolveInputsCursor = (
  witness: ValidationMachineWorkWitness,
): number => {
  const control = asArray(
    decodeSingleCbor(witness.cbor),
    "resolve_inputs_control",
  );
  if (control.length !== 11) {
    throw new Error("resolve_inputs_control has an invalid field count");
  }
  const cursor = Number(asBigInt(control[4], "resolve_inputs_control.cursor"));
  if (!Number.isSafeInteger(cursor) || cursor < 0) {
    throw new Error("resolve_inputs_control cursor is invalid");
  }
  return cursor;
};

export const ledgerDeltaControlStatus = (
  witness: ValidationMachineWorkWitness,
): {
  readonly stage: number;
  readonly pendingStage: number | null;
} => {
  const control = asArray(
    decodeSingleCbor(witness.cbor),
    "ledger_delta_control",
  );
  if (control.length !== 14) {
    throw new Error("ledger_delta_control has an invalid field count");
  }
  const stage = Number(asBigInt(control[4], "ledger_delta_control.stage"));
  if (!Number.isSafeInteger(stage) || stage < 0 || stage > 2) {
    throw new Error("ledger_delta_control stage is invalid");
  }
  const pendingCbor = asBytes(
    control[12],
    "ledger_delta_control.pending_mutation",
  );
  if (pendingCbor.length === 0) {
    return { stage, pendingStage: null };
  }
  const pending = asArray(
    decodeSingleCbor(pendingCbor),
    "ledger_delta_pending_mutation",
  );
  if (
    pending.length !== 10 ||
    asBigInt(pending[0], "ledger_delta_pending_mutation.version") !== 1n
  ) {
    throw new Error("ledger_delta pending mutation is invalid");
  }
  const pendingStage = Number(
    asBigInt(pending[1], "ledger_delta_pending_mutation.stage"),
  );
  if (pendingStage !== 0 && pendingStage !== 1) {
    throw new Error("ledger_delta pending mutation stage is invalid");
  }
  return { stage, pendingStage };
};

/**
 * The four cek semantic resolvers, in their `semantic_resolver_script_hashes`
 * order under the `cek_v1` prepare validator (lib `verify_cek`): the
 * ValueAndMint hand-off (`cek_finish_semantic_v1`), the execution selection
 * (`cek_execution_selection_semantic_v1`), the context step
 * (`cek_context_step_semantic_v1`) and the core step
 * (`cek_core_step_semantic_v1`).
 */
export type CekStepKind = "finish" | "selection" | "context" | "core";

/**
 * The cek work witness is the nine-field list
 * `[native_control, context_control, execution_cursor, completed_cpu,
 * completed_memory, active_state_hash, program_envelope_hash,
 * execution_cpu_limit, execution_memory_limit]` that
 * `encodeMidgardCekValidationWitnessV1` writes and the on-chain
 * `cek_witness_control_v1` decodes. The four cek semantic resolvers partition
 * the step space on the control alone, in the order the on-chain
 * discriminators are consulted (`cek_control_is_core_step_v1`,
 * `cek_control_is_context_step_v1`, `cek_control_is_finish_v1`, else the
 * execution selection): a core step carries an active state, a context step
 * carries a context control and no active state, and a step with neither is
 * the ValueAndMint hand-off when no execution is left to select (cursor at
 * the execution count) and an execution selection otherwise. The language
 * bitmap takes no part in the discrimination: a transaction whose executions
 * are all native carries `language_bitmap == 0` and still has one selection
 * step per execution, exactly as the machine emits them (#629). The
 * auxiliary is not consulted: each on-chain semantic
 * resolver `expect`s the auxiliary shape of its own kind (none, a
 * `NativeExecutionScanWitness`, a cek context witness, a
 * `CekCoreStepWitness`), so a witness whose auxiliary does not match the kind
 * its control names is refused at the submission encoder, exactly as the
 * resolver would refuse it. Note that a Plutus trace never emits a `finish`
 * step: the hand-off to ValueAndMint is claimed by the last core step (or the
 * last native selection) itself, and `finish` is the stand-alone hand-off of
 * a trace with nothing left to select.
 */
export const cekKind = (witness: ValidationMachineWorkWitness): CekStepKind => {
  const control = asArray(decodeSingleCbor(witness.cbor), "cek_witness");
  if (control.length !== 9) {
    throw new Error("cek_witness has an invalid field count");
  }
  const nativeControl = asArray(
    decodeSingleCbor(asBytes(control[0], "cek_witness.native_control")),
    "cek_witness.native_control",
  );
  if (nativeControl.length !== 26) {
    throw new Error("cek_witness native control has an invalid field count");
  }
  const executionCount = asBigInt(
    nativeControl[21],
    "cek_witness.native_control.execution_count",
  );
  const executionCursor = asBigInt(control[2], "cek_witness.execution_cursor");
  const hasContextControl =
    asBytes(control[1], "cek_witness.context_control").length > 0;
  const hasActiveState =
    asBytes(control[5], "cek_witness.active_state_hash").length > 0;
  const selectionExhausted = executionCursor === executionCount;
  if (hasActiveState) {
    return "core";
  }
  if (hasContextControl) {
    return "context";
  }
  return selectionExhausted ? "finish" : "selection";
};

/**
 * The eleven ValueAndMint semantic resolvers, in their
 * `semantic_resolver_script_hashes` order under the `value_and_mint_v1`
 * prepare validator (lib `verify_value_and_mint`), one per reachable
 * `(stage, auxiliary)` pair of the stage bodies.
 */
export type ValueAndMintStepKind =
  | "begin"
  | "replayBegin"
  | "replayInput"
  | "replayAsset"
  | "replayFinish"
  | "outputDescriptor"
  | "outputAsset"
  | "outputFinish"
  | "mintAsset"
  | "mintFinish"
  | "finalize";
