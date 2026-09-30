import { readFileSync } from "node:fs";
import { resolve } from "node:path";

import { canonicalPlutusDataCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import { Constr, Data } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import {
  buildValidationOneStepArgument,
  cekKind,
  type DeterministicValidationMachineTrace,
  validationSemanticResolverIndex,
  valueAndMintKind,
} from "../src/index.js";

export const root = (byte: number): string =>
  Buffer.alloc(32, byte).toString("hex");

const validationBlueprintPath =
  process.env.MIDGARD_REAL_BLUEPRINT_PATH ??
  resolve(process.cwd(), "../../onchain/aiken/plutus.json");

export const validationDisputeBlueprint = JSON.parse(
  readFileSync(validationBlueprintPath, "utf8"),
) as unknown;

// The auxiliary witness holds no ABI position in a regenerated blueprint:
// every consumer reads it as `Data` through the yield dispatchers'
// builtin decodes, so Aiken emits no definition for
// `midgard/validation_machine/machine_types/ValidationAuxiliaryWitnessV1`.
// The wire pin therefore lives in the generated 40-arm tag/arity corpus
// (`validation-controls-abi.test.ts` freezes its bytes and blake2b digest)
// plus the cross-language producer vectors in
// `onchain/aiken/lib/midgard/validation-one-step-cross-language.test.ak`;
// this helper is the envelope half of that pin — canonical Plutus Data,
// a frozen constructor tag, the frozen arity, and the argument size cap.
const auxiliaryArityByTag = new Map(
  (
    JSON.parse(
      readFileSync(
        new URL(
          "./fixtures/validation-auxiliary-witness-v1.generated.json",
          import.meta.url,
        ),
        "utf8",
      ),
    ) as {
      readonly constructors: readonly {
        readonly tag: number;
        readonly arity: number;
      }[];
    }
  ).constructors.map((entry) => [entry.tag, entry.arity] as const),
);

export const assertPinnedAuxiliaryEnvelope = (cbor: Buffer): void => {
  if (cbor.length > 16 * 1024 - 1) {
    throw new Error("auxiliary witness exceeds the argument envelope");
  }
  const hex = cbor.toString("hex");
  canonicalPlutusDataCbor(hex);
  const decoded = Data.from(hex);
  if (!(decoded instanceof Constr)) {
    throw new Error("auxiliary witness must be a constructor");
  }
  const arity = auxiliaryArityByTag.get(decoded.index);
  if (arity === undefined) {
    throw new Error(
      `auxiliary witness tag ${decoded.index.toString()} is not a frozen V1 arm`,
    );
  }
  if (decoded.fields.length !== arity) {
    throw new Error(
      `auxiliary witness tag ${decoded.index.toString()} carries ${decoded.fields.length.toString()} fields, frozen arity is ${arity.toString()}`,
    );
  }
};

/**
 * The exact redeemer definition each semantic-resolver validator declares:
 * most modules declare their own `SpendRedeemer`, but the yield-dispatching
 * item resolvers share a lib-level redeemer type (e.g.
 * `phase_a_native_scripts_item_semantic_v1` declares
 * `midgard/fraud_proofs/validation_trace/phase_a_native_item_yield_v1/SpendRedeemer`),
 * so the name is read from the validator's own blueprint entry rather than
 * assumed from the module name.
 */
export const spendRedeemerDefinitionName = (moduleName: string): string => {
  const { validators } = validationDisputeBlueprint as {
    readonly validators: readonly {
      readonly title: string;
      readonly redeemer?: { readonly schema?: { readonly $ref?: string } };
    }[];
  };
  const title = `fraud_proofs/validation_trace/${moduleName}.main.spend`;
  const reference = validators.find((validator) => validator.title === title)
    ?.redeemer?.schema?.$ref;
  if (reference === undefined || !reference.startsWith("#/definitions/")) {
    throw new Error(
      `validator ${title} declares no referenced spend redeemer definition`,
    );
  }
  return reference
    .slice("#/definitions/".length)
    .split("~1")
    .join("/")
    .split("~0")
    .join("~");
};

export const semanticResolverDefinitions = [
  "canonical_decode_empty_semantic_v1",
  "canonical_decode_item_semantic_v1",
  "compact_binding_semantic_v1",
  "static_ledger_rules_semantic_v1",
  "input_sets_empty_semantic_v1",
  "input_sets_item_semantic_v1",
  "signatures_advance_semantic_v1",
  "signatures_address_item_semantic_v1",
  "signatures_required_item_semantic_v1",
  "signatures_handoff_semantic_v1",
  "phase_a_native_scripts_advance_semantic_v1",
  "phase_a_native_scripts_item_semantic_v1",
  "phase_a_native_scripts_token_head_semantic_v1",
  "phase_a_native_scripts_all_or_any_container_frame_payload_semantic_v1",
  "phase_a_native_scripts_all_or_any_empty_container_payload_semantic_v1",
  "phase_a_native_scripts_at_least_container_frame_payload_semantic_v1",
  "phase_a_native_scripts_at_least_empty_container_payload_semantic_v1",
  "phase_a_native_scripts_timelock_payload_semantic_v1",
  "phase_a_native_scripts_signature_membership_payload_semantic_v1",
  "phase_a_native_scripts_signature_empty_payload_semantic_v1",
  "phase_a_native_scripts_signature_below_first_payload_semantic_v1",
  "phase_a_native_scripts_signature_above_last_payload_semantic_v1",
  "phase_a_native_scripts_signature_between_payload_semantic_v1",
  "phase_a_native_scripts_frame_semantic_v1",
  "phase_a_script_preconditions_semantic_v1",
  "phase_a_script_preconditions_item_semantic_v1",
  "resolve_inputs_initial_semantic_v1",
  "resolve_inputs_finish_semantic_v1",
  "resolve_inputs_membership_begin_semantic_v1",
  "resolve_inputs_membership_step_semantic_v1",
  "resolve_inputs_membership_finalize_semantic_v1",
  "resolve_inputs_non_membership_semantic_v1",
  "script_sources_non_output_semantic_v1",
  "script_sources_output_proof_begin_semantic_v1",
  "script_sources_output_proof_step_semantic_v1",
  "script_sources_output_proof_finalize_semantic_v1",
  "script_sources_output_proof_finish_semantic_v1",
  "script_sources_stage_zero_begin_semantic_v1",
  "script_sources_stage_zero_finish_semantic_v1",
  "script_sources_stage_zero_hash_block_semantic_v1",
  "script_sources_stage_zero_hash_advance_semantic_v1",
  "script_sources_stage_zero_hash_terminal_semantic_v1",
  "script_sources_stage_nine_mismatch_semantic_v1",
  "script_sources_stage_nine_native_match_semantic_v1",
  "script_sources_stage_nine_effectful_match_semantic_v1",
  "script_sources_stage_nine_missing_semantic_v1",
  "script_sources_stage_one_finish_semantic_v1",
  "script_sources_stage_one_redeemer_semantic_v1",
  "script_sources_stage_eleven_finish_semantic_v1",
  "script_sources_stage_eleven_source_semantic_v1",
  "script_sources_stage_twelve_finish_semantic_v1",
  "script_sources_stage_twelve_redeemer_semantic_v1",
  "script_sources_stage_ten_missing_semantic_v1",
  "script_sources_stage_ten_mismatch_semantic_v1",
  "script_sources_stage_ten_match_semantic_v1",
  "script_sources_stage_eight_finish_semantic_v1",
  "script_sources_stage_eight_purpose_semantic_v1",
  "script_sources_stage_seven_observer_semantic_v1",
  "script_sources_stage_seven_receive_semantic_v1",
  "script_sources_stage_seven_finish_semantic_v1",
  "native_scripts_terminal_semantic_v1",
  "native_scripts_native_semantic_v1",
  "native_scripts_effectful_semantic_v1",
  "script_integrity_authentication_semantic_v1",
  "script_integrity_compact_semantic_v1",
  "script_integrity_witness_set_semantic_v1",
  "script_integrity_finalize_semantic_v1",
  "cek_finish_semantic_v1",
  "cek_execution_selection_semantic_v1",
  "cek_context_step_semantic_v1",
  "cek_core_step_semantic_v1",
  "value_and_mint_begin_semantic_v1",
  "value_and_mint_replay_begin_semantic_v1",
  "value_and_mint_replay_input_semantic_v1",
  "value_and_mint_replay_asset_semantic_v1",
  "value_and_mint_replay_finish_semantic_v1",
  "value_and_mint_output_descriptor_semantic_v1",
  "value_and_mint_output_asset_semantic_v1",
  "value_and_mint_output_finish_semantic_v1",
  "value_and_mint_mint_asset_semantic_v1",
  "value_and_mint_mint_finish_semantic_v1",
  "value_and_mint_finalize_semantic_v1",
  "ledger_delta_operation_semantic_v1",
  "ledger_delta_replay_semantic_v1",
  "ledger_delta_replay_finish_semantic_v1",
  "ledger_delta_output_semantic_v1",
  "ledger_delta_output_finish_semantic_v1",
  "ledger_delta_proof_frame_semantic_v1",
  "ledger_delta_finalize_semantic_v1",
  "ledger_delta_terminal_semantic_v1",
] as const;

export const semanticResolverOffsets = [
  0, 2, 3, 4, 6, 10, 24, 26, 32, 60, 63, 67, 71, 82,
] as const;

// R5 item 1: the cek and ValueAndMint indices decompose into semantic kinds
// in `cek_v1` / `value_and_mint_v1` prepare order. The index function is total
// over every witness a deterministic trace emits; these tables pin the
// kind → semantic-resolver-index map the builders and the totality verifier
// both read.
const cekSemanticIndexByKind = {
  finish: 0,
  selection: 1,
  context: 2,
  core: 3,
} as const;

export const valueAndMintSemanticIndexByKind = {
  begin: 0,
  replayBegin: 1,
  replayInput: 2,
  replayAsset: 3,
  replayFinish: 4,
  outputDescriptor: 5,
  outputAsset: 6,
  outputFinish: 7,
  mintAsset: 8,
  mintFinish: 9,
  finalize: 10,
} as const;

export const expectCekAndValueAndMintTotality = (
  trace: DeterministicValidationMachineTrace,
) => {
  const cekWitnesses = trace.witnesses.filter(
    (witness) => witness.phase === "cek",
  );
  const cekKinds = cekWitnesses.map(cekKind);
  expect(cekWitnesses.map(validationSemanticResolverIndex)).toEqual(
    cekKinds.map((kind) => cekSemanticIndexByKind[kind]),
  );
  for (const [index, witness] of cekWitnesses.entries()) {
    const kind = cekKinds[index];
    const auxiliaryKind = witness.auxiliary?.kind ?? null;
    if (kind === "finish") {
      // The stand-alone hand-off exists only when there is nothing to
      // select (`cek_control_is_finish_v1`): a Plutus trace hands off from
      // its last core step, a native selection from its last selection.
      expect(auxiliaryKind).toBeNull();
      expect(cekWitnesses).toHaveLength(1);
    } else if (kind === "selection") {
      expect(auxiliaryKind).toBe("nativeExecutionScan");
    } else if (kind === "core") {
      expect(auxiliaryKind).toBe("cekCoreStep");
    } else {
      expect(auxiliaryKind).not.toBe("cekCoreStep");
      expect(auxiliaryKind).not.toBe("nativeExecutionScan");
    }
  }
  const valueAndMintWitnesses = trace.witnesses.filter(
    (witness) => witness.phase === "valueAndMint",
  );
  const valueAndMintKinds = valueAndMintWitnesses.map(valueAndMintKind);
  expect(valueAndMintWitnesses.map(validationSemanticResolverIndex)).toEqual(
    valueAndMintKinds.map((kind) => valueAndMintSemanticIndexByKind[kind]),
  );
  expect(valueAndMintKinds[0]).toBe("begin");
  expect(valueAndMintKinds[1]).toBe("replayBegin");
  expect(valueAndMintKinds.at(-1)).toBe("finalize");
  expect(valueAndMintKinds.at(-2)).toBe("mintFinish");
  for (const [index, witness] of valueAndMintWitnesses.entries()) {
    const kind = valueAndMintKinds[index];
    const auxiliaryKind = witness.auxiliary?.kind ?? null;
    const expectedAuxiliaryKind = {
      begin: null,
      replayBegin: null,
      replayInput: "resolvedInputReplay",
      replayAsset: "valueInputAsset",
      replayFinish: null,
      outputDescriptor: "valueOutputDescriptor",
      outputAsset: "valueOutputAsset",
      outputFinish: null,
      mintAsset: "valueMintAsset",
      mintFinish: null,
      finalize: null,
    }[kind];
    expect(auxiliaryKind).toBe(expectedAuxiliaryKind);
  }
  // Every witness in the trace names a semantic resolver inside its index's
  // count (the totality property the verifier reads live).
  const counts = [2, 1, 1, 2, 4, 14, 2, 6, 29, 3, 4, 4, 11, 8] as const;
  for (const oneStep of trace.states
    .slice(0, -1)
    .map((_state, stateIndex) =>
      buildValidationOneStepArgument({ trace, stateIndex }),
    )) {
    expect(oneStep.semanticResolverIndex).toBeGreaterThanOrEqual(0);
    expect(oneStep.semanticResolverIndex).toBeLessThan(
      counts[oneStep.resolverIndex]!,
    );
  }
  return { cekKinds, valueAndMintKinds };
};
