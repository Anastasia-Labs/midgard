import type { BlueprintMismatch } from "@al-ft/midgard-sdk/blueprint-abi";

/** Why one step validator's datum may differ from its TypeScript schema. */
export type StepDatumAllowance = Readonly<{
  reason: string;
  /** The exact report allowed; any other report, or none, fails. */
  mismatches: readonly BlueprintMismatch[];
}>;

const OPAQUE_STEP_STATE: StepDatumAllowance = Object.freeze({
  reason:
    "the family declares this step with FraudProofComputationThreadStepDatum, whose state is opaque Data",
  mismatches: Object.freeze([
    {
      path: "$.StepDatum.data.Some.0",
      message: "blueprint declares constructors, TypeScript any Data",
    },
  ]),
});

const MIN_ADA_STEP_04_RESERVED_ARM: StepDatumAllowance = Object.freeze({
  reason:
    "Lucid collapses a one-constructor enum to void, so TypeScript adds a second arm it never emits",
  mismatches: Object.freeze([
    {
      path: "$.StepDatum.data.Some.0",
      message: "blueprint declares 1 constructor(s), TypeScript 2",
    },
  ]),
});

/** Step validators the registry declares with an opaque thread state. */
const OPAQUE_STEP_STATE_TITLES = [
  "fraud_proofs/direct_script_derivation/all_purposes_mint.main.spend",
  "fraud_proofs/direct_script_derivation/all_purposes_mint_grammar.main.spend",
  "fraud_proofs/direct_script_derivation/all_purposes_observe.main.spend",
  "fraud_proofs/direct_script_derivation/all_purposes_observe_open.main.spend",
  "fraud_proofs/direct_script_derivation/all_purposes_receive.main.spend",
  "fraud_proofs/direct_script_derivation/all_purposes_receive_grammar.main.spend",
  "fraud_proofs/direct_script_derivation/all_purposes_spend.main.spend",
  "fraud_proofs/direct_script_derivation/all_purposes_spend_resolve.main.spend",
  "fraud_proofs/direct_script_derivation/bind_event.main.spend",
  "fraud_proofs/direct_script_derivation/bind_pre_state.main.spend",
  "fraud_proofs/direct_script_derivation/claim_adjudicate.main.spend",
  "fraud_proofs/direct_script_derivation/claim_mint_grammar.main.spend",
  "fraud_proofs/direct_script_derivation/claim_mint_scan.main.spend",
  "fraud_proofs/direct_script_derivation/claim_receive_declaration.main.spend",
  "fraud_proofs/direct_script_derivation/claim_receive_finish.main.spend",
  "fraud_proofs/direct_script_derivation/claim_receive_grammar.main.spend",
  "fraud_proofs/direct_script_derivation/claim_receive_outputs.main.spend",
  "fraud_proofs/direct_script_derivation/claim_spend_scan.main.spend",
  "fraud_proofs/direct_script_derivation/claim_spend_start.main.spend",
  "fraud_proofs/direct_script_derivation/claim_start.main.spend",
  "fraud_proofs/direct_script_derivation/claim_terminal.main.spend",
  "fraud_proofs/direct_script_derivation/claimed_redeemer_grammar.main.spend",
  "fraud_proofs/direct_script_derivation/claimed_redeemer_scan.main.spend",
  "fraud_proofs/direct_script_derivation/mint_grammar.main.spend",
  "fraud_proofs/direct_script_derivation/mint_scan.main.spend",
  "fraud_proofs/direct_script_derivation/purpose_select.main.spend",
  "fraud_proofs/direct_script_derivation/receive_declaration.main.spend",
  "fraud_proofs/direct_script_derivation/receive_finish.main.spend",
  "fraud_proofs/direct_script_derivation/receive_grammar.main.spend",
  "fraud_proofs/direct_script_derivation/receive_outputs.main.spend",
  "fraud_proofs/direct_script_derivation/receive_start.main.spend",
  "fraud_proofs/direct_script_derivation/source_authenticate.main.spend",
  "fraud_proofs/direct_script_derivation/source_finish.main.spend",
  "fraud_proofs/direct_script_derivation/source_grammar.main.spend",
  "fraud_proofs/direct_script_derivation/source_input_finish.main.spend",
  "fraud_proofs/direct_script_derivation/source_lookup.main.spend",
  "fraud_proofs/direct_script_derivation/source_map.main.spend",
  "fraud_proofs/direct_script_derivation/source_resolved.main.spend",
  "fraud_proofs/direct_script_derivation/source_start.main.spend",
  "fraud_proofs/direct_script_derivation/source_witnesses.main.spend",
  "fraud_proofs/direct_script_derivation/spend_scan.main.spend",
  "fraud_proofs/direct_script_derivation/spend_select.main.spend",
  "fraud_proofs/distinct_asset_accumulation_limit/family_entry.main.spend",
  "fraud_proofs/distinct_asset_accumulation_limit/input_rank.main.spend",
  "fraud_proofs/distinct_asset_accumulation_limit/inputs.main.spend",
  "fraud_proofs/distinct_asset_accumulation_limit/inputs_resolve.main.spend",
  "fraud_proofs/distinct_asset_accumulation_limit/mint.main.spend",
  "fraud_proofs/distinct_asset_accumulation_limit/mint_grammar.main.spend",
  "fraud_proofs/distinct_asset_accumulation_limit/outputs.main.spend",
  "fraud_proofs/distinct_asset_accumulation_limit/outputs_grammar.main.spend",
  "fraud_proofs/distinct_asset_accumulation_limit/terminal.main.spend",
  "fraud_proofs/execution_native_script_invalid/evaluate.main.spend",
  "fraud_proofs/execution_native_script_invalid/family_entry.main.spend",
  "fraud_proofs/execution_native_script_invalid/lookup.main.spend",
  "fraud_proofs/execution_native_script_invalid/signers.main.spend",
  "fraud_proofs/execution_native_script_invalid/spend_key.main.spend",
  "fraud_proofs/execution_native_script_invalid/spend_rank.main.spend",
  "fraud_proofs/execution_native_script_invalid/terminal.main.spend",
  "fraud_proofs/execution_source_script_decoding/terminal.main.spend",
  "fraud_proofs/missing_redeemer/family_entry.main.spend",
  "fraud_proofs/missing_redeemer/step_02.main.spend",
  "fraud_proofs/missing_redeemer/step_03.main.spend",
  "fraud_proofs/missing_redeemer/step_04.main.spend",
  "fraud_proofs/missing_script_source/family_entry.main.spend",
  "fraud_proofs/missing_script_source/observed_decision.main.spend",
  "fraud_proofs/receive_purpose_language/family_entry.main.spend",
  "fraud_proofs/receive_purpose_language/lookup.main.spend",
  "fraud_proofs/receive_purpose_language/terminal.main.spend",
  "fraud_proofs/script_integrity_hash_mismatch/family_entry.main.spend",
  "fraud_proofs/script_integrity_hash_mismatch/terminal.main.spend",
  "fraud_proofs/transition_trace/route_v1.main.spend",
  "fraud_proofs/unused_redeemer/family_entry.main.spend",
  "fraud_proofs/unused_redeemer/redeemer_grammar.main.spend",
  "fraud_proofs/unused_redeemer/redeemer_scan.main.spend",
  "fraud_proofs/unused_script_witness/family_entry.main.spend",
  "fraud_proofs/unused_script_witness/terminal.main.spend",
  "fraud_proofs/unused_script_witness/witness_entry.main.spend",
  "fraud_proofs/unused_script_witness/witness_grammar.main.spend",
  "fraud_proofs/unused_script_witness/witness_scan.main.spend",
] as const;

/**
 * Step validators whose datum the registered TypeScript step schema does not
 * yet refine, by validator title. Shrink-only: an entry whose pairs start to
 * refine fails the conformance test until it is deleted, and an entry no
 * registered step reaches fails it too.
 */
export const STEP_DATUM_ALLOWANCES: ReadonlyMap<string, StepDatumAllowance> =
  new Map([
    ...OPAQUE_STEP_STATE_TITLES.map(
      (title) => [title, OPAQUE_STEP_STATE] as const,
    ),
    ["fraud_proofs/min_ada/step_04.main.spend", MIN_ADA_STEP_04_RESERVED_ARM],
  ]);
