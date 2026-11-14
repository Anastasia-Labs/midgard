import type { BlueprintMismatch } from "@al-ft/midgard-sdk/blueprint-abi";

/** Why one step redeemer twin may differ from its validator's redeemer. */
export type StepRedeemerAllowance = Readonly<{
  /** The convention twin the report belongs to. */
  twin: string;
  reason: string;
  /** The exact report allowed; any other report, or none, fails. */
  mismatches: readonly BlueprintMismatch[];
}>;

const withdrawalMistagOpaqueTree = (step: number): StepRedeemerAllowance =>
  Object.freeze({
    twin: `WithdrawalMistagStep0${step}SpendRedeemerSchema`,
    reason:
      "the structured-evidence tree is raw Data that structuredDataTreeData builds, not a schema value",
    mismatches: Object.freeze([
      {
        path: "$.Continue.0.Args.payload.StructuredEvidence.tree",
        message: "blueprint declares constructors, TypeScript any Data",
      },
    ]),
  });

/**
 * Step validators whose convention twin does not refine the redeemer, by
 * validator title. Shrink-only: an entry whose twin starts to refine fails
 * the conformance test until it is deleted.
 */
export const STEP_REDEEMER_ALLOWANCES: ReadonlyMap<
  string,
  StepRedeemerAllowance
> = new Map([
  [
    "fraud_proofs/min_fee/step_02.main.spend",
    {
      twin: "MinFeeStep02SpendRedeemerSchema",
      reason:
        "TypeScript fixes the nine field carriages as a tuple, which Lucid encodes as the same list",
      mismatches: [
        {
          path: "$.Continue.0.Args.field_carriages",
          message: "blueprint declares a list, TypeScript tuple",
        },
      ],
    },
  ],
  [
    "fraud_proofs/mint_authorization/step_02.main.spend",
    {
      twin: "MintAuthorizationStep02SpendRedeemerSchema",
      reason:
        "the convention twin builds only the inline Args arm (constructor 0); the submitter builds the PublishedArgs and AdvanceScan arms with MintAuthorizationStep02PublishedSpendRedeemerSchema, and no off-chain path decodes this redeemer",
      mismatches: [
        {
          path: "$.Continue.0",
          message: "blueprint declares 3 constructor(s), TypeScript 1",
        },
        {
          path: "$.Continue.0.PublishedArgs",
          message: "TypeScript has no constructor 1",
        },
        {
          path: "$.Continue.0.AdvanceScan",
          message: "TypeScript has no constructor 2",
        },
      ],
    },
  ],
  ...[1, 2, 3, 4].map(
    (step) =>
      [
        `fraud_proofs/withdrawal_mistag/step_0${step}.main.spend`,
        withdrawalMistagOpaqueTree(step),
      ] as const,
  ),
]);

/** Registered step titles the naming convention pairs with no twin, by reason. */
export const UNPAIRED_STEP_REDEEMER_GROUPS: readonly Readonly<{
  reason: string;
  titles: readonly string[];
}>[] = [
  {
    reason:
      "TypeScript names the family differently (Direct, DistinctAsset or Conservation, FieldPreimageLength, NonExistentInput), so no export carries the Aiken family name",
    titles: [
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
      "fraud_proofs/direct_script_derivation/step_01.main.spend",
      "fraud_proofs/distinct_asset_accumulation_limit/family_entry.main.spend",
      "fraud_proofs/distinct_asset_accumulation_limit/input_rank.main.spend",
      "fraud_proofs/distinct_asset_accumulation_limit/inputs.main.spend",
      "fraud_proofs/distinct_asset_accumulation_limit/inputs_resolve.main.spend",
      "fraud_proofs/distinct_asset_accumulation_limit/mint.main.spend",
      "fraud_proofs/distinct_asset_accumulation_limit/mint_grammar.main.spend",
      "fraud_proofs/distinct_asset_accumulation_limit/outputs.main.spend",
      "fraud_proofs/distinct_asset_accumulation_limit/outputs_grammar.main.spend",
      "fraud_proofs/distinct_asset_accumulation_limit/terminal.main.spend",
      "fraud_proofs/field_preimage_length_mismatch/step_01.main.spend",
      "fraud_proofs/field_preimage_length_mismatch/step_02_accepted.main.spend",
      "fraud_proofs/field_preimage_length_mismatch/step_02_forced.main.spend",
      "fraud_proofs/field_preimage_length_mismatch/step_03.main.spend",
      "fraud_proofs/no_input/step_01.main.spend",
      "fraud_proofs/no_input/step_02.main.spend",
      "fraud_proofs/no_input/step_03.main.spend",
      "fraud_proofs/no_input/step_04.main.spend",
    ],
  },
  {
    reason:
      "the family's twins are named for a worker role (Entry, Grammar, Scan, Route, Final, ...) or shared across families, not for the validator",
    titles: [
      "fraud_proofs/execution_native_script_invalid/family_entry.main.spend",
      "fraud_proofs/execution_native_script_invalid/lookup.main.spend",
      "fraud_proofs/execution_native_script_invalid/terminal.main.spend",
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
      "fraud_proofs/transition_trace/accepted_transaction_v1.main.spend",
      "fraud_proofs/transition_trace/control_v1.main.spend",
      "fraud_proofs/transition_trace/deposit_v1.main.spend",
      "fraud_proofs/transition_trace/duplicate_v1.main.spend",
      "fraud_proofs/transition_trace/forced_v1.main.spend",
      "fraud_proofs/transition_trace/l1_event_v1.main.spend",
      "fraud_proofs/transition_trace/route_v1.main.spend",
      "fraud_proofs/transition_trace/source_v1.main.spend",
      "fraud_proofs/transition_trace/withdrawal_v1.main.spend",
      "fraud_proofs/unused_redeemer/family_entry.main.spend",
      "fraud_proofs/unused_redeemer/redeemer_grammar.main.spend",
      "fraud_proofs/unused_redeemer/redeemer_scan.main.spend",
      "fraud_proofs/unused_script_witness/family_entry.main.spend",
      "fraud_proofs/unused_script_witness/terminal.main.spend",
      "fraud_proofs/unused_script_witness/witness_grammar.main.spend",
      "fraud_proofs/unused_script_witness/witness_scan.main.spend",
    ],
  },
  {
    reason: "the family exports no TypeScript redeemer schema",
    titles: [
      "fraud_proofs/execution_source_script_decoding/terminal.main.spend",
    ],
  },
];
