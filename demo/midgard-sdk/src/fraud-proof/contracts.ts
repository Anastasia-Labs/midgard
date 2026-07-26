import {
  Address,
  applyParamsToScript,
  Data,
  MintingPolicy,
  mintingPolicyToId,
  Network,
  SpendingValidator as LucidSpendingValidator,
  validatorToAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  AddressData,
  addressDataFromBech32,
  AuthenticatedValidator,
  MintingValidator,
  SpendingValidator,
} from "@/common.js";

export type FaultProofBlueprintValidator = {
  readonly title: string;
  readonly compiledCode: string;
};

export type FaultProofBlueprint = {
  readonly validators: readonly FaultProofBlueprintValidator[];
};

export const DOUBLE_SPEND_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/double_spend/step_01.main.spend",
  step02: "fraud_proofs/double_spend/step_02.main.spend",
  step03: "fraud_proofs/double_spend/step_03.main.spend",
  step04: "fraud_proofs/double_spend/step_04.main.spend",
} as const;

export const NON_EXISTENT_INPUT_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/no_input/step_01.main.spend",
  step02: "fraud_proofs/no_input/step_02.main.spend",
  step03: "fraud_proofs/no_input/step_03.main.spend",
  step04: "fraud_proofs/no_input/step_04.main.spend",
} as const;

export const INVALID_RANGE_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/invalid_range/step_01.main.spend",
  step02: "fraud_proofs/invalid_range/step_02.main.spend",
} as const;

export const TRANSITION_TRACE_FAULT_PROOF_TITLES = {
  route: "fraud_proofs/transition_trace/route_v1.main.spend",
  control: "fraud_proofs/transition_trace/control_v1.main.spend",
  source: "fraud_proofs/transition_trace/source_v1.main.spend",
  withdrawal: "fraud_proofs/transition_trace/withdrawal_v1.main.spend",
  forced: "fraud_proofs/transition_trace/forced_v1.main.spend",
  accepted: "fraud_proofs/transition_trace/accepted_transaction_v1.main.spend",
  deposit: "fraud_proofs/transition_trace/deposit_v1.main.spend",
  l1Event: "fraud_proofs/transition_trace/l1_event_v1.main.spend",
  duplicate: "fraud_proofs/transition_trace/duplicate_v1.main.spend",
} as const;

export const VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES = {
  dispute: "fraud_proofs/validation_trace/dispute_v1.main.spend",
  source: "fraud_proofs/validation_trace/source_v1.main.spend",
  game: "fraud_proofs/validation_trace/game_v1.main.spend",
  boundary: "fraud_proofs/validation_trace/boundary_v1.main.spend",
  timeout: "fraud_proofs/validation_trace/timeout_v1.main.spend",
  award: "fraud_proofs/validation_trace/award_v1.main.spend",
  prepares: {
    canonicalDecode:
      "fraud_proofs/validation_trace/canonical_decode_v1.main.spend",
    compactBinding:
      "fraud_proofs/validation_trace/compact_binding_v1.main.spend",
    staticLedgerRules:
      "fraud_proofs/validation_trace/static_ledger_rules_v1.main.spend",
    inputSets: "fraud_proofs/validation_trace/input_sets_v1.main.spend",
    signatures: "fraud_proofs/validation_trace/signatures_v1.main.spend",
    phaseANativeScripts:
      "fraud_proofs/validation_trace/phase_a_native_scripts_v1.main.spend",
    phaseAScriptPreconditions:
      "fraud_proofs/validation_trace/phase_a_script_preconditions_v1.main.spend",
    resolveInputs:
      "fraud_proofs/validation_trace/resolve_inputs_v1.main.spend",
    scriptSources:
      "fraud_proofs/validation_trace/script_sources_v1.main.spend",
    nativeScripts:
      "fraud_proofs/validation_trace/native_scripts_v1.main.spend",
    scriptIntegrity:
      "fraud_proofs/validation_trace/script_integrity_v1.main.spend",
    ledgerDelta:
      "fraud_proofs/validation_trace/ledger_delta_v1.main.spend",
  },
  semantics: {
    canonicalDecodeEmpty:
      "fraud_proofs/validation_trace/canonical_decode_empty_semantic_v1.main.spend",
    canonicalDecodeChunk:
      "fraud_proofs/validation_trace/canonical_decode_chunk_semantic_v1.main.spend",
    compactBinding:
      "fraud_proofs/validation_trace/compact_binding_semantic_v1.main.spend",
    staticLedgerRules:
      "fraud_proofs/validation_trace/static_ledger_rules_semantic_v1.main.spend",
    inputSetsEmpty:
      "fraud_proofs/validation_trace/input_sets_empty_semantic_v1.main.spend",
    inputSetsItem:
      "fraud_proofs/validation_trace/input_sets_item_semantic_v1.main.spend",
    signaturesAdvance:
      "fraud_proofs/validation_trace/signatures_advance_semantic_v1.main.spend",
    signaturesAddressItem:
      "fraud_proofs/validation_trace/signatures_address_item_semantic_v1.main.spend",
    signaturesRequiredItem:
      "fraud_proofs/validation_trace/signatures_required_item_semantic_v1.main.spend",
    signaturesHandoff:
      "fraud_proofs/validation_trace/signatures_handoff_semantic_v1.main.spend",
    phaseANativeScriptsAdvance:
      "fraud_proofs/validation_trace/phase_a_native_scripts_advance_semantic_v1.main.spend",
    phaseANativeScriptsItem:
      "fraud_proofs/validation_trace/phase_a_native_scripts_item_semantic_v1.main.spend",
    phaseANativeScriptsTokenHead:
      "fraud_proofs/validation_trace/phase_a_native_scripts_token_head_semantic_v1.main.spend",
    phaseANativeScriptsAllOrAnyContainerFramePayload:
      "fraud_proofs/validation_trace/phase_a_native_scripts_all_or_any_container_frame_payload_semantic_v1.main.spend",
    phaseANativeScriptsAllOrAnyEmptyContainerPayload:
      "fraud_proofs/validation_trace/phase_a_native_scripts_all_or_any_empty_container_payload_semantic_v1.main.spend",
    phaseANativeScriptsAtLeastContainerFramePayload:
      "fraud_proofs/validation_trace/phase_a_native_scripts_at_least_container_frame_payload_semantic_v1.main.spend",
    phaseANativeScriptsAtLeastEmptyContainerPayload:
      "fraud_proofs/validation_trace/phase_a_native_scripts_at_least_empty_container_payload_semantic_v1.main.spend",
    phaseANativeScriptsTimelockPayload:
      "fraud_proofs/validation_trace/phase_a_native_scripts_timelock_payload_semantic_v1.main.spend",
    phaseANativeScriptsSignatureMembershipPayload:
      "fraud_proofs/validation_trace/phase_a_native_scripts_signature_membership_payload_semantic_v1.main.spend",
    phaseANativeScriptsSignatureEmptyPayload:
      "fraud_proofs/validation_trace/phase_a_native_scripts_signature_empty_payload_semantic_v1.main.spend",
    phaseANativeScriptsSignatureBelowFirstPayload:
      "fraud_proofs/validation_trace/phase_a_native_scripts_signature_below_first_payload_semantic_v1.main.spend",
    phaseANativeScriptsSignatureAboveLastPayload:
      "fraud_proofs/validation_trace/phase_a_native_scripts_signature_above_last_payload_semantic_v1.main.spend",
    phaseANativeScriptsSignatureBetweenPayload:
      "fraud_proofs/validation_trace/phase_a_native_scripts_signature_between_payload_semantic_v1.main.spend",
    phaseANativeScriptsFrame:
      "fraud_proofs/validation_trace/phase_a_native_scripts_frame_semantic_v1.main.spend",
    phaseAScriptPreconditions:
      "fraud_proofs/validation_trace/phase_a_script_preconditions_semantic_v1.main.spend",
    phaseAScriptPreconditionsItem:
      "fraud_proofs/validation_trace/phase_a_script_preconditions_item_semantic_v1.main.spend",
    resolveInputsInitial:
      "fraud_proofs/validation_trace/resolve_inputs_initial_semantic_v1.main.spend",
    resolveInputsFinish:
      "fraud_proofs/validation_trace/resolve_inputs_finish_semantic_v1.main.spend",
    resolveInputsMembershipBegin:
      "fraud_proofs/validation_trace/resolve_inputs_membership_begin_semantic_v1.main.spend",
    resolveInputsMembershipStep:
      "fraud_proofs/validation_trace/resolve_inputs_membership_step_semantic_v1.main.spend",
    resolveInputsMembershipFinalize:
      "fraud_proofs/validation_trace/resolve_inputs_membership_finalize_semantic_v1.main.spend",
    resolveInputsNonMembership:
      "fraud_proofs/validation_trace/resolve_inputs_non_membership_semantic_v1.main.spend",
    scriptSourcesNonOutput:
      "fraud_proofs/validation_trace/script_sources_non_output_semantic_v1.main.spend",
    scriptSourcesOutputProofBegin:
      "fraud_proofs/validation_trace/script_sources_output_proof_begin_semantic_v1.main.spend",
    scriptSourcesOutputProofStep:
      "fraud_proofs/validation_trace/script_sources_output_proof_step_semantic_v1.main.spend",
    scriptSourcesOutputProofFinalize:
      "fraud_proofs/validation_trace/script_sources_output_proof_finalize_semantic_v1.main.spend",
    scriptSourcesOutputProofFinish:
      "fraud_proofs/validation_trace/script_sources_output_proof_finish_semantic_v1.main.spend",
    scriptSourcesStageZeroBegin:
      "fraud_proofs/validation_trace/script_sources_stage_zero_begin_semantic_v1.main.spend",
    scriptSourcesStageZeroFinish:
      "fraud_proofs/validation_trace/script_sources_stage_zero_finish_semantic_v1.main.spend",
    scriptSourcesStageZeroHashBlock:
      "fraud_proofs/validation_trace/script_sources_stage_zero_hash_block_semantic_v1.main.spend",
    scriptSourcesStageZeroHashAdvance:
      "fraud_proofs/validation_trace/script_sources_stage_zero_hash_advance_semantic_v1.main.spend",
    scriptSourcesStageZeroHashTerminal:
      "fraud_proofs/validation_trace/script_sources_stage_zero_hash_terminal_semantic_v1.main.spend",
    scriptSourcesStageNineMismatch:
      "fraud_proofs/validation_trace/script_sources_stage_nine_mismatch_semantic_v1.main.spend",
    scriptSourcesStageNineNativeMatch:
      "fraud_proofs/validation_trace/script_sources_stage_nine_native_match_semantic_v1.main.spend",
    scriptSourcesStageNineEffectfulMatch:
      "fraud_proofs/validation_trace/script_sources_stage_nine_effectful_match_semantic_v1.main.spend",
    scriptSourcesStageNineMissing:
      "fraud_proofs/validation_trace/script_sources_stage_nine_missing_semantic_v1.main.spend",
    scriptSourcesStageOneFinish:
      "fraud_proofs/validation_trace/script_sources_stage_one_finish_semantic_v1.main.spend",
    scriptSourcesStageOneRedeemer:
      "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_semantic_v1.main.spend",
    nativeScriptsTerminal:
      "fraud_proofs/validation_trace/native_scripts_terminal_semantic_v1.main.spend",
    nativeScriptsNative:
      "fraud_proofs/validation_trace/native_scripts_native_semantic_v1.main.spend",
    nativeScriptsEffectful:
      "fraud_proofs/validation_trace/native_scripts_effectful_semantic_v1.main.spend",
    scriptIntegrityAuthentication:
      "fraud_proofs/validation_trace/script_integrity_authentication_semantic_v1.main.spend",
    scriptIntegrityCompact:
      "fraud_proofs/validation_trace/script_integrity_compact_semantic_v1.main.spend",
    scriptIntegrityWitnessSet:
      "fraud_proofs/validation_trace/script_integrity_witness_set_semantic_v1.main.spend",
    scriptIntegrityFinalize:
      "fraud_proofs/validation_trace/script_integrity_finalize_semantic_v1.main.spend",
    ledgerDeltaOperation:
      "fraud_proofs/validation_trace/ledger_delta_operation_semantic_v1.main.spend",
    ledgerDeltaReplay:
      "fraud_proofs/validation_trace/ledger_delta_replay_semantic_v1.main.spend",
    ledgerDeltaReplayFinish:
      "fraud_proofs/validation_trace/ledger_delta_replay_finish_semantic_v1.main.spend",
    ledgerDeltaOutput:
      "fraud_proofs/validation_trace/ledger_delta_output_semantic_v1.main.spend",
    ledgerDeltaOutputFinish:
      "fraud_proofs/validation_trace/ledger_delta_output_finish_semantic_v1.main.spend",
    ledgerDeltaProofFrame:
      "fraud_proofs/validation_trace/ledger_delta_proof_frame_semantic_v1.main.spend",
    ledgerDeltaFinalize:
      "fraud_proofs/validation_trace/ledger_delta_finalize_semantic_v1.main.spend",
    ledgerDeltaTerminal:
      "fraud_proofs/validation_trace/ledger_delta_terminal_semantic_v1.main.spend",
  },
  directResolvers: {
    cek: "fraud_proofs/validation_trace/cek_v1.main.spend",
    valueAndMint: "fraud_proofs/validation_trace/value_and_mint_v1.main.spend",
  },
} as const;

export const VALIDATION_TRACE_RESOLVER_COUNT_V1 = 14;

export const FAULT_PROOF_SHARED_TITLES = {
  computationThreadMint: "computation_thread.mint.mint",
  fraudProofMint: "fraud_proof.mint.mint",
  fraudProofSpend: "fraud_proof.spend.else",
} as const;

export type FraudProofChain = {
  readonly firstStep: SpendingValidator;
  readonly steps: readonly [SpendingValidator, ...SpendingValidator[]];
};

export type DoubleSpendFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly doubleSpend: FraudProofChain & {
    readonly steps: readonly [
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
    ];
  };
};

export type NonExistentInputFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly nonExistentInput: FraudProofChain & {
    readonly steps: readonly [
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
    ];
  };
};

export type InvalidRangeFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly invalidRange: FraudProofChain & {
    readonly steps: readonly [SpendingValidator, SpendingValidator];
  };
};

export type TransitionTraceFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly transitionTrace: FraudProofChain & {
    readonly route: SpendingValidator;
    readonly finals: readonly [
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
    ];
    readonly steps: readonly [
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
    ];
  };
};

export type ValidationTraceDisputeFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly validationTraceDispute: FraudProofChain & {
    readonly opener: SpendingValidator;
    readonly source: SpendingValidator;
    readonly game: SpendingValidator;
    readonly boundary: SpendingValidator;
    readonly timeout: SpendingValidator;
    readonly award: SpendingValidator;
    readonly prepareResolvers: readonly [
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
    ];
    readonly semanticResolvers: readonly [
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
    ];
    readonly directResolvers: readonly [
      SpendingValidator,
      SpendingValidator,
    ];
    readonly resolvers: readonly [
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
    ];
  };
};

export type FaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly doubleSpend: DoubleSpendFaultProofContracts["doubleSpend"];
  readonly nonExistentInput: NonExistentInputFaultProofContracts["nonExistentInput"];
  readonly invalidRange: InvalidRangeFaultProofContracts["invalidRange"];
  readonly transitionTrace: TransitionTraceFaultProofContracts["transitionTrace"];
  readonly validationTraceDispute: ValidationTraceDisputeFaultProofContracts["validationTraceDispute"];
};

type SharedFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fraudProofTokenAddressData: Data;
};

export type BuildFaultProofContractsParams = {
  readonly blueprint: FaultProofBlueprint;
  readonly network: Network;
  readonly hubOraclePolicyId: string;
  readonly fraudProofCataloguePolicyId: string;
};

export type BuildDoubleSpendFaultProofContractsParams =
  BuildFaultProofContractsParams;

export type BuildNonExistentInputFaultProofContractsParams =
  BuildFaultProofContractsParams;

export type BuildInvalidRangeFaultProofContractsParams =
  BuildFaultProofContractsParams;

export type BuildTransitionTraceFaultProofContractsParams =
  BuildFaultProofContractsParams;

export type BuildValidationTraceDisputeFaultProofContractsParams =
  BuildFaultProofContractsParams;

export const parseFaultProofBlueprint = (
  value: unknown,
): FaultProofBlueprint => {
  if (typeof value !== "object" || value === null) {
    throw new Error("Fault proof blueprint must be a JSON object");
  }

  const validators = (value as { readonly validators?: unknown }).validators;
  if (!Array.isArray(validators)) {
    throw new Error("Fault proof blueprint must contain validators[]");
  }

  return {
    validators: validators.map((validator, index) => {
      if (typeof validator !== "object" || validator === null) {
        throw new Error(`validators[${index}] must be an object`);
      }
      const candidate = validator as {
        readonly title?: unknown;
        readonly compiledCode?: unknown;
      };
      if (typeof candidate.title !== "string") {
        throw new Error(`validators[${index}].title must be a string`);
      }
      if (typeof candidate.compiledCode !== "string") {
        throw new Error(`validators[${index}].compiledCode must be a string`);
      }
      return {
        title: candidate.title,
        compiledCode: candidate.compiledCode,
      };
    }),
  };
};

const getCompiledScript = (
  blueprint: FaultProofBlueprint,
  title: string,
): string => {
  const found = blueprint.validators.find(
    (validator) => validator.title === title,
  );
  if (found === undefined) {
    throw new Error(`Validator with title "${title}" not found in blueprint`);
  }
  return found.compiledCode;
};

const makeMintingPolicy = (mintingScriptCBOR: string): MintingValidator => {
  const mintingScript: MintingPolicy = {
    type: "PlutusV3",
    script: mintingScriptCBOR,
  };
  return {
    mintingScriptCBOR,
    mintingScript,
    policyId: mintingPolicyToId(mintingScript),
  };
};

const makeSpendingValidator = (
  network: Network,
  spendingScriptCBOR: string,
): SpendingValidator => {
  const spendingScript: LucidSpendingValidator = {
    type: "PlutusV3",
    script: spendingScriptCBOR,
  };
  return {
    spendingScriptCBOR,
    spendingScript,
    spendingScriptAddress: validatorToAddress(network, spendingScript),
    spendingScriptHash: validatorToScriptHash(spendingScript),
  };
};

const makeAuthenticatedValidator = (
  network: Network,
  mintingScriptCBOR: string,
  spendingScriptCBOR: string,
): AuthenticatedValidator => ({
  ...makeSpendingValidator(network, spendingScriptCBOR),
  ...makeMintingPolicy(mintingScriptCBOR),
});

const asAddressDataParam = (address: Address): Effect.Effect<Data, Error> =>
  addressDataFromBech32(address).pipe(
    Effect.map((addressData) => Data.from(Data.to(addressData, AddressData))),
    Effect.mapError(
      (cause) =>
        new Error(
          `Failed to encode fraud proof token address parameter: ${cause.message}`,
        ),
    ),
  );

const tryBuild = <A>(
  description: string,
  build: () => A,
): Effect.Effect<A, Error> =>
  Effect.try({
    try: build,
    catch: (cause) =>
      new Error(
        `${description}: ${cause instanceof Error ? cause.message : String(cause)}`,
      ),
  });

const buildSharedFaultProofContracts = ({
  blueprint,
  network,
  hubOraclePolicyId,
  fraudProofCataloguePolicyId,
}: BuildFaultProofContractsParams): Effect.Effect<
  SharedFaultProofContracts,
  Error
> =>
  Effect.gen(function* () {
    const computationThread = yield* tryBuild(
      "Failed to build computation-thread minting policy",
      () =>
        makeMintingPolicy(
          applyParamsToScript(
            getCompiledScript(
              blueprint,
              FAULT_PROOF_SHARED_TITLES.computationThreadMint,
            ),
            [fraudProofCataloguePolicyId, hubOraclePolicyId],
          ),
        ),
    );

    const fraudProof = yield* tryBuild(
      "Failed to build fraud-proof token validator",
      () =>
        makeAuthenticatedValidator(
          network,
          applyParamsToScript(
            getCompiledScript(
              blueprint,
              FAULT_PROOF_SHARED_TITLES.fraudProofMint,
            ),
            [computationThread.policyId],
          ),
          getCompiledScript(
            blueprint,
            FAULT_PROOF_SHARED_TITLES.fraudProofSpend,
          ),
        ),
    );

    const fraudProofTokenAddressData = yield* asAddressDataParam(
      fraudProof.spendingScriptAddress,
    );

    return {
      computationThread,
      fraudProof,
      fraudProofTokenAddressData,
    };
  });

const buildDoubleSpendChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
}: {
  readonly blueprint: FaultProofBlueprint;
  readonly network: Network;
  readonly hubOraclePolicyId: string;
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fraudProofTokenAddressData: Data;
}): Effect.Effect<DoubleSpendFaultProofContracts["doubleSpend"], Error> =>
  Effect.gen(function* () {
    const step04 = yield* tryBuild("Failed to build double-spend step 04", () =>
      makeSpendingValidator(
        network,
        applyParamsToScript(
          getCompiledScript(blueprint, DOUBLE_SPEND_FAULT_PROOF_TITLES.step04),
          [
            computationThread.policyId,
            fraudProof.policyId,
            fraudProofTokenAddressData,
          ],
        ),
      ),
    );

    const step03 = yield* tryBuild("Failed to build double-spend step 03", () =>
      makeSpendingValidator(
        network,
        applyParamsToScript(
          getCompiledScript(blueprint, DOUBLE_SPEND_FAULT_PROOF_TITLES.step03),
          [step04.spendingScriptHash, computationThread.policyId],
        ),
      ),
    );

    const step02 = yield* tryBuild("Failed to build double-spend step 02", () =>
      makeSpendingValidator(
        network,
        applyParamsToScript(
          getCompiledScript(blueprint, DOUBLE_SPEND_FAULT_PROOF_TITLES.step02),
          [
            step03.spendingScriptHash,
            computationThread.policyId,
            hubOraclePolicyId,
          ],
        ),
      ),
    );

    const step01 = yield* tryBuild("Failed to build double-spend step 01", () =>
      makeSpendingValidator(
        network,
        applyParamsToScript(
          getCompiledScript(blueprint, DOUBLE_SPEND_FAULT_PROOF_TITLES.step01),
          [
            step02.spendingScriptHash,
            computationThread.policyId,
            hubOraclePolicyId,
          ],
        ),
      ),
    );

    return {
      firstStep: step01,
      steps: [step01, step02, step03, step04],
    };
  });

const buildNonExistentInputChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
}: {
  readonly blueprint: FaultProofBlueprint;
  readonly network: Network;
  readonly hubOraclePolicyId: string;
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fraudProofTokenAddressData: Data;
}): Effect.Effect<
  NonExistentInputFaultProofContracts["nonExistentInput"],
  Error
> =>
  Effect.gen(function* () {
    const step04 = yield* tryBuild(
      "Failed to build non-existent-input step 04",
      () =>
        makeSpendingValidator(
          network,
          applyParamsToScript(
            getCompiledScript(
              blueprint,
              NON_EXISTENT_INPUT_FAULT_PROOF_TITLES.step04,
            ),
            [
              fraudProof.policyId,
              fraudProofTokenAddressData,
              computationThread.policyId,
            ],
          ),
        ),
    );

    const step03 = yield* tryBuild(
      "Failed to build non-existent-input step 03",
      () =>
        makeSpendingValidator(
          network,
          applyParamsToScript(
            getCompiledScript(
              blueprint,
              NON_EXISTENT_INPUT_FAULT_PROOF_TITLES.step03,
            ),
            [step04.spendingScriptHash, computationThread.policyId],
          ),
        ),
    );

    const step02 = yield* tryBuild(
      "Failed to build non-existent-input step 02",
      () =>
        makeSpendingValidator(
          network,
          applyParamsToScript(
            getCompiledScript(
              blueprint,
              NON_EXISTENT_INPUT_FAULT_PROOF_TITLES.step02,
            ),
            [step03.spendingScriptHash, computationThread.policyId],
          ),
        ),
    );

    const step01 = yield* tryBuild(
      "Failed to build non-existent-input step 01",
      () =>
        makeSpendingValidator(
          network,
          applyParamsToScript(
            getCompiledScript(
              blueprint,
              NON_EXISTENT_INPUT_FAULT_PROOF_TITLES.step01,
            ),
            [
              step02.spendingScriptHash,
              computationThread.policyId,
              hubOraclePolicyId,
            ],
          ),
        ),
    );

    return {
      firstStep: step01,
      steps: [step01, step02, step03, step04],
    };
  });

const buildInvalidRangeChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
}: {
  readonly blueprint: FaultProofBlueprint;
  readonly network: Network;
  readonly hubOraclePolicyId: string;
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fraudProofTokenAddressData: Data;
}): Effect.Effect<InvalidRangeFaultProofContracts["invalidRange"], Error> =>
  Effect.gen(function* () {
    const invalidRangeStep02 = yield* tryBuild(
      "Failed to build invalid-range step 02",
      () =>
        makeSpendingValidator(
          network,
          applyParamsToScript(
            getCompiledScript(
              blueprint,
              INVALID_RANGE_FAULT_PROOF_TITLES.step02,
            ),
            [
              fraudProof.policyId,
              fraudProofTokenAddressData,
              computationThread.policyId,
            ],
          ),
        ),
    );

    const invalidRangeStep01 = yield* tryBuild(
      "Failed to build invalid-range step 01",
      () =>
        makeSpendingValidator(
          network,
          applyParamsToScript(
            getCompiledScript(
              blueprint,
              INVALID_RANGE_FAULT_PROOF_TITLES.step01,
            ),
            [
              invalidRangeStep02.spendingScriptHash,
              computationThread.policyId,
              hubOraclePolicyId,
            ],
          ),
        ),
    );

    return {
      firstStep: invalidRangeStep01,
      steps: [invalidRangeStep01, invalidRangeStep02],
    };
  });

const buildTransitionTraceChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
}: {
  readonly blueprint: FaultProofBlueprint;
  readonly network: Network;
  readonly hubOraclePolicyId: string;
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fraudProofTokenAddressData: Data;
}): Effect.Effect<
  TransitionTraceFaultProofContracts["transitionTrace"],
  Error
> =>
  Effect.gen(function* () {
    const finalSpecs = [
      ["control", false],
      ["source", false],
      ["withdrawal", false],
      ["forced", false],
      ["accepted", false],
      ["deposit", true],
      ["l1Event", true],
      ["duplicate", false],
    ] as const;
    const builtFinals: SpendingValidator[] = [];
    for (const [name, needsHub] of finalSpecs) {
      builtFinals.push(
        yield* tryBuild(
          `Failed to build transition-trace ${name} final validator`,
          () =>
            makeSpendingValidator(
              network,
              applyParamsToScript(
                getCompiledScript(
                  blueprint,
                  TRANSITION_TRACE_FAULT_PROOF_TITLES[name],
                ),
                [
                  computationThread.policyId,
                  fraudProof.policyId,
                  fraudProofTokenAddressData,
                  ...(needsHub ? [hubOraclePolicyId] : []),
                ],
              ),
            ),
        ),
      );
    }
    const finals = [
      builtFinals[0]!,
      builtFinals[1]!,
      builtFinals[2]!,
      builtFinals[3]!,
      builtFinals[4]!,
      builtFinals[5]!,
      builtFinals[6]!,
      builtFinals[7]!,
    ] as const;
    if (
      new Set(finals.map(({ spendingScriptHash }) => spendingScriptHash))
        .size !== finals.length
    ) {
      return yield* Effect.fail(
        new Error("Transition-trace final validator hashes must be distinct"),
      );
    }
    const finalHashesSchema = Data.Array(Data.Bytes());
    type FinalHashes = Data.Static<typeof finalHashesSchema>;
    const FinalHashes = finalHashesSchema as unknown as FinalHashes;
    const finalHashesData = Data.from(
      Data.to(
        finals.map(({ spendingScriptHash }) => spendingScriptHash),
        FinalHashes,
      ),
    );
    const route = yield* tryBuild(
      "Failed to build transition-trace route validator",
      () =>
        makeSpendingValidator(
          network,
          applyParamsToScript(
            getCompiledScript(
              blueprint,
              TRANSITION_TRACE_FAULT_PROOF_TITLES.route,
            ),
            [finalHashesData, computationThread.policyId],
          ),
        ),
    );

    return {
      firstStep: route,
      route,
      finals,
      steps: [route, ...finals],
    };
  });

const buildValidationTraceDisputeChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
}: {
  readonly blueprint: FaultProofBlueprint;
  readonly network: Network;
  readonly hubOraclePolicyId: string;
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fraudProofTokenAddressData: Data;
}): Effect.Effect<
  ValidationTraceDisputeFaultProofContracts["validationTraceDispute"],
  Error
> =>
  Effect.gen(function* () {
    const award = yield* tryBuild(
      "Failed to build validation-trace award validator",
      () =>
        makeSpendingValidator(
          network,
          applyParamsToScript(
            getCompiledScript(
              blueprint,
              VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.award,
            ),
            [
              computationThread.policyId,
              fraudProof.policyId,
              fraudProofTokenAddressData,
            ],
          ),
        ),
    );

    const semanticTitles = Object.values(
      VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.semantics,
    );
    const builtSemanticResolvers: SpendingValidator[] = [];
    for (const [index, title] of semanticTitles.entries()) {
      builtSemanticResolvers.push(
        yield* tryBuild(
          `Failed to build validation-trace semantic resolver ${index.toString()}`,
          () =>
            makeSpendingValidator(
              network,
              applyParamsToScript(getCompiledScript(blueprint, title), [
                award.spendingScriptHash,
                computationThread.policyId,
              ]),
            ),
        ),
      );
    }
    if (builtSemanticResolvers.length !== 63) {
      return yield* Effect.fail(
        new Error("Validation-trace semantic resolver set is incomplete"),
      );
    }
    const semanticResolvers = [
      builtSemanticResolvers[0]!,
      builtSemanticResolvers[1]!,
      builtSemanticResolvers[2]!,
      builtSemanticResolvers[3]!,
      builtSemanticResolvers[4]!,
      builtSemanticResolvers[5]!,
      builtSemanticResolvers[6]!,
      builtSemanticResolvers[7]!,
      builtSemanticResolvers[8]!,
      builtSemanticResolvers[9]!,
      builtSemanticResolvers[10]!,
      builtSemanticResolvers[11]!,
      builtSemanticResolvers[12]!,
      builtSemanticResolvers[13]!,
      builtSemanticResolvers[14]!,
      builtSemanticResolvers[15]!,
      builtSemanticResolvers[16]!,
      builtSemanticResolvers[17]!,
      builtSemanticResolvers[18]!,
      builtSemanticResolvers[19]!,
      builtSemanticResolvers[20]!,
      builtSemanticResolvers[21]!,
      builtSemanticResolvers[22]!,
      builtSemanticResolvers[23]!,
      builtSemanticResolvers[24]!,
      builtSemanticResolvers[25]!,
      builtSemanticResolvers[26]!,
      builtSemanticResolvers[27]!,
      builtSemanticResolvers[28]!,
      builtSemanticResolvers[29]!,
      builtSemanticResolvers[30]!,
      builtSemanticResolvers[31]!,
      builtSemanticResolvers[32]!,
      builtSemanticResolvers[33]!,
      builtSemanticResolvers[34]!,
      builtSemanticResolvers[35]!,
      builtSemanticResolvers[36]!,
      builtSemanticResolvers[37]!,
      builtSemanticResolvers[38]!,
      builtSemanticResolvers[39]!,
      builtSemanticResolvers[40]!,
      builtSemanticResolvers[41]!,
      builtSemanticResolvers[42]!,
      builtSemanticResolvers[43]!,
      builtSemanticResolvers[44]!,
      builtSemanticResolvers[45]!,
      builtSemanticResolvers[46]!,
      builtSemanticResolvers[47]!,
      builtSemanticResolvers[48]!,
      builtSemanticResolvers[49]!,
      builtSemanticResolvers[50]!,
      builtSemanticResolvers[51]!,
      builtSemanticResolvers[52]!,
      builtSemanticResolvers[53]!,
      builtSemanticResolvers[54]!,
      builtSemanticResolvers[55]!,
      builtSemanticResolvers[56]!,
      builtSemanticResolvers[57]!,
      builtSemanticResolvers[58]!,
      builtSemanticResolvers[59]!,
      builtSemanticResolvers[60]!,
      builtSemanticResolvers[61]!,
      builtSemanticResolvers[62]!,
    ] as const;
    const semanticResolverGroups = [
      [semanticResolvers[0], semanticResolvers[1]],
      [semanticResolvers[2]],
      [semanticResolvers[3]],
      [semanticResolvers[4], semanticResolvers[5]],
      [
        semanticResolvers[6],
        semanticResolvers[7],
        semanticResolvers[8],
        semanticResolvers[9],
      ],
      [
        semanticResolvers[10],
        semanticResolvers[11],
        semanticResolvers[12],
        semanticResolvers[13],
        semanticResolvers[14],
        semanticResolvers[15],
        semanticResolvers[16],
        semanticResolvers[17],
        semanticResolvers[18],
        semanticResolvers[19],
        semanticResolvers[20],
        semanticResolvers[21],
        semanticResolvers[22],
        semanticResolvers[23],
      ],
      [semanticResolvers[24], semanticResolvers[25]],
      [
        semanticResolvers[26],
        semanticResolvers[27],
        semanticResolvers[28],
        semanticResolvers[29],
        semanticResolvers[30],
        semanticResolvers[31],
      ],
      [
        semanticResolvers[32],
        semanticResolvers[33],
        semanticResolvers[34],
        semanticResolvers[35],
        semanticResolvers[36],
        semanticResolvers[37],
        semanticResolvers[38],
        semanticResolvers[39],
        semanticResolvers[40],
        semanticResolvers[41],
        semanticResolvers[42],
        semanticResolvers[43],
        semanticResolvers[44],
        semanticResolvers[45],
        semanticResolvers[46],
        semanticResolvers[47],
      ],
      [
        semanticResolvers[48],
        semanticResolvers[49],
        semanticResolvers[50],
      ],
      [
        semanticResolvers[51],
        semanticResolvers[52],
        semanticResolvers[53],
        semanticResolvers[54],
      ],
      [
        semanticResolvers[55],
        semanticResolvers[56],
        semanticResolvers[57],
        semanticResolvers[58],
        semanticResolvers[59],
        semanticResolvers[60],
        semanticResolvers[61],
        semanticResolvers[62],
      ],
    ] as const;
    const semanticResolverHashesSchema = Data.Array(Data.Bytes());
    type SemanticResolverHashes = Data.Static<
      typeof semanticResolverHashesSchema
    >;
    const SemanticResolverHashes =
      semanticResolverHashesSchema as unknown as SemanticResolverHashes;

    const prepareTitles = Object.values(
      VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.prepares,
    );
    const builtPrepareResolvers: SpendingValidator[] = [];
    for (const [index, title] of prepareTitles.entries()) {
      const semanticResolverHashesData = Data.from(
        Data.to(
          semanticResolverGroups[index]!.map(
            ({ spendingScriptHash }) => spendingScriptHash,
          ),
          SemanticResolverHashes,
        ),
      ) as Data;
      builtPrepareResolvers.push(
        yield* tryBuild(
          `Failed to build validation-trace prepare resolver ${index.toString()}`,
          () =>
            makeSpendingValidator(
              network,
              applyParamsToScript(getCompiledScript(blueprint, title), [
                semanticResolverHashesData,
                computationThread.policyId,
              ]),
            ),
        ),
      );
    }
    if (builtPrepareResolvers.length !== 12) {
      return yield* Effect.fail(
        new Error("Validation-trace prepare resolver set is incomplete"),
      );
    }
    const prepareResolvers = [
      builtPrepareResolvers[0]!,
      builtPrepareResolvers[1]!,
      builtPrepareResolvers[2]!,
      builtPrepareResolvers[3]!,
      builtPrepareResolvers[4]!,
      builtPrepareResolvers[5]!,
      builtPrepareResolvers[6]!,
      builtPrepareResolvers[7]!,
      builtPrepareResolvers[8]!,
      builtPrepareResolvers[9]!,
      builtPrepareResolvers[10]!,
      builtPrepareResolvers[11]!,
    ] as const;

    const directTitles = Object.values(
      VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.directResolvers,
    );
    const builtDirectResolvers: SpendingValidator[] = [];
    for (const [index, title] of directTitles.entries()) {
      builtDirectResolvers.push(
        yield* tryBuild(
          `Failed to build validation-trace direct resolver ${index.toString()}`,
          () =>
            makeSpendingValidator(
              network,
              applyParamsToScript(getCompiledScript(blueprint, title), [
                computationThread.policyId,
                fraudProof.policyId,
                fraudProofTokenAddressData,
              ]),
            ),
        ),
      );
    }
    if (builtDirectResolvers.length !== 2) {
      return yield* Effect.fail(
        new Error("Validation-trace direct resolver set is incomplete"),
      );
    }
    const directResolvers = [
      builtDirectResolvers[0]!,
      builtDirectResolvers[1]!,
    ] as const;
    const resolvers = [
      prepareResolvers[0],
      prepareResolvers[1],
      prepareResolvers[2],
      prepareResolvers[3],
      prepareResolvers[4],
      prepareResolvers[5],
      prepareResolvers[6],
      prepareResolvers[7],
      prepareResolvers[8],
      prepareResolvers[9],
      prepareResolvers[10],
      directResolvers[0],
      directResolvers[1],
      prepareResolvers[11],
    ] as const;
    if (
      new Set(resolvers.map(({ spendingScriptHash }) => spendingScriptHash))
        .size !== VALIDATION_TRACE_RESOLVER_COUNT_V1
    ) {
      return yield* Effect.fail(
        new Error("Validation-trace resolver hashes must be distinct"),
      );
    }
    const resolverHashesSchema = Data.Array(Data.Bytes());
    type ResolverHashes = Data.Static<typeof resolverHashesSchema>;
    const ResolverHashes = resolverHashesSchema as unknown as ResolverHashes;
    const resolverHashesData = Data.from(
      Data.to(
        resolvers.map(({ spendingScriptHash }) => spendingScriptHash),
        ResolverHashes,
      ),
    );

    const boundary = yield* tryBuild(
      "Failed to build validation-trace boundary validator",
      () =>
        makeSpendingValidator(
          network,
          applyParamsToScript(
            getCompiledScript(
              blueprint,
              VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.boundary,
            ),
            [resolverHashesData, computationThread.policyId],
          ),
        ),
    );
    const timeout = yield* tryBuild(
      "Failed to build validation-trace timeout validator",
      () =>
        makeSpendingValidator(
          network,
          applyParamsToScript(
            getCompiledScript(
              blueprint,
              VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.timeout,
            ),
            [
              computationThread.policyId,
              fraudProof.policyId,
              fraudProofTokenAddressData,
            ],
          ),
        ),
    );
    const game = yield* tryBuild(
      "Failed to build validation-trace midpoint game validator",
      () =>
        makeSpendingValidator(
          network,
          applyParamsToScript(
            getCompiledScript(
              blueprint,
              VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.game,
            ),
            [
              boundary.spendingScriptHash,
              timeout.spendingScriptHash,
              computationThread.policyId,
            ],
          ),
        ),
    );
    const source = yield* tryBuild(
      "Failed to build validation-trace source validator",
      () =>
        makeSpendingValidator(
          network,
          applyParamsToScript(
            getCompiledScript(
              blueprint,
              VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.source,
            ),
            [
              game.spendingScriptHash,
              award.spendingScriptHash,
              computationThread.policyId,
            ],
          ),
        ),
    );
    const dispute = yield* tryBuild(
      "Failed to build validation-trace dispute opener",
      () =>
        makeSpendingValidator(
          network,
          applyParamsToScript(
            getCompiledScript(
              blueprint,
              VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.dispute,
            ),
            [
              source.spendingScriptHash,
              computationThread.policyId,
              hubOraclePolicyId,
            ],
          ),
        ),
    );

    return {
      firstStep: dispute,
      steps: [
        dispute,
        source,
        game,
        boundary,
        timeout,
        award,
        ...semanticResolvers,
        ...prepareResolvers,
        ...directResolvers,
      ],
      opener: dispute,
      source,
      game,
      boundary,
      timeout,
      award,
      prepareResolvers,
      semanticResolvers,
      directResolvers,
      resolvers,
    };
  });

export const buildFaultProofContracts = (
  params: BuildFaultProofContractsParams,
): Effect.Effect<FaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const doubleSpend = yield* buildDoubleSpendChain({
      ...params,
      ...shared,
    });
    const nonExistentInput = yield* buildNonExistentInputChain({
      ...params,
      ...shared,
    });
    const invalidRange = yield* buildInvalidRangeChain({
      ...params,
      ...shared,
    });
    const transitionTrace = yield* buildTransitionTraceChain({
      ...params,
      ...shared,
    });
    const validationTraceDispute = yield* buildValidationTraceDisputeChain({
      ...params,
      ...shared,
    });

    return {
      computationThread: shared.computationThread,
      fraudProof: shared.fraudProof,
      doubleSpend,
      nonExistentInput,
      invalidRange,
      transitionTrace,
      validationTraceDispute,
    };
  });

export const buildDoubleSpendFaultProofContracts = (
  params: BuildDoubleSpendFaultProofContractsParams,
): Effect.Effect<DoubleSpendFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const doubleSpend = yield* buildDoubleSpendChain({
      ...params,
      ...shared,
    });
    return {
      computationThread: shared.computationThread,
      fraudProof: shared.fraudProof,
      doubleSpend,
    };
  });

export const buildNonExistentInputFaultProofContracts = (
  params: BuildNonExistentInputFaultProofContractsParams,
): Effect.Effect<NonExistentInputFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const nonExistentInput = yield* buildNonExistentInputChain({
      ...params,
      ...shared,
    });
    return {
      computationThread: shared.computationThread,
      fraudProof: shared.fraudProof,
      nonExistentInput,
    };
  });

export const buildInvalidRangeFaultProofContracts = (
  params: BuildInvalidRangeFaultProofContractsParams,
): Effect.Effect<InvalidRangeFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const invalidRange = yield* buildInvalidRangeChain({
      ...params,
      ...shared,
    });
    return {
      computationThread: shared.computationThread,
      fraudProof: shared.fraudProof,
      invalidRange,
    };
  });

export const buildTransitionTraceFaultProofContracts = (
  params: BuildTransitionTraceFaultProofContractsParams,
): Effect.Effect<TransitionTraceFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const transitionTrace = yield* buildTransitionTraceChain({
      ...params,
      ...shared,
    });
    return {
      computationThread: shared.computationThread,
      fraudProof: shared.fraudProof,
      transitionTrace,
    };
  });

export const buildValidationTraceDisputeFaultProofContracts = (
  params: BuildValidationTraceDisputeFaultProofContractsParams,
): Effect.Effect<ValidationTraceDisputeFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const validationTraceDispute = yield* buildValidationTraceDisputeChain({
      ...params,
      ...shared,
    });
    return {
      computationThread: shared.computationThread,
      fraudProof: shared.fraudProof,
      validationTraceDispute,
    };
  });
