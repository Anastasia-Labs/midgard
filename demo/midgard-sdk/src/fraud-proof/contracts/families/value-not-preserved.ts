import { Effect } from "effect";

import {
  AuthenticatedValidator,
  MintingValidator,
  SpendingValidator,
} from "../../../common.js";
import {
  buildFaultProofSpendingStep,
  buildSharedFaultProofContracts,
  type SharedFaultProofContracts,
} from "../shared.js";
import {
  type BuildFaultProofContractsParams,
  type FraudProofChain,
} from "../types.js";

export const VALUE_NOT_PRESERVED_FAULT_PROOF_TITLES = {
  unionAcceptedSource:
    "fraud_proofs/value_not_preserved/union_accepted_source.main.spend",
  unionForcedSource:
    "fraud_proofs/value_not_preserved/union_forced_source.main.spend",
  unionEvent: "fraud_proofs/value_not_preserved/union_event.main.spend",
  unionPreState: "fraud_proofs/value_not_preserved/union_pre_state.main.spend",
  unionInputs: "fraud_proofs/value_not_preserved/union_inputs.main.spend",
  unionInputValue:
    "fraud_proofs/value_not_preserved/union_input_value.main.spend",
  unionAssets: "fraud_proofs/value_not_preserved/union_assets.main.spend",
  unionFieldGrammar:
    "fraud_proofs/value_not_preserved/union_field_grammar.main.spend",
  unionOutputs: "fraud_proofs/value_not_preserved/union_outputs.main.spend",
  unionOutputScan:
    "fraud_proofs/value_not_preserved/union_output_scan.main.spend",
  unionMint: "fraud_proofs/value_not_preserved/union_mint.main.spend",
  unionUpdate: "fraud_proofs/value_not_preserved/union_update.main.spend",
  unionTerminal: "fraud_proofs/value_not_preserved/union_terminal.main.spend",
  step01: "fraud_proofs/value_not_preserved/step_01.main.spend",
  step02: "fraud_proofs/value_not_preserved/step_02.main.spend",
  step03: "fraud_proofs/value_not_preserved/step_03.main.spend",
  step04: "fraud_proofs/value_not_preserved/step_04.main.spend",
} as const;

export type ValueNotPreservedFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly valueNotPreserved: FraudProofChain & {
    readonly unionAcceptedSource: SpendingValidator;
    readonly unionForcedSource: SpendingValidator;
    readonly unionEvent: SpendingValidator;
    readonly unionPreState: SpendingValidator;
    readonly unionInputs: SpendingValidator;
    readonly unionInputValue: SpendingValidator;
    readonly unionAssets: SpendingValidator;
    readonly unionFieldGrammar: SpendingValidator;
    readonly unionOutputs: SpendingValidator;
    readonly unionOutputScan: SpendingValidator;
    readonly unionMint: SpendingValidator;
    readonly unionUpdate: SpendingValidator;
    readonly unionTerminal: SpendingValidator;
    readonly steps: readonly [
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
    ];
  };
};

export type BuildValueNotPreservedFaultProofContractsParams =
  BuildFaultProofContractsParams;

export const buildValueNotPreservedChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
}: BuildFaultProofContractsParams & SharedFaultProofContracts): Effect.Effect<
  ValueNotPreservedFaultProofContracts["valueNotPreserved"],
  Error
> =>
  Effect.gen(function* () {
    const context = { blueprint, network };
    const step04 = yield* buildFaultProofSpendingStep(
      context,
      VALUE_NOT_PRESERVED_FAULT_PROOF_TITLES.step04,
      [
        fraudProof.policyId,
        fraudProofTokenAddressData,
        computationThread.policyId,
      ],
      "Failed to build value-not-preserved step 04",
    );
    const step03 = yield* buildFaultProofSpendingStep(
      context,
      VALUE_NOT_PRESERVED_FAULT_PROOF_TITLES.step03,
      [
        step04.spendingScriptHash,
        computationThread.policyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build value-not-preserved step 03",
    );
    const step02 = yield* buildFaultProofSpendingStep(
      context,
      VALUE_NOT_PRESERVED_FAULT_PROOF_TITLES.step02,
      [
        step03.spendingScriptHash,
        computationThread.policyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build value-not-preserved step 02",
    );
    const unionTerminal = yield* buildFaultProofSpendingStep(
      context,
      VALUE_NOT_PRESERVED_FAULT_PROOF_TITLES.unionTerminal,
      [
        fraudProof.policyId,
        fraudProofTokenAddressData,
        computationThread.policyId,
      ],
      "Failed to build value conservation terminal",
    );
    const unionUpdate = yield* buildFaultProofSpendingStep(
      context,
      VALUE_NOT_PRESERVED_FAULT_PROOF_TITLES.unionUpdate,
      [computationThread.policyId],
      "Failed to build value conservation update",
    );
    const unionAssets = yield* buildFaultProofSpendingStep(
      context,
      VALUE_NOT_PRESERVED_FAULT_PROOF_TITLES.unionAssets,
      [unionUpdate.spendingScriptHash, computationThread.policyId],
      "Failed to build value conservation assets",
    );
    const unionOutputScan = yield* buildFaultProofSpendingStep(
      context,
      VALUE_NOT_PRESERVED_FAULT_PROOF_TITLES.unionOutputScan,
      [unionAssets.spendingScriptHash, computationThread.policyId],
      "Failed to build value conservation outputScan",
    );
    const unionOutputs = yield* buildFaultProofSpendingStep(
      context,
      VALUE_NOT_PRESERVED_FAULT_PROOF_TITLES.unionOutputs,
      [
        unionOutputScan.spendingScriptHash,
        fieldPreimageCertificatePolicyId,
        computationThread.policyId,
      ],
      "Failed to build value conservation outputs",
    );
    const unionMint = yield* buildFaultProofSpendingStep(
      context,
      VALUE_NOT_PRESERVED_FAULT_PROOF_TITLES.unionMint,
      [
        unionUpdate.spendingScriptHash,
        unionTerminal.spendingScriptHash,
        fieldPreimageCertificatePolicyId,
        computationThread.policyId,
      ],
      "Failed to build value conservation mint",
    );
    const unionFieldGrammar = yield* buildFaultProofSpendingStep(
      context,
      VALUE_NOT_PRESERVED_FAULT_PROOF_TITLES.unionFieldGrammar,
      [
        unionOutputs.spendingScriptHash,
        unionMint.spendingScriptHash,
        fieldPreimageCertificatePolicyId,
        computationThread.policyId,
      ],
      "Failed to build value conservation fieldGrammar",
    );
    const unionInputValue = yield* buildFaultProofSpendingStep(
      context,
      VALUE_NOT_PRESERVED_FAULT_PROOF_TITLES.unionInputValue,
      [unionAssets.spendingScriptHash, computationThread.policyId],
      "Failed to build value conservation inputValue",
    );
    const unionInputs = yield* buildFaultProofSpendingStep(
      context,
      VALUE_NOT_PRESERVED_FAULT_PROOF_TITLES.unionInputs,
      [
        unionInputValue.spendingScriptHash,
        unionFieldGrammar.spendingScriptHash,
        fieldPreimageCertificatePolicyId,
        computationThread.policyId,
      ],
      "Failed to build value conservation inputs",
    );
    const unionPreState = yield* buildFaultProofSpendingStep(
      context,
      VALUE_NOT_PRESERVED_FAULT_PROOF_TITLES.unionPreState,
      [unionInputs.spendingScriptHash, computationThread.policyId],
      "Failed to build value conservation preState",
    );
    const unionEvent = yield* buildFaultProofSpendingStep(
      context,
      VALUE_NOT_PRESERVED_FAULT_PROOF_TITLES.unionEvent,
      [unionPreState.spendingScriptHash, computationThread.policyId],
      "Failed to build value conservation event",
    );
    const unionAcceptedSource = yield* buildFaultProofSpendingStep(
      context,
      VALUE_NOT_PRESERVED_FAULT_PROOF_TITLES.unionAcceptedSource,
      [
        unionEvent.spendingScriptHash,
        computationThread.policyId,
        hubOraclePolicyId,
      ],
      "Failed to build value conservation acceptedSource",
    );
    const unionForcedSource = yield* buildFaultProofSpendingStep(
      context,
      VALUE_NOT_PRESERVED_FAULT_PROOF_TITLES.unionForcedSource,
      [unionEvent.spendingScriptHash, computationThread.policyId],
      "Failed to build value conservation forcedSource",
    );
    const step01 = yield* buildFaultProofSpendingStep(
      context,
      VALUE_NOT_PRESERVED_FAULT_PROOF_TITLES.step01,
      [
        step02.spendingScriptHash,
        computationThread.policyId,
        hubOraclePolicyId,
        unionAcceptedSource.spendingScriptHash,
        unionForcedSource.spendingScriptHash,
      ],
      "Failed to build value-not-preserved step 01",
    );
    return {
      firstStep: step01,
      steps: [step01, step02, step03, step04],
      unionAcceptedSource,
      unionForcedSource,
      unionEvent,
      unionPreState,
      unionInputs,
      unionInputValue,
      unionAssets,
      unionFieldGrammar,
      unionOutputs,
      unionOutputScan,
      unionMint,
      unionUpdate,
      unionTerminal,
    };
  });

export const buildValueNotPreservedFaultProofContracts = (
  params: BuildValueNotPreservedFaultProofContractsParams,
): Effect.Effect<ValueNotPreservedFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const valueNotPreserved = yield* buildValueNotPreservedChain({
      ...params,
      ...shared,
    });
    return { ...shared, valueNotPreserved };
  });
