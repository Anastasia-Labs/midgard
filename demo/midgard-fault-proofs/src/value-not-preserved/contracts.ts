import type { Script } from "@lucid-evolution/lucid";

/** Human-readable family label used in every local failure message. */
export const VALUE_NOT_PRESERVED_CATEGORY_LABEL = "value-not-preserved";

/** Blueprint titles of the registered family validators. */
export const VALUE_NOT_PRESERVED_BLUEPRINT_TITLES = {
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

/** One deployed step of the `value-not-preserved` chain. */
export type ValueNotPreservedStepContract = {
  readonly spendingScript: Script;
  readonly spendingScriptHash: string;
  readonly spendingScriptAddress: string;
};

/**
 * The already-resolved contracts a `value-not-preserved` submission needs.
 *
 * There is deliberately no `categoryId` field: the id is assigned at
 * catalogue registration (`00000019` reserved but not promised), so callers
 * that need one — thread-token asset names, catalogue lookups — take it
 * separately from the deployment they are actually talking to.
 */
export type ValueNotPreservedContracts = {
  readonly unionAcceptedSource: ValueNotPreservedStepContract;
  readonly unionForcedSource: ValueNotPreservedStepContract;
  readonly unionEvent: ValueNotPreservedStepContract;
  readonly unionPreState: ValueNotPreservedStepContract;
  readonly unionInputs: ValueNotPreservedStepContract;
  readonly unionInputValue: ValueNotPreservedStepContract;
  readonly unionAssets: ValueNotPreservedStepContract;
  readonly unionFieldGrammar: ValueNotPreservedStepContract;
  readonly unionOutputs: ValueNotPreservedStepContract;
  readonly unionOutputScan: ValueNotPreservedStepContract;
  readonly unionMint: ValueNotPreservedStepContract;
  readonly unionUpdate: ValueNotPreservedStepContract;
  readonly unionTerminal: ValueNotPreservedStepContract;
  /** Steps 01..04, in order. */
  readonly steps: readonly [
    ValueNotPreservedStepContract,
    ValueNotPreservedStepContract,
    ValueNotPreservedStepContract,
    ValueNotPreservedStepContract,
  ];
  readonly computationThread: {
    readonly policyId: string;
    readonly mintingScript: Script;
  };
  readonly fraudProof: {
    readonly policyId: string;
    readonly mintingScript: Script;
    readonly spendingScriptAddress: string;
  };
  readonly hubOraclePolicyId: string;
  readonly stateQueuePolicyId: string;
  /**
   * Policy id steps 02/03 were parameterized with for §8.6 field-preimage
   * certificates. The installed workflow binds the published certificate
   * policy to its deployment manifest. The step-03
   * submitter selects §8.4 tiers purely from preimage size, so a field over
   * the tier-2 window would name its §8.6 manifest by this policy id.
   */
  readonly fieldPreimageCertificatePolicyId: string;
};

export const CONSERVATION_POSITIONS = [
  "unionAcceptedSource",
  "unionForcedSource",
  "unionEvent",
  "unionPreState",
  "unionInputs",
  "unionInputValue",
  "unionAssets",
  "unionFieldGrammar",
  "unionOutputs",
  "unionOutputScan",
  "unionMint",
  "unionUpdate",
  "unionTerminal",
] as const;
export const conservationManifestName = (
  position: (typeof CONSERVATION_POSITIONS)[number],
): string =>
  `fraudProofValueNotPreserved${position[0]!.toUpperCase()}${position.slice(1)}`;
