import {
  type DeploymentContract,
  deploymentFamilyStepNames,
  deploymentValidatorTitle as deployedTitle,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import { Effect } from "effect";

import {
  AuthenticatedValidator,
  MintingValidator,
  SpendingValidator,
} from "../../../common.js";
import { buildDirectAllPurposesContracts } from "../direct-all-purposes.js";
import { buildDirectBindingContracts } from "../direct-binding.js";
import { buildDirectSourceContracts } from "../direct-source.js";
import type { CertifiedFaultProofChainInputs } from "../shared.js";
import {
  buildFaultProofSpendingStep,
  buildSharedFaultProofContracts,
} from "../shared.js";
import {
  type BuildFaultProofContractsParams,
  type FraudProofChain,
} from "../types.js";

export const UNUSED_SCRIPT_WITNESS_FAULT_PROOF_TITLES = {
  step01: deployedTitle("fraudProofUnusedScriptWitness"),
  familyEntry: deployedTitle("fraudProofUnusedScriptWitnessFamilyEntry"),
  witnessEntry: deployedTitle("fraudProofUnusedScriptWitnessWitnessEntry"),
  witnessGrammar: deployedTitle("fraudProofUnusedScriptWitnessWitnessGrammar"),
  witnessScan: deployedTitle("fraudProofUnusedScriptWitnessWitnessScan"),
  terminal: deployedTitle("fraudProofUnusedScriptWitnessTerminal"),
} as const;

/** Each spending role bound once to its deployment member, in deployment step
 * order. Off-chain code reads named roles through `unusedScriptWitnessRoles`,
 * so a manifest-restored chain carries them. */
const UNUSED_SCRIPT_WITNESS_ROLE_MEMBERS = [
  ["step01", "fraudProofUnusedScriptWitness"],
  ["bindEvent", "fraudProofUnusedScriptWitnessBindEvent"],
  ["bindPreState", "fraudProofUnusedScriptWitnessBindPreState"],
  ["sourceStart", "fraudProofUnusedScriptWitnessSourceStart"],
  ["sourceResolved", "fraudProofUnusedScriptWitnessSourceResolved"],
  ["sourceAuthenticate", "fraudProofUnusedScriptWitnessSourceAuthenticate"],
  ["sourceMap", "fraudProofUnusedScriptWitnessSourceMap"],
  ["sourceInputFinish", "fraudProofUnusedScriptWitnessSourceInputFinish"],
  ["sourceGrammar", "fraudProofUnusedScriptWitnessSourceGrammar"],
  ["sourceWitnesses", "fraudProofUnusedScriptWitnessSourceWitnesses"],
  ["sourceFinish", "fraudProofUnusedScriptWitnessSourceFinish"],
  ["familyEntry", "fraudProofUnusedScriptWitnessFamilyEntry"],
  ["allPurposesSpend", "fraudProofUnusedScriptWitnessAllPurposesSpend"],
  [
    "allPurposesSpendResolve",
    "fraudProofUnusedScriptWitnessAllPurposesSpendResolve",
  ],
  [
    "allPurposesMintGrammar",
    "fraudProofUnusedScriptWitnessAllPurposesMintGrammar",
  ],
  ["allPurposesMint", "fraudProofUnusedScriptWitnessAllPurposesMint"],
  [
    "allPurposesObserveOpen",
    "fraudProofUnusedScriptWitnessAllPurposesObserveOpen",
  ],
  ["allPurposesObserve", "fraudProofUnusedScriptWitnessAllPurposesObserve"],
  [
    "allPurposesReceiveGrammar",
    "fraudProofUnusedScriptWitnessAllPurposesReceiveGrammar",
  ],
  ["allPurposesReceive", "fraudProofUnusedScriptWitnessAllPurposesReceive"],
  ["witnessEntry", "fraudProofUnusedScriptWitnessWitnessEntry"],
  ["witnessGrammar", "fraudProofUnusedScriptWitnessWitnessGrammar"],
  ["witnessScan", "fraudProofUnusedScriptWitnessWitnessScan"],
  ["terminal", "fraudProofUnusedScriptWitnessTerminal"],
] as const satisfies readonly (readonly [string, DeploymentContract])[];

export type UnusedScriptWitnessRole =
  (typeof UNUSED_SCRIPT_WITNESS_ROLE_MEMBERS)[number][0];

export const UNUSED_SCRIPT_WITNESS_ROLE_ORDER: readonly UnusedScriptWitnessRole[] =
  Object.freeze(UNUSED_SCRIPT_WITNESS_ROLE_MEMBERS.map(([role]) => role));

/** Refuses, by role name, a role whose member the given manifest step order
 * places at another step of the `unusedScriptWitness` family. */
export const checkUnusedScriptWitnessRoleOrder = (
  steps: readonly DeploymentContract[],
): void => {
  if (steps.length !== UNUSED_SCRIPT_WITNESS_ROLE_MEMBERS.length)
    throw new Error(
      `unused-script-witness has ${UNUSED_SCRIPT_WITNESS_ROLE_MEMBERS.length} roles; the manifest has ${steps.length} steps`,
    );
  UNUSED_SCRIPT_WITNESS_ROLE_MEMBERS.forEach(([role, member], index) => {
    if (steps[index] !== member)
      throw new Error(
        `unused-script-witness role ${role} is ${member}, but manifest step ${index} is ${String(steps[index])}`,
      );
  });
};

export type UnusedScriptWitnessChain = FraudProofChain;

export type UnusedScriptWitnessRoles = Readonly<
  Record<UnusedScriptWitnessRole, SpendingValidator>
>;

/** Named roles of a built or manifest-restored chain. */
export const unusedScriptWitnessRoles = (
  chain: FraudProofChain,
): UnusedScriptWitnessRoles => {
  checkUnusedScriptWitnessRoleOrder(
    deploymentFamilyStepNames("unusedScriptWitness"),
  );
  if (chain.steps.length !== UNUSED_SCRIPT_WITNESS_ROLE_ORDER.length)
    throw new Error(
      `unused-script-witness chain has ${chain.steps.length} steps; expected ${UNUSED_SCRIPT_WITNESS_ROLE_ORDER.length}`,
    );
  return Object.freeze(
    Object.fromEntries(
      UNUSED_SCRIPT_WITNESS_ROLE_ORDER.map((role, index) => [
        role,
        chain.steps[index]!,
      ]),
    ) as Record<UnusedScriptWitnessRole, SpendingValidator>,
  );
};

export type UnusedScriptWitnessFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fieldPreimageCertificate: MintingValidator;
  readonly unusedScriptWitness: UnusedScriptWitnessChain;
};

export type BuildUnusedScriptWitnessFaultProofContractsParams =
  BuildFaultProofContractsParams;

/** Builds the shared bind and source graph, the family entry that opens the
 * shared all-purposes walk with the witness hash as its probe, the field-6
 * witness walk and the terminal decision. */
export const buildUnusedScriptWitnessChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
}: CertifiedFaultProofChainInputs): Effect.Effect<
  UnusedScriptWitnessFaultProofContracts["unusedScriptWitness"],
  Error
> =>
  Effect.gen(function* () {
    const context = { blueprint, network };
    const computationThreadPolicyId = computationThread.policyId;
    const certified = [
      computationThreadPolicyId,
      fieldPreimageCertificatePolicyId,
    ] as const;
    const terminal = yield* buildFaultProofSpendingStep(
      context,
      UNUSED_SCRIPT_WITNESS_FAULT_PROOF_TITLES.terminal,
      [
        fraudProof.policyId,
        fraudProofTokenAddressData,
        computationThreadPolicyId,
      ],
      "Failed to build unused-script-witness terminal",
    );
    const witnessScan = yield* buildFaultProofSpendingStep(
      context,
      UNUSED_SCRIPT_WITNESS_FAULT_PROOF_TITLES.witnessScan,
      [terminal.spendingScriptHash, ...certified],
      "Failed to build unused-script-witness witness scan",
    );
    const witnessGrammar = yield* buildFaultProofSpendingStep(
      context,
      UNUSED_SCRIPT_WITNESS_FAULT_PROOF_TITLES.witnessGrammar,
      [witnessScan.spendingScriptHash, ...certified],
      "Failed to build unused-script-witness witness grammar",
    );
    const witnessEntry = yield* buildFaultProofSpendingStep(
      context,
      UNUSED_SCRIPT_WITNESS_FAULT_PROOF_TITLES.witnessEntry,
      [witnessGrammar.spendingScriptHash, ...certified],
      "Failed to build unused-script-witness witness entry",
    );
    const walk = yield* buildDirectAllPurposesContracts({
      blueprint,
      network,
      computationThreadPolicyId,
      fieldPreimageCertificatePolicyId,
      nextStepScriptHash: witnessEntry.spendingScriptHash,
    });
    const familyEntry = yield* buildFaultProofSpendingStep(
      context,
      UNUSED_SCRIPT_WITNESS_FAULT_PROOF_TITLES.familyEntry,
      [
        walk.spend.spendingScriptHash,
        walk.mintGrammar.spendingScriptHash,
        ...certified,
      ],
      "Failed to build unused-script-witness family entry",
    );
    const source = yield* buildDirectSourceContracts({
      blueprint,
      network,
      computationThreadPolicyId,
      fieldPreimageCertificatePolicyId,
      nextStepScriptHash: familyEntry.spendingScriptHash,
    });
    const binding = yield* buildDirectBindingContracts({
      blueprint,
      network,
      computationThreadPolicyId,
      nextStepScriptHash: source.start.spendingScriptHash,
    });
    const step01 = yield* buildFaultProofSpendingStep(
      context,
      UNUSED_SCRIPT_WITNESS_FAULT_PROOF_TITLES.step01,
      [
        binding.event.spendingScriptHash,
        computationThreadPolicyId,
        hubOraclePolicyId,
      ],
      "Failed to build unused-script-witness step 01",
    );
    const steps: [SpendingValidator, ...SpendingValidator[]] = [
      step01,
      binding.event,
      binding.preState,
      source.start,
      source.resolved,
      source.authenticate,
      source.map,
      source.inputFinish,
      source.grammar,
      source.witnesses,
      source.finish,
      familyEntry,
      walk.spend,
      walk.spendResolve,
      walk.mintGrammar,
      walk.mint,
      walk.observeOpen,
      walk.observe,
      walk.receiveGrammar,
      walk.receive,
      witnessEntry,
      witnessGrammar,
      witnessScan,
      terminal,
    ];
    const chain = { firstStep: step01, steps };
    // Fails closed when this order, the role names and the manifest drift apart.
    unusedScriptWitnessRoles(chain);
    return chain;
  });

export const buildUnusedScriptWitnessFaultProofContracts = (
  params: BuildUnusedScriptWitnessFaultProofContractsParams,
): Effect.Effect<UnusedScriptWitnessFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const unusedScriptWitness = yield* buildUnusedScriptWitnessChain({
      ...params,
      ...shared,
    });
    return { ...shared, unusedScriptWitness };
  });
