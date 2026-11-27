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
import { buildDirectBindingContracts } from "../direct-binding.js";
import {
  buildDirectClaimContracts,
  buildDirectClaimTerminal,
} from "../direct-claim.js";
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

export const UNUSED_REDEEMER_FAULT_PROOF_TITLES = {
  step01: deployedTitle("fraudProofUnusedRedeemer"),
  familyEntry: deployedTitle("fraudProofUnusedRedeemerFamilyEntry"),
  redeemerGrammar: deployedTitle("fraudProofUnusedRedeemerRedeemerGrammar"),
  redeemerScan: deployedTitle("fraudProofUnusedRedeemerRedeemerScan"),
} as const;

/** Each spending role bound once to its deployment member, in deployment step
 * order. Off-chain code reads named roles through `unusedRedeemerRoles`, so a
 * manifest-restored chain carries them. The walk hands claim custody a
 * redeemer pointer, which adjudication resolves by source lookup alone, so
 * the claimed field-8 scan roles are not part of this graph. */
const UNUSED_REDEEMER_ROLE_MEMBERS = [
  ["step01", "fraudProofUnusedRedeemer"],
  ["bindEvent", "fraudProofUnusedRedeemerBindEvent"],
  ["bindPreState", "fraudProofUnusedRedeemerBindPreState"],
  ["sourceStart", "fraudProofUnusedRedeemerSourceStart"],
  ["sourceResolved", "fraudProofUnusedRedeemerSourceResolved"],
  ["sourceAuthenticate", "fraudProofUnusedRedeemerSourceAuthenticate"],
  ["sourceMap", "fraudProofUnusedRedeemerSourceMap"],
  ["sourceInputFinish", "fraudProofUnusedRedeemerSourceInputFinish"],
  ["sourceGrammar", "fraudProofUnusedRedeemerSourceGrammar"],
  ["sourceWitnesses", "fraudProofUnusedRedeemerSourceWitnesses"],
  ["sourceFinish", "fraudProofUnusedRedeemerSourceFinish"],
  ["familyEntry", "fraudProofUnusedRedeemerFamilyEntry"],
  ["redeemerGrammar", "fraudProofUnusedRedeemerRedeemerGrammar"],
  ["redeemerScan", "fraudProofUnusedRedeemerRedeemerScan"],
  ["claimStart", "fraudProofUnusedRedeemerClaimStart"],
  ["claimSpendStart", "fraudProofUnusedRedeemerClaimSpendStart"],
  ["claimSpendScan", "fraudProofUnusedRedeemerClaimSpendScan"],
  ["claimMintGrammar", "fraudProofUnusedRedeemerClaimMintGrammar"],
  ["claimMintScan", "fraudProofUnusedRedeemerClaimMintScan"],
  ["claimReceiveGrammar", "fraudProofUnusedRedeemerClaimReceiveGrammar"],
  ["claimReceiveOutputs", "fraudProofUnusedRedeemerClaimReceiveOutputs"],
  ["claimReceiveFinish", "fraudProofUnusedRedeemerClaimReceiveFinish"],
  [
    "claimReceiveDeclaration",
    "fraudProofUnusedRedeemerClaimReceiveDeclaration",
  ],
  ["claimAdjudicate", "fraudProofUnusedRedeemerClaimAdjudicate"],
  ["terminal", "fraudProofUnusedRedeemerTerminal"],
] as const satisfies readonly (readonly [string, DeploymentContract])[];

export type UnusedRedeemerRole =
  (typeof UNUSED_REDEEMER_ROLE_MEMBERS)[number][0];

export const UNUSED_REDEEMER_ROLE_ORDER: readonly UnusedRedeemerRole[] =
  Object.freeze(UNUSED_REDEEMER_ROLE_MEMBERS.map(([role]) => role));

/** Refuses, by role name, a role whose member the given manifest step order
 * places at another step of the `unusedRedeemer` family. */
export const checkUnusedRedeemerRoleOrder = (
  steps: readonly DeploymentContract[],
): void => {
  if (steps.length !== UNUSED_REDEEMER_ROLE_MEMBERS.length)
    throw new Error(
      `unused-redeemer has ${UNUSED_REDEEMER_ROLE_MEMBERS.length} roles; the manifest has ${steps.length} steps`,
    );
  UNUSED_REDEEMER_ROLE_MEMBERS.forEach(([role, member], index) => {
    if (steps[index] !== member)
      throw new Error(
        `unused-redeemer role ${role} is ${member}, but manifest step ${index} is ${String(steps[index])}`,
      );
  });
};

export type UnusedRedeemerChain = FraudProofChain;

export type UnusedRedeemerRoles = Readonly<
  Record<UnusedRedeemerRole, SpendingValidator>
>;

/** Named roles of a built or manifest-restored chain. */
export const unusedRedeemerRoles = (
  chain: FraudProofChain,
): UnusedRedeemerRoles => {
  checkUnusedRedeemerRoleOrder(deploymentFamilyStepNames("unusedRedeemer"));
  if (chain.steps.length !== UNUSED_REDEEMER_ROLE_ORDER.length)
    throw new Error(
      `unused-redeemer chain has ${chain.steps.length} steps; expected ${UNUSED_REDEEMER_ROLE_ORDER.length}`,
    );
  return Object.freeze(
    Object.fromEntries(
      UNUSED_REDEEMER_ROLE_ORDER.map((role, index) => [
        role,
        chain.steps[index]!,
      ]),
    ) as Record<UnusedRedeemerRole, SpendingValidator>,
  );
};

export type UnusedRedeemerFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fieldPreimageCertificate: MintingValidator;
  readonly unusedRedeemer: UnusedRedeemerChain;
};

export type BuildUnusedRedeemerFaultProofContractsParams =
  BuildFaultProofContractsParams;

/** Builds the shared direct-derivation binding and source walk, the family's
 * field-8 walk after source exhaustion, and the shared claim custody ahead of
 * the terminal. */
export const buildUnusedRedeemerChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
}: CertifiedFaultProofChainInputs): Effect.Effect<
  UnusedRedeemerFaultProofContracts["unusedRedeemer"],
  Error
> =>
  Effect.gen(function* () {
    const context = { blueprint, network };
    const computationThreadPolicyId = computationThread.policyId;
    const certified = [
      computationThreadPolicyId,
      fieldPreimageCertificatePolicyId,
    ] as const;
    const terminal = yield* buildDirectClaimTerminal({
      blueprint,
      network,
      fraudProofPolicyId: fraudProof.policyId,
      fraudProofTokenAddressData,
      computationThreadPolicyId,
    });
    const claim = yield* buildDirectClaimContracts({
      blueprint,
      network,
      computationThreadPolicyId,
      fieldPreimageCertificatePolicyId,
      terminalScriptHash: terminal.spendingScriptHash,
    });
    const redeemerScan = yield* buildFaultProofSpendingStep(
      context,
      UNUSED_REDEEMER_FAULT_PROOF_TITLES.redeemerScan,
      [
        claim.start.spendingScriptHash,
        terminal.spendingScriptHash,
        ...certified,
      ],
      "Failed to build unused-redeemer redeemer scan",
    );
    const redeemerGrammar = yield* buildFaultProofSpendingStep(
      context,
      UNUSED_REDEEMER_FAULT_PROOF_TITLES.redeemerGrammar,
      [redeemerScan.spendingScriptHash, ...certified],
      "Failed to build unused-redeemer redeemer grammar",
    );
    const familyEntry = yield* buildFaultProofSpendingStep(
      context,
      UNUSED_REDEEMER_FAULT_PROOF_TITLES.familyEntry,
      [redeemerGrammar.spendingScriptHash, ...certified],
      "Failed to build unused-redeemer family entry",
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
      UNUSED_REDEEMER_FAULT_PROOF_TITLES.step01,
      [
        binding.event.spendingScriptHash,
        computationThreadPolicyId,
        hubOraclePolicyId,
      ],
      "Failed to build unused-redeemer step 01",
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
      redeemerGrammar,
      redeemerScan,
      claim.start,
      claim.spendStart,
      claim.spendScan,
      claim.mintGrammar,
      claim.mintScan,
      claim.receiveGrammar,
      claim.receiveOutputs,
      claim.receiveFinish,
      claim.receiveDeclaration,
      claim.adjudicate,
      terminal,
    ];
    const chain = { firstStep: step01, steps };
    // Fails closed when this order, the role names and the manifest drift apart.
    unusedRedeemerRoles(chain);
    return chain;
  });

export const buildUnusedRedeemerFaultProofContracts = (
  params: BuildUnusedRedeemerFaultProofContractsParams,
): Effect.Effect<UnusedRedeemerFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const unusedRedeemer = yield* buildUnusedRedeemerChain({
      ...params,
      ...shared,
    });
    return { ...shared, unusedRedeemer };
  });
