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
import { buildDirectPurposeContracts } from "../direct-purpose.js";
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

export const MISSING_REDEEMER_FAULT_PROOF_TITLES = {
  step01: deployedTitle("fraudProofMissingRedeemer"),
  step02: deployedTitle("fraudProofMissingRedeemerStep02"),
  step03: deployedTitle("fraudProofMissingRedeemerStep03"),
  step04: deployedTitle("fraudProofMissingRedeemerStep04"),
  familyEntry: deployedTitle("fraudProofMissingRedeemerFamilyEntry"),
} as const;

/** Each spending role bound once to its deployment member, in deployment step
 * order. Off-chain code reads named roles through `missingRedeemerRoles`, so a
 * manifest-restored chain carries them. */
const MISSING_REDEEMER_ROLE_MEMBERS = [
  ["step01", "fraudProofMissingRedeemer"],
  ["bindEvent", "fraudProofMissingRedeemerBindEvent"],
  ["bindPreState", "fraudProofMissingRedeemerBindPreState"],
  ["sourceStart", "fraudProofMissingRedeemerSourceStart"],
  ["sourceResolved", "fraudProofMissingRedeemerSourceResolved"],
  ["sourceAuthenticate", "fraudProofMissingRedeemerSourceAuthenticate"],
  ["sourceMap", "fraudProofMissingRedeemerSourceMap"],
  ["sourceInputFinish", "fraudProofMissingRedeemerSourceInputFinish"],
  ["sourceGrammar", "fraudProofMissingRedeemerSourceGrammar"],
  ["sourceWitnesses", "fraudProofMissingRedeemerSourceWitnesses"],
  ["sourceFinish", "fraudProofMissingRedeemerSourceFinish"],
  ["familyEntry", "fraudProofMissingRedeemerFamilyEntry"],
  ["purposeSelect", "fraudProofMissingRedeemerPurposeSelect"],
  ["spendSelect", "fraudProofMissingRedeemerSpendSelect"],
  ["spendScan", "fraudProofMissingRedeemerSpendScan"],
  ["mintGrammar", "fraudProofMissingRedeemerMintGrammar"],
  ["mintScan", "fraudProofMissingRedeemerMintScan"],
  ["receiveStart", "fraudProofMissingRedeemerReceiveStart"],
  ["receiveGrammar", "fraudProofMissingRedeemerReceiveGrammar"],
  ["receiveOutputs", "fraudProofMissingRedeemerReceiveOutputs"],
  ["receiveFinish", "fraudProofMissingRedeemerReceiveFinish"],
  ["receiveDeclaration", "fraudProofMissingRedeemerReceiveDeclaration"],
  ["sourceLookup", "fraudProofMissingRedeemerSourceLookup"],
  ["observedStart", "fraudProofMissingRedeemerStep02"],
  ["observedGrammar", "fraudProofMissingRedeemerStep03"],
  ["observedScan", "fraudProofMissingRedeemerStep04"],
  ["claimStart", "fraudProofMissingRedeemerClaimStart"],
  ["claimSpendStart", "fraudProofMissingRedeemerClaimSpendStart"],
  ["claimSpendScan", "fraudProofMissingRedeemerClaimSpendScan"],
  ["claimMintGrammar", "fraudProofMissingRedeemerClaimMintGrammar"],
  ["claimMintScan", "fraudProofMissingRedeemerClaimMintScan"],
  ["claimReceiveGrammar", "fraudProofMissingRedeemerClaimReceiveGrammar"],
  ["claimReceiveOutputs", "fraudProofMissingRedeemerClaimReceiveOutputs"],
  ["claimReceiveFinish", "fraudProofMissingRedeemerClaimReceiveFinish"],
  [
    "claimReceiveDeclaration",
    "fraudProofMissingRedeemerClaimReceiveDeclaration",
  ],
  ["claimAdjudicate", "fraudProofMissingRedeemerClaimAdjudicate"],
  ["claimedRedeemerGrammar", "fraudProofMissingRedeemerClaimedRedeemerGrammar"],
  ["claimedRedeemerScan", "fraudProofMissingRedeemerClaimedRedeemerScan"],
  ["terminal", "fraudProofMissingRedeemerStep05"],
] as const satisfies readonly (readonly [string, DeploymentContract])[];

export type MissingRedeemerRole =
  (typeof MISSING_REDEEMER_ROLE_MEMBERS)[number][0];

export const MISSING_REDEEMER_ROLE_ORDER: readonly MissingRedeemerRole[] =
  Object.freeze(MISSING_REDEEMER_ROLE_MEMBERS.map(([role]) => role));

/** Refuses, by role name, a role whose member the given manifest step order
 * places at another step of the `missingRedeemer` family. */
export const checkMissingRedeemerRoleOrder = (
  steps: readonly DeploymentContract[],
): void => {
  if (steps.length !== MISSING_REDEEMER_ROLE_MEMBERS.length)
    throw new Error(
      `missing-redeemer has ${MISSING_REDEEMER_ROLE_MEMBERS.length} roles; the manifest has ${steps.length} steps`,
    );
  MISSING_REDEEMER_ROLE_MEMBERS.forEach(([role, member], index) => {
    if (steps[index] !== member)
      throw new Error(
        `missing-redeemer role ${role} is ${member}, but manifest step ${index} is ${String(steps[index])}`,
      );
  });
};

export type MissingRedeemerChain = FraudProofChain;

export type MissingRedeemerRoles = Readonly<
  Record<MissingRedeemerRole, SpendingValidator>
>;

/** Named roles of a built or manifest-restored chain. */
export const missingRedeemerRoles = (
  chain: FraudProofChain,
): MissingRedeemerRoles => {
  checkMissingRedeemerRoleOrder(deploymentFamilyStepNames("missingRedeemer"));
  if (chain.steps.length !== MISSING_REDEEMER_ROLE_ORDER.length)
    throw new Error(
      `missing-redeemer chain has ${chain.steps.length} steps; expected ${MISSING_REDEEMER_ROLE_ORDER.length}`,
    );
  return Object.freeze(
    Object.fromEntries(
      MISSING_REDEEMER_ROLE_ORDER.map((role, index) => [
        role,
        chain.steps[index]!,
      ]),
    ) as Record<MissingRedeemerRole, SpendingValidator>,
  );
};

export type MissingRedeemerFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fieldPreimageCertificate: MintingValidator;
  readonly missingRedeemer: MissingRedeemerChain;
};

export type BuildMissingRedeemerFaultProofContractsParams =
  BuildFaultProofContractsParams;

/** Builds the shared direct-derivation graph in place, with the family entry
 * after source exhaustion and claim custody ahead of the terminal. */
export const buildMissingRedeemerChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
}: CertifiedFaultProofChainInputs): Effect.Effect<
  MissingRedeemerFaultProofContracts["missingRedeemer"],
  Error
> =>
  Effect.gen(function* () {
    const context = { blueprint, network };
    const computationThreadPolicyId = computationThread.policyId;
    const certified = [
      computationThreadPolicyId,
      fieldPreimageCertificatePolicyId,
    ];
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
    const scan = yield* buildFaultProofSpendingStep(
      context,
      MISSING_REDEEMER_FAULT_PROOF_TITLES.step04,
      [claim.start.spendingScriptHash, ...certified],
      "Failed to build missing-redeemer step 04",
    );
    const grammar = yield* buildFaultProofSpendingStep(
      context,
      MISSING_REDEEMER_FAULT_PROOF_TITLES.step03,
      [scan.spendingScriptHash, ...certified],
      "Failed to build missing-redeemer step 03",
    );
    const start = yield* buildFaultProofSpendingStep(
      context,
      MISSING_REDEEMER_FAULT_PROOF_TITLES.step02,
      [grammar.spendingScriptHash, ...certified],
      "Failed to build missing-redeemer step 02",
    );
    const purpose = yield* buildDirectPurposeContracts({
      blueprint,
      network,
      computationThreadPolicyId,
      fieldPreimageCertificatePolicyId,
      nextStepScriptHash: start.spendingScriptHash,
    });
    const familyEntry = yield* buildFaultProofSpendingStep(
      context,
      MISSING_REDEEMER_FAULT_PROOF_TITLES.familyEntry,
      [
        purpose.select.spendingScriptHash,
        claim.start.spendingScriptHash,
        computationThreadPolicyId,
      ],
      "Failed to build missing-redeemer family entry",
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
      MISSING_REDEEMER_FAULT_PROOF_TITLES.step01,
      [
        binding.event.spendingScriptHash,
        computationThreadPolicyId,
        hubOraclePolicyId,
      ],
      "Failed to build missing-redeemer step 01",
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
      purpose.select,
      purpose.spendSelect,
      purpose.spendScan,
      purpose.mintGrammar,
      purpose.mintScan,
      purpose.receive.start,
      purpose.receive.grammar,
      purpose.receive.outputs,
      purpose.receive.finish,
      purpose.receive.declaration,
      purpose.lookup,
      start,
      grammar,
      scan,
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
      claim.claimedGrammar,
      claim.claimedScan,
      terminal,
    ];
    const chain = { firstStep: step01, steps };
    // Fails closed when this order, the role names and the manifest drift apart.
    missingRedeemerRoles(chain);
    return chain;
  });

export const buildMissingRedeemerFaultProofContracts = (
  params: BuildMissingRedeemerFaultProofContractsParams,
): Effect.Effect<MissingRedeemerFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const missingRedeemer = yield* buildMissingRedeemerChain({
      ...params,
      ...shared,
    });
    return { ...shared, missingRedeemer };
  });
