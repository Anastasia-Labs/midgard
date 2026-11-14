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

export const MISSING_SCRIPT_SOURCE_FAULT_PROOF_TITLES = {
  step01: deployedTitle("fraudProofMissingScriptSource"),
  familyEntry: deployedTitle("fraudProofMissingScriptSourceFamilyEntry"),
  observedDecision: deployedTitle(
    "fraudProofMissingScriptSourceObservedDecision",
  ),
} as const;

/** Each spending role bound once to its deployment member, in deployment step
 * order. Off-chain code reads named roles through `missingScriptSourceRoles`,
 * so a manifest-restored chain carries them. */
const MISSING_SCRIPT_SOURCE_ROLE_MEMBERS = [
  ["step01", "fraudProofMissingScriptSource"],
  ["bindEvent", "fraudProofMissingScriptSourceBindEvent"],
  ["bindPreState", "fraudProofMissingScriptSourceBindPreState"],
  ["sourceStart", "fraudProofMissingScriptSourceSourceStart"],
  ["sourceResolved", "fraudProofMissingScriptSourceSourceResolved"],
  ["sourceAuthenticate", "fraudProofMissingScriptSourceSourceAuthenticate"],
  ["sourceMap", "fraudProofMissingScriptSourceSourceMap"],
  ["sourceInputFinish", "fraudProofMissingScriptSourceSourceInputFinish"],
  ["sourceGrammar", "fraudProofMissingScriptSourceSourceGrammar"],
  ["sourceWitnesses", "fraudProofMissingScriptSourceSourceWitnesses"],
  ["sourceFinish", "fraudProofMissingScriptSourceSourceFinish"],
  ["familyEntry", "fraudProofMissingScriptSourceFamilyEntry"],
  ["purposeSelect", "fraudProofMissingScriptSourcePurposeSelect"],
  ["spendSelect", "fraudProofMissingScriptSourceSpendSelect"],
  ["spendScan", "fraudProofMissingScriptSourceSpendScan"],
  ["mintGrammar", "fraudProofMissingScriptSourceMintGrammar"],
  ["mintScan", "fraudProofMissingScriptSourceMintScan"],
  ["receiveStart", "fraudProofMissingScriptSourceReceiveStart"],
  ["receiveGrammar", "fraudProofMissingScriptSourceReceiveGrammar"],
  ["receiveOutputs", "fraudProofMissingScriptSourceReceiveOutputs"],
  ["receiveFinish", "fraudProofMissingScriptSourceReceiveFinish"],
  ["receiveDeclaration", "fraudProofMissingScriptSourceReceiveDeclaration"],
  ["sourceLookup", "fraudProofMissingScriptSourceSourceLookup"],
  ["observedDecision", "fraudProofMissingScriptSourceObservedDecision"],
  ["claimStart", "fraudProofMissingScriptSourceClaimStart"],
  ["claimSpendStart", "fraudProofMissingScriptSourceClaimSpendStart"],
  ["claimSpendScan", "fraudProofMissingScriptSourceClaimSpendScan"],
  ["claimMintGrammar", "fraudProofMissingScriptSourceClaimMintGrammar"],
  ["claimMintScan", "fraudProofMissingScriptSourceClaimMintScan"],
  ["claimReceiveGrammar", "fraudProofMissingScriptSourceClaimReceiveGrammar"],
  ["claimReceiveOutputs", "fraudProofMissingScriptSourceClaimReceiveOutputs"],
  ["claimReceiveFinish", "fraudProofMissingScriptSourceClaimReceiveFinish"],
  [
    "claimReceiveDeclaration",
    "fraudProofMissingScriptSourceClaimReceiveDeclaration",
  ],
  ["claimAdjudicate", "fraudProofMissingScriptSourceClaimAdjudicate"],
  [
    "claimedRedeemerGrammar",
    "fraudProofMissingScriptSourceClaimedRedeemerGrammar",
  ],
  ["claimedRedeemerScan", "fraudProofMissingScriptSourceClaimedRedeemerScan"],
  ["terminal", "fraudProofMissingScriptSourceTerminal"],
] as const satisfies readonly (readonly [string, DeploymentContract])[];

export type MissingScriptSourceRole =
  (typeof MISSING_SCRIPT_SOURCE_ROLE_MEMBERS)[number][0];

export const MISSING_SCRIPT_SOURCE_ROLE_ORDER: readonly MissingScriptSourceRole[] =
  Object.freeze(MISSING_SCRIPT_SOURCE_ROLE_MEMBERS.map(([role]) => role));

/** Refuses, by role name, a role whose member the given manifest step order
 * places at another step of the `missingScriptSource` family. */
export const checkMissingScriptSourceRoleOrder = (
  steps: readonly DeploymentContract[],
): void => {
  if (steps.length !== MISSING_SCRIPT_SOURCE_ROLE_MEMBERS.length)
    throw new Error(
      `missing-script-source has ${MISSING_SCRIPT_SOURCE_ROLE_MEMBERS.length} roles; the manifest has ${steps.length} steps`,
    );
  MISSING_SCRIPT_SOURCE_ROLE_MEMBERS.forEach(([role, member], index) => {
    if (steps[index] !== member)
      throw new Error(
        `missing-script-source role ${role} is ${member}, but manifest step ${index} is ${String(steps[index])}`,
      );
  });
};

export type MissingScriptSourceChain = FraudProofChain;

export type MissingScriptSourceRoles = Readonly<
  Record<MissingScriptSourceRole, SpendingValidator>
>;

/** Named roles of a built or manifest-restored chain. */
export const missingScriptSourceRoles = (
  chain: FraudProofChain,
): MissingScriptSourceRoles => {
  checkMissingScriptSourceRoleOrder(
    deploymentFamilyStepNames("missingScriptSource"),
  );
  if (chain.steps.length !== MISSING_SCRIPT_SOURCE_ROLE_ORDER.length)
    throw new Error(
      `missing-script-source chain has ${chain.steps.length} steps; expected ${MISSING_SCRIPT_SOURCE_ROLE_ORDER.length}`,
    );
  return Object.freeze(
    Object.fromEntries(
      MISSING_SCRIPT_SOURCE_ROLE_ORDER.map((role, index) => [
        role,
        chain.steps[index]!,
      ]),
    ) as Record<MissingScriptSourceRole, SpendingValidator>,
  );
};

export type MissingScriptSourceFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fieldPreimageCertificate: MintingValidator;
  readonly missingScriptSource: MissingScriptSourceChain;
};

export type BuildMissingScriptSourceFaultProofContractsParams =
  BuildFaultProofContractsParams;

/** Builds the shared direct-derivation graph in place, with the family entry
 * after source exhaustion, the observed missing-source decision after the
 * source lookup, and claim custody ahead of the terminal. */
export const buildMissingScriptSourceChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
}: CertifiedFaultProofChainInputs): Effect.Effect<
  MissingScriptSourceFaultProofContracts["missingScriptSource"],
  Error
> =>
  Effect.gen(function* () {
    const context = { blueprint, network };
    const computationThreadPolicyId = computationThread.policyId;
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
    const observedDecision = yield* buildFaultProofSpendingStep(
      context,
      MISSING_SCRIPT_SOURCE_FAULT_PROOF_TITLES.observedDecision,
      [claim.start.spendingScriptHash, computationThreadPolicyId],
      "Failed to build missing-script-source observed decision",
    );
    const purpose = yield* buildDirectPurposeContracts({
      blueprint,
      network,
      computationThreadPolicyId,
      fieldPreimageCertificatePolicyId,
      nextStepScriptHash: observedDecision.spendingScriptHash,
    });
    const familyEntry = yield* buildFaultProofSpendingStep(
      context,
      MISSING_SCRIPT_SOURCE_FAULT_PROOF_TITLES.familyEntry,
      [
        purpose.select.spendingScriptHash,
        claim.start.spendingScriptHash,
        computationThreadPolicyId,
      ],
      "Failed to build missing-script-source family entry",
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
      MISSING_SCRIPT_SOURCE_FAULT_PROOF_TITLES.step01,
      [
        binding.event.spendingScriptHash,
        computationThreadPolicyId,
        hubOraclePolicyId,
      ],
      "Failed to build missing-script-source step 01",
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
      observedDecision,
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
    missingScriptSourceRoles(chain);
    return chain;
  });

export const buildMissingScriptSourceFaultProofContracts = (
  params: BuildMissingScriptSourceFaultProofContractsParams,
): Effect.Effect<MissingScriptSourceFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const missingScriptSource = yield* buildMissingScriptSourceChain({
      ...params,
      ...shared,
    });
    return { ...shared, missingScriptSource };
  });
