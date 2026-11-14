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
import { DIRECT_CLAIM_TITLES } from "../direct-claim.js";
import { buildDirectReceiveContracts } from "../direct-receive.js";
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

export const RECEIVE_PURPOSE_LANGUAGE_FAULT_PROOF_TITLES = {
  step01: deployedTitle("fraudProofReceivePurposeLanguage"),
  familyEntry: deployedTitle("fraudProofReceivePurposeLanguageFamilyEntry"),
  sourceLookup: deployedTitle("fraudProofReceivePurposeLanguageSourceLookup"),
  terminal: deployedTitle("fraudProofReceivePurposeLanguageTerminal"),
} as const;

/** Each spending role bound once to its deployment member, in deployment step
 * order. Off-chain code reads named roles through
 * `receivePurposeLanguageRoles`, so a manifest-restored chain carries them.
 * The family entry hands the binding to the shared receive route; its
 * declaration hands the selection to the family's lookup, which either
 * decides the fact or opens the shared claimed field-8 scan. */
const RECEIVE_PURPOSE_LANGUAGE_ROLE_MEMBERS = [
  ["step01", "fraudProofReceivePurposeLanguage"],
  ["bindEvent", "fraudProofReceivePurposeLanguageBindEvent"],
  ["bindPreState", "fraudProofReceivePurposeLanguageBindPreState"],
  ["sourceStart", "fraudProofReceivePurposeLanguageSourceStart"],
  ["sourceResolved", "fraudProofReceivePurposeLanguageSourceResolved"],
  ["sourceAuthenticate", "fraudProofReceivePurposeLanguageSourceAuthenticate"],
  ["sourceMap", "fraudProofReceivePurposeLanguageSourceMap"],
  ["sourceInputFinish", "fraudProofReceivePurposeLanguageSourceInputFinish"],
  ["sourceGrammar", "fraudProofReceivePurposeLanguageSourceGrammar"],
  ["sourceWitnesses", "fraudProofReceivePurposeLanguageSourceWitnesses"],
  ["sourceFinish", "fraudProofReceivePurposeLanguageSourceFinish"],
  ["familyEntry", "fraudProofReceivePurposeLanguageFamilyEntry"],
  ["receiveStart", "fraudProofReceivePurposeLanguageReceiveStart"],
  ["receiveGrammar", "fraudProofReceivePurposeLanguageReceiveGrammar"],
  ["receiveOutputs", "fraudProofReceivePurposeLanguageReceiveOutputs"],
  ["receiveFinish", "fraudProofReceivePurposeLanguageReceiveFinish"],
  ["receiveDeclaration", "fraudProofReceivePurposeLanguageReceiveDeclaration"],
  ["sourceLookup", "fraudProofReceivePurposeLanguageSourceLookup"],
  [
    "claimedRedeemerGrammar",
    "fraudProofReceivePurposeLanguageClaimedRedeemerGrammar",
  ],
  [
    "claimedRedeemerScan",
    "fraudProofReceivePurposeLanguageClaimedRedeemerScan",
  ],
  ["terminal", "fraudProofReceivePurposeLanguageTerminal"],
] as const satisfies readonly (readonly [string, DeploymentContract])[];

export type ReceivePurposeLanguageRole =
  (typeof RECEIVE_PURPOSE_LANGUAGE_ROLE_MEMBERS)[number][0];

export const RECEIVE_PURPOSE_LANGUAGE_ROLE_ORDER: readonly ReceivePurposeLanguageRole[] =
  Object.freeze(RECEIVE_PURPOSE_LANGUAGE_ROLE_MEMBERS.map(([role]) => role));

/** Refuses, by role name, a role whose member the given manifest step order
 * places at another step of the `receivePurposeLanguage` family. */
export const checkReceivePurposeLanguageRoleOrder = (
  steps: readonly DeploymentContract[],
): void => {
  if (steps.length !== RECEIVE_PURPOSE_LANGUAGE_ROLE_MEMBERS.length)
    throw new Error(
      `receive-purpose-language has ${RECEIVE_PURPOSE_LANGUAGE_ROLE_MEMBERS.length} roles; the manifest has ${steps.length} steps`,
    );
  RECEIVE_PURPOSE_LANGUAGE_ROLE_MEMBERS.forEach(([role, member], index) => {
    if (steps[index] !== member)
      throw new Error(
        `receive-purpose-language role ${role} is ${member}, but manifest step ${index} is ${String(steps[index])}`,
      );
  });
};

export type ReceivePurposeLanguageChain = FraudProofChain;

export type ReceivePurposeLanguageRoles = Readonly<
  Record<ReceivePurposeLanguageRole, SpendingValidator>
>;

/** Named roles of a built or manifest-restored chain. */
export const receivePurposeLanguageRoles = (
  chain: FraudProofChain,
): ReceivePurposeLanguageRoles => {
  checkReceivePurposeLanguageRoleOrder(
    deploymentFamilyStepNames("receivePurposeLanguage"),
  );
  if (chain.steps.length !== RECEIVE_PURPOSE_LANGUAGE_ROLE_ORDER.length)
    throw new Error(
      `receive-purpose-language chain has ${chain.steps.length} steps; expected ${RECEIVE_PURPOSE_LANGUAGE_ROLE_ORDER.length}`,
    );
  return Object.freeze(
    Object.fromEntries(
      RECEIVE_PURPOSE_LANGUAGE_ROLE_ORDER.map((role, index) => [
        role,
        chain.steps[index]!,
      ]),
    ) as Record<ReceivePurposeLanguageRole, SpendingValidator>,
  );
};

export type ReceivePurposeLanguageFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fieldPreimageCertificate: MintingValidator;
  readonly receivePurposeLanguage: ReceivePurposeLanguageChain;
};

export type BuildReceivePurposeLanguageFaultProofContractsParams =
  BuildFaultProofContractsParams;

/** Builds the family from the terminal backwards: the shared claimed field-8
 * scan, the family lookup, the shared receive route, the family entry, and
 * the shared source walk and binding ahead of step-01. */
export const buildReceivePurposeLanguageChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
}: CertifiedFaultProofChainInputs): Effect.Effect<
  ReceivePurposeLanguageFaultProofContracts["receivePurposeLanguage"],
  Error
> =>
  Effect.gen(function* () {
    const context = { blueprint, network };
    const computationThreadPolicyId = computationThread.policyId;
    const certified = [
      computationThreadPolicyId,
      fieldPreimageCertificatePolicyId,
    ] as const;
    const T = RECEIVE_PURPOSE_LANGUAGE_FAULT_PROOF_TITLES;
    const terminal = yield* buildFaultProofSpendingStep(
      context,
      T.terminal,
      [
        fraudProof.policyId,
        fraudProofTokenAddressData,
        computationThreadPolicyId,
      ],
      "Failed to build receive-purpose-language terminal",
    );
    const claimedScan = yield* buildFaultProofSpendingStep(
      context,
      DIRECT_CLAIM_TITLES.claimedScan,
      [terminal.spendingScriptHash, ...certified],
      "Failed to build receive-purpose-language claimed redeemer scan",
    );
    const claimedGrammar = yield* buildFaultProofSpendingStep(
      context,
      DIRECT_CLAIM_TITLES.claimedGrammar,
      [claimedScan.spendingScriptHash, ...certified],
      "Failed to build receive-purpose-language claimed redeemer grammar",
    );
    const lookup = yield* buildFaultProofSpendingStep(
      context,
      T.sourceLookup,
      [
        terminal.spendingScriptHash,
        claimedGrammar.spendingScriptHash,
        ...certified,
      ],
      "Failed to build receive-purpose-language lookup",
    );
    const receive = yield* buildDirectReceiveContracts({
      blueprint,
      network,
      computationThreadPolicyId,
      fieldPreimageCertificatePolicyId,
      nextStepScriptHash: lookup.spendingScriptHash,
    });
    const familyEntry = yield* buildFaultProofSpendingStep(
      context,
      T.familyEntry,
      [
        receive.start.spendingScriptHash,
        terminal.spendingScriptHash,
        computationThreadPolicyId,
      ],
      "Failed to build receive-purpose-language family entry",
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
      T.step01,
      [
        binding.event.spendingScriptHash,
        computationThreadPolicyId,
        hubOraclePolicyId,
      ],
      "Failed to build receive-purpose-language step 01",
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
      receive.start,
      receive.grammar,
      receive.outputs,
      receive.finish,
      receive.declaration,
      lookup,
      claimedGrammar,
      claimedScan,
      terminal,
    ];
    const chain = { firstStep: step01, steps };
    // Fails closed when this order, the role names and the manifest drift apart.
    receivePurposeLanguageRoles(chain);
    return chain;
  });

export const buildReceivePurposeLanguageFaultProofContracts = (
  params: BuildReceivePurposeLanguageFaultProofContractsParams,
): Effect.Effect<ReceivePurposeLanguageFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const receivePurposeLanguage = yield* buildReceivePurposeLanguageChain({
      ...params,
      ...shared,
    });
    return { ...shared, receivePurposeLanguage };
  });
