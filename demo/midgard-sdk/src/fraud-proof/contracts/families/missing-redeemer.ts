import { deploymentValidatorTitle as deployedTitle } from "@al-ft/midgard-core/deployment-manifest-identity";
import { Effect } from "effect";

import {
  AuthenticatedValidator,
  MintingValidator,
  SpendingValidator,
} from "../../../common.js";
import { buildDirectBindingContracts } from "../direct-binding.js";
import { buildDirectPurposeContracts } from "../direct-purpose.js";
import { buildDirectSourceContracts } from "../direct-source.js";
import {
  buildMissingRedeemerClaimContracts,
  buildMissingRedeemerFamilyEntry,
} from "../missing-redeemer-claim.js";
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
  step05: deployedTitle("fraudProofMissingRedeemerStep05"),
} as const;

/** Spending roles in deployment step order. Off-chain code reads named roles
 * through `missingRedeemerRoles`, so a manifest-restored chain carries them. */
export const MISSING_REDEEMER_ROLE_ORDER = [
  "step01",
  "bindEvent",
  "bindPreState",
  "sourceStart",
  "sourceResolved",
  "sourceAuthenticate",
  "sourceMap",
  "sourceInputFinish",
  "sourceGrammar",
  "sourceWitnesses",
  "sourceFinish",
  "familyEntry",
  "purposeSelect",
  "spendSelect",
  "spendScan",
  "mintGrammar",
  "mintScan",
  "receiveStart",
  "receiveGrammar",
  "receiveOutputs",
  "receiveFinish",
  "receiveDeclaration",
  "sourceLookup",
  "observedStart",
  "observedGrammar",
  "observedScan",
  "claimStart",
  "claimSpendStart",
  "claimSpendScan",
  "claimMintGrammar",
  "claimMintScan",
  "claimReceiveGrammar",
  "claimReceiveOutputs",
  "claimReceiveFinish",
  "claimReceiveDeclaration",
  "claimAdjudicate",
  "claimedRedeemerGrammar",
  "claimedRedeemerScan",
  "terminal",
] as const;

export type MissingRedeemerRole = (typeof MISSING_REDEEMER_ROLE_ORDER)[number];

export type MissingRedeemerChain = FraudProofChain;

export type MissingRedeemerRoles = Readonly<
  Record<MissingRedeemerRole, SpendingValidator>
>;

/** Named roles of a built or manifest-restored chain. */
export const missingRedeemerRoles = (
  chain: FraudProofChain,
): MissingRedeemerRoles => {
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
    const terminal = yield* buildFaultProofSpendingStep(
      context,
      MISSING_REDEEMER_FAULT_PROOF_TITLES.step05,
      [
        fraudProof.policyId,
        fraudProofTokenAddressData,
        computationThreadPolicyId,
      ],
      "Failed to build missing-redeemer step 05",
    );
    const claim = yield* buildMissingRedeemerClaimContracts({
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
    const familyEntry = yield* buildMissingRedeemerFamilyEntry({
      blueprint,
      network,
      computationThreadPolicyId,
      purposeSelectScriptHash: purpose.select.spendingScriptHash,
      claimStartScriptHash: claim.start.spendingScriptHash,
    });
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
    // Fails closed when the order above and the role names drift apart.
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
