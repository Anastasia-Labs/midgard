import * as SDK from "@al-ft/midgard-sdk";
import { validatorToScriptHash } from "@lucid-evolution/lucid";

import {
  CEK_CORE_STAGE_ORDER,
  entrySpec,
  MIN_ADA_YIELD_CONTRACT_NAMES,
  mint,
  type Section,
  spend,
  TRANSITION_TRACE_YIELD_CONTRACT_NAMES,
  VALIDATION_TRACE_CEK_MATERIAL_YIELD_KEYS,
  VALIDATION_TRACE_PHASE_A_SEMANTIC_PREFIXES,
  VALIDATION_TRACE_REDEEMER_NORMALIZATION_SEMANTIC_CONTRACT,
  VALIDATION_TRACE_REDEEMER_NORMALIZATION_SEMANTIC_INDEX,
  VALIDATION_TRACE_SCRIPT_SOURCES_SEMANTIC_PREFIXES,
  VALIDATION_TRACE_SCRIPT_SOURCES_YIELD_KEYS,
  withdraw,
} from "./deployable-scripts.cek-core-stage-order.js";
import { TRANSITION_TRACE_FINAL_CONTRACT_NAMES } from "./deployable-scripts.fault-proof-step-contract-names.js";
import {
  categoriesFrom,
  entries,
  legacyChainSteps,
  NOT_PUBLISHED,
  registeredChainSteps,
  semanticResolvers,
  spendStep,
  validationTraceYields,
  vtd,
  vtdControl,
  vtdControlPresent,
  withdrawStep,
} from "./deployable-scripts.legacy-chain-steps.js";
import { loadPhasMembershipWithdrawalScript } from "./phas-membership.js";

// ---------------------------------------------------------------------------
// The catalogue, in manifest order
// ---------------------------------------------------------------------------

export const DEPLOYABLE_SCRIPT_CATALOGUE = {
  referenceScriptAuth: entries(
    mint("referenceScriptAuthMint", (c) => c.referenceScriptAuth, {
      commands: ["protocol-init", "reference-script-auth"],
    }),
  ),
  hubOracle: entries(
    mint("hubOracleMint", (c) => c.hubOracle, {
      commands: ["protocol-init", "hub-oracle"],
    }),
  ),
  daParamsGovernor: entries(
    spend("daParamsGovernorSpend", (c) => c.daParamsGovernor, {
      commands: ["da"],
    }),
    mint("daParamsGovernorMint", (c) => c.daParamsGovernor, {
      commands: ["protocol-init", "da"],
    }),
  ),
  daBondPool: entries(
    spend("daBondPoolSpend", (c) => c.daBondPool, { commands: ["da"] }),
    mint("daBondPoolMint", (c) => c.daBondPool, {
      commands: ["protocol-init", "da"],
    }),
  ),
  daAttestation: entries(
    spend("daAttestationSpend", (c) => c.daAttestation, { commands: ["da"] }),
    mint("daAttestationMint", (c) => c.daAttestation, {
      commands: ["protocol-init", "da"],
    }),
  ),
  stateQueue: entries(
    spend("stateQueueSpend", (c) => c.stateQueue, {
      commands: ["state-queue"],
    }),
    mint("stateQueueMint", (c) => c.stateQueue, {
      commands: ["protocol-init", "state-queue"],
    }),
    withdraw("stateQueueCommitWithdraw", (c) => c.stateQueue.yields.commit, {
      commands: ["protocol-init", "state-queue"],
    }),
    withdraw(
      "stateQueueUnattestedTimeoutWithdraw",
      (c) => c.stateQueue.yields.unattestedTimeout,
      { commands: ["protocol-init", "state-queue"] },
    ),
    withdraw(
      "stateQueueUnavailableTimeoutWithdraw",
      (c) => c.stateQueue.yields.unavailableTimeout,
      { commands: ["protocol-init", "state-queue"] },
    ),
    withdraw(
      "stateQueueFraudRemovalWithdraw",
      (c) => c.stateQueue.yields.fraudRemoval,
      { commands: ["protocol-init", "state-queue"] },
    ),
    withdraw("stateQueueMergeWithdraw", (c) => c.stateQueue.yields.merge, {
      commands: ["protocol-init", "state-queue"],
    }),
  ),
  scheduler: entries(
    spend("schedulerSpend", (c) => c.scheduler, { commands: ["scheduler"] }),
    mint("schedulerMint", (c) => c.scheduler, {
      commands: ["protocol-init", "scheduler"],
    }),
  ),
  registeredOperators: entries(
    spend("registeredOperatorsSpend", (c) => c.registeredOperators, {
      commands: ["registered-operators"],
    }),
    mint("registeredOperatorsMint", (c) => c.registeredOperators, {
      commands: ["protocol-init", "registered-operators"],
    }),
  ),
  activeOperators: entries(
    spend("activeOperatorsSpend", (c) => c.activeOperators, {
      commands: ["active-operators"],
    }),
    mint("activeOperatorsMint", (c) => c.activeOperators, {
      commands: ["protocol-init", "active-operators"],
    }),
  ),
  retiredOperators: entries(
    spend("retiredOperatorsSpend", (c) => c.retiredOperators, {
      commands: ["retired-operators"],
    }),
    mint("retiredOperatorsMint", (c) => c.retiredOperators, {
      commands: ["protocol-init", "retired-operators"],
    }),
  ),
  escapeHatch: entries(
    spend("escapeHatchSpend", (c) => c.escapeHatch, NOT_PUBLISHED),
    mint("escapeHatchMint", (c) => c.escapeHatch, NOT_PUBLISHED),
  ),
  fraudProofCatalogue: entries(
    spend(
      "fraudProofCatalogueSpend",
      (c) => c.fraudProofCatalogue,
      NOT_PUBLISHED,
    ),
    mint("fraudProofCatalogueMint", (c) => c.fraudProofCatalogue, {
      commands: ["protocol-init"],
    }),
  ),
  fraudProofToken: entries(
    spend("fraudProofSpend", (c) => c.fraudProof, NOT_PUBLISHED),
    mint("fraudProofMint", (c) => c.fraudProof),
  ),
  depositHistory: entries(
    spend(
      "depositHistoryRetentionSpend",
      (c) => SDK.requireEventHistoryContracts(c).deposit.retention,
      { commands: ["deposit"] },
    ),
    withdraw(
      "depositHistoryRetirementWithdraw",
      (c) => SDK.requireEventHistoryContracts(c).deposit.retirement,
      { commands: ["deposit"] },
    ),
  ),
  withdrawalHistory: entries(
    spend(
      "withdrawalHistoryRetentionSpend",
      (c) => SDK.requireEventHistoryContracts(c).withdrawal.retention,
      { commands: ["withdrawal"] },
    ),
    withdraw(
      "withdrawalHistoryRetirementWithdraw",
      (c) => SDK.requireEventHistoryContracts(c).withdrawal.retirement,
      { commands: ["withdrawal"] },
    ),
  ),
  depositSpend: entries(
    spend("depositSpend", (c) => c.deposit, { commands: ["deposit"] }),
  ),
  depositMint: entries(
    mint("depositMint", (c) => c.deposit, { commands: ["deposit"] }),
  ),
  withdrawalSpend: entries(
    spend("withdrawalSpend", (c) => c.withdrawal, {
      commands: ["withdrawal"],
    }),
  ),
  withdrawalMint: entries(
    mint("withdrawalMint", (c) => c.withdrawal, { commands: ["withdrawal"] }),
  ),
  txOrder: entries(
    spend("txOrderSpend", (c) => c.txOrder, NOT_PUBLISHED),
    mint("txOrderMint", (c) => c.txOrder, NOT_PUBLISHED),
  ),
  fieldPreimageCertificate: entries(
    spend("fieldPreimageCertificateSpend", (c) => c.fieldPreimageCertificate),
    mint("fieldPreimageCertificateMint", (c) => c.fieldPreimageCertificate),
  ),
  cekProgramMaterial: entries(
    spend("cekProgramMaterialSpend", (c) => c.cekProgramMaterial, {
      // Under the always-succeeds contract set the CEK validator is the
      // tx-order spend script. Do not publish that stand-in as a distinct
      // deployed script.
      publishWhen: (c) =>
        c.cekProgramMaterial.spendingScriptHash !==
        c.txOrder.spendingScriptHash,
    }),
  ),
  settlement: entries(
    spend("settlementSpend", (c) => c.settlement, NOT_PUBLISHED),
    mint("settlementMint", (c) => c.settlement, {
      commands: ["settlement"],
    }),
  ),
  payout: entries(
    spend("payoutSpend", (c) => c.payout, { commands: ["payout"] }),
    mint("payoutMint", (c) => c.payout, { commands: ["payout"] }),
  ),
  reserve: entries(
    spend("reserveSpend", (c) => c.reserve, { commands: ["reserve"] }),
    withdraw("reserveWithdraw", (c) => c.reserve, { commands: ["reserve"] }),
  ),
  phasMembership: (contracts) => [
    // Selected from the blueprint: the membership-proof observer is not part
    // of the resolved SDK bundle.
    entrySpec(
      contracts,
      "phasMembershipWithdraw",
      "withdraw",
      () => {
        const script = loadPhasMembershipWithdrawalScript();
        return { script, scriptHash: validatorToScriptHash(script) };
      },
      { commands: ["phas-membership"] },
    ),
  ],
  // Step 01 of each legacy family (`chain.steps[0]`, which IS the family's
  // `fraudProofs` entry). Later steps follow in `legacyFaultProofLaterSteps`;
  // publication interleaves the two per family.
  legacyFaultProofFirstSteps: legacyChainSteps("first"),
  validationTraceDisputeControl: entries(
    spend("validationTraceDispute", vtdControl, {
      publishWhen: vtdControlPresent,
    }),
    spend("validationTraceDisputeSource", (c) => vtdControl(c).source, {
      publishWhen: vtdControlPresent,
    }),
    spend("validationTraceDisputeGame", (c) => vtdControl(c).game, {
      publishWhen: vtdControlPresent,
    }),
    spend("validationTraceDisputeBoundary", (c) => vtdControl(c).boundary, {
      publishWhen: vtdControlPresent,
    }),
    spend("validationTraceDisputeTimeout", (c) => vtdControl(c).timeout, {
      publishWhen: vtdControlPresent,
    }),
    spend("validationTraceDisputeAward", (c) => vtdControl(c).award, {
      publishWhen: vtdControlPresent,
    }),
  ),
  validationTraceScriptSourcesSemantics: semanticResolvers(
    VALIDATION_TRACE_SCRIPT_SOURCES_SEMANTIC_PREFIXES,
  ),
  validationTraceRedeemerItem: (contracts) => [
    ...SDK.sharedRedeemerItemReferenceScripts(
      vtd(contracts).scriptSourcesStageOneRedeemerStages,
    ).map(({ deploymentEntry, validator }) =>
      spendStep(contracts, deploymentEntry, validator),
    ),
    spendStep(
      contracts,
      "validationTraceDisputeRedeemerItemSettlement",
      vtd(contracts).scriptSourcesStageOneRedeemerStages.settlement,
    ),
  ],
  validationTraceRedeemerNormalizationSemantic: (contracts) => [
    spendStep(
      contracts,
      VALIDATION_TRACE_REDEEMER_NORMALIZATION_SEMANTIC_CONTRACT,
      vtd(contracts).semanticResolvers[
        VALIDATION_TRACE_REDEEMER_NORMALIZATION_SEMANTIC_INDEX
      ],
    ),
  ],
  validationTraceScriptSourcesYields: validationTraceYields(
    VALIDATION_TRACE_SCRIPT_SOURCES_YIELD_KEYS,
  ),
  validationTraceCanonical: (contracts) =>
    SDK.canonicalValidationTraceReferenceScripts(vtd(contracts)).map(
      ({ deploymentEntry, validator }) =>
        spendStep(contracts, deploymentEntry, validator),
    ),
  validationTracePhaseASemantics: semanticResolvers(
    VALIDATION_TRACE_PHASE_A_SEMANTIC_PREFIXES,
  ),
  // The manifest records the linear categories as [A, C, B]; publication
  // walks them in list order [A, B, C].
  registeredChainsA: registeredChainSteps(
    categoriesFrom("fabricatedDeposit", "unusedScriptWitness"),
  ),
  registeredChainsC: registeredChainSteps(
    categoriesFrom("unusedRedeemer", "distinctAssetAccumulationLimit"),
  ),
  registeredChainsB: registeredChainSteps(
    categoriesFrom("missingScriptSource", "missingRedeemer"),
  ),
  // Registered-chain members that are applied like steps but are deliberately
  // not members of `steps`, so the chain walk never reaches them. They still
  // spend from their own script addresses and carry their own roles.
  registeredChainAuxiliaries: (contracts) => {
    const chains = contracts.fraudProofContracts;
    const vnp = chains.valueNotPreserved;
    return [
      spendStep(
        contracts,
        "fraudProofTransitionTrace",
        chains.transitionTrace.route,
      ),
      ...chains.transitionTrace.finals.map((validator, index) => {
        const contract = TRANSITION_TRACE_FINAL_CONTRACT_NAMES[index];
        if (contract === undefined) {
          throw new Error(
            `transitionTrace exposes an unexpected final index ${index.toString()}`,
          );
        }
        return spendStep(contracts, contract, validator);
      }),
      ...(
        [
          [
            "fraudProofValueNotPreservedUnionAcceptedSource",
            vnp.unionAcceptedSource,
          ],
          [
            "fraudProofValueNotPreservedUnionForcedSource",
            vnp.unionForcedSource,
          ],
          ["fraudProofValueNotPreservedUnionEvent", vnp.unionEvent],
          ["fraudProofValueNotPreservedUnionPreState", vnp.unionPreState],
          ["fraudProofValueNotPreservedUnionInputs", vnp.unionInputs],
          ["fraudProofValueNotPreservedUnionInputValue", vnp.unionInputValue],
          ["fraudProofValueNotPreservedUnionAssets", vnp.unionAssets],
          [
            "fraudProofValueNotPreservedUnionFieldGrammar",
            vnp.unionFieldGrammar,
          ],
          ["fraudProofValueNotPreservedUnionOutputs", vnp.unionOutputs],
          ["fraudProofValueNotPreservedUnionOutputScan", vnp.unionOutputScan],
          ["fraudProofValueNotPreservedUnionMint", vnp.unionMint],
          ["fraudProofValueNotPreservedUnionUpdate", vnp.unionUpdate],
          ["fraudProofValueNotPreservedUnionTerminal", vnp.unionTerminal],
          [
            "fraudProofMissingSignatureForcedStep",
            chains.missingSignature.forcedStep,
          ],
          [
            "fraudProofMissingSignatureForcedSigner",
            chains.missingSignature.forcedSigner,
          ],
          [
            "fraudProofMissingSignatureForcedWitness",
            chains.missingSignature.forcedWitness,
          ],
          // The network-id forced (wrongful-rejection) door and the resumable
          // output scan it hands off to.
          ["fraudProofNetworkIdForcedStep", chains.networkId.forcedStep],
          ["fraudProofNetworkIdForcedScan", chains.networkId.forcedScan],
        ] as const
      ).map(([contract, validator]) =>
        spendStep(contracts, contract, validator),
      ),
    ];
  },
  computationThread: entries(
    mint("computationThreadMint", (c) => c.computationThread),
  ),
  chunkedVerify: entries(
    withdraw("chunkedVerifyWithdraw", (c) => c.chunkedVerify),
  ),
  pexcludes: entries(withdraw("pexcludesWithdraw", (c) => c.pexcludes)),
  legacyFaultProofLaterSteps: legacyChainSteps("later"),
  validationTraceCekContext: (contracts) =>
    SDK.cekContextReferenceScripts(
      vtd(contracts).cekContextStages,
      vtd(contracts).cekContextItemStages,
    ).map(({ deploymentEntry, validator }) =>
      spendStep(contracts, deploymentEntry, validator),
    ),
  validationTraceCekCore: (contracts) =>
    CEK_CORE_STAGE_ORDER.map((stage) =>
      spendStep(
        contracts,
        SDK.CEK_CORE_STAGE_REFERENCES[stage].deployment,
        vtd(contracts).cekCoreStages[stage],
      ),
    ),
  validationTraceCekMaterial: (contracts) => [
    spendStep(
      contracts,
      "validationTraceDisputeCekMaterialTraversal",
      vtd(contracts).cekMaterialTraversal,
    ),
    ...validationTraceYields(VALIDATION_TRACE_CEK_MATERIAL_YIELD_KEYS)(
      contracts,
    ),
  ],
  transitionTraceYields: (contracts) =>
    TRANSITION_TRACE_YIELD_CONTRACT_NAMES.map(([key, contract]) =>
      withdrawStep(
        contracts,
        contract,
        contracts.fraudProofContracts.transitionTrace.yields[key],
      ),
    ),
  minAdaYields: (contracts) =>
    MIN_ADA_YIELD_CONTRACT_NAMES.map(([key, contract]) =>
      withdrawStep(
        contracts,
        contract,
        contracts.fraudProofContracts.minAda.yields[key],
      ),
    ),
  correctionLock: entries(
    spend("correctionLockSpend", (c) => c.correctionLock, {
      commands: ["state-queue"],
    }),
  ),
  availabilityChallenge: entries(
    spend("availabilityChallengeSpend", (c) => c.availabilityChallenge),
    mint("availabilityChallengeMint", (c) => c.availabilityChallenge, {
      commands: ["da"],
    }),
    withdraw(
      "availabilityChallengeOpenWithdraw",
      (c) => c.availabilityChallenge.yields.open,
    ),
    withdraw(
      "availabilityChallengeSettleWithdraw",
      (c) => c.availabilityChallenge.yields.settle,
    ),
    withdraw(
      "availabilityChallengeCloseWithdraw",
      (c) => c.availabilityChallenge.yields.close,
    ),
    withdraw(
      "availabilityChallengeTimeoutWithdraw",
      (c) => c.availabilityChallenge.yields.timeout,
    ),
  ),
} as const satisfies Record<string, Section>;
