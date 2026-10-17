import { requireEventHistoryContracts } from "../user-events/history-deployment.js";
import { mint, phasMembership, spend, withdraw } from "./recipes.types.js";

export const protocolRecipes = {
  referenceScriptAuthMint: mint((c) => c.referenceScriptAuth),
  hubOracleMint: mint((c) => c.hubOracle),
  daParamsGovernorSpend: spend((c) => c.daParamsGovernor),
  daParamsGovernorMint: mint((c) => c.daParamsGovernor),
  daBondPoolSpend: spend((c) => c.daBondPool),
  daBondPoolMint: mint((c) => c.daBondPool),
  daAttestationSpend: spend((c) => c.daAttestation),
  daAttestationMint: mint((c) => c.daAttestation),
  stateQueueSpend: spend((c) => c.stateQueue),
  stateQueueMint: mint((c) => c.stateQueue),
  stateQueueCommitWithdraw: withdraw((c) => c.stateQueue.yields.commit),
  stateQueueUnattestedTimeoutWithdraw: withdraw(
    (c) => c.stateQueue.yields.unattestedTimeout,
  ),
  stateQueueUnavailableTimeoutWithdraw: withdraw(
    (c) => c.stateQueue.yields.unavailableTimeout,
  ),
  stateQueueFraudRemovalWithdraw: withdraw(
    (c) => c.stateQueue.yields.fraudRemoval,
  ),
  stateQueueMergeWithdraw: withdraw((c) => c.stateQueue.yields.merge),
  schedulerSpend: spend((c) => c.scheduler),
  schedulerMint: mint((c) => c.scheduler),
  registeredOperatorsSpend: spend((c) => c.registeredOperators),
  registeredOperatorsMint: mint((c) => c.registeredOperators),
  activeOperatorsSpend: spend((c) => c.activeOperators),
  activeOperatorsMint: mint((c) => c.activeOperators),
  retiredOperatorsSpend: spend((c) => c.retiredOperators),
  retiredOperatorsMint: mint((c) => c.retiredOperators),
  fraudProofCatalogueSpend: spend((c) => c.fraudProofCatalogue),
  fraudProofCatalogueMint: mint((c) => c.fraudProofCatalogue),
  fraudProofSpend: spend((c) => c.fraudProof),
  fraudProofMint: mint((c) => c.fraudProof),
  depositHistoryRetentionSpend: spend(
    (c) => requireEventHistoryContracts(c).deposit.retention,
  ),
  depositHistoryRetirementWithdraw: withdraw(
    (c) => requireEventHistoryContracts(c).deposit.retirement,
  ),
  withdrawalHistoryRetentionSpend: spend(
    (c) => requireEventHistoryContracts(c).withdrawal.retention,
  ),
  withdrawalHistoryRetirementWithdraw: withdraw(
    (c) => requireEventHistoryContracts(c).withdrawal.retirement,
  ),
  depositSpend: spend((c) => requireEventHistoryContracts(c).deposit.list),
  depositMint: mint((c) => requireEventHistoryContracts(c).deposit.list),
  withdrawalSpend: spend(
    (c) => requireEventHistoryContracts(c).withdrawal.list,
  ),
  withdrawalMint: mint((c) => requireEventHistoryContracts(c).withdrawal.list),
  txOrderSpend: spend((c) => c.txOrder),
  txOrderMint: mint((c) => c.txOrder),
  fieldPreimageCertificateSpend: spend((c) => c.fieldPreimageCertificate),
  fieldPreimageCertificateMint: mint((c) => c.fieldPreimageCertificate),
  // The explicit always-succeeds bundle uses the tx-order stand-in here.
  cekProgramMaterialSpend: spend(
    (c) => c.cekProgramMaterial,
    (c) =>
      c.cekProgramMaterial.spendingScriptHash !== c.txOrder.spendingScriptHash,
  ),
  settlementSpend: spend((c) => c.settlement),
  settlementMint: mint((c) => c.settlement),
  payoutSpend: spend((c) => c.payout),
  payoutMint: mint((c) => c.payout),
  reserveSpend: spend((c) => c.reserve),
  reserveWithdraw: withdraw((c) => c.reserve),
  phasMembershipWithdraw: phasMembership,
  computationThreadMint: mint((c) => c.computationThread),
  chunkedVerifyWithdraw: withdraw((c) => c.chunkedVerify),
  pexcludesWithdraw: withdraw((c) => c.pexcludes),
  correctionLockSpend: spend((c) => c.correctionLock),
  availabilityChallengeSpend: spend((c) => c.availabilityChallenge),
  availabilityChallengeMint: mint((c) => c.availabilityChallenge),
  availabilityChallengeOpenWithdraw: withdraw(
    (c) => c.availabilityChallenge.yields.open,
  ),
  availabilityChallengeSettleWithdraw: withdraw(
    (c) => c.availabilityChallenge.yields.settle,
  ),
  availabilityChallengeCloseWithdraw: withdraw(
    (c) => c.availabilityChallenge.yields.close,
  ),
  availabilityChallengeTimeoutWithdraw: withdraw(
    (c) => c.availabilityChallenge.yields.timeout,
  ),
} as const;
