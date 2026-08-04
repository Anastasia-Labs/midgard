import {
  replayGenuineDepositAuthorityScenarioV1,
  replayGenuineForcedTerminalAuthorityScenarioV1,
  replayGenuineWithdrawalAuthorityScenarioV1,
  type W15AcceptedAuthorityScenarioV1,
  type W15AuthorityScenarioInputV1,
} from "./w15-authority-scenarios.js";
import {
  replayAcceptedW16AuthorityScenarioV1,
  replayGenuineAbsorbToReserveAuthorityScenarioV1,
  replayGenuineRefundWithdrawalAuthorityScenarioV1,
  replayGenuineSpawnSettlementAuthorityScenarioV1,
  type W16AcceptedAuthorityScenarioV1,
  type W16AuthorityScenarioInputV1,
} from "./w16-authority-scenarios.js";

/**
 * W25 deliberately has no record factory.  Each facade call replays the
 * caller's authenticated W15/W16 context through the production parser and
 * returns the original opaque context plus digest evidence.
 */
export type W25UserEventAuthorityFixtureInputV1 = W15AuthorityScenarioInputV1;
export type W25SettlementAuthorityFixtureInputV1 = W16AuthorityScenarioInputV1;

export type AcceptedW25UserEventAuthorityFixtureV1 =
  W15AcceptedAuthorityScenarioV1 & Readonly<{ rawResultDigest: string }>;

export type AcceptedW25SettlementAuthorityFixtureV1 =
  W16AcceptedAuthorityScenarioV1 & Readonly<{ rawResultDigest: string }>;

const userEventFacade = (
  scenario: W15AcceptedAuthorityScenarioV1,
): AcceptedW25UserEventAuthorityFixtureV1 =>
  Object.freeze({ ...scenario, rawResultDigest: scenario.result.resultDigest });

const settlementFacade = (
  scenario: W16AcceptedAuthorityScenarioV1,
): AcceptedW25SettlementAuthorityFixtureV1 =>
  Object.freeze({ ...scenario, rawResultDigest: scenario.result.resultDigest });

export const makeAcceptedW25DepositAuthorityFixtureV1 = (
  input: W25UserEventAuthorityFixtureInputV1,
): AcceptedW25UserEventAuthorityFixtureV1 =>
  userEventFacade(replayGenuineDepositAuthorityScenarioV1(input));

export const makeAcceptedW25WithdrawalAuthorityFixtureV1 = (
  input: W25UserEventAuthorityFixtureInputV1,
): AcceptedW25UserEventAuthorityFixtureV1 =>
  userEventFacade(replayGenuineWithdrawalAuthorityScenarioV1(input));

export const makeAcceptedW25ForcedAuthorityFixtureV1 = (
  input: W25UserEventAuthorityFixtureInputV1,
): AcceptedW25UserEventAuthorityFixtureV1 =>
  userEventFacade(replayGenuineForcedTerminalAuthorityScenarioV1(input));

export const makeAcceptedW25SpawnSettlementAuthorityFixtureV1 = (
  input: W25SettlementAuthorityFixtureInputV1,
): AcceptedW25SettlementAuthorityFixtureV1 =>
  settlementFacade(replayGenuineSpawnSettlementAuthorityScenarioV1(input));

export const makeAcceptedW25AbsorbToReserveAuthorityFixtureV1 = (
  input: W25SettlementAuthorityFixtureInputV1,
): AcceptedW25SettlementAuthorityFixtureV1 =>
  settlementFacade(replayGenuineAbsorbToReserveAuthorityScenarioV1(input));

export const makeAcceptedW25InitializePayoutAuthorityFixtureV1 = (
  input: W25SettlementAuthorityFixtureInputV1,
): AcceptedW25SettlementAuthorityFixtureV1 =>
  settlementFacade(
    replayAcceptedW16AuthorityScenarioV1(input, "initialize_payout"),
  );

export const makeAcceptedW25RefundWithdrawalAuthorityFixtureV1 = (
  input: W25SettlementAuthorityFixtureInputV1,
): AcceptedW25SettlementAuthorityFixtureV1 =>
  settlementFacade(replayGenuineRefundWithdrawalAuthorityScenarioV1(input));
