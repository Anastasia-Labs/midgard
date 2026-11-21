import policy from "../../devnet/preprod/funding-policy.json" with { type: "json" };
import { WALLET_ROLES, type WalletRole } from "./identities.js";

export const FUNDING_POLICY = Object.freeze(policy);
export const ROLE_COLLATERAL_LOVELACE = BigInt(
  FUNDING_POLICY.collateralLovelace,
);

/** Initial protocol capital, deployment outputs and journey holdings, apart
 * from fee runway. No live role-wallet refill is scheduled. */
const INITIAL_CAPITAL_ADA: Record<WalletRole, bigint> = {
  operator: 200_000n,
  merge: 50_000n,
  referenceScript: 100_000n,
  settlement: 50_000n,
  daCosigner: 1_000n,
  daSubmitter0: 20_000n,
  daSubmitter1: 20_000n,
  daAvailability0: 20_000n,
  daAvailability1: 20_000n,
  watcherProver: 50_000n,
  watcherAvailability: 20_000n,
  userA: 50_000n,
  userB: 50_000n,
  userC: 50_000n,
};

/** Planning rates, not measured rates or transaction admission limits.
 * 4,320/day is one transaction per mean Cardano block (20 seconds); the 5x
 * margin covers variation. Reference publication is paid from initial capital
 * (397 observed transactions/deploy); the cosigner signs off-chain only. */
export const roleTransactionsPerDay = (role: WalletRole): number => {
  switch (role) {
    case "referenceScript":
    case "daCosigner":
      return 0;
    case "userA":
    case "userB":
    case "userC":
      return FUNDING_POLICY.userTransactionsPerDay;
    case "operator":
    case "merge":
    case "settlement":
    case "daSubmitter0":
    case "daSubmitter1":
    case "daAvailability0":
    case "daAvailability1":
    case "watcherProver":
    case "watcherAvailability":
      return FUNDING_POLICY.operationalTransactionsPerDay;
  }
};

export const roleFeeRunwayLovelace = (role: WalletRole): bigint =>
  BigInt(roleTransactionsPerDay(role)) *
  BigInt(FUNDING_POLICY.planningFeeLovelace) *
  BigInt(FUNDING_POLICY.targetDays) *
  BigInt(FUNDING_POLICY.margin);

export const roleBudgetLovelace = (role: WalletRole): bigint =>
  INITIAL_CAPITAL_ADA[role] * 1_000_000n + roleFeeRunwayLovelace(role);

export const totalRoleFundingLovelace = (): bigint =>
  WALLET_ROLES.reduce(
    (sum, role) => sum + roleBudgetLovelace(role) + ROLE_COLLATERAL_LOVELACE,
    0n,
  );
