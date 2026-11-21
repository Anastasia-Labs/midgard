import { defineDeploymentFamily } from "../members.types.js";

export const protocolUserEventsMembers = defineDeploymentFamily("protocol", {
  depositHistoryRetentionSpend: {
    purpose: "spend",
    reference: {
      role: "deposit history retention",
      token: "DepositHistoryRetention",
    },
    commands: ["deposit"],
    validatorTitle: "user_events/history_data.retention.spend",
    familyPaths: [],
  },
  depositHistoryRetirementWithdraw: {
    purpose: "withdraw",
    reference: {
      role: "deposit history retirement",
      token: "DepositHistoryRetirement",
    },
    commands: ["deposit"],
    validatorTitle:
      "user_events/history_retirement.retirement_observer.withdraw",
    familyPaths: [],
  },
  withdrawalHistoryRetentionSpend: {
    purpose: "spend",
    reference: {
      role: "withdrawal history retention",
      token: "WithdrawalHistoryRetention",
    },
    commands: ["withdrawal"],
    validatorTitle: "user_events/history_data.retention.spend",
    familyPaths: [],
  },
  withdrawalHistoryRetirementWithdraw: {
    purpose: "withdraw",
    reference: {
      role: "withdrawal history retirement",
      token: "WithdrawalHistoryRetirement",
    },
    commands: ["withdrawal"],
    validatorTitle:
      "user_events/history_retirement.retirement_observer.withdraw",
    familyPaths: [],
  },
  depositSpend: {
    purpose: "spend",
    reference: { role: "deposit spending", token: "DepositSpend" },
    commands: ["deposit"],
    validatorTitle: "user_events/history.history.spend",
    familyPaths: [],
  },
  depositMint: {
    purpose: "mint",
    reference: { role: "deposit minting", token: "DepositMint" },
    commands: ["deposit"],
    validatorTitle: "user_events/history.history.spend",
    familyPaths: [],
  },
  withdrawalSpend: {
    purpose: "spend",
    reference: { role: "withdrawal spending", token: "WithdrawalSpend" },
    commands: ["withdrawal"],
    validatorTitle: "user_events/history.history.spend",
    familyPaths: [],
  },
  withdrawalMint: {
    purpose: "mint",
    reference: { role: "withdrawal minting", token: "WithdrawalMint" },
    commands: ["withdrawal"],
    validatorTitle: "user_events/history.history.spend",
    familyPaths: [],
  },
  txOrderSpend: {
    purpose: "spend",
    reference: null,
    commands: [],
    validatorTitle: "user_events/tx_order_v1.spend.spend",
    familyPaths: [],
  },
  txOrderMint: {
    purpose: "mint",
    reference: null,
    commands: [],
    validatorTitle: "user_events/tx_order_v1.mint.mint",
    familyPaths: [],
  },
  payoutSpend: {
    purpose: "spend",
    reference: { role: "payout spending", token: "PayoutSpend" },
    commands: ["payout"],
    validatorTitle: "payout.spend.spend",
    familyPaths: [],
  },
  payoutMint: {
    purpose: "mint",
    reference: { role: "payout minting", token: "PayoutMint" },
    commands: ["payout"],
    validatorTitle: "payout.mint.mint",
    familyPaths: [],
  },
  reserveSpend: {
    purpose: "spend",
    reference: { role: "reserve spending", token: "ReserveSpend" },
    commands: ["reserve"],
    validatorTitle: "reserve.spend.spend",
    familyPaths: [],
  },
  reserveWithdraw: {
    purpose: "withdraw",
    reference: { role: "reserve observer", token: "ReserveObserver" },
    commands: ["reserve"],
    validatorTitle: "reserve.withdraw.else",
    familyPaths: [],
  },
});
