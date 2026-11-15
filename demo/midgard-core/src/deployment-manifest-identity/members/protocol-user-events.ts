import { defineDeploymentFamily } from "../members.types.js";

export const protocolUserEventsMembers = defineDeploymentFamily("protocol", {
  depositHistoryRetentionSpend: {
    purpose: "spend",
    referenceRole: "deposit history retention",
    commands: ["deposit"],
    validatorTitle: "user_events/history_data.retention.spend",
    familyPaths: [],
  },
  depositHistoryRetirementWithdraw: {
    purpose: "withdraw",
    referenceRole: "deposit history retirement",
    commands: ["deposit"],
    validatorTitle:
      "user_events/history_retirement.retirement_observer.withdraw",
    familyPaths: [],
  },
  withdrawalHistoryRetentionSpend: {
    purpose: "spend",
    referenceRole: "withdrawal history retention",
    commands: ["withdrawal"],
    validatorTitle: "user_events/history_data.retention.spend",
    familyPaths: [],
  },
  withdrawalHistoryRetirementWithdraw: {
    purpose: "withdraw",
    referenceRole: "withdrawal history retirement",
    commands: ["withdrawal"],
    validatorTitle:
      "user_events/history_retirement.retirement_observer.withdraw",
    familyPaths: [],
  },
  depositSpend: {
    purpose: "spend",
    referenceRole: "deposit spending",
    commands: ["deposit"],
    validatorTitle: "user_events/history.history.spend",
    familyPaths: [],
  },
  depositMint: {
    purpose: "mint",
    referenceRole: "deposit minting",
    commands: ["deposit"],
    validatorTitle: "user_events/history.history.spend",
    familyPaths: [],
  },
  withdrawalSpend: {
    purpose: "spend",
    referenceRole: "withdrawal spending",
    commands: ["withdrawal"],
    validatorTitle: "user_events/history.history.spend",
    familyPaths: [],
  },
  withdrawalMint: {
    purpose: "mint",
    referenceRole: "withdrawal minting",
    commands: ["withdrawal"],
    validatorTitle: "user_events/history.history.spend",
    familyPaths: [],
  },
  txOrderSpend: {
    purpose: "spend",
    referenceRole: null,
    commands: [],
    validatorTitle: "user_events/tx_order_v1.spend.spend",
    familyPaths: [],
  },
  txOrderMint: {
    purpose: "mint",
    referenceRole: null,
    commands: [],
    validatorTitle: "user_events/tx_order_v1.mint.mint",
    familyPaths: [],
  },
  payoutSpend: {
    purpose: "spend",
    referenceRole: "payout spending",
    commands: ["payout"],
    validatorTitle: "payout.spend.spend",
    familyPaths: [],
  },
  payoutMint: {
    purpose: "mint",
    referenceRole: "payout minting",
    commands: ["payout"],
    validatorTitle: "payout.mint.mint",
    familyPaths: [],
  },
  reserveSpend: {
    purpose: "spend",
    referenceRole: "reserve spending",
    commands: ["reserve"],
    validatorTitle: "reserve.spend.spend",
    familyPaths: [],
  },
  reserveWithdraw: {
    purpose: "withdraw",
    referenceRole: "reserve observer",
    commands: ["reserve"],
    validatorTitle: "reserve.withdraw.else",
    familyPaths: [],
  },
});
