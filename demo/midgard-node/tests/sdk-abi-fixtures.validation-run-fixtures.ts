import type * as SDK from "@al-ft/midgard-sdk/core";
import type * as SDKFraudProof from "@al-ft/midgard-sdk/fraud-proof";

import { proof } from "./sdk-abi-fixtures.header-fixture.js";

export const sourceKeyOpeningFixtures = (
  withdrawalSourceMembership: SDK.WithdrawalSourceMembershipProof,
  depositSourceMembership: SDK.DepositSourceLeafMembershipProof,
) => {
  const sourceMemberships = {
    withdrawal: {
      WithdrawalKeyOpening: {
        withdrawal_id: withdrawalSourceMembership.key,
        value_hash: "71".repeat(32),
        phas_root: withdrawalSourceMembership.phas_root,
        proof,
      },
    },
    deposit: {
      DepositKeyOpening: {
        deposit_id: depositSourceMembership.key,
        value_hash: "72".repeat(32),
        phas_root: depositSourceMembership.phas_root,
        proof,
      },
    },
  } satisfies Record<string, SDKFraudProof.SourceKeyOpening>;
  return sourceMemberships;
};
