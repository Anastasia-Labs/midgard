import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import { h32, proof } from "./sdk-abi-fixtures.header-fixture.js";

export const sourceKeyOpeningFixtures = (
  withdrawalSourceMembership: SDK.WithdrawalSourceMembershipProof,
  depositSourceMembership: SDK.DepositSourceMembershipProof,
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
  } satisfies Record<string, SDK.SourceKeyOpening>;
  return sourceMemberships;
};

export const validationRunFaultFixtures = (
  l2SourceMembership: SDK.L2TransactionSourceMembershipProof,
): Record<string, SDK.TransitionFault> => ({
  "transition-fault.source-membership.missing-validation-run":
    SDK.sourceMembershipMismatchFault({
      SourceEventMissingValidationRun: {
        source: {
          L2KeyOpening: {
            tx_id: h32,
            value_hash: "73".repeat(32),
            phas_root: l2SourceMembership.phas_root,
            proof,
          },
        },
        run_phas_root: "74".repeat(32),
        run_absence_proof: proof,
      },
    }),
  "transition-fault.source-membership.foreign-validation-run":
    SDK.sourceMembershipMismatchFault({
      ForeignValidationRun: {
        event_key: Data.to(
          { L2TransactionEventKey: { tx_id: h32 } },
          SDK.EventKey,
        ),
        value_hash: "73".repeat(32),
        run_phas_root: "74".repeat(32),
        run_proof: proof,
        source_phas_root: "75".repeat(32),
        source_absence_proof: proof,
      },
    }),
  "transition-fault.source-membership.malformed-validation-run":
    SDK.sourceMembershipMismatchFault({
      MalformedValidationRun: {
        event_key: { L2TransactionEventKey: { tx_id: h32 } },
        value: "ff",
        run_phas_root: "74".repeat(32),
        run_proof: proof,
      },
    }),
});
