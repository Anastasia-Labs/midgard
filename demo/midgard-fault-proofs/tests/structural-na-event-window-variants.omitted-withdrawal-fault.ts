import * as SDK from "@al-ft/midgard-sdk";

import {
  depositInfo,
  EVENT_ASSET_NAME,
  EVENT_REF_INPUT_INDEX,
  forcedInclusionTx,
  membership,
  nonMembership,
  outRef,
  withdrawalInfo,
} from "./structural-na-event-window-variants.expect-arm-matches-blueprint.js";

export const omittedWithdrawalFault = ({
  domain = SDK.ROOT_DOMAINS.withdrawals,
  key = outRef(0n),
}: {
  readonly domain?: SDK.RootDomain;
  readonly key?: SDK.OutputReference;
} = {}): SDK.TransitionFault =>
  SDK.omittedDueL1EventFault({
    OmittedDueWithdrawal: {
      source_non_membership: nonMembership({
        domain,
        key,
      }) as SDK.WithdrawalSourceNonMembershipProof,
    },
  });

export const omittedForcedFault = (): SDK.TransitionFault =>
  SDK.omittedDueL1EventFault({
    OmittedDueForcedTransaction: {
      event_ref_input_index: EVENT_REF_INPUT_INDEX,
      event_asset_name: EVENT_ASSET_NAME,
      validity_override: "ForcedTxValid",
      source_non_membership: nonMembership({
        domain: SDK.ROOT_DOMAINS.forcedTransactionsV1,
        key: outRef(0n),
      }) as SDK.ForcedTransactionSourceNonMembershipProof,
    },
  });

export const outOfWindowDepositFault = (): SDK.TransitionFault =>
  SDK.outOfWindowSourceEventFault({
    OutOfWindowDeposit: {
      source_membership: membership({
        domain: SDK.ROOT_DOMAINS.deposits,
        key: outRef(0n),
        value: depositInfo,
      }) as SDK.DepositSourceMembershipProof,
    },
  });

export const outOfWindowWithdrawalFault = (): SDK.TransitionFault =>
  SDK.outOfWindowSourceEventFault({
    OutOfWindowWithdrawal: {
      source_membership: membership({
        domain: SDK.ROOT_DOMAINS.withdrawals,
        key: outRef(0n),
        value: withdrawalInfo("WithdrawalIsValid"),
      }) as SDK.WithdrawalSourceMembershipProof,
    },
  });

export const outOfWindowForcedFault = (): SDK.TransitionFault =>
  SDK.outOfWindowSourceEventFault({
    OutOfWindowForcedTransaction: {
      event_ref_input_index: EVENT_REF_INPUT_INDEX,
      event_asset_name: EVENT_ASSET_NAME,
      validity_override: "ForcedTxValid",
      source_membership: membership({
        domain: SDK.ROOT_DOMAINS.forcedTransactionsV1,
        key: outRef(0n),
        value: forcedInclusionTx,
      }) as SDK.ForcedTransactionSourceMembershipProof,
    },
  });
