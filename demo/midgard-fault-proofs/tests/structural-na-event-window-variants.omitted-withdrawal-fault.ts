import * as SDK from "@al-ft/midgard-sdk/core";
import * as SDKFraudProof from "@al-ft/midgard-sdk/fraud-proof";
import { Data } from "@lucid-evolution/lucid";

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
} = {}): SDKFraudProof.TransitionFault =>
  SDKFraudProof.omittedDueL1EventFault({
    OmittedDueWithdrawal: {
      source_non_membership: nonMembership({
        domain,
        key,
      }) as SDKFraudProof.WithdrawalSourceNonMembershipProof,
    },
  });

export const omittedForcedFault = (): SDKFraudProof.TransitionFault =>
  SDKFraudProof.omittedDueL1EventFault({
    OmittedDueForcedTransaction: {
      event_ref_input_index: EVENT_REF_INPUT_INDEX,
      event_asset_name: EVENT_ASSET_NAME,
      validity_override: "ForcedTxValid",
      source_non_membership: nonMembership({
        domain: SDK.ROOT_DOMAINS.forcedTransactionsV1,
        key: outRef(0n),
      }) as SDKFraudProof.ForcedTransactionSourceNonMembershipProof,
    },
  });

export const outOfWindowDepositFault = (): SDKFraudProof.TransitionFault =>
  SDKFraudProof.outOfWindowSourceEventFault({
    OutOfWindowDeposit: {
      source_membership: membership({
        domain: SDK.ROOT_DOMAINS.deposits,
        key: outRef(0n),
        value: Data.to(depositInfo, SDK.DepositInfo),
      }) as SDK.DepositSourceLeafMembershipProof,
    },
  });

export const outOfWindowWithdrawalFault = (): SDKFraudProof.TransitionFault =>
  SDKFraudProof.outOfWindowSourceEventFault({
    OutOfWindowWithdrawal: {
      source_membership: membership({
        domain: SDK.ROOT_DOMAINS.withdrawals,
        key: outRef(0n),
        value: withdrawalInfo("WithdrawalIsValid"),
      }) as SDK.WithdrawalSourceMembershipProof,
    },
  });

export const outOfWindowForcedFault = (): SDKFraudProof.TransitionFault =>
  SDKFraudProof.outOfWindowSourceEventFault({
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
