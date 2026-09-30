import {
  type BuildTxWithRedeemer,
  Data,
  LucidEvolution,
  MintingPolicy,
  TxBuilder,
  TxSignBuilder,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  ActiveOperatorDatum,
  ActiveOperatorSpendRedeemer,
  FetchActiveOperatorParams,
  fetchActiveOperatorUTxOs,
  requireActiveOperatorUTxO,
} from "./active-operators.js";
import {
  AuthenticatedValidator,
  DataCoercionError,
  HashingError,
  LucidError,
  makeReturn,
} from "./common.js";
import { fetchHubOracleUTxOProgram, HubOracleError } from "./hub-oracle.js";
import { FetchRetiredOperatorParams } from "./retired-operators.js";
import { fetchSchedulerUTxOProgram, SchedulerError } from "./scheduler.js";
import {
  type AttachResolutionClaimParams,
  EventType,
  incompleteAttachResolutionClaimTxProgram,
  outputMatches,
  SettlementError,
  type SettlementUTxO,
  UnresolvedError,
  type UpdateBondHoldNewSettlementTxParams,
} from "./settlement.incomplete-attach-resolution-claim-tx-program.js";
import { EventSettlementMembershipProof } from "./transition-trace.js";
import { completeTxWithLocalUPLCEvalProgram } from "./tx-completion.js";
import {
  requireInputIndex,
  requireOwnSpendPurpose,
  requireReferenceInputIndex,
  requireSpendRedeemerIndex,
  requireUniqueOutputIndex,
} from "./tx-context-redeemer.js";
import {
  DepositUTxO,
  fetchDepositUTxOsProgram,
} from "./user-events/deposit.js";
import { type EventHistoryDeployment } from "./user-events/history-query.js";
import { TxOrderUTxOV1, utxosToTxOrderUTxOs } from "./user-events/tx-order.js";
import {
  fetchWithdrawalUTxOsProgram,
  WithdrawalUTxO,
} from "./user-events/withdrawal.js";

/**
 * ActiveOperators Node
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const incompleteUpdateBondHoldNewSettlementTxProgram = (
  lucid: LucidEvolution,
  params: UpdateBondHoldNewSettlementTxParams,
): Effect.Effect<
  TxBuilder,
  | HashingError
  | DataCoercionError
  | LucidError
  | HubOracleError
  | SchedulerError
> =>
  Effect.gen(function* () {
    const activeOperatorsUTxOs = yield* fetchActiveOperatorUTxOs(
      params.activeOperatorParams,
      lucid,
    );
    const activeOperatorsInputUtxo = yield* requireActiveOperatorUTxO(
      activeOperatorsUTxOs,
      params.activeOperatorParams.operator,
    );

    const updatedDatum: ActiveOperatorDatum = {
      ...activeOperatorsInputUtxo.datum,
      bond_unlock_time:
        activeOperatorsInputUtxo.datum.bond_unlock_time === null ||
        params.newBondUnlockTime >
          activeOperatorsInputUtxo.datum.bond_unlock_time
          ? params.newBondUnlockTime
          : activeOperatorsInputUtxo.datum.bond_unlock_time,
    };
    const updatedDatumCBOR = Data.to(updatedDatum, ActiveOperatorDatum);

    const hubOracleRefUTxO = yield* fetchHubOracleUTxOProgram(lucid, {
      hubOracleAddress: params.hubOracleValidator.spendingScriptAddress,
      hubOraclePolicyId: params.hubOracleValidator.policyId,
    });

    const schedulerRefUTxO = yield* fetchSchedulerUTxOProgram(lucid, {
      schedulerAddress: params.schedulerValidator.spendingScriptAddress,
      schedulerPolicyId: params.schedulerValidator.policyId,
    });

    const spendRedeemerCBOR = ((ctx) => {
      requireOwnSpendPurpose(
        ctx,
        activeOperatorsInputUtxo.utxo,
        "update bond hold new settlement active operator",
      );
      return Data.to(
        {
          UpdateBondHoldNewSettlement: {
            active_operator: params.activeOperatorParams.operator,
            active_node_input_index: requireInputIndex(
              ctx,
              activeOperatorsInputUtxo.utxo,
              "update bond hold new settlement active operator",
            ),
            active_node_output_index: requireUniqueOutputIndex(
              ctx.outputs,
              outputMatches({
                address: params.activeOperatorParams.activeOperatorAddress,
                datum: updatedDatumCBOR,
                assets: activeOperatorsInputUtxo.utxo.assets,
              }),
              "update bond hold new settlement active operator",
            ),
            hub_oracle_ref_input_index: requireReferenceInputIndex(
              ctx,
              hubOracleRefUTxO.utxo,
              "update bond hold new settlement hub oracle",
            ),
            settlement_input_index: requireInputIndex(
              ctx,
              params.settlementUTxO.utxo,
              "update bond hold new settlement settlement",
            ),
            settlement_redeemer_index: requireSpendRedeemerIndex(
              ctx,
              params.settlementUTxO.utxo,
              "update bond hold new settlement settlement",
            ),
            resolution_time: params.newBondUnlockTime,
          },
        } satisfies ActiveOperatorSpendRedeemer,
        ActiveOperatorSpendRedeemer,
      );
    }) satisfies BuildTxWithRedeemer;

    const txUpperBound = Date.now() + 2 * 60_000;

    const buildUpdateBondHoldNewSettlementTx = lucid
      .newTx()
      .collectFrom([activeOperatorsInputUtxo.utxo], spendRedeemerCBOR)
      .readFrom([hubOracleRefUTxO.utxo])
      .readFrom([schedulerRefUTxO.utxo])
      .pay.ToAddressWithData(
        params.activeOperatorParams.activeOperatorAddress,
        {
          kind: "inline",
          value: updatedDatumCBOR,
        },
        activeOperatorsInputUtxo.utxo.assets,
      )
      .validTo(txUpperBound);
    return buildUpdateBondHoldNewSettlementTx;
  }).pipe(
    Effect.catchAllDefect((defect) => {
      return Effect.fail(
        new LucidError({
          message: "Caught defect from UpdateBondHoldNewSettlementTxProgram",
          cause: defect,
        }),
      );
    }),
  );

export const unsignedAttachResolutionClaimTxProgram = (
  lucid: LucidEvolution,
  params: AttachResolutionClaimParams,
): Effect.Effect<
  TxSignBuilder,
  | HashingError
  | DataCoercionError
  | LucidError
  | SettlementError
  | UnresolvedError
  | HubOracleError
  | SchedulerError
> =>
  Effect.gen(function* () {
    const attachResolutionClaimTx =
      yield* incompleteAttachResolutionClaimTxProgram(lucid, params);
    const updateBondHoldNewSettlementTx =
      yield* incompleteUpdateBondHoldNewSettlementTxProgram(lucid, {
        ...params.updateBondHoldNewSettlementParams,
        settlementUTxO: params.settlementUTxO,
      });
    const composedTx = attachResolutionClaimTx.compose(
      updateBondHoldNewSettlementTx,
    );
    const completedTx: TxSignBuilder =
      yield* completeTxWithLocalUPLCEvalProgram(
        composedTx,
        (e) =>
          new SettlementError({
            message: `Failed to build the transaction: ${String(e)}`,
            cause: e,
          }),
      );
    return completedTx;
  });

/**
 * Builds completed tx for attaching resolution claims using the provided
 * `LucidEvolution` instance, `AttachResolutionClaimParams` and `ActiveOperatorsParams` parameters.
 *
 * @param lucid - The `LucidEvolution` API object.
 * @param attachResolutionParams - Parameters required for attaching resolution claim.
 * @param updateBondHoldNewSettlementParams - Parameters required for selecting active operator and updating bond unlock time.
 * @returns A promise that resolves to a `TxSignBuilder` instance.
 */
export const unsignedAttachResolutionClaimTx = (
  lucid: LucidEvolution,
  params: AttachResolutionClaimParams,
): Promise<TxSignBuilder> =>
  makeReturn(unsignedAttachResolutionClaimTxProgram(lucid, params)).unsafeRun();

export const fetchUserEventRefUTxO = (
  userEventType: EventType,
  userEventAddress: string,
  userEventPolicyId: string,
  lucid: LucidEvolution,
  eventHistory: EventHistoryDeployment | null,
): Effect.Effect<
  DepositUTxO | WithdrawalUTxO | TxOrderUTxOV1,
  LucidError | DataCoercionError
> =>
  Effect.gen(function* () {
    let authenticUTxOs: (DepositUTxO | WithdrawalUTxO | TxOrderUTxOV1)[];
    if (typeof userEventType !== "string" && "TxOrder" in userEventType) {
      const allUTxOs = yield* Effect.tryPromise({
        try: () => lucid.utxosAt(userEventAddress),
        catch: (cause) =>
          new LucidError({
            message: "Failed to fetch forced order UTxOs",
            cause,
          }),
      });
      authenticUTxOs = yield* utxosToTxOrderUTxOs(allUTxOs, userEventPolicyId);
    } else {
      if (
        eventHistory === null ||
        eventHistory.address !== userEventAddress ||
        eventHistory.policyId !== userEventPolicyId
      )
        return yield* Effect.fail(
          new LucidError({
            message:
              "Deposit/withdrawal reference lookup requires the matching authenticated history deployment",
            cause: userEventType,
          }),
        );
      authenticUTxOs = yield* userEventType === "Deposit"
        ? fetchDepositUTxOsProgram(lucid, eventHistory)
        : fetchWithdrawalUTxOsProgram(lucid, eventHistory);
    }
    const authenticUTxO = authenticUTxOs[0];

    if (authenticUTxO) {
      return authenticUTxO;
    }
    return yield* Effect.fail(
      new LucidError({
        message: "No Unresolved User Event UTxO found",
        cause: `No valid authentic UTxOs found for type: ${
          typeof userEventType === "string"
            ? userEventType
            : Object.keys(userEventType)[0]
        }`,
      }),
    );
  });

export type DisproveResolutionClaimParams = {
  settlementAddress: string;
  resolutionClaimOperator: string;
  membershipProof: EventSettlementMembershipProof;
  hubOracleValidator: AuthenticatedValidator;
  schedulerScriptAddress: string;
  schedulerPolicyId: string;
  settlementPolicyId: string;
  operatorIsActive: boolean;
  eventType: EventType;
  eventAssetName: string;
  eventAddress: string;
  eventPolicyId: string;
  settlementUTxO: SettlementUTxO;
  removeOperatorBadSettlementParams: RemoveOperatorBadSettlementParams;
};

/**
 * Bad-settlement slashing carries no fraud-prover payout.
 *
 * The 2026-08-11 owner ruling 7 (D4) routes the `fraud_prover_reward`
 * exclusively through the once-per-header `RemoveFraudulentBlockHeader`
 * transaction, so this route pays no reward and therefore needs no prover
 * destination. The former `fraudProverAddress`/`fraudProverDatum` fields and
 * the 60% bond remainder they were paid with are deleted, not relocated: F04
 * §2.5 records that remainder rule as a non-authority that "must not be
 * revived".
 */
export type RemoveOperatorBadSettlementParams = {
  slashedOperatorKey: string;
  /** Exact release-manifest slashing fee; never infer it from a network label. */
  slashingPenaltyLovelace: bigint;
  activeOperatorMintingPolicy: MintingPolicy;
  hubOracleValidator: AuthenticatedValidator;
  eventType: EventType;
  eventAddress: string;
  eventPolicyId: string;
  eventHistory: EventHistoryDeployment | null;
  activeOperatorParams: FetchActiveOperatorParams;
  retiredOperatorParams: FetchRetiredOperatorParams;
};
