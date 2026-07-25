import { type Assets, assetsEqual } from "@al-ft/midgard-core/assets";
import {
  type BuildTxWithRedeemer,
  Data,
  LucidEvolution,
  MintingPolicy,
  Script,
  toUnit,
  TxBuilder,
  type TxOutput,
  TxSignBuilder,
} from "@lucid-evolution/lucid";
import { Data as EffectData, Effect } from "effect";

import {
  ActiveOperatorDatum,
  ActiveOperatorSpendRedeemer,
  ActiveOperatorUTxO,
  FetchActiveOperatorParams,
  fetchActiveOperatorUTxOs,
  requireActiveOperatorUTxO,
} from "@/active-operators.js";
import {
  AssetError,
  AuthenticatedValidator,
  DataCoercionError,
  findOperatorByPKH,
  GenericErrorFields,
  HashingError,
  LucidError,
  makeReturn,
  MerkleRootSchema,
  POSIXTimeSchema,
  UnspecifiedNetworkError,
  VerificationKeyHashSchema,
} from "@/common.js";
import { fetchHubOracleUTxOProgram, HubOracleError } from "@/hub-oracle.js";
import { AuthenticUTxO } from "@/internals.js";
import {
  MidgardTxValiditySchema,
  WithdrawalValiditySchema,
} from "@/ledger-state.js";
import { getProtocolParameters } from "@/protocol-parameters.js";
import {
  FetchRetiredOperatorParams,
  fetchRetiredOperatorUTxOs,
  RetiredOperatorUTxO,
} from "@/retired-operators.js";
import { fetchSchedulerUTxOProgram, SchedulerError } from "@/scheduler.js";
import {
  EventSettlementMembershipProof,
  EventSettlementMembershipProofSchema,
} from "@/transition-trace.js";
import { completeTxWithLocalUPLCEvalProgram } from "@/tx-completion.js";
import {
  requireInputIndex,
  requireOwnMintPurpose,
  requireOwnSpendPurpose,
  requireReferenceInputIndex,
  requireSpendRedeemerIndex,
  requireUniqueOutputIndex,
} from "@/tx-context-redeemer.js";
import { outputDatumCborMatches } from "@/tx-output-utils.js";

import { DepositUTxO, utxosToDepositUTxOs } from "./user-events/deposit.js";
import {
  TxOrderUTxOV1,
  utxosToTxOrderUTxOsV1,
} from "./user-events/tx-order.js";
import {
  utxosToWithdrawalUTxOs,
  WithdrawalUTxO,
} from "./user-events/withdrawal.js";

export const ResolutionClaimSchema = Data.Object({
  resolution_time: POSIXTimeSchema,
  operator: VerificationKeyHashSchema,
});
export type ResolutionClaim = Data.Static<typeof ResolutionClaimSchema>;
export const ResolutionClaim =
  ResolutionClaimSchema as unknown as ResolutionClaim;

export const SettlementDatumSchema = Data.Object({
  deposits_root: MerkleRootSchema,
  withdrawals_root: MerkleRootSchema,
  forced_transactions_root: MerkleRootSchema,
  transactions_root: MerkleRootSchema,
  resolution_claim: Data.Nullable(ResolutionClaimSchema),
});
export type SettlementDatum = Data.Static<typeof SettlementDatumSchema>;
export const SettlementDatum =
  SettlementDatumSchema as unknown as SettlementDatum;

export const EventTypeSchema = Data.Enum([
  Data.Literal("Deposit"),
  Data.Object({
    Withdrawal: Data.Object({
      validity_override: WithdrawalValiditySchema,
    }),
  }),
  Data.Object({
    TxOrder: Data.Object({
      validity_override: MidgardTxValiditySchema,
    }),
  }),
]);
export type EventType = Data.Static<typeof EventTypeSchema>;
export const EventType = EventTypeSchema as unknown as EventType;

export const SettlementSpendRedeemerSchema = Data.Enum([
  Data.Object({
    AttachResolutionClaim: Data.Object({
      settlement_input_index: Data.Integer(),
      settlement_output_index: Data.Integer(),
      hub_ref_input_index: Data.Integer(),
      active_operators_node_input_index: Data.Integer(),
      active_operators_redeemer_index: Data.Integer(),
      operator: VerificationKeyHashSchema,
      scheduler_ref_input_index: Data.Integer(),
    }),
  }),
  Data.Object({
    DisproveResolutionClaim: Data.Object({
      settlement_input_index: Data.Integer(),
      settlement_output_index: Data.Integer(),
      hub_ref_input_index: Data.Integer(),
      operators_redeemer_index: Data.Integer(),
      operator: VerificationKeyHashSchema,
      operator_is_active: Data.Boolean(),
      unresolved_event_ref_input_index: Data.Integer(),
      unresolved_event_asset_name: Data.Bytes(),
      event_type: EventTypeSchema,
      membership_proof: EventSettlementMembershipProofSchema,
      inclusion_proof_script_withdraw_redeemer_index: Data.Integer(),
    }),
  }),
  Data.Object({
    Resolve: Data.Object({
      settlement_id: Data.Bytes(),
    }),
  }),
]);
export type SettlementSpendRedeemer = Data.Static<
  typeof SettlementSpendRedeemerSchema
>;
export const SettlementSpendRedeemer =
  SettlementSpendRedeemerSchema as unknown as SettlementSpendRedeemer;

export const SettlementMintRedeemerSchema = Data.Enum([
  Data.Object({
    Spawn: Data.Object({
      settlement_id: Data.Bytes(),
      output_index: Data.Integer(),
      state_queue_merge_redeemer_index: Data.Integer(),
      hub_ref_input_index: Data.Integer(),
    }),
  }),
  Data.Object({
    Remove: Data.Object({
      settlement_id: Data.Bytes(),
      input_index: Data.Integer(),
      spend_redeemer_index: Data.Integer(),
    }),
  }),
]);
export type SettlementMintRedeemer = Data.Static<
  typeof SettlementMintRedeemerSchema
>;
export const SettlementMintRedeemer =
  SettlementMintRedeemerSchema as unknown as SettlementMintRedeemer;

export type AttachResolutionClaimParams = {
  settlementValidator: AuthenticatedValidator;
  resolutionClaimOperator: string;
  newBondUnlockTime: bigint;
  hubOracleValidator: AuthenticatedValidator;
  schedulerValidator: AuthenticatedValidator;
  settlementUTxO: SettlementUTxO;
  updateBondHoldNewSettlementParams: UpdateBondHoldNewSettlementParams;
};

export type SettlementUTxO = AuthenticUTxO<SettlementDatum>;

const outputMatches =
  ({
    address,
    datum,
    assets,
  }: {
    readonly address: string;
    readonly datum: string;
    readonly assets: Assets;
  }) =>
  (output: TxOutput): boolean =>
    output.address === address &&
    outputDatumCborMatches(output, datum) &&
    assetsEqual(output.assets, assets);

/**
 * Settlement
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const incompleteAttachResolutionClaimTxProgram = (
  lucid: LucidEvolution,
  params: AttachResolutionClaimParams,
): Effect.Effect<
  TxBuilder,
  | HashingError
  | DataCoercionError
  | LucidError
  | UnresolvedError
  | HubOracleError
  | SchedulerError
> =>
  Effect.gen(function* () {
    const updatedDatum: SettlementDatum = {
      ...params.settlementUTxO.datum,
      resolution_claim: {
        resolution_time: params.newBondUnlockTime,
        operator: params.resolutionClaimOperator,
      },
    };
    const updatedDatumCBOR = Data.to(updatedDatum, SettlementDatum);

    const hubOracleRefUTxO = yield* fetchHubOracleUTxOProgram(lucid, {
      hubOracleAddress: params.hubOracleValidator.spendingScriptAddress,
      hubOraclePolicyId: params.hubOracleValidator.policyId,
    });

    const schedulerRefUTxO = yield* fetchSchedulerUTxOProgram(lucid, {
      schedulerAddress: params.schedulerValidator.spendingScriptAddress,
      schedulerPolicyId: params.schedulerValidator.policyId,
    });

    const activeOperatorsUTxOs = yield* fetchActiveOperatorUTxOs(
      params.updateBondHoldNewSettlementParams.activeOperatorParams,
      lucid,
    );
    const activeOperatorsInputUtxo = yield* requireActiveOperatorUTxO(
      activeOperatorsUTxOs,
      params.resolutionClaimOperator,
    );

    const spendRedeemerCBOR = ((ctx) => {
      requireOwnSpendPurpose(
        ctx,
        params.settlementUTxO.utxo,
        "attach resolution claim settlement",
      );
      return Data.to(
        {
          AttachResolutionClaim: {
            settlement_input_index: requireInputIndex(
              ctx,
              params.settlementUTxO.utxo,
              "attach resolution claim settlement",
            ),
            settlement_output_index: requireUniqueOutputIndex(
              ctx.outputs,
              outputMatches({
                address: params.settlementValidator.spendingScriptAddress,
                datum: updatedDatumCBOR,
                assets: params.settlementUTxO.utxo.assets,
              }),
              "attach resolution claim settlement",
            ),
            hub_ref_input_index: requireReferenceInputIndex(
              ctx,
              hubOracleRefUTxO.utxo,
              "attach resolution claim hub oracle",
            ),
            active_operators_node_input_index: requireInputIndex(
              ctx,
              activeOperatorsInputUtxo.utxo,
              "attach resolution claim active operator",
            ),
            active_operators_redeemer_index: requireSpendRedeemerIndex(
              ctx,
              activeOperatorsInputUtxo.utxo,
              "attach resolution claim active operator",
            ),
            operator: params.resolutionClaimOperator,
            scheduler_ref_input_index: requireReferenceInputIndex(
              ctx,
              schedulerRefUTxO.utxo,
              "attach resolution claim scheduler",
            ),
          },
        } satisfies SettlementSpendRedeemer,
        SettlementSpendRedeemer,
      );
    }) satisfies BuildTxWithRedeemer;

    const txUpperBound = Date.now() + 2 * 60_000;

    const buildsettlementTx = lucid
      .newTx()
      .collectFrom([params.settlementUTxO.utxo], spendRedeemerCBOR)
      .readFrom([hubOracleRefUTxO.utxo])
      .readFrom([schedulerRefUTxO.utxo])
      .pay.ToAddressWithData(
        params.settlementValidator.spendingScriptAddress,
        {
          kind: "inline",
          value: updatedDatumCBOR,
        },
        params.settlementUTxO.utxo.assets,
      )
      .validTo(txUpperBound)
      .addSignerKey(params.resolutionClaimOperator);
    return buildsettlementTx;
  }).pipe(
    Effect.catchAllDefect((defect) => {
      return Effect.fail(
        new LucidError({
          message: "Caught defect from attachResolutionClaimTxBuilder",
          cause: defect,
        }),
      );
    }),
  );

export type UpdateBondHoldNewSettlementParams = {
  newBondUnlockTime: bigint;
  hubOracleValidator: AuthenticatedValidator;
  schedulerValidator: AuthenticatedValidator;
  activeOperatorParams: FetchActiveOperatorParams;
};

export type UpdateBondHoldNewSettlementTxParams =
  UpdateBondHoldNewSettlementParams & {
    readonly settlementUTxO: SettlementUTxO;
  };

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
            message: `Failed to build the transaction: ${e}`,
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
): Effect.Effect<
  DepositUTxO | WithdrawalUTxO | TxOrderUTxOV1,
  LucidError | DataCoercionError
> =>
  Effect.gen(function* () {
    const allUTxOs = yield* Effect.tryPromise({
      try: () => lucid.utxosAt(userEventAddress),
      catch: (err) =>
        new LucidError({
          message: "Failed to fetch User Event UTxOs",
          cause: err,
        }),
    });

    const authenticUTxOs = yield* userEventType === "Deposit"
      ? utxosToDepositUTxOs(allUTxOs, userEventPolicyId)
      : "TxOrder" in userEventType
        ? utxosToTxOrderUTxOsV1(allUTxOs, userEventPolicyId)
        : "Withdrawal" in userEventType
          ? utxosToWithdrawalUTxOs(allUTxOs, userEventPolicyId)
          : Effect.fail(
              new LucidError({
                message: "Invalid Event Type",
                cause:
                  "Event Type must be either Deposit or object with TxOrder/Withdrawal",
              }),
            );
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
 * Build the transaction that shows invalidity of the attached resolution claim
 * to the specified `SettlementUTxO`. Spends the UTxO and reproduces it without
 * a resolution claim.
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const incompleteDisproveResolutionClaimTxProgram = (
  _lucid: LucidEvolution,
  params: DisproveResolutionClaimParams,
): Effect.Effect<
  TxBuilder,
  | HashingError
  | DataCoercionError
  | LucidError
  | HubOracleError
  | SettlementError
> =>
  Effect.fail(
    new SettlementError({
      message:
        "Cannot build disprove-resolution-claim transaction without canonical slashing and membership redeemer arguments",
      cause: {
        operator: params.resolutionClaimOperator,
        eventAssetName: params.eventAssetName,
      },
    }),
  );

export type RemoveOperatorBadSettlementParams = {
  slashedOperatorKey: string;
  activeOperatorMintingPolicy: MintingPolicy;
  fraudProverAddress: string;
  fraudProverDatum: string;
  hubOracleValidator: AuthenticatedValidator;
  eventType: EventType;
  eventAddress: string;
  eventPolicyId: string;
  activeOperatorParams: FetchActiveOperatorParams;
  retiredOperatorParams: FetchRetiredOperatorParams;
};

export const createSlashedOperatorMintRedeemerCBOR = (
  operatorInputUTxO:
    | (ActiveOperatorUTxO & { isActive: true })
    | (RetiredOperatorUTxO & { isActive: false }),
  slashedOperatorKey: string,
): Effect.Effect<string, SettlementError> =>
  Effect.fail(
    new SettlementError({
      message:
        "Cannot build slashed-operator mint redeemer without canonical slashing arguments",
      cause: {
        slashedOperatorKey,
        operatorIsActive: operatorInputUTxO.isActive,
      },
    }),
  );

export const getOperatorNFT = (
  operatorInputUTxO:
    | (ActiveOperatorUTxO & { isActive: true })
    | (RetiredOperatorUTxO & { isActive: false }),
  activeOperatorPolicyId: string,
  retiredOperatorPolicyId: string,
): Effect.Effect<string> => {
  if (operatorInputUTxO.isActive === true) {
    return Effect.succeed(
      toUnit(activeOperatorPolicyId, operatorInputUTxO.assetName),
    );
  } else {
    return Effect.succeed(
      toUnit(retiredOperatorPolicyId, operatorInputUTxO.assetName),
    );
  }
};

export const incompleteRemoveOperatorBadSettlementTxProgram = (
  lucid: LucidEvolution,
  params: RemoveOperatorBadSettlementParams,
): Effect.Effect<
  TxBuilder,
  | HashingError
  | DataCoercionError
  | LucidError
  | HubOracleError
  | SettlementError
  | UnspecifiedNetworkError
> =>
  Effect.gen(function* () {
    const activeOperatorUTxOs: ActiveOperatorUTxO[] =
      yield* fetchActiveOperatorUTxOs(params.activeOperatorParams, lucid);

    const retiredOperatorUTxOs: RetiredOperatorUTxO[] =
      yield* fetchRetiredOperatorUTxOs(params.retiredOperatorParams, lucid);

    const operatorInputUTxO = yield* findOperatorByPKH(
      activeOperatorUTxOs,
      retiredOperatorUTxOs,
      params.slashedOperatorKey,
    );

    const mintRedeemerCBOR = yield* createSlashedOperatorMintRedeemerCBOR(
      operatorInputUTxO,
      params.slashedOperatorKey,
    );
    const bondAmount = (operatorInputUTxO.utxo.assets.lovelace * 60n) / 100n;

    const operatorNFT = yield* getOperatorNFT(
      operatorInputUTxO,
      params.activeOperatorParams.activeOperatorPolicyId,
      params.retiredOperatorParams.retiredOperatorPolicyId,
    );

    const hubOracleRefUTxO = yield* fetchHubOracleUTxOProgram(lucid, {
      hubOracleAddress: params.hubOracleValidator.spendingScriptAddress,
      hubOraclePolicyId: params.hubOracleValidator.policyId,
    });

    const network = lucid.config().network;
    if (!network) {
      return yield* new UnspecifiedNetworkError({
        message: "",
        cause: "Cardano network not found",
      });
    }

    const slashingPenalty = getProtocolParameters(network).slashing_penalty;

    const userEventRefUTxO = yield* fetchUserEventRefUTxO(
      params.eventType,
      params.eventAddress,
      params.eventPolicyId,
      lucid,
    );

    const buildsettlementTx = lucid
      .newTx()
      .collectFrom([operatorInputUTxO.utxo], mintRedeemerCBOR)
      .readFrom([hubOracleRefUTxO.utxo])
      .readFrom([userEventRefUTxO.utxo])
      .mintAssets(
        {
          [operatorNFT]: -1n,
        },
        mintRedeemerCBOR,
      )
      .pay.ToAddressWithData(
        params.fraudProverAddress,
        { kind: "inline", value: params.fraudProverDatum },
        { lovelace: bondAmount },
      )
      .attach.MintingPolicy(params.activeOperatorMintingPolicy)
      .setMinFee(slashingPenalty);
    return buildsettlementTx;
  }).pipe(
    Effect.catchAllDefect((defect) => {
      return Effect.fail(
        new LucidError({
          message: "Caught defect from disproveResolutionClaimTxBuilder",
          cause: defect,
        }),
      );
    }),
  );

export const unsignedDisproveResolutionClaimTxProgram = (
  lucid: LucidEvolution,
  params: DisproveResolutionClaimParams,
): Effect.Effect<
  TxSignBuilder,
  | HashingError
  | DataCoercionError
  | LucidError
  | SettlementError
  | HubOracleError
  | UnspecifiedNetworkError
> =>
  Effect.gen(function* () {
    const disproveResolutionClaimTx =
      yield* incompleteDisproveResolutionClaimTxProgram(lucid, params);
    const removeOperatorBadSettlementTx =
      yield* incompleteRemoveOperatorBadSettlementTxProgram(
        lucid,
        params.removeOperatorBadSettlementParams,
      );
    const composedTx = disproveResolutionClaimTx.compose(
      removeOperatorBadSettlementTx,
    );
    const completedTx: TxSignBuilder =
      yield* completeTxWithLocalUPLCEvalProgram(
        composedTx,
        (e) =>
          new SettlementError({
            message: `Failed to build the transaction: ${e}`,
            cause: e,
          }),
      );
    return completedTx;
  });

/**
 * Builds completed tx for disproving resolution claims using the provided
 * `LucidEvolution` instance, `DisproveResolutionClaimParams` and `RemoveOperatorBadSettlementParams` parameters.
 *
 * @param lucid - The `LucidEvolution` API object.
 * @param disproveResolutionClaimParams - Parameters required for disproving resolution claim.
 * @param removeOperatorBadSettlementParams - Parameters required for removing the slashed active/retired operator.
 * @returns A promise that resolves to a `TxSignBuilder` instance.
 */
export const unsignedDisproveResolutionClaimTx = (
  lucid: LucidEvolution,
  params: DisproveResolutionClaimParams,
): Promise<TxSignBuilder> =>
  makeReturn(
    unsignedDisproveResolutionClaimTxProgram(lucid, params),
  ).unsafeRun();

export class SettlementError extends EffectData.TaggedError(
  "SettlementError",
)<GenericErrorFields> {}

/*
Resolve Settlement
*/
export type ResolveSettlementParams = {
  settlementAddress: string;
  resolutionClaimOperator: string;
  settlementId: string;
  changeAddress: string;
  settlementPolicyId: string;
  settlementMintingPolicy: Script;
  settlementUTxO: SettlementUTxO;
};

/**
 * Settlement
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const incompleteResolveSettlementProgram = (
  lucid: LucidEvolution,
  params: ResolveSettlementParams,
): Effect.Effect<
  TxBuilder,
  HashingError | DataCoercionError | LucidError | AssetError
> =>
  Effect.gen(function* () {
    const spendRedeemer: SettlementSpendRedeemer = {
      Resolve: {
        settlement_id: params.settlementId,
      },
    };
    const spendRedeemerCBOR = Data.to(spendRedeemer, SettlementSpendRedeemer);
    const mintRedeemerCBOR = ((ctx) => {
      requireOwnMintPurpose(
        ctx,
        params.settlementPolicyId,
        "resolve settlement mint",
      );
      return Data.to(
        {
          Remove: {
            settlement_id: params.settlementId,
            input_index: requireInputIndex(
              ctx,
              params.settlementUTxO.utxo,
              "resolve settlement",
            ),
            spend_redeemer_index: requireSpendRedeemerIndex(
              ctx,
              params.settlementUTxO.utxo,
              "resolve settlement",
            ),
          },
        } satisfies SettlementMintRedeemer,
        SettlementMintRedeemer,
      );
    }) satisfies BuildTxWithRedeemer;

    const resolutionTime = Number(
      params.settlementUTxO.datum.resolution_claim?.resolution_time ?? 0n,
    );
    const txLowerBound = resolutionTime + 1 * 60_000;
    const txSigner = params.settlementUTxO.datum.resolution_claim?.operator!;
    const changeAmount = 1_000_000n;

    const settlementNFT = toUnit(
      params.settlementPolicyId,
      params.settlementUTxO.assetName,
    );

    const buildsettlementTx = lucid
      .newTx()
      .collectFrom([params.settlementUTxO.utxo], spendRedeemerCBOR)
      .mintAssets(
        {
          [settlementNFT]: -1n,
        },
        mintRedeemerCBOR,
      )
      .pay.ToAddress(params.changeAddress, { lovelace: changeAmount })
      .addSignerKey(txSigner)
      .attach.MintingPolicy(params.settlementMintingPolicy)
      .validFrom(txLowerBound);
    return buildsettlementTx;
  }).pipe(
    Effect.catchAllDefect((defect) => {
      return Effect.fail(
        new LucidError({
          message: "Caught defect from resolveSettlementTxBuilder",
          cause: defect,
        }),
      );
    }),
  );

export const unsignedResolveSettlementTxProgram = (
  lucid: LucidEvolution,
  params: ResolveSettlementParams,
): Effect.Effect<
  TxSignBuilder,
  HashingError | DataCoercionError | LucidError | SettlementError | AssetError
> =>
  Effect.gen(function* () {
    const resolveSettlementTx = yield* incompleteResolveSettlementProgram(
      lucid,
      params,
    );
    const completedTx: TxSignBuilder =
      yield* completeTxWithLocalUPLCEvalProgram(
        resolveSettlementTx,
        (e) =>
          new SettlementError({
            message: `Failed to build the transaction: ${e}`,
            cause: e,
          }),
      );
    return completedTx;
  });

/**
 * Builds completed tx for resolving settlement using the provided
 * `LucidEvolution` instance, `ResolveSettlementParams` parameters.
 *
 * @param lucid - The `LucidEvolution` API object.
 * @param params - Parameters required for resolving settlement.
 * @returns A promise that resolves to a `TxSignBuilder` instance.
 */
export const unsignedResolveSettlementTx = (
  lucid: LucidEvolution,
  params: ResolveSettlementParams,
): Promise<TxSignBuilder> =>
  makeReturn(unsignedResolveSettlementTxProgram(lucid, params)).unsafeRun();

export class UnresolvedError extends EffectData.TaggedError(
  "UnresolvedError",
)<GenericErrorFields> {}
