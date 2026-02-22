import {
  Data,
  LucidEvolution,
  MintingPolicy,
  Script,
  toUnit,
  TxBuilder,
  TxSignBuilder,
} from "@lucid-evolution/lucid";
import {
  AssetError,
  AuthenticatedValidator,
  AuthenticUTxO,
  DataCoercionError,
  GenericErrorFields,
  HashingError,
  LucidError,
  makeReturn,
  MerkleRootSchema,
  POSIXTimeSchema,
  Proof,
  ProofSchema,
  VerificationKeyHashSchema,
  findOperatorByPKH,
} from "@/common.js";
import {
  Data as EffectData,
  Effect,
} from "effect";
import { fetchHubOracleUTxOProgram, HubOracleError } from "@/hub-oracle.js";
import { fetchSchedulerUTxOProgram, SchedulerError } from "@/scheduler.js";
import { DepositDatum, DepositUTxO,utxosToDepositUTxOs } from "./user-events/deposit.js";
import { TxOrderDatum, TxOrderUTxO, utxosToTxOrderUTxOs } from "./user-events/tx-order.js";
import {
  utxosToWithdrawalUTxOs,
  WithdrawalOrderDatum,
  WithdrawalUTxO,
} from "./user-events/withdrawal.js";
import {
  MidgardTxValiditySchema,
  WithdrawalValiditySchema,
} from "@/ledger-state.js";
import { getProtocolParameters } from "@/protocol-parameters.js";
import {
  ActiveOperatorDatum,
  ActiveOperatorMintRedeemer,
  ActiveOperatorSpendRedeemer,
  ActiveOperatorUTxO,
  FetchActiveOperatorParams,
  getActiveOperatorUTxOs,
} from "@/active-operators.js";
import {
  RetiredOperatorMintRedeemer,
  RetiredOperatorUTxO,
  FetchRetiredOperatorParams,
  getRetiredOperatorUTxOs,
} from "@/retired-operators.js";

export const ResolutionClaimSchema = Data.Object({
  resolutionTime: POSIXTimeSchema,
  operator: VerificationKeyHashSchema,
});
export type ResolutionClaim = Data.Static<typeof ResolutionClaimSchema>;
export const ResolutionClaim =
  ResolutionClaimSchema as unknown as ResolutionClaim;

export const SettlementDatumSchema = Data.Object({
  depositsRoot: MerkleRootSchema,
  withdrawalsRoot: MerkleRootSchema,
  transactionsRoot: MerkleRootSchema,
  resolutionClaim: Data.Nullable(ResolutionClaimSchema),
});
export type SettlementDatum = Data.Static<typeof SettlementDatumSchema>;
export const SettlementDatum =
  SettlementDatumSchema as unknown as SettlementDatum;

export const OperatorStatusSchema = Data.Enum([
  Data.Literal("ActiveOperator"),
  Data.Literal("RetiredOperator"),
]);
export type OperatorStatus = Data.Static<typeof OperatorStatusSchema>;
export const OperatorStatus = OperatorStatusSchema as unknown as OperatorStatus;

export const EventTypeSchema = Data.Enum([
  Data.Literal("Deposit"),
  Data.Object({
    Withdrawal: Data.Object({
      validityOverride: WithdrawalValiditySchema,
    }),
  }),
  Data.Object({
    TxOrder: Data.Object({
      validityOverride: MidgardTxValiditySchema,
    }),
  }),
]);
export type EventType = Data.Static<typeof EventTypeSchema>;
export const EventType = EventTypeSchema as unknown as EventType;

export const SettlementSpendRedeemerSchema = Data.Enum([
  Data.Object({
    AttachResolutionClaim: Data.Object({
      settlementInputIndex: Data.Integer(),
      settlementOutputIndex: Data.Integer(),
      hubRefInputIndex: Data.Integer(),
      activeOperatorsNodeInputIndex: Data.Integer(),
      activeOperatorsRedeemerIndex: Data.Integer(),
      operator: VerificationKeyHashSchema,
      schedulerRefInputIndex: Data.Integer(),
    }),
  }),
  Data.Object({
    DisproveResolutionClaim: Data.Object({
      settlementInputIndex: Data.Integer(),
      settlementOutputIndex: Data.Integer(),
      hubRefInputIndex: Data.Integer(),
      operatorsRedeemerIndex: Data.Integer(),
      operator: VerificationKeyHashSchema,
      operatorStatus: OperatorStatusSchema,
      unresolvedEventRefInputIndex: Data.Integer(),
      unresolvedEventAssetName: Data.Bytes(),
      eventType: EventTypeSchema,
      membershipProof: ProofSchema,
    }),
  }),
  Data.Object({
    Resolve: Data.Object({
      settlementId: Data.Bytes(),
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
      settlementId: Data.Bytes(),
      outputIndex: Data.Integer(),
      stateQueueMergeRedeemerIndex: Data.Integer(),
      hubRefInputIndex: Data.Integer(),
    }),
  }),
  Data.Object({
    Remove: Data.Object({
      settlementId: Data.Bytes(),
      inputIndex: Data.Integer(),
      spendRedeemerIndex: Data.Integer(),
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
};

export type SettlementUTxO = AuthenticUTxO<SettlementDatum>;

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
    const spendRedeemer: SettlementSpendRedeemer = {
      AttachResolutionClaim: {
        settlementInputIndex: 0n,
        settlementOutputIndex: 0n,
        hubRefInputIndex: 0n,
        activeOperatorsNodeInputIndex: 0n,
        activeOperatorsRedeemerIndex: 0n,
        operator: params.resolutionClaimOperator,
        schedulerRefInputIndex: 0n,
      },
    };
    const spendRedeemerCBOR = Data.to(spendRedeemer, SettlementSpendRedeemer);

    const updatedDatum: SettlementDatum = {
      ...params.settlementUTxO.datum,
      resolutionClaim: {
        resolutionTime: params.newBondUnlockTime,
        operator: params.resolutionClaimOperator,
      },
    };
    const updatedDatumCBOR = Data.to(updatedDatum, SettlementDatum);

    const hubOracleRefUTxO = yield* fetchHubOracleUTxOProgram(lucid, {
      hubOracleAddress: params.hubOracleValidator.spendScriptAddress,
      hubOraclePolicyId: params.hubOracleValidator.policyId,
    });

    const schedulerRefUTxO = yield* fetchSchedulerUTxOProgram(lucid, {
      schedulerAddress: params.schedulerValidator.spendScriptAddress,
      schedulerPolicyId: params.schedulerValidator.policyId,
    });

    const txUpperBound = Date.now() + 2 * 60_000;

    const buildsettlementTx = lucid
      .newTx()
      .collectFrom([params.settlementUTxO.utxo], spendRedeemerCBOR)
      .readFrom([hubOracleRefUTxO.utxo])
      .readFrom([schedulerRefUTxO.utxo])
      .pay.ToAddressWithData(params.settlementValidator.spendScriptAddress, {
        kind: "inline",
        value: updatedDatumCBOR,
      })
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
}


/**
 * ActiveOperators Node
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const incompleteUpdateBondHoldNewSettlementTxProgram = (
  lucid: LucidEvolution,
  params: UpdateBondHoldNewSettlementParams,
): Effect.Effect<
  TxBuilder,
  | HashingError
  | DataCoercionError
  | LucidError
  | HubOracleError
  | SchedulerError
> =>
  Effect.gen(function* () {
    const spendRedeemer: ActiveOperatorSpendRedeemer = {
      UpdateBondHoldNewSettlement: {
        activeNodeOutputIndex: 0n,
        hubOracleRefInputIndex: 0n,
        settlementQueueInputIndex: 0n,
        settlementQueueRedeemerIndex: 0n,
        newBondUnlockTime: params.newBondUnlockTime,
      },
    };
    const spendRedeemerCBOR = Data.to(
      spendRedeemer,
      ActiveOperatorSpendRedeemer,
    );

    const activeOperatorsUTxOs = yield* getActiveOperatorUTxOs(params.activeOperatorParams, lucid);

    const activeOperatorsInputUtxo =
      activeOperatorsUTxOs.find((utxo) => utxo.datum.key === params.activeOperatorParams.operator) ??
      (() => {
        throw new LucidError({
          message: "No Active Operator UTxO with given operator found",
          cause: "Active Operators Tx not initiated",
        });
      })();

    const updatedDatum: ActiveOperatorDatum = {
      ...activeOperatorsInputUtxo.datum,
      bondUnlockTime: params.newBondUnlockTime,
    };
    const updatedDatumCBOR = Data.to(updatedDatum, ActiveOperatorDatum);

    const hubOracleRefUTxO = yield* fetchHubOracleUTxOProgram(lucid, {
      hubOracleAddress: params.hubOracleValidator.spendScriptAddress,
      hubOraclePolicyId: params.hubOracleValidator.policyId,
    });

    const schedulerRefUTxO = yield* fetchSchedulerUTxOProgram(lucid, {
      schedulerAddress: params.schedulerValidator.spendScriptAddress,
      schedulerPolicyId: params.schedulerValidator.policyId,
    });

    const txUpperBound = Date.now() + 2 * 60_000;

    const buildUpdateBondHoldNewSettlementTx = lucid
      .newTx()
      .collectFrom([activeOperatorsInputUtxo.utxo], spendRedeemerCBOR)
      .readFrom([hubOracleRefUTxO.utxo])
      .readFrom([schedulerRefUTxO.utxo])
      .pay.ToAddressWithData(params.activeOperatorParams.activeOperatorsAddress, {
        kind: "inline",
        value: updatedDatumCBOR,
      })
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
  attachResolutionParams: AttachResolutionClaimParams,
  params:  UpdateBondHoldNewSettlementParams,
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
      yield* incompleteAttachResolutionClaimTxProgram(
        lucid,
        attachResolutionParams,
      );
    const updateBondHoldNewSettlementTx =
      yield* incompleteUpdateBondHoldNewSettlementTxProgram(lucid, params);
    const composedTx = attachResolutionClaimTx.compose(
      updateBondHoldNewSettlementTx,
    );
    const completedTx: TxSignBuilder = yield* Effect.tryPromise({
      try: () => composedTx.complete({ localUPLCEval: false }),
      catch: (e) =>
        new SettlementError({
          message: `Failed to build the transaction: ${e}`,
          cause: e,
        }),
    });
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
  attachResolutionParams: AttachResolutionClaimParams,
  updateBondHoldNewSettlementParams: UpdateBondHoldNewSettlementParams,
): Promise<TxSignBuilder> =>
  makeReturn(
    unsignedAttachResolutionClaimTxProgram(
      lucid,
      attachResolutionParams,
      updateBondHoldNewSettlementParams,
    ),
  ).unsafeRun();

/*Disprove Resolution Claim */
export type DisproveResolutionClaimParams = {
  settlementAddress: string;
  resolutionClaimOperator: string;
  membershipProof: Proof;
  hubOracleValidator: AuthenticatedValidator;
  schedulerScriptAddress: string;
  schedulerPolicyId: string;
  settlementPolicyId: string;
  operatorStatus: OperatorStatus;
  eventType: EventType;
  eventAssetName: string;
  eventAddress: string;
  eventPolicyId: string;
  settlementUTxO: SettlementUTxO;
};

export const fetchUserEventRefUTxO = (
  userEventType: EventType,
  userEventAddress: string,
  userEventPolicyId: string,
  lucid: LucidEvolution,
): Effect.Effect<
  DepositUTxO | WithdrawalUTxO | TxOrderUTxO, 
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

    const authenticUTxOs = yield* (
      userEventType === "Deposit"
        ? utxosToDepositUTxOs(allUTxOs, userEventPolicyId, DepositDatum)
        : "TxOrder" in userEventType
        ? utxosToTxOrderUTxOs(allUTxOs, userEventPolicyId, TxOrderDatum)
        : "Withdrawal" in userEventType
        ? utxosToWithdrawalUTxOs(allUTxOs, userEventPolicyId, WithdrawalOrderDatum)
        : Effect.fail(new LucidError({ message: "Invalid Event Type", cause: "Event Type must be either Deposit or object with TxOrder/Withdrawal" }))
    );
    const authenticUTxO = authenticUTxOs[0];

    if (authenticUTxO) {
      return authenticUTxO;
    }
    return yield* Effect.fail(
      new LucidError({
        message: "No Unresolved User Event UTxO found",
        cause: `No valid authentic UTxOs found for type: ${
          typeof userEventType === "string" ? userEventType : Object.keys(userEventType)[0]
        }`,
      }),
    );
  });

/**
 * Settlement
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const incompleteDisproveResolutionClaimTxProgram = (
  lucid: LucidEvolution,
  params: DisproveResolutionClaimParams,
): Effect.Effect<
  TxBuilder,
  HashingError | DataCoercionError | LucidError | HubOracleError
> =>
  Effect.gen(function* () {
    const spendRedeemer: SettlementSpendRedeemer = {
      DisproveResolutionClaim: {
        settlementInputIndex: 0n,
        settlementOutputIndex: 0n,
        hubRefInputIndex: 0n,
        operatorsRedeemerIndex: 0n,
        operator: params.resolutionClaimOperator,
        operatorStatus: params.operatorStatus,
        unresolvedEventRefInputIndex: 0n,
        unresolvedEventAssetName: params.eventAssetName,
        eventType: params.eventType,
        membershipProof: params.membershipProof,
      },
    };
    const spendRedeemerCBOR = Data.to(spendRedeemer, SettlementSpendRedeemer);

    const updatedDatum: SettlementDatum = {
      ...params.settlementUTxO.datum,
      resolutionClaim: null,
    };
    const updatedDatumCBOR = Data.to(updatedDatum, SettlementDatum);

    const hubOracleRefUTxO = yield* fetchHubOracleUTxOProgram(lucid, {
      hubOracleAddress: params.hubOracleValidator.spendScriptAddress,
      hubOraclePolicyId: params.hubOracleValidator.policyId,
    });

    const userEventRefUTxO = yield* fetchUserEventRefUTxO(
      params.eventType,
      params.eventAddress,
      params.eventPolicyId,
      lucid,
    );

    const resolutionTime = Number(
      params.settlementUTxO.datum.resolutionClaim?.resolutionTime ?? 0n,
    );
    const bufferTime = Date.now() + 2 * 60_000;
    if (resolutionTime < bufferTime) {
      throw new Error("Cannot disprove before resolution time");
    }
    const txUpperBound = resolutionTime - 1 * 60_000;

    const buildsettlementTx = lucid
      .newTx()
      .collectFrom([params.settlementUTxO.utxo], spendRedeemerCBOR)
      .readFrom([hubOracleRefUTxO.utxo])
      .readFrom([userEventRefUTxO.utxo])
      .pay.ToAddressWithData(params.settlementAddress, {
        kind: "inline",
        value: updatedDatumCBOR,
      })
      .validTo(txUpperBound);
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
  operatorInputUTxO: ActiveOperatorUTxO | RetiredOperatorUTxO,
  slashedOperatorKey: string,
): Effect.Effect<string, LucidError> => {
  if (operatorInputUTxO.extra?.status === "ActiveOperator") {
    const mintRedeemer: ActiveOperatorMintRedeemer = {
      RemoveOperatorBadSettlement: {
        slashedActiveOperatorKey: slashedOperatorKey,
        hubOracleRefInputIndex: 0n,
        activeOperatorSlashedNodeInputIndex: 0n,
        activeOperatorAnchorNodeInputIndex: 0n,
        settlementInputIndex: 0n,
        settlementRedeemerIndex: 0n,
      },
    };
    const mintRedeemerCBOR = Data.to(mintRedeemer, ActiveOperatorMintRedeemer);
    return Effect.succeed(mintRedeemerCBOR);
  } else if (operatorInputUTxO.extra?.status === "RetiredOperator") {
    const mintRedeemer: RetiredOperatorMintRedeemer = {
      RemoveOperatorBadSettlement: {
        slashedRetiredOperatorKey: slashedOperatorKey,
        hubOracleRefInputIndex: 0n,
        retiredOperatorSlashedNodeInputIndex: 0n,
        retiredOperatorAnchorNodeInputIndex: 0n,
        settlementInputIndex: 0n,
        settlementRedeemerIndex: 0n,
      },
    };
    const mintRedeemerCBOR = Data.to(mintRedeemer, RetiredOperatorMintRedeemer);
    return Effect.succeed(mintRedeemerCBOR);
  } else {
    return Effect.fail(
      new LucidError({
        message: `Invalid operator status: ${operatorInputUTxO.extra}`,
        cause: "Expected 'ActiveOperator' or 'RetiredOperator'",
      }),
    );
  }
};

export const getOperatorNFT = (
  operatorInputUTxO: ActiveOperatorUTxO | RetiredOperatorUTxO,
  activeOperatorPolicyId: string,
  retiredOperatorPolicyId: string,
): Effect.Effect<string, LucidError> => {
  if (operatorInputUTxO.extra?.status === "ActiveOperator") {
    return Effect.succeed(
      toUnit(activeOperatorPolicyId, operatorInputUTxO.assetName),
    );
  } else if (operatorInputUTxO.extra?.status === "RetiredOperator") {
    return Effect.succeed(
      toUnit(retiredOperatorPolicyId, operatorInputUTxO.assetName),
    );
  } else
    return Effect.fail(
      new LucidError({
        message: `Invalid operator status: ${operatorInputUTxO.extra}`,
        cause: "Expected 'ActiveOperator' or 'RetiredOperator'",
      }),
    );
};

export const incompleteRemoveOperatorBadSettlementTxProgram = (
  lucid: LucidEvolution,
  params: RemoveOperatorBadSettlementParams,
): Effect.Effect<
  TxBuilder,
  HashingError | DataCoercionError | LucidError | HubOracleError
> =>
  Effect.gen(function* () {
    const activeOperatorUTxOs: ActiveOperatorUTxO[] = yield* getActiveOperatorUTxOs(params.activeOperatorParams, lucid);

    const retiredOperatorUTxOs: RetiredOperatorUTxO[] = yield* getRetiredOperatorUTxOs(params.retiredOperatorParams, lucid);
    
    const operatorInputUTxO = yield* findOperatorByPKH(
      activeOperatorUTxOs,
      retiredOperatorUTxOs,
      params.slashedOperatorKey,
    );

    const mintRedeemerCBOR = yield* createSlashedOperatorMintRedeemerCBOR(
      operatorInputUTxO,
      params.slashedOperatorKey,
    );
    const bondAmount =
      (operatorInputUTxO.utxo.assets.lovelace * 60n) / 100n;

    const operatorNFT = yield* getOperatorNFT(
      operatorInputUTxO,
      params.activeOperatorParams.activeOperatorsPolicyId,
      params.retiredOperatorParams.retiredOperatorsPolicyId,
    );

    const hubOracleRefUTxO = yield* fetchHubOracleUTxOProgram(lucid, {
      hubOracleAddress: params.hubOracleValidator.spendScriptAddress,
      hubOraclePolicyId: params.hubOracleValidator.policyId,
    });

    const network = lucid.config().network ?? "Mainnet";
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
  disproveResolutionClaimParams: DisproveResolutionClaimParams,
  removeOperatorBadSettlementParams: RemoveOperatorBadSettlementParams,
): Effect.Effect<
  TxSignBuilder,
  | HashingError
  | DataCoercionError
  | LucidError
  | SettlementError
  | HubOracleError
> =>
  Effect.gen(function* () {
    const disproveResolutionClaimTx =
      yield* incompleteDisproveResolutionClaimTxProgram(
        lucid,
        disproveResolutionClaimParams,
      );
    const removeOperatorBadSettlementTx =
      yield* incompleteRemoveOperatorBadSettlementTxProgram(
        lucid,
        removeOperatorBadSettlementParams,
      );
    const composedTx = disproveResolutionClaimTx.compose(
      removeOperatorBadSettlementTx,
    );
    const completedTx: TxSignBuilder = yield* Effect.tryPromise({
      try: () => composedTx.complete({ localUPLCEval: false }),
      catch: (e) =>
        new SettlementError({
          message: `Failed to build the transaction: ${e}`,
          cause: e,
        }),
    });
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
  disproveResolutionClaimParams: DisproveResolutionClaimParams,
  removeOperatorBadSettlementParams: RemoveOperatorBadSettlementParams,
): Promise<TxSignBuilder> =>
  makeReturn(
    unsignedDisproveResolutionClaimTxProgram(
      lucid,
      disproveResolutionClaimParams,
      removeOperatorBadSettlementParams,
    ),
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
        settlementId: params.settlementId,
      },
    };
    const spendRedeemerCBOR = Data.to(spendRedeemer, SettlementSpendRedeemer);

    const mintRedeemer: SettlementMintRedeemer = {
      Remove: {
        settlementId: params.settlementId,
        inputIndex: 0n,
        spendRedeemerIndex: 0n,
      },
    };
    const mintRedeemerCBOR = Data.to(mintRedeemer, SettlementMintRedeemer);

    const resolutionTime = Number(
      params.settlementUTxO.datum.resolutionClaim?.resolutionTime ?? 0n,
    );
    const txLowerBound = resolutionTime + 1 * 60_000;
    const txSigner = params.settlementUTxO.datum.resolutionClaim?.operator!;
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
    const completedTx: TxSignBuilder = yield* Effect.tryPromise({
      try: () => resolveSettlementTx.complete({ localUPLCEval: false }),
      catch: (e) =>
        new SettlementError({
          message: `Failed to build the transaction: ${e}`,
          cause: e,
        }),
    });
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
