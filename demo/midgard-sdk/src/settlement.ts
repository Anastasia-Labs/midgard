import {
  Data,
  fromHex,
  LucidEvolution,
  MintingPolicy,
  Script,
  toUnit,
  TxBuilder,
  TxSignBuilder,
  UTxO,
} from "@lucid-evolution/lucid";
import {
  AssetError,
  AuthenticatedValidator,
  DataCoercionError,
  GenericErrorFields,
  getStateToken,
  HashingError,
  LucidError,
  makeReturn,
  MerkleRootSchema,
  OutputReference,
  POSIXTimeSchema,
  Proof,
  ProofSchema,
  UnauthenticUtxoError,
  VerificationKeyHashSchema,
} from "@/common.js";
import {
  Data as EffectData,
  Array as EffectArray,
  Effect,
  Option,
} from "effect";
import {
  fetchHubOracleUTxOProgram,
  HubOracleError,
} from "@/hub-oracle.js";
import { fetchSchedulerUTxOProgram, SchedulerError } from "@/scheduler.js";
import { utxosToDepositUTxOs } from "./user-events/deposit.js";
import { utxosToTxOrderUTxOs } from "./user-events/tx-order.js";
import { WithdrawalOrderDatum } from "./user-events/withdrawal.js";
import {
  MidgardTxValiditySchema,
  WithdrawalInfo,
  WithdrawalValiditySchema,
} from "@/ledger-state.js";
import { getProtocolParameters } from "@/protocol-parameters.js";
import {
  ActiveOperatorDatum,
  ActiveOperatorMintRedeemer,
  ActiveOperatorSpendRedeemer,
  ActiveOperatorUTxO,
  utxosToActiveOperatorUTxOs,
} from "@/active-operators.js";
import {
  RetiredOperatorDatum,
  RetiredOperatorMintRedeemer,
  RetiredOperatorUTxO,
  utxosToRetiredOperatorUTxOs,
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
  depositUTxOs: UTxO[];
  withdrawalUTxOs: UTxO[];
  txOrderUTxOs: UTxO[];
};

export type SettlementUTxO = {
  utxo: UTxO;
  datum: SettlementDatum;
  assetName: string;
};

export const getSettlementDatumFromUTxO = (
  nodeUTxO: UTxO,
): Effect.Effect<SettlementDatum, DataCoercionError> => {
  const datumCBOR = nodeUTxO.datum;
  if (datumCBOR) {
    try {
      const settlementDatum = Data.from(datumCBOR, SettlementDatum);
      return Effect.succeed(settlementDatum);
    } catch (e) {
      return Effect.fail(
        new DataCoercionError({
          message: `Could not coerce UTxO's datum to a settlement datum`,
          cause: e,
        }),
      );
    }
  } else {
    return Effect.fail(
      new DataCoercionError({
        message: `Settlement datum coercion failed`,
        cause: `No datum found`,
      }),
    );
  }
};

/**
 * Validates correctness of datum, and having a single NFT.
 */
export const utxoToSettlementUTxO = (
  utxo: UTxO,
  nftPolicy: string,
): Effect.Effect<SettlementUTxO, DataCoercionError | UnauthenticUtxoError> =>
  Effect.gen(function* () {
    const datum = yield* getSettlementDatumFromUTxO(utxo);
    const [sym, assetName] = yield* getStateToken(utxo.assets);
    if (sym !== nftPolicy) {
      yield* Effect.fail(
        new UnauthenticUtxoError({
          message: "Failed to convert UTxO to `SettlementUTxO`",
          cause: "UTxO's NFT policy ID is not the same as the settlement's",
        }),
      );
    }
    return {
      utxo,
      datum,
      assetName,
    };
  });

/**
 * Silently drops invalid UTxOs.
 */
export const utxosToSettlementUTxOs = (
  utxos: UTxO[],
  nftPolicy: string,
): Effect.Effect<SettlementUTxO[]> => {
  const effects = utxos.map((u) => utxoToSettlementUTxO(u, nftPolicy));
  return Effect.allSuccesses(effects);
};

export const getSettlementUTxOWithoutClaim = (
  settlementUTxOs: SettlementUTxO[],
  depositUTxOs: UTxO[],
  withdrawalUTxOs: UTxO[],
  txOrderUTxOs: UTxO[],
  lucid: LucidEvolution,
): Effect.Effect<SettlementUTxO, LucidError | UnresolvedError> =>
  Effect.gen(function* () {
    const allEvents = [...depositUTxOs, ...withdrawalUTxOs, ...txOrderUTxOs];
    const stillUnspent = yield* Effect.promise(() =>
      lucid.utxosByOutRef(allEvents),
    );

    if (stillUnspent.length > 0) {
      return yield* Effect.fail(
        new UnresolvedError({
          message:
            "All userevent UTxOs in the settlement UTxO are not resolved",
          cause: "Some user events are still unspent",
        }),
      );
    }

    const settlementUTxOWithoutClaim = settlementUTxOs.find(
      ({ datum }) => datum.resolutionClaim === null,
    );
    if (settlementUTxOWithoutClaim) {
      return settlementUTxOWithoutClaim;
    }

    return yield* Effect.fail(
      new LucidError({
        message: "No settlement UTxO without resolution claim found",
        cause: "Settlement Tx not initiated",
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

    const allUTxOs: UTxO[] = yield* Effect.tryPromise({
      try: () => lucid.utxosAt(params.settlementValidator.spendScriptAddress),
      catch: (err) =>
        new LucidError({
          message: "Failed to fetch Settlement UTxOs",
          cause: err,
        }),
    });
    if (allUTxOs.length === 0) {
      yield* new LucidError({
        message: "Failed to build the Settlement transaction",
        cause: "No UTxOs found in Settlement contract address",
      });
    }

    const settlementUTxOs = yield* utxosToSettlementUTxOs(
      allUTxOs,
      params.settlementValidator.policyId,
    );

    const settlementInputUtxo = yield* getSettlementUTxOWithoutClaim(
      settlementUTxOs,
      params.depositUTxOs,
      params.withdrawalUTxOs,
      params.txOrderUTxOs,
      lucid,
    );

    const updatedDatum: SettlementDatum = {
      ...settlementInputUtxo.datum,
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
      .collectFrom([settlementInputUtxo.utxo], spendRedeemerCBOR)
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

export type FetchActiveOperatorParams = {
  activeOperatorsAddress: string;
  operator: string;
  activeOperatorsPolicyId: string;
};

export type UpdateBondHoldNewSettlementParams = {
  newBondUnlockTime: bigint;
  hubOracleValidator: AuthenticatedValidator;
  schedulerValidator: AuthenticatedValidator;
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
  params: UpdateBondHoldNewSettlementParams & FetchActiveOperatorParams,
): Effect.Effect<
  TxBuilder,
  HashingError | DataCoercionError | LucidError | HubOracleError | SchedulerError
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

    const allUtxos: UTxO[] = yield* Effect.tryPromise({
      try: () => lucid.utxosAt(params.activeOperatorsAddress),
      catch: (err) =>
        new LucidError({
          message: "Failed to fetch Active Operators UTxOs",
          cause: err,
        }),
    });
    if (allUtxos.length === 0) {
      yield* new LucidError({
        message: "Failed to build the Active Operators transaction",
        cause: "No UTxOs found in Active Operators Contract address",
      });
    }
    const activeOperatorsUTxOs = yield* utxosToActiveOperatorUTxOs(
      allUtxos,
      params.activeOperatorsPolicyId,
    );

    const activeOperatorsInputUtxo =
      activeOperatorsUTxOs.find((utxo) => utxo.datum.key === params.operator) ??
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
      .pay.ToAddressWithData(params.activeOperatorsAddress, {
        kind: "inline",
        value: updatedDatumCBOR,
      })
      .validTo(txUpperBound);
    return buildUpdateBondHoldNewSettlementTx;
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

export const unsignedAttachResolutionClaimTxProgram = (
  lucid: LucidEvolution,
  attachResolutionParams: AttachResolutionClaimParams,
  params: FetchActiveOperatorParams & UpdateBondHoldNewSettlementParams,
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
  updateBondHoldNewSettlementParams: FetchActiveOperatorParams &
    UpdateBondHoldNewSettlementParams,
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
};
// To be removed from here and should be merged from Preliminary-OffChain-for-Withdrawals or moved to user-events/withdrawal.ts
export type WithdrawalUTxO = {
  utxo: UTxO;
  datum: WithdrawalOrderDatum;
  assetName: string;
  idCbor: Buffer;
  infoCbor: Buffer;
  inclusionTime: Date;
};

export const getWithdrawalDatumFromUTxO = (
  nodeUTxO: UTxO,
): Effect.Effect<WithdrawalOrderDatum, DataCoercionError> => {
  const datumCBOR = nodeUTxO.datum;
  if (datumCBOR) {
    try {
      const withdrawalDatum = Data.from(datumCBOR, WithdrawalOrderDatum);
      return Effect.succeed(withdrawalDatum);
    } catch (e) {
      return Effect.fail(
        new DataCoercionError({
          message: `Could not coerce UTxO's datum to a withdrawal datum`,
          cause: e,
        }),
      );
    }
  } else {
    return Effect.fail(
      new DataCoercionError({
        message: `Withdrawal datum coercion failed`,
        cause: `No datum found`,
      }),
    );
  }
};

/**
 * Validates correctness of datum, and having a single NFT.
 */
export const utxoToWithdrawalUTxO = (
  utxo: UTxO,
  nftPolicy: string,
): Effect.Effect<WithdrawalUTxO, DataCoercionError | UnauthenticUtxoError> =>
  Effect.gen(function* () {
    const datum = yield* getWithdrawalDatumFromUTxO(utxo);
    const [sym, assetName] = yield* getStateToken(utxo.assets);
    if (sym !== nftPolicy) {
      yield* Effect.fail(
        new UnauthenticUtxoError({
          message: "Failed to convert UTxO to `WithdrawalUTxO`",
          cause: "UTxO's NFT policy ID is not the same as the withdrawal's",
        }),
      );
    }
    return {
      utxo,
      datum,
      assetName,
      idCbor: Buffer.from(fromHex(Data.to(datum.event.id, OutputReference))),
      infoCbor: Buffer.from(fromHex(Data.to(datum.event.info, WithdrawalInfo))),
      inclusionTime: new Date(Number(datum.inclusionTime)),
    };
  });

/**
 * Silently drops invalid UTxOs.
 */
export const utxosToWithdrawalUTxOs = (
  utxos: UTxO[],
  nftPolicy: string,
): Effect.Effect<WithdrawalUTxO[]> => {
  const effects = utxos.map((u) => utxoToWithdrawalUTxO(u, nftPolicy));
  return Effect.allSuccesses(effects);
};

export type UserEventUTxO =
  | { eventType: "Deposit"; utxo: UTxO }
  | { eventType: "TxOrder"; utxo: UTxO }
  | { eventType: "Withdrawal"; utxo: UTxO };

export const fetchUserEventRefUTxO = (
  userEventType: EventType,
  userEventAddress: string,
  userEventPolicyId: string,
  lucid: LucidEvolution,
): Effect.Effect<UserEventUTxO, LucidError> =>
  Effect.gen(function* () {
    const allUTxOs = yield* Effect.tryPromise({
      try: () => lucid.utxosAt(userEventAddress),
      catch: (err) =>
        new LucidError({
          message: "Failed to fetch User Event UTxOs",
          cause: err,
        }),
    });

    const getHeadOfList = (list: { utxo: UTxO }[]) =>
      Option.map(EffectArray.head(list), (item) => item.utxo);

    const unresolvedUTxO: Option.Option<UTxO> = yield* userEventType ===
    "Deposit"
      ? utxosToDepositUTxOs(allUTxOs, userEventPolicyId).pipe(
          Effect.map(getHeadOfList),
        )
      : "TxOrder" in userEventType
        ? utxosToTxOrderUTxOs(allUTxOs, userEventPolicyId).pipe(
            Effect.map(getHeadOfList),
          )
        : "Withdrawal" in userEventType
          ? utxosToWithdrawalUTxOs(allUTxOs, userEventPolicyId).pipe(
              Effect.map(getHeadOfList),
            )
          : Effect.fail(
              new LucidError({
                message: "Invalid Event Type",
                cause:
                  "Event Type must be either Deposit or object with TxOrder/Withdrawal",
              }),
            );

    if (Option.isSome(unresolvedUTxO)) {
      return {
        eventType: userEventType,
        utxo: unresolvedUTxO.value,
      } as UserEventUTxO;
    }
    return yield* Effect.fail(
      new LucidError({
        message: "No Unresolved User Event UTxO found",
        cause: `No unspent UTxOs matching the ${
          typeof userEventType === "string"
            ? userEventType
            : Object.keys(userEventType)[0]
        } event type.`,
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

    const allUTxOs: UTxO[] = yield* Effect.tryPromise({
      try: () => lucid.utxosAt(params.settlementAddress),
      catch: (err) =>
        new LucidError({
          message: "Failed to fetch Settlement UTxOs",
          cause: err,
        }),
    });
    if (allUTxOs.length === 0) {
      yield* new LucidError({
        message: "Failed to build the Settlement transaction",
        cause: "No UTxOs found in Settlement contract address",
      });
    }

    const settlementUTxOs = yield* utxosToSettlementUTxOs(
      allUTxOs,
      params.settlementPolicyId,
    );
    const settlementInputUtxo =
      settlementUTxOs.find(
        (utxo) =>
          utxo.datum.resolutionClaim !== null &&
          utxo.datum.resolutionClaim.operator ===
            params.resolutionClaimOperator,
      ) ??
      (() => {
        throw new LucidError({
          message:
            "No settlement UTxO with resolution claim for given operator found",
          cause: "Settlement Tx with resolution claim not initiated",
        });
      })();

    const updatedDatum: SettlementDatum = {
      ...settlementInputUtxo.datum,
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
      settlementInputUtxo.datum.resolutionClaim?.resolutionTime ?? 0n,
    );
    const bufferTime = Date.now() + 2 * 60_000;
    if (resolutionTime < bufferTime) {
      throw new Error("Cannot disprove before resolution time");
    }
    const txUpperBound = resolutionTime - 1 * 60_000;

    const buildsettlementTx = lucid
      .newTx()
      .collectFrom([settlementInputUtxo.utxo], spendRedeemerCBOR)
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
  activeOperatorAddress: string;
  retiredOperatorAddress: string;
  slashedOperatorKey: string;
  activeOperatorPolicyId: string;
  retiredOperatorPolicyId: string;
  activeOperatorMintingPolicy: MintingPolicy;
  fraudProverAddress: string;
  fraudProverDatum: string;
  hubOracleValidator: AuthenticatedValidator;
  eventType: EventType;
  eventAddress: string;
  eventPolicyId: string;
};

export type OperatorNodeUTxO = {
  utxo: ActiveOperatorUTxO | RetiredOperatorUTxO;
  datum: ActiveOperatorDatum | RetiredOperatorDatum;
  status: OperatorStatus;
};

export const getOperatorNodeUTxO = (
  activeOperatorUtxos: ActiveOperatorUTxO[],
  retiredOperatorUtxos: RetiredOperatorUTxO[],
  operatorPKH: string,
): Effect.Effect<OperatorNodeUTxO, DataCoercionError | LucidError> => {
  const searchOperator = (
    utxos: ActiveOperatorUTxO[] | RetiredOperatorUTxO[],
    datum: ActiveOperatorDatum | RetiredOperatorDatum,
    status: "ActiveOperator" | "RetiredOperator",
  ) =>
    Effect.gen(function* () {
      const matchedOperator = EffectArray.findFirst(utxos, (utxo) => {
        if (!utxo.datum) return false;
        try {
          const parsedDatum = Data.from(utxo.utxo.datum!, datum);
          return parsedDatum.key === operatorPKH;
        } catch {
          return false;
        }
      });
      if (Option.isNone(matchedOperator)) {
        return yield* Effect.fail("NotFound");
      }

      const utxo = matchedOperator.value;

      return yield* Effect.try({
        try: () => ({
          utxo,
          datum: Data.from(utxo.utxo.datum!, datum),
          status,
        }),
        catch: (err) =>
          new DataCoercionError({
            message: `Could not coerce UTxO's datum to a ActiveOperator/retiredOperator datum`,
            cause: err,
          }),
      });
    });
  return searchOperator(
    activeOperatorUtxos,
    ActiveOperatorDatum,
    "ActiveOperator",
  ).pipe(
    Effect.orElse(() =>
      searchOperator(
        retiredOperatorUtxos,
        RetiredOperatorDatum,
        "RetiredOperator",
      ),
    ),
    Effect.mapError((err) =>
      err instanceof DataCoercionError
        ? err
        : new LucidError({
            message: `No Operator UTxO with key "${operatorPKH}" found`,
            cause: "Operator not found in active or retired UTxOs",
          }),
    ),
  );
};

export const createMintRedeemerCBOR = (
  operatorInputUTxO: OperatorNodeUTxO,
  slashedOperatorKey: string,
): Effect.Effect<string, LucidError> => {
  if (operatorInputUTxO.status === "ActiveOperator") {
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
  } else if (operatorInputUTxO.status === "RetiredOperator") {
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
        message: `Invalid operator status: ${operatorInputUTxO.status}`,
        cause: "Expected 'ActiveOperator' or 'RetiredOperator'",
      }),
    );
  }
};

export const getOperatorNFT = (
  operatorInputUTxO: OperatorNodeUTxO,
  activeOperatorPolicyId: string,
  retiredOperatorPolicyId: string,
): Effect.Effect<string, LucidError> => {
  if (operatorInputUTxO.status === "ActiveOperator") {
    return Effect.succeed(
      toUnit(activeOperatorPolicyId, operatorInputUTxO.utxo.assetName),
    );
  } else if (operatorInputUTxO.status === "RetiredOperator") {
    return Effect.succeed(
      toUnit(retiredOperatorPolicyId, operatorInputUTxO.utxo.assetName),
    );
  } else
    return Effect.fail(
      new LucidError({
        message: `Invalid operator status: ${operatorInputUTxO.status}`,
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
    const activeOperatorAllUtxos: UTxO[] = yield* Effect.tryPromise({
      try: () => lucid.utxosAt(params.activeOperatorAddress),
      catch: (err) =>
        new LucidError({
          message: "Failed to fetch Active Operators UTxOs",
          cause: err,
        }),
    });
    if (activeOperatorAllUtxos.length === 0) {
      yield* new LucidError({
        message: "Failed to build the Active Operators transaction",
        cause: "No UTxOs found in Active Operators Contract address",
      });
    }
    const activeOperatorUTxOs = yield* utxosToActiveOperatorUTxOs(
      activeOperatorAllUtxos,
      params.activeOperatorPolicyId,
    );

    const retiredOperatorAllUtxos: UTxO[] = yield* Effect.tryPromise({
      try: () => lucid.utxosAt(params.retiredOperatorAddress),
      catch: (err) =>
        new LucidError({
          message: "Failed to fetch Retired Operators UTxOs",
          cause: err,
        }),
    });
    if (retiredOperatorAllUtxos.length === 0) {
      yield* new LucidError({
        message: "Failed to build the Retired Operators transaction",
        cause: "No UTxOs found in Retired Operators Contract address",
      });
    }
    const retiredOperatorUTxOs = yield* utxosToRetiredOperatorUTxOs(
      retiredOperatorAllUtxos,
      params.retiredOperatorPolicyId,
    );

    const operatorInputUTxO = yield* getOperatorNodeUTxO(
      activeOperatorUTxOs,
      retiredOperatorUTxOs,
      params.slashedOperatorKey,
    );
    const mintRedeemerCBOR = yield* createMintRedeemerCBOR(
      operatorInputUTxO,
      params.slashedOperatorKey,
    );
    const bondAmount =
      (operatorInputUTxO.utxo.utxo.assets.lovelace * 60n) / 100n;

    const operatorNFT = yield* getOperatorNFT(
      operatorInputUTxO,
      params.activeOperatorPolicyId,
      params.retiredOperatorPolicyId,
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
      .collectFrom([operatorInputUTxO.utxo.utxo], mintRedeemerCBOR)
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

    const allUTxOs: UTxO[] = yield* Effect.tryPromise({
      try: () => lucid.utxosAt(params.settlementAddress),
      catch: (err) =>
        new LucidError({
          message: "Failed to fetch Settlement UTxOs",
          cause: err,
        }),
    });
    if (allUTxOs.length === 0) {
      yield* new LucidError({
        message: "Failed to build the Settlement transaction",
        cause: "No UTxOs found in Settlement contract address",
      });
    }

    const settlementUTxOs = yield* utxosToSettlementUTxOs(
      allUTxOs,
      params.settlementPolicyId,
    );

    const settlementInputUtxo =
      settlementUTxOs.find((utxo) => utxo.datum.resolutionClaim !== null) ??
      (() => {
        throw new LucidError({
          message: "No settlement UTxO with resolution claim found",
          cause: "Settlement Tx not initiated",
        });
      })();

    const resolutionTime = Number(
      settlementInputUtxo.datum.resolutionClaim?.resolutionTime ?? 0n,
    );
    const txLowerBound = resolutionTime + 1 * 60_000;
    const txSigner = settlementInputUtxo.datum.resolutionClaim?.operator!;
    const changeAmount = 1_000_000n;

    const settlementNFT = toUnit(
      params.settlementPolicyId,
      settlementInputUtxo.assetName,
    );

    const buildsettlementTx = lucid
      .newTx()
      .collectFrom([settlementInputUtxo.utxo], spendRedeemerCBOR)
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
