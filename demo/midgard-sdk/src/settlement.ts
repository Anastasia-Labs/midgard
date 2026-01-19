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
  getSingleAssetApartFromAda,
  getStateToken,
  HashingError,
  LucidError,
  makeReturn,
  MerkleRoot,
  MerkleRootSchema,
  OutputReference,
  POSIXTimeSchema,
  Proof,
  ProofSchema,
  UnauthenticUtxoError,
  UnresolvedError,
  VerificationKeyHashSchema,
} from "@/common.js";
import {
  Data as EffectData,
  Array as EffectArray,
  Effect,
  Option,
} from "effect";
import { HubOracleUTxO, utxosToHubOracleUTxOs } from "@/hub-oracle.js";
import { SchedulerUTxO, utxosToSchedulerUTxOs } from "@/scheduler.js";
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
} from "@/active-operators.js";
import {
  RetiredOperatorDatum,
  RetiredOperatorMintRedeemer,
} from "./retired-operators.js";

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
  settlementAddress: string;
  resolutionClaimOperator: string;
  newBondUnlockTime: bigint;
  hubOracleValidator: AuthenticatedValidator;
  schedulerScriptAddress: string;
  schedulerPolicyId: string;
  depositsRoot: MerkleRoot;
  withdrawalsRoot: MerkleRoot;
  transactionsRoot: MerkleRoot;
  depositUTxOs: UTxO[];
  withdrawalUTxOs: UTxO[];
  txOrderUTxOs: UTxO[];
};

export type SettlementUTxO = {
  utxo: UTxO;
  datum: SettlementDatum;
};

export const getSettlementUTxOWithoutClaim = (
  settlementUTxOs: UTxO[],
  params: AttachResolutionClaimParams,
  lucid: LucidEvolution,
): Effect.Effect<
  SettlementUTxO,
  DataCoercionError | LucidError | UnresolvedError
> =>
  Effect.gen(function* () {
    const allEvents = [
      ...params.depositUTxOs,
      ...params.withdrawalUTxOs,
      ...params.txOrderUTxOs,
    ];
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
    const results = yield* Effect.forEach(settlementUTxOs, (utxo) =>
      Effect.try({
        try: () => {
          if (!utxo.datum) throw new Error("No datum");
          const parsedDatum = Data.from(utxo.datum, SettlementDatum);
          return { utxo, datum: parsedDatum };
        },
        catch: (err) =>
          new DataCoercionError({
            message: "Could not coerce UTxO's datum to a settlement datum",
            cause: err,
          }),
      }).pipe(Effect.either),
    );
    const allSuccesses = yield* Effect.allSuccesses(results);

    const settlementUTxOWithoutClaim = allSuccesses.find(
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

const fetchHubOracleRefUTxO = (
  hubOracleScriptAddress: string,
  hubOraclePolicyId: string,
  lucid: LucidEvolution,
): Effect.Effect<HubOracleUTxO, LucidError> =>
  Effect.gen(function* () {
    const allUTxOs = yield* Effect.tryPromise({
      try: () => lucid.utxosAt(hubOracleScriptAddress),
      catch: (err) =>
        new LucidError({
          message: "Failed to fetch Hub Oracle UTxOs",
          cause: err,
        }),
    });
    const hubOracleRefUTxOs = yield* utxosToHubOracleUTxOs(
      allUTxOs,
      hubOraclePolicyId,
    );
    if (hubOracleRefUTxOs.length === 0) {
      yield* new LucidError({
        message: "Failed to build the HubOracle transaction",
        cause: "No UTxOs found in Hub Oracle contract address",
      });
    }
    return hubOracleRefUTxOs[0];
  });

const fetchSchedulerRefUTxO = (
  schedulerScriptAddress: string,
  schedulerPolicyId: string,
  lucid: LucidEvolution,
): Effect.Effect<SchedulerUTxO, LucidError> =>
  Effect.gen(function* () {
    const allUTxOs = yield* Effect.tryPromise({
      try: () => lucid.utxosAt(schedulerScriptAddress),
      catch: (err) =>
        new LucidError({
          message: "Failed to fetch Scheduler UTxOs",
          cause: err,
        }),
    });
    const schedulerRefUTxOs = yield* utxosToSchedulerUTxOs(
      allUTxOs,
      schedulerPolicyId,
    );
    if (schedulerRefUTxOs.length === 0) {
      yield* new LucidError({
        message: "Failed to build the Scheduler transaction",
        cause: "No UTxOs found in Scheduler contract address",
      });
    }
    return schedulerRefUTxOs[0];
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
  HashingError | DataCoercionError | LucidError | UnresolvedError
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

    const settlementAllUtxos: UTxO[] = yield* Effect.tryPromise({
      try: () => lucid.utxosAt(params.settlementAddress),
      catch: (err) =>
        new LucidError({
          message: "Failed to fetch Settlement UTxOs",
          cause: err,
        }),
    });
    if (settlementAllUtxos.length === 0) {
      yield* new LucidError({
        message: "Failed to build the Settlement transaction",
        cause: "No UTxOs found in Settlement contract address",
      });
    }
    const settlementInputUtxo = yield* getSettlementUTxOWithoutClaim(
      settlementAllUtxos,
      params,
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

    const hubOracleRefUTxO = yield* fetchHubOracleRefUTxO(
      params.hubOracleValidator.spendScriptAddress,
      params.hubOracleValidator.policyId,
      lucid,
    );

    const schedulerRefUTxO = yield* fetchSchedulerRefUTxO(
      params.schedulerScriptAddress,
      params.schedulerPolicyId,
      lucid,
    );

    const txUpperBound = Date.now() + 2 * 60_000;

    const buildsettlementTx = lucid
      .newTx()
      .collectFrom([settlementInputUtxo.utxo], spendRedeemerCBOR)
      .readFrom([hubOracleRefUTxO.utxo])
      .readFrom([schedulerRefUTxO.utxo])
      .pay.ToAddressWithData(params.settlementAddress, {
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
};

export type UpdateBondHoldNewSettlementParams = {
  newBondUnlockTime: bigint;
  hubOracleValidator: AuthenticatedValidator;
  schedulerScriptAddress: string;
  schedulerPolicyId: string;
};

export type ActiveOperatorNodeUTxO = {
  utxo: UTxO;
  datum: ActiveOperatorDatum;
};

export const getActiveOperatorNodeUTxO = (
  activeOperatorsUTxOs: UTxO[],
  params: FetchActiveOperatorParams,
): Effect.Effect<ActiveOperatorNodeUTxO, DataCoercionError | LucidError> =>
  Effect.gen(function* () {
    const results = yield* Effect.forEach(activeOperatorsUTxOs, (utxo) =>
      Effect.try({
        try: () => {
          if (!utxo.datum) throw new Error("No datum present");
          const parsedDatum = Data.from(utxo.datum, ActiveOperatorDatum);
          return { utxo, datum: parsedDatum };
        },
        catch: (err) =>
          new DataCoercionError({
            message: `Could not coerce UTxO's datum to an active operator's node datum`,
            cause: err,
          }),
      }).pipe(Effect.either),
    );
    const allSuccesses = yield* Effect.allSuccesses(results);
    const activeOperatorNode = allSuccesses.find(
      ({ datum }) => datum.key === params.operator,
    );

    if (activeOperatorNode) {
      return activeOperatorNode;
    }
    return yield* Effect.fail(
      new LucidError({
        message: "No Active Operator UTxO with given operator found",
        cause: "Active Operators Tx not initiated",
      }),
    );
  });

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
): Effect.Effect<TxBuilder, HashingError | DataCoercionError | LucidError> =>
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

    const activeOperatorsUtxos: UTxO[] = yield* Effect.tryPromise({
      try: () => lucid.utxosAt(params.activeOperatorsAddress),
      catch: (err) =>
        new LucidError({
          message: "Failed to fetch Active Operators UTxOs",
          cause: err,
        }),
    });
    if (activeOperatorsUtxos.length === 0) {
      yield* new LucidError({
        message: "Failed to build the Active Operators transaction",
        cause: "No UTxOs found in Active Operators Contract address",
      });
    }
    const activeOperatorsInputUtxo = yield* getActiveOperatorNodeUTxO(
      activeOperatorsUtxos,
      params,
    );

    const updatedDatum: ActiveOperatorDatum = {
      ...activeOperatorsInputUtxo.datum,
      bondUnlockTime: params.newBondUnlockTime,
    };
    const updatedDatumCBOR = Data.to(updatedDatum, ActiveOperatorDatum);

    const hubOracleRefUTxO = yield* fetchHubOracleRefUTxO(
      params.hubOracleValidator.spendScriptAddress,
      params.hubOracleValidator.policyId,
      lucid,
    );

    const schedulerRefUTxO = yield* fetchSchedulerRefUTxO(
      params.schedulerScriptAddress,
      params.schedulerPolicyId,
      lucid,
    );

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
export const getSettlementUTxOWithClaim = (
  settlementUTxOs: UTxO[],
  resolutionClaimOperator: string,
): Effect.Effect<SettlementUTxO, DataCoercionError | LucidError> =>
  Effect.gen(function* () {
    const filteredUTxOs = yield* Effect.forEach(settlementUTxOs, (utxo) =>
      Effect.try({
        try: () => {
          if (!utxo.datum) throw new Error("No datum");
          const parsedDatum = Data.from(utxo.datum, SettlementDatum);
          if (
            parsedDatum.resolutionClaim !== null &&
            parsedDatum.resolutionClaim.operator === resolutionClaimOperator
          ) {
            return { utxo, datum: parsedDatum };
          }
          return null;
        },
        catch: (err) =>
          new DataCoercionError({
            message: `Could not coerce UTxO's datum to a settlement datum`,
            cause: err,
          }),
      }).pipe(Effect.either),
    );

    const settlementUTxOsWithClaim = (yield* Effect.allSuccesses(
      filteredUTxOs,
    )).filter((item): item is SettlementUTxO => item !== null);
    const settlementUTxOWithClaim = EffectArray.head(settlementUTxOsWithClaim);

    if (Option.isSome(settlementUTxOWithClaim)) {
      return settlementUTxOWithClaim.value;
    }

    return yield* Effect.fail(
      new LucidError({
        message: "No settlement UTxO with a resolution claim found",
        cause: "No resolution claims attached",
      }),
    );
  });

export type DisproveResolutionClaimParams = {
  settlementAddress: string;
  resolutionClaimOperator: string;
  membershipProof: Proof;
  hubOracleValidator: AuthenticatedValidator;
  schedulerScriptAddress: string;
  schedulerPolicyId: string;
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

// Have to include the logic to find one userevent Utxo that is not resolved yet
// deposit roots is just a hash, how to check a specific utxo that is not resolved?
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
    if (userEventType === "Deposit") {
      const depositRefUTxOs = yield* utxosToDepositUTxOs(
        allUTxOs,
        userEventPolicyId,
      );
      if (depositRefUTxOs.length === 0) {
        yield* Effect.fail(
          new LucidError({
            message: "Failed to build the User Event transaction",
            cause: "No UTxOs found in User Event contract address",
          }),
        );
      }
      return { eventType: "Deposit", utxo: depositRefUTxOs[0].utxo };
    } else if ("TxOrder" in userEventType) {
      const txOrderRefUTxOs = yield* utxosToTxOrderUTxOs(
        allUTxOs,
        userEventPolicyId,
      );
      if (txOrderRefUTxOs.length === 0) {
        yield* Effect.fail(
          new LucidError({
            message: "Failed to build the User Event transaction",
            cause: "No UTxOs found in User Event contract address",
          }),
        );
      }
      return { eventType: "TxOrder", utxo: txOrderRefUTxOs[0].utxo };
    } else if ("Withdrawal" in userEventType) {
      const withdrawalRefUTxOs = yield* utxosToWithdrawalUTxOs(
        allUTxOs,
        userEventPolicyId,
      );
      if (withdrawalRefUTxOs.length === 0) {
        yield* Effect.fail(
          new LucidError({
            message: "Failed to build the User Event transaction",
            cause: "No UTxOs found in User Event contract address",
          }),
        );
      }
      return { eventType: "Withdrawal", utxo: withdrawalRefUTxOs[0].utxo };
    } else {
      return yield* Effect.fail(
        new LucidError({
          message: "Invalid Event Type",
          cause:
            "Event Type must be either Deposit or object with TxOrder/Withdrawal",
        }),
      );
    }
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
): Effect.Effect<TxBuilder, HashingError | DataCoercionError | LucidError> =>
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

    const settlementAllUtxos: UTxO[] = yield* Effect.tryPromise({
      try: () => lucid.utxosAt(params.settlementAddress),
      catch: (err) =>
        new LucidError({
          message: "Failed to fetch Settlement UTxOs",
          cause: err,
        }),
    });
    if (settlementAllUtxos.length === 0) {
      yield* new LucidError({
        message: "Failed to build the Settlement transaction",
        cause: "No UTxOs found in Settlement contract address",
      });
    }
    const settlementInputUtxo = yield* getSettlementUTxOWithClaim(
      settlementAllUtxos,
      params.resolutionClaimOperator,
    );

    const updatedDatum: SettlementDatum = {
      ...settlementInputUtxo.datum,
      resolutionClaim: null,
    };
    const updatedDatumCBOR = Data.to(updatedDatum, SettlementDatum);

    const hubOracleRefUTxO = yield* fetchHubOracleRefUTxO(
      params.hubOracleValidator.spendScriptAddress,
      params.hubOracleValidator.policyId,
      lucid,
    );

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
  slashedOeratorKey: string;
  activeOperatorNFT: string;
  retiredOperatorNFT: string;
  activeOperatorMintingPolicy: MintingPolicy;
  fraudProverAddress: string;
  fraudProverDatum: string;
  hubOracleValidator: AuthenticatedValidator;
  eventType: EventType;
  eventAddress: string;
  eventPolicyId: string;
};

export type OperatorNodeUTxO = {
  utxo: UTxO;
  datum: ActiveOperatorDatum | RetiredOperatorDatum;
  status: OperatorStatus;
};

export const getOperatorNodeUTxO = (
  activeOperatorAllUtxos: UTxO[],
  retiredOperatorAllUtxos: UTxO[],
  operatorPKH: string,
): Effect.Effect<OperatorNodeUTxO, DataCoercionError | LucidError> => {
  const searchOperator = (utxos: UTxO[], datum: ActiveOperatorDatum | RetiredOperatorDatum, status: "ActiveOperator" | "RetiredOperator") =>
    Effect.gen(function* () {
      const matchedOperator = EffectArray.findFirst(utxos, (utxo) => {
        if (!utxo.datum) return false
        try {
          const parsedDatum = Data.from(utxo.datum, datum);
          return parsedDatum.key === operatorPKH;
        } catch { return false }
      });
      if (Option.isNone(matchedOperator)) {
      return yield* Effect.fail("NotFound");
    }

    const utxo = matchedOperator.value;

    return yield* Effect.try({
      try: () => ({
        utxo,
        datum: Data.from(utxo.datum!, datum),
        status
      }),
      catch: (err) => new DataCoercionError({
        message: `Could not coerce UTxO's datum to a ActiveOperator/retiredOperator datum`,
        cause: err
      })
    });
  });
  return searchOperator(activeOperatorAllUtxos, ActiveOperatorDatum, "ActiveOperator").pipe(
    Effect.orElse(() => 
      searchOperator(retiredOperatorAllUtxos, RetiredOperatorDatum, "RetiredOperator")
    ),
    Effect.mapError((err) => 
      err instanceof DataCoercionError 
    ? err
    : new LucidError({
           message: `No Operator UTxO with key "${operatorPKH}" found`,
           cause: "Operator not found in active or retired UTxOs",
       })
    )
  );
};

export const createMintRedeemerCBOR = (
  operatorInputUTxO: OperatorNodeUTxO,
  params: RemoveOperatorBadSettlementParams,
): Effect.Effect<string, LucidError> => {
  if (operatorInputUTxO.status === "ActiveOperator") {
    const mintRedeemer: ActiveOperatorMintRedeemer = {
      RemoveOperatorBadSettlement: {
        slashedActiveOperatorKey: params.slashedOeratorKey,
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
        slashedRetiredOperatorKey: params.slashedOeratorKey,
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
  params: RemoveOperatorBadSettlementParams,
): Effect.Effect<string, LucidError> => {
  if (operatorInputUTxO.status === "ActiveOperator") {
    return Effect.succeed(params.activeOperatorNFT);
  } else if (operatorInputUTxO.status === "RetiredOperator") {
    return Effect.succeed(params.retiredOperatorNFT);
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
): Effect.Effect<TxBuilder, HashingError | DataCoercionError | LucidError> =>
  Effect.gen(function* () {
    const activeOperatorAllUtxos: UTxO[] = yield* Effect.tryPromise({
      try: () => lucid.utxosAt(params.activeOperatorAddress),
      catch: (err) =>
        new LucidError({
          message: "Failed to fetch Active Operator UTxOs",
          cause: err,
        }),
    });
    if (activeOperatorAllUtxos.length === 0) {
      yield* new LucidError({
        message: "Failed to build the Active Operator node transaction",
        cause: "No UTxOs found in Active Operator contract address",
      });
    }

    const retiredOperatorAllUtxos: UTxO[] = yield* Effect.tryPromise({
      try: () => lucid.utxosAt(params.retiredOperatorAddress),
      catch: (err) =>
        new LucidError({
          message: "Failed to fetch Retired Operator UTxOs",
          cause: err,
        }),
    });
    if (retiredOperatorAllUtxos.length === 0) {
      yield* new LucidError({
        message: "Failed to build the Retired Operator node transaction",
        cause: "No UTxOs found in Retired Operator contract address",
      });
    }
    const operatorInputUTxO = yield* getOperatorNodeUTxO(
      activeOperatorAllUtxos,
      retiredOperatorAllUtxos,
      params.slashedOeratorKey,
    );
    const mintRedeemerCBOR = yield* createMintRedeemerCBOR(
      operatorInputUTxO,
      params,
    );
    const bondAmount = (operatorInputUTxO.utxo.assets.lovelace * 60n) / 100n;
    const operatorNFT = yield* getOperatorNFT(operatorInputUTxO, params);

    const hubOracleRefUTxO = yield* fetchHubOracleRefUTxO(
      params.hubOracleValidator.spendScriptAddress,
      params.hubOracleValidator.policyId,
      lucid,
    );

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
      .collectFrom([operatorInputUTxO.utxo])
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
  HashingError | DataCoercionError | LucidError | SettlementError
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

export const getSettlementUTxOWithoutClaimResolve = (
  settlementUTxOs: UTxO[],
): Effect.Effect<SettlementUTxO, DataCoercionError | LucidError> => {
  return Effect.gen(function* () {
    const results = yield* Effect.forEach(settlementUTxOs, (utxo) =>
      Effect.try({
        try: () => {
          if (!utxo.datum) throw new Error("No datum");
          const parsedDatum = Data.from(utxo.datum, SettlementDatum);
          return { utxo, datum: parsedDatum };
        },
        catch: (err) =>
          new DataCoercionError({
            message: "Could not coerce UTxO's datum to a settlement datum",
            cause: err,
          }),
      }).pipe(Effect.either),
    );
    const allSuccesses = yield* Effect.allSuccesses(results);

    const settlementUTxOWithoutClaim = allSuccesses.find(
      ({ datum }) => datum.resolutionClaim !== null,
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

    const settlementAllUtxos: UTxO[] = yield* Effect.tryPromise({
      try: () => lucid.utxosAt(params.settlementAddress),
      catch: (err) =>
        new LucidError({
          message: "Failed to fetch Settlement UTxOs",
          cause: err,
        }),
    });
    if (settlementAllUtxos.length === 0) {
      yield* new LucidError({
        message: "Failed to build the Settlement transaction",
        cause: "No UTxOs found in Settlement contract address",
      });
    }
    const settlementInputUtxo =
      yield* getSettlementUTxOWithoutClaimResolve(settlementAllUtxos);

    const resolutionTime = Number(
      settlementInputUtxo.datum.resolutionClaim?.resolutionTime ?? 0n,
    );
    const txLowerBound = resolutionTime + 1 * 60_000;
    const txSigner = settlementInputUtxo.datum.resolutionClaim?.operator!;
    const changeAmount = 1_000_000n;

    const [policyId, assetName] = yield* getSingleAssetApartFromAda(
      settlementInputUtxo.utxo.assets,
    );
    const settlementNFT = toUnit(policyId, assetName);
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
