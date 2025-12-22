import {
  Data,
  fromHex,
  LucidEvolution,
  MintingPolicy,
  TxBuilder,
  TxSignBuilder,
  UTxO,
} from "@lucid-evolution/lucid";
import {
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
  ProofStep,
  UnauthenticUtxoError,
  VerificationKeyHashSchema,
} from "./common.js";
import { Data as EffectData, Effect } from "effect";
import { HubOracleUTxO, utxosToHubOracleUTxOs } from "./hub-oracle.js";
import { SchedulerUTxO, utxosToSchedulerUTxOs } from "./scheduler.js";
import { DepositUTxO, utxosToDepositUTxOs } from "./user-events/deposit.js";
import { TxOrderUTxO, utxosToTxOrderUTxOs } from "./user-events/tx-order.js";
import { WithdrawalOrderDatum } from "./user-events/withdrawal.js";
import { WithdrawalInfo } from "./ledger-state.js";
import { getProtocolParameters } from "./protocol-parameters.js";

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
  Data.Literal("Withdrawal"),
  Data.Literal("TxOrder"),
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

export const ActiveOperatorSpendRedeemerSchema = Data.Enum([
  Data.Literal("ListStateTransition"),
  Data.Object({
    UpdateBondHoldNewState: Data.Object({
      activeNodeOutputIndex: Data.Integer(),
      hubOracleRefInputIndex: Data.Integer(),
      stateQueueRedeemerIndex: Data.Integer(),
    }),
  }),
  Data.Object({
    UpdateBondHoldNewSettlement: Data.Object({
      activeNodeOutputIndex: Data.Integer(),
      hubOracleRefInputIndex: Data.Integer(),
      settlementQueueInputIndex: Data.Integer(),
      settlementQueueRedeemerIndex: Data.Integer(),
      newBondUnlockTime: POSIXTimeSchema,
    }),
  }),
]);
export type ActiveOperatorSpendRedeemer = Data.Static<
  typeof ActiveOperatorSpendRedeemerSchema
>;
export const ActiveOperatorSpendRedeemer =
  ActiveOperatorSpendRedeemerSchema as unknown as ActiveOperatorSpendRedeemer;

export const ActiveOperatorMintRedeemerSchema = Data.Enum([
  Data.Literal("Init"),
  Data.Literal("Deinit"),
  Data.Object({
    ActivateOperator: Data.Object({
      newActiveOperatorKey: Data.Bytes(),
      hubOracleRefInputIndex: Data.Integer(),
      activeOperatorAppendedNodeOutputIndex: Data.Integer(),
      activeOperatorAnchorNodeOutputIndex: Data.Integer(),
      registeredOperatorsRedeemerIndex: Data.Integer(),
    }),
  }),
  Data.Object({
    RemoveOperatorBadState: Data.Object({
      slashedActiveOperatorKey: Data.Bytes(),
      hubOracleRefInputIndex: Data.Integer(),
      activeOperatorSlashedNodeInputIndex: Data.Integer(),
      activeOperatorAnchorNodeInputIndex: Data.Integer(),
      stateQueueRedeemerIndex: Data.Integer(),
    }),
  }),
  Data.Object({
    RemoveOperatorBadSettlement: Data.Object({
      slashedActiveOperatorKey: Data.Bytes(),
      hubOracleRefInputIndex: Data.Integer(),
      activeOperatorSlashedNodeInputIndex: Data.Integer(),
      activeOperatorAnchorNodeInputIndex: Data.Integer(),
      settlementInputIndex: Data.Integer(),
      settlementRedeemerIndex: Data.Integer(),
    }),
  }),
  Data.Object({
    RetireOperator: Data.Object({
      activeOperatorKey: Data.Bytes(),
      hubOracleRefInputIndex: Data.Integer(),
      activeOperatorRemovedNodeInputIndex: Data.Integer(),
      activeOperatorAnchorNodeInputIndex: Data.Integer(),
      retiredOperatorInsertedNodeOutputIndex: Data.Integer(),
      retiredOperatorsRedeemerIndex: Data.Integer(),
    }),
  }),
]);
export type ActiveOperatorMintRedeemer = Data.Static<
  typeof ActiveOperatorMintRedeemerSchema
>;
export const ActiveOperatorMintRedeemer =
  ActiveOperatorMintRedeemerSchema as unknown as ActiveOperatorMintRedeemer;

export const ActiveOperatorSpendDatumSchema = Data.Object({
  key: Data.Nullable(Data.Bytes()),
  link: Data.Nullable(Data.Bytes()),
  bondUnlockTime: Data.Nullable(POSIXTimeSchema),
});
export type ActiveOperatorSpendDatum = Data.Static<
  typeof ActiveOperatorSpendDatumSchema
>;
export const ActiveOperatorSpendDatum =
  ActiveOperatorSpendDatumSchema as unknown as ActiveOperatorSpendDatum;

export type AttachResolutionClaimParams = {
  settlementAddress: string;
  resolutionClaimOperator: string;
  newBondUnlockTime: bigint;
  hubOracleValidator: AuthenticatedValidator;
  schedulerScriptAddress: string;
  schedulerPolicyId: string;
};

export type SettlementUTxO = {
  utxo: UTxO;
  datum: SettlementDatum;
};

export const getSettlementUTxOWithoutClaim = (
  settlementUTxOs: UTxO[],
): Effect.Effect<SettlementUTxO, DataCoercionError | LucidError> => {
  for (const utxo of settlementUTxOs) {
    const datumCBOR = utxo.datum;
    if (!datumCBOR) {
      continue;
    }
    try {
      const parsedDatum = Data.from(datumCBOR, SettlementDatum);
      if (parsedDatum.resolutionClaim === null) {
        return Effect.succeed({ utxo, datum: parsedDatum });
      }
    } catch (err) {
      return Effect.fail(
        new DataCoercionError({
          message: `Could not coerce UTxO's datum to a settlement datum`,
          cause: err,
        }),
      );
    }
  }
  return Effect.fail(
    new LucidError({
      message: "No settlement UTxO without resolution claim found",
      cause: "Settlement Tx not initiated",
    }),
  );
};

const fetchHubOracleRefUTxOs = (
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
    if (allUTxOs.length === 0) {
      yield* new LucidError({
        message: "Failed to build the HubOracle transaction",
        cause: "No UTxOs found in Hub Oracle contract address",
      });
    }
    const hubOracleRefUTxOs = yield* utxosToHubOracleUTxOs(allUTxOs, hubOraclePolicyId);
    return hubOracleRefUTxOs[0];
  });

const fetchSchedulerRefUTxOs = (
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
    if (allUTxOs.length === 0) {
      yield* new LucidError({
        message: "Failed to build the Scheduler transaction",
        cause: "No UTxOs found in Scheduler contract address",
      });
    }
    const schedulerRefUTxO = yield* utxosToSchedulerUTxOs(allUTxOs, schedulerPolicyId);
    return schedulerRefUTxO[0];
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
): Effect.Effect<TxBuilder, HashingError | DataCoercionError | LucidError> =>
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
    const settlementInputUtxo =
      yield* getSettlementUTxOWithoutClaim(settlementAllUtxos);

    const spendDatum: SettlementDatum = {
      depositsRoot: settlementInputUtxo.datum.depositsRoot,
      withdrawalsRoot: settlementInputUtxo.datum.withdrawalsRoot,
      transactionsRoot: settlementInputUtxo.datum.transactionsRoot,
      resolutionClaim: {
        resolutionTime: params.newBondUnlockTime,
        operator: params.resolutionClaimOperator,
      },
    };
    const spendDatumCBOR = Data.to(spendDatum, SettlementDatum);

    const hubOracleRefUTxO = yield* fetchHubOracleRefUTxOs(
      params.hubOracleValidator.spendScriptAddress,
      params.hubOracleValidator.policyId,
      lucid,
    );

    const schedulerRefUTxO = yield* fetchSchedulerRefUTxOs(
      params.schedulerScriptAddress,
      params.schedulerPolicyId,
      lucid,
    );

    const txUpperBound = Date.now() + 2000;

    const buildsettlementTx = lucid
      .newTx()
      .collectFrom([settlementInputUtxo.utxo], spendRedeemerCBOR)
      .readFrom([hubOracleRefUTxO.utxo])
      .readFrom([schedulerRefUTxO.utxo])
      .pay.ToAddressWithData(params.settlementAddress, {
        kind: "inline",
        value: spendDatumCBOR,
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

export type ActiveOperatorsParams = {
  activeOperatorsAddress: string;
  operator: string;
  newBondUnlockTime: bigint;
  hubOracleValidator: AuthenticatedValidator;
  schedulerScriptAddress: string;
  schedulerPolicyId: string;
};

export type ActiveOperatorNodeUTxO = {
  utxo: UTxO;
  datum: ActiveOperatorSpendDatum;
};

export const getActiveOperatorNodeUTxO = (
  activeOperatorsUTxOs: UTxO[],
  params: ActiveOperatorsParams,
): Effect.Effect<ActiveOperatorNodeUTxO, DataCoercionError | LucidError> => {
  for (const utxo of activeOperatorsUTxOs) {
    const datumCBOR = utxo.datum;
    if (!datumCBOR) {
      continue;
    }
    try {
      const parsedDatum = Data.from(datumCBOR, ActiveOperatorSpendDatum);
      if (parsedDatum.key === params.operator) {
        return Effect.succeed({ utxo, datum: parsedDatum });
      }
    } catch (err) {
      return Effect.fail(
        new DataCoercionError({
          message: `Could not coerce UTxO's datum to an active operator's node datum`,
          cause: err,
        }),
      );
    }
  }
  return Effect.fail(
    new LucidError({
      message: "No Active Operator UTxO with given operator found",
      cause: "Active Operators Tx not initiated not initiated",
    }),
  );
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
  params: ActiveOperatorsParams,
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

    const spendDatum: ActiveOperatorSpendDatum = {
      ...activeOperatorsInputUtxo.datum,
      bondUnlockTime: params.newBondUnlockTime,
    };
    const spendDatumCBOR = Data.to(spendDatum, ActiveOperatorSpendDatum);

    const hubOracleRefUTxO = yield* fetchHubOracleRefUTxOs(
      params.hubOracleValidator.spendScriptAddress,
      params.hubOracleValidator.policyId,
      lucid,
    );

    const schedulerRefUTxO = yield* fetchSchedulerRefUTxOs(
      params.schedulerScriptAddress,
      params.schedulerPolicyId,
      lucid,
    );

    const txUpperBound = Date.now() + 2000;

    const buildUpdateBondHoldNewSettlementTx = lucid
      .newTx()
      .collectFrom([activeOperatorsInputUtxo.utxo], spendRedeemerCBOR)
      .readFrom([hubOracleRefUTxO.utxo])
      .readFrom([schedulerRefUTxO.utxo])
      .pay.ToAddressWithData(params.activeOperatorsAddress, {
        kind: "inline",
        value: spendDatumCBOR,
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
  activeOperatorParams: ActiveOperatorsParams,
): Effect.Effect<
  TxSignBuilder,
  HashingError | DataCoercionError | LucidError | SettlementError
> =>
  Effect.gen(function* () {
    const attachResolutionClaimTx = yield* incompleteAttachResolutionClaimTxProgram(
      lucid,
      attachResolutionParams,
    );
    const updateBondHoldNewSettlementTx = yield* incompleteUpdateBondHoldNewSettlementTxProgram(
      lucid,
      activeOperatorParams,
    );
    const composedTx = attachResolutionClaimTx.compose(updateBondHoldNewSettlementTx);
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
 * @param activeOperatorParams - Parameters required for selecting active operator.
 * @returns A promise that resolves to a `TxSignBuilder` instance.
 */
export const unsignedAttachResolutionClaimTx = (
  lucid: LucidEvolution,
  attachResolutionParams: AttachResolutionClaimParams,
  activeOperatorParams: ActiveOperatorsParams,
): Promise<TxSignBuilder> =>
  makeReturn(
    unsignedAttachResolutionClaimTxProgram(
      lucid,
      attachResolutionParams,
      activeOperatorParams,
    ),
  ).unsafeRun();

export class SettlementError extends EffectData.TaggedError(
  "SettlementError",
)<GenericErrorFields> {}

export const getSettlementUTxOWithClaim = (
  settlementUTxOs: UTxO[],
  resolutionClaimOperator: string
): Effect.Effect<SettlementUTxO, DataCoercionError | LucidError> => {
  for (const utxo of settlementUTxOs) {
    const datumCBOR = utxo.datum;
    if (!datumCBOR) {
      continue;
    }
    try {
      const parsedDatum = Data.from(datumCBOR, SettlementDatum);
      if (parsedDatum.resolutionClaim !== null && parsedDatum.resolutionClaim.operator === resolutionClaimOperator) {
        return Effect.succeed({ utxo, datum: parsedDatum });
      }
    } catch (err) {
      return Effect.fail(
        new DataCoercionError({
          message: `Could not coerce UTxO's datum to a settlement datum`,
          cause: err,
        }),
      );
    }
  }
  return Effect.fail(
    new LucidError({
      message: "No settlement UTxO without resolution claim found",
      cause: "Settlement Tx not initiated",
    }),
  );
};

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
}
// To be removed from here and should be merged from Preliminary-OffChain-for-Withdrawals
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

export const fetchUserEventRefUTxOs = (
  userEventType:EventType,
  userEventAddress: string,
  userEventPolicyId: string,
  lucid: LucidEvolution,
): Effect.Effect<DepositUTxO | TxOrderUTxO | WithdrawalUTxO, LucidError> =>
  Effect.gen(function* () {
    const allUTxOs = yield* Effect.tryPromise({
      try: () => lucid.utxosAt(userEventAddress),
      catch: (err) =>
        new LucidError({
          message: "Failed to fetch User Event UTxOs",
          cause: err,
        }),
    });
    if (allUTxOs.length === 0) {
      yield* new LucidError({
        message: "Failed to build the User Event transaction",
        cause: "No UTxOs found in User Event contract address",
      });
    }
    if(userEventType === "Deposit"){
    const depositRefUTxOs = yield* utxosToDepositUTxOs(allUTxOs, userEventPolicyId)
    return depositRefUTxOs[0];
    }
    else if(userEventType === "TxOrder"){
    const txOrderRefUTxOs = yield* utxosToTxOrderUTxOs(allUTxOs, userEventPolicyId)
    return txOrderRefUTxOs[0]; 
    }
    else {
    const withdrawalRefUTxOs = yield* utxosToWithdrawalUTxOs(allUTxOs, userEventPolicyId)
    return withdrawalRefUTxOs[0];
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
    const settlementInputUtxo =
      yield* getSettlementUTxOWithClaim(settlementAllUtxos,params.resolutionClaimOperator);

    const spendDatum: SettlementDatum = {
      depositsRoot: settlementInputUtxo.datum.depositsRoot,
      withdrawalsRoot: settlementInputUtxo.datum.withdrawalsRoot,
      transactionsRoot: settlementInputUtxo.datum.transactionsRoot,
      resolutionClaim: null
    };
    const spendDatumCBOR = Data.to(spendDatum, SettlementDatum);

    const hubOracleRefUTxO = yield* fetchHubOracleRefUTxOs(
      params.hubOracleValidator.spendScriptAddress,
      params.hubOracleValidator.policyId,
      lucid,
    );
    // placeholder for the user event UTxO
    //const userEventRefUTxO :UTxO = yield* fetchUserEventRefUTxOs(params.eventType,params.eventAddress,params.eventPolicyId,lucid);
    const userEventRefUTxO = yield* fetchUserEventRefUTxOs(
      params.eventType,
      params.eventAddress,
      params.eventPolicyId,
      lucid,
    );
    const txUpperBound = Number(settlementInputUtxo.datum.resolutionClaim?.resolutionTime??0n - 2000n);

    const buildsettlementTx = lucid
      .newTx()
      .collectFrom([settlementInputUtxo.utxo], spendRedeemerCBOR)
      .readFrom([hubOracleRefUTxO.utxo])
      .readFrom([userEventRefUTxO.utxo])
      .pay.ToAddressWithData(params.settlementAddress, {
        kind: "inline",
        value: spendDatumCBOR,
      })
      .validTo(txUpperBound)
      .addSignerKey(params.resolutionClaimOperator);
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

export type RemoveOperatorBadSettlementParams={
  operatorStatus: OperatorStatus;
  activeOperatorAddress: string;
  activeOperatorKey: string;
  activeOperatorNFT: string;
  activeOperatorMintingPolicy: MintingPolicy;
  activeOperatorADABond: bigint;
  fraudProverAddress: string;
  hubOracleValidator: AuthenticatedValidator;
}
export const getSlashedActiveOperatorNodeUTxO = (
  activeOperatorsUTxOs: UTxO[],
  params: RemoveOperatorBadSettlementParams,
): Effect.Effect<ActiveOperatorNodeUTxO, DataCoercionError | LucidError> => {
  for (const utxo of activeOperatorsUTxOs) {
    const datumCBOR = utxo.datum;
    if (!datumCBOR) {
      continue;
    }
    try {
      const parsedDatum = Data.from(datumCBOR, ActiveOperatorSpendDatum);
      if (parsedDatum.key === params.activeOperatorKey) {
        return Effect.succeed({ utxo, datum: parsedDatum });
      }
    } catch (err) {
      return Effect.fail(
        new DataCoercionError({
          message: `Could not coerce UTxO's datum to an active operator's node datum`,
          cause: err,
        }),
      );
    }
  }
  return Effect.fail(
    new LucidError({
      message: "No Active Operator UTxO with given operator found",
      cause: "Active Operators Tx not initiated not initiated",
    }),
  );
};

export const incompleteRemoveOperatorBadSettlementTxProgram = (
  lucid: LucidEvolution,
  params: RemoveOperatorBadSettlementParams,
): Effect.Effect<TxBuilder, HashingError | DataCoercionError | LucidError> =>
  Effect.gen(function* () {
    const mintRedeemer: ActiveOperatorMintRedeemer = {
      RemoveOperatorBadSettlement: {
        slashedActiveOperatorKey: params.activeOperatorKey,
        hubOracleRefInputIndex: 0n,
        activeOperatorSlashedNodeInputIndex: 0n,
        activeOperatorAnchorNodeInputIndex: 0n,
        settlementInputIndex: 0n,
        settlementRedeemerIndex: 0n,
      },
    };
    const mintRedeemerCBOR = Data.to(mintRedeemer, ActiveOperatorMintRedeemer);

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
    };

    const activeOperatorInputUTxO = yield* getSlashedActiveOperatorNodeUTxO(
      activeOperatorAllUtxos,
      params,
    );

    const hubOracleRefUTxO = yield* fetchHubOracleRefUTxOs(
      params.hubOracleValidator.spendScriptAddress,
      params.hubOracleValidator.policyId,
      lucid,
    );

    const network = lucid.config().network ?? "Mainnet";
    const slashingPenalty = getProtocolParameters(network).slashing_Penalty;

    const buildsettlementTx = lucid
      .newTx()
      .collectFrom([activeOperatorInputUTxO.utxo])
      .readFrom([hubOracleRefUTxO.utxo])
      .mintAssets(
        {
          [params.activeOperatorNFT]: -1n,
        },
        mintRedeemerCBOR
      )
      .pay.ToAddressWithData(params.fraudProverAddress,
        { kind: "inline",
          value: "undefined" //TODO
        },
        { lovelace: params.activeOperatorADABond }
      )
      .attach.MintingPolicy(params.activeOperatorMintingPolicy)
      .setMinFee(slashingPenalty)
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
    const disproveResolutionClaimTx = yield* incompleteDisproveResolutionClaimTxProgram(
      lucid,
      disproveResolutionClaimParams,
    );
    const removeOperatorBadSettlementTx = yield* incompleteRemoveOperatorBadSettlementTxProgram(
      lucid,
      removeOperatorBadSettlementParams,
    );
    const composedTx = disproveResolutionClaimTx.compose(removeOperatorBadSettlementTx);
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
 * @param removeOperatorBadSettlementParams - Parameters required for removing the slashed active operator.
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