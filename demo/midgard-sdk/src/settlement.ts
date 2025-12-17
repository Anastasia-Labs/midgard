import { Data, LucidEvolution, Parameters, TxBuilder, TxSignBuilder, UTxO, utxoToCore } from "@lucid-evolution/lucid";
import { AuthenticatedValidator, GenericErrorFields, HashingError, LucidError, makeReturn, MerkleRootSchema, POSIXTimeSchema, ProofSchema, utxosAtByNFTPolicyId, VerificationKeyHashSchema } from "./common.js";
import { Data as EffectData, Effect } from "effect";
import { HubOracleUTxO, utxosToHubOracleUTxOs } from "./hub-oracle.js";
import { SchedulerUTxO, utxosToSchedulerUTxOs } from "./scheduler.js";

export const ResolutionClaimSchema = Data.Object({
  resolutionTime: POSIXTimeSchema,
  operator: VerificationKeyHashSchema
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
export const OperatorStatus =
  OperatorStatusSchema as unknown as OperatorStatus;

export const EventTypeSchema = Data.Enum([
  Data.Literal("Deposit"),
  Data.Literal("Withdrawal"),
  Data.Literal("TxOrder"),
]);
export type EventType = Data.Static<typeof EventTypeSchema>;
export const EventType =
  EventTypeSchema as unknown as EventType;

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
export type SettlementSpendRedeemer = Data.Static<typeof SettlementSpendRedeemerSchema>;
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
export type SettlementMintRedeemer = Data.Static<typeof SettlementMintRedeemerSchema>;
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
export type ActiveOperatorSpendRedeemer = Data.Static<typeof ActiveOperatorSpendRedeemerSchema>;
export const ActiveOperatorSpendRedeemer =
  ActiveOperatorSpendRedeemerSchema as unknown as ActiveOperatorSpendRedeemer;

export const ActiveOperatorSpendDatumSchema = Data.Object({
  key: Data.Nullable(Data.Bytes()),
  link: Data.Nullable(Data.Bytes()),
  bondUnlockTime: Data.Nullable(POSIXTimeSchema),
});
export type ActiveOperatorSpendDatum = Data.Static<typeof ActiveOperatorSpendDatumSchema>;
export const ActiveOperatorSpendDatum =
  ActiveOperatorSpendDatumSchema as unknown as ActiveOperatorSpendDatum;

export type AttachResolutionClaimParams ={
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
  settlementUTxOs: UTxO[]
): Effect.Effect<SettlementUTxO,LucidError> => {
  return Effect.gen(function* () {
    for (const utxo of settlementUTxOs) {
      const datum = utxo.datum;
      if (!datum) {
        continue;
      }
      try {
        const parsedDatum = Data.from(datum, SettlementDatum);
        if (parsedDatum.resolutionClaim === null) {
          return {utxo, datum: parsedDatum};
        }
      } catch (error) {
        continue;
      }
    }
    return yield* Effect.fail(
      new LucidError({
        message: "No settlement UTxO without resolution claim found",
        cause: "Settlement not initiated"
      }),
    );
  });
};

const fetchHubOracleRefUTxOs = (
  hubOracleScriptAddress: string,
  policyId: string,
  lucid: LucidEvolution
): Effect.Effect<HubOracleUTxO[], LucidError> =>
  Effect.gen(function* (_) {
    const hubOracleAllUTxOs = yield* Effect.tryPromise({
      try: () => lucid.utxosAt(hubOracleScriptAddress),
      catch: (err) =>
        new LucidError({
          message: "Failed to fetch Hub Oracle UTxOs",
          cause: err,
        }),
    });
    if (hubOracleAllUTxOs.length === 0) {
      yield* new LucidError({
        message: "Failed to build the HubOracle transaction",
        cause: "No UTxOs found in Hub Oracle contract address",
      });
    }

    return yield* utxosToHubOracleUTxOs(hubOracleAllUTxOs, policyId);
  });

const fetchSchedulerRefUTxOs = (
  schedulerScriptAddress: string,
  policyId: string,
  lucid: LucidEvolution
): Effect.Effect<SchedulerUTxO[], LucidError> =>
  Effect.gen(function* (_) {
    const schedulerAllUTxOs = yield* Effect.tryPromise({
      try: () => lucid.utxosAt(schedulerScriptAddress),
      catch: (err) =>
        new LucidError({
          message: "Failed to fetch Scheduler UTxOs",
          cause: err,
        }),
    });
    if (schedulerAllUTxOs.length === 0) {
      yield* new LucidError({
        message: "Failed to build the Scheduler transaction",
        cause: "No UTxOs found in Scheduler contract address",
      });
    }

    return yield* utxosToSchedulerUTxOs(schedulerAllUTxOs, policyId);
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
): Effect.Effect<TxBuilder, HashingError | LucidError> =>
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
    const settlementInputUtxo = yield* getSettlementUTxOWithoutClaim(settlementAllUtxos);

    const txUpperBound = Date.now() + 2000;
    // const network = lucid.config().network ?? "Mainnet";
    // const maturity_duration = getProtocolParameters(network).maturity_duration; 
    // const new_bond_unlock_time = maturity_duration + BigInt(txUpperBound);

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

    const hubOracleRefUTxOs = yield* fetchHubOracleRefUTxOs(params.hubOracleValidator.spendScriptAddress,params.hubOracleValidator.policyId,lucid);

    const schedulerRefUTxOs = yield* fetchSchedulerRefUTxOs(params.schedulerScriptAddress, params.schedulerPolicyId,lucid);
    
    const buildsettlementTx = lucid
      .newTx()
      .collectFrom([settlementInputUtxo.utxo],spendRedeemerCBOR)
      .readFrom([hubOracleRefUTxOs[0].utxo])
      .readFrom([schedulerRefUTxOs[0].utxo])
      .pay.ToAddressWithData(
        params.settlementAddress,
        {
          kind: "inline",
          value: spendDatumCBOR,
        },
      )
      .validTo(txUpperBound)
    return buildsettlementTx;
  });

export type ActiveOperatorsParams ={
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
  params: ActiveOperatorsParams
): Effect.Effect<ActiveOperatorNodeUTxO,LucidError> => {
  return Effect.gen(function* () {
    for (const utxo of activeOperatorsUTxOs) {
      const datum = utxo.datum;
      if (!datum) {
        continue;
      }
      try {
        const parsedDatum = Data.from(datum, ActiveOperatorSpendDatum);
        if (parsedDatum.key === params.operator) {
          return {utxo, datum: parsedDatum};
        }
      } catch (error) {
        continue;
      }
    }
    return yield* Effect.fail(
      new LucidError({
        message: "No Active Operator UTxO with given operator found",
        cause: "Active Operators Tx not initiated"
      }),
    );
  });
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
): Effect.Effect<TxBuilder, HashingError | LucidError> =>
  Effect.gen(function* () {
    
    const txUpperBound = Date.now() + 2000;
    // const network = lucid.config().network ?? "Mainnet";
    // const maturity_duration = getProtocolParameters(network).maturity_duration; 
    // const new_bond_unlock_time = maturity_duration + BigInt(txUpperBound);

    const spendRedeemer: ActiveOperatorSpendRedeemer = {
          UpdateBondHoldNewSettlement: {
            activeNodeOutputIndex: 0n,
            hubOracleRefInputIndex: 0n,
            settlementQueueInputIndex: 0n,
            settlementQueueRedeemerIndex: 0n,
            newBondUnlockTime: params.newBondUnlockTime,
          },
        };
    const spendRedeemerCBOR = Data.to(spendRedeemer, ActiveOperatorSpendRedeemer);
    
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
    const activeOperatorsInputUtxo = yield* getActiveOperatorNodeUTxO(activeOperatorsUtxos, params);
    
    const spendDatum: ActiveOperatorSpendDatum = {
      ...activeOperatorsInputUtxo.datum,
      bondUnlockTime: params.newBondUnlockTime,
    };
    const spendDatumCBOR = Data.to(spendDatum, ActiveOperatorSpendDatum);

    const hubOracleRefUTxOs = yield* fetchHubOracleRefUTxOs(params.hubOracleValidator.spendScriptAddress,params.hubOracleValidator.policyId,lucid);

    const schedulerRefUTxOs = yield* fetchSchedulerRefUTxOs(params.schedulerScriptAddress, params.schedulerPolicyId,lucid);

    const buildUpdateBondHoldNewSettlementTx = lucid
      .newTx()
      .collectFrom([activeOperatorsInputUtxo.utxo],spendRedeemerCBOR)
      .readFrom([hubOracleRefUTxOs[0].utxo])
      .readFrom([schedulerRefUTxOs[0].utxo])
      .pay.ToAddressWithData(
        params.activeOperatorsAddress,
        {
          kind: "inline",
          value: spendDatumCBOR,
        },
      )
      .validTo(txUpperBound)
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
  attachParams: AttachResolutionClaimParams,
  operatorParams: ActiveOperatorsParams,
): Effect.Effect<TxSignBuilder, HashingError | LucidError | SettlementError> =>
  Effect.gen(function* () {
    const attachTx = yield* incompleteAttachResolutionClaimTxProgram(lucid, attachParams);
    const activeTx = yield* incompleteUpdateBondHoldNewSettlementTxProgram(lucid, operatorParams);
    const composedTx = attachTx.compose(activeTx);
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
   * @param attachParams - Parameters required for attaching resolution claim.
   * @param operatorParams - Parameters required for selecting active operator.
   * @returns A promise that resolves to a `TxSignBuilder` instance.
   */
  export const unsignedAttachResolutionClaimTx = (
    lucid: LucidEvolution,
    attachParams: AttachResolutionClaimParams,
    operatorParams: ActiveOperatorsParams,
    
  ): Promise<TxSignBuilder> =>
    makeReturn(unsignedAttachResolutionClaimTxProgram(lucid, attachParams, operatorParams)).unsafeRun();
  
  export class SettlementError extends EffectData.TaggedError(
    "SettlementError",
  )<GenericErrorFields> {}
  
  
  
  
  