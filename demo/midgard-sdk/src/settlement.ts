import { Data, LucidEvolution, Parameters, TxBuilder, UTxO, utxoToCore } from "@lucid-evolution/lucid";
import { HashingError, LucidError, MerkleRootSchema, POSIXTimeSchema, ProofSchema, VerificationKeyHashSchema } from "./common.js";
import { Effect } from "effect";
import { getProtocolParameters} from "./protocol-parameters.js";

export const ResolutionClaimSchema = Data.Object({
  resolution_time: POSIXTimeSchema,
  operator: VerificationKeyHashSchema
});
export type ResolutionClaim = Data.Static<typeof ResolutionClaimSchema>;
export const ResolutionClaim =
  ResolutionClaimSchema as unknown as ResolutionClaim;

export const SettlementDatumSchema = Data.Object({
  deposits_root: MerkleRootSchema,
  withdrawals_root: MerkleRootSchema,
  transactions_root: MerkleRootSchema,
  resolution_claim: Data.Nullable(ResolutionClaimSchema),
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
       operator_status: OperatorStatusSchema,
       unresolved_event_ref_input_index: Data.Integer(),
       unresolved_event_asset_name: Data.Bytes(),
       event_type: EventTypeSchema,
       membership_proof: ProofSchema,
    }),
  }),
  Data.Object({
    Resolve: Data.Object({
       settlement_id: Data.Bytes(),
    }),
  }),
]);
export type SettlementSpendRedeemer = Data.Static<typeof SettlementSpendRedeemerSchema>;
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
export type SettlementMintRedeemer = Data.Static<typeof SettlementMintRedeemerSchema>;
export const SettlementMintRedeemer =
  SettlementMintRedeemerSchema as unknown as SettlementMintRedeemer;

export const ActiveOperatorSpendRedeemerSchema = Data.Enum([
  Data.Literal("ListStateTransition"),
  Data.Object({
    UpdateBondHoldNewState: Data.Object({ 
       active_node_output_index: Data.Integer(),
       hub_oracle_ref_input_index: Data.Integer(),
       state_queue_redeemer_index: Data.Integer(),
    }),
  }),
  Data.Object({
    UpdateBondHoldNewSettlement: Data.Object({
       active_node_output_index: Data.Integer(),
       hub_oracle_ref_input_index: Data.Integer(),
       settlement_queue_input_index: Data.Integer(),
       settlement_queue_redeemer_index: Data.Integer(),
       new_bond_unlock_time: POSIXTimeSchema,
    }),
  }),
]);
export type ActiveOperatorSpendRedeemer = Data.Static<typeof ActiveOperatorSpendRedeemerSchema>;
export const ActiveOperatorSpendRedeemer =
  ActiveOperatorSpendRedeemerSchema as unknown as ActiveOperatorSpendRedeemer;

export const ActiveOperatorSpendDatumSchema = Data.Object({
  key: Data.Nullable(Data.Bytes()),
  link: Data.Nullable(Data.Bytes()),
  bond_unlock_time: Data.Nullable(POSIXTimeSchema),
});
export type ActiveOperatorSpendDatum = Data.Static<typeof ActiveOperatorSpendDatumSchema>;
export const ActiveOperatorSpendDatum =
  ActiveOperatorSpendDatumSchema as unknown as ActiveOperatorSpendDatum;

export type AttachResolutionClaimParams ={
    settlementAddress: string;
    resolutionClaimOperator: string;
    newBondUnlockTime: bigint;
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
        if (parsedDatum.resolution_claim === null) {
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
            settlement_input_index: 0n,
            settlement_output_index: 0n,
            hub_ref_input_index: 0n,
            active_operators_node_input_index: 0n,
            active_operators_redeemer_index: 0n,
            operator: params.resolutionClaimOperator,
            scheduler_ref_input_index: 0n,
          },
        };
    const spendRedeemerCBOR = Data.to(spendRedeemer, SettlementSpendRedeemer);
    
    const settlementUtxos: UTxO[] = yield* Effect.tryPromise({
      try: () => lucid.utxosAt(params.settlementAddress),
      catch: (err) =>
        new LucidError({
          message: "Failed to fetch Settlement UTxOs",
          cause: err,
        }),
    });
    if (settlementUtxos.length === 0) {
      yield* new LucidError({
        message: "Failed to build the Settlement transaction",
        cause: "No UTxOs found in Settlement contract address",
      });
    }
    const settlementInputUtxo = yield* getSettlementUTxOWithoutClaim(settlementUtxos);

    const txUpperBound = Date.now() + 2000;
    // const network = lucid.config().network ?? "Mainnet";
    // const maturity_duration = getProtocolParameters(network).maturity_duration; 
    // const new_bond_unlock_time = maturity_duration + BigInt(txUpperBound);

    const spendDatum: SettlementDatum = {
      deposits_root: settlementInputUtxo.datum.deposits_root,
      withdrawals_root: settlementInputUtxo.datum.withdrawals_root,
      transactions_root: settlementInputUtxo.datum.transactions_root,
      resolution_claim: {
        resolution_time: params.newBondUnlockTime,
        operator: params.resolutionClaimOperator,
      },
    };
    const spendDatumCBOR = Data.to(spendDatum, SettlementDatum);

    const buildsettlementTx = lucid
      .newTx()
      .collectFrom([settlementInputUtxo.utxo],spendRedeemerCBOR)
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
            active_node_output_index: 0n,
            hub_oracle_ref_input_index: 0n,
            settlement_queue_input_index: 0n,
            settlement_queue_redeemer_index: 0n,
            new_bond_unlock_time: params.newBondUnlockTime,
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
      bond_unlock_time: params.newBondUnlockTime,
    };
    const spendDatumCBOR = Data.to(spendDatum, ActiveOperatorSpendDatum);

    const buildUpdateBondHoldNewSettlementTx = lucid
      .newTx()
      .collectFrom([activeOperatorsInputUtxo.utxo],spendRedeemerCBOR)
      .pay.ToAddressWithData(
        params.activeOperatorsAddress,
        {
          kind: "inline",
          value: spendDatumCBOR,
        },
      )
      .validTo(txUpperBound)
    return buildUpdateBondHoldNewSettlementTx;
  });
  
  
  // .pipe(
  //   Effect.catchAllDefect((defect) => {
  //     return Effect.fail(
  //       new LucidError({
  //         message: "Caught defect from attachResolutionClaimTxBuilder",
  //         cause: defect,
  //       }),
  //     );
  //   }),
  // );
  
  
  
  
  
  
  
  
  
  
  