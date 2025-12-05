import { Data, LucidEvolution, TxBuilder, UTxO, utxoToCore } from "@lucid-evolution/lucid";
import { HashingError, LucidError, MerkleRootSchema, POSIXTimeSchema, ProofSchema, VerificationKeyHashSchema } from "./common.js";
import { Effect } from "effect";

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

export type attachResolutionClaimParams ={
    settlementAddress: string;
};

export type SettlementUTxO = {
    utxo: UTxO;
    datum: SettlementDatum;
}; 

export const findSettlementUTxOWithoutClaim = (
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
export const attachResolutionClaimTxProgram = (
  lucid: LucidEvolution,
  params: attachResolutionClaimParams,
): Effect.Effect<TxBuilder, HashingError | LucidError> =>
  Effect.gen(function* () {
    const spendRedeemer: SettlementSpendRedeemer = {
          AttachResolutionClaim: {
            settlement_input_index: 0n,
            settlement_output_index: 0n,
            hub_ref_input_index: 0n,
            active_operators_node_input_index: 0n,
            active_operators_redeemer_index: 0n,
            operator: '',
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
    const settlementInputUtxo = yield* findSettlementUTxOWithoutClaim(settlementUtxos);
    
    const spendDatum: SettlementDatum = {
      deposits_root: settlementInputUtxo.datum.deposits_root,
      withdrawals_root: settlementInputUtxo.datum.withdrawals_root,
      transactions_root: settlementInputUtxo.datum.transactions_root,
      resolution_claim: {
        resolution_time: BigInt("TODO_fill_resolution_time"),
        operator: 'TODO_fill_operator_vk_hash',
      },
    };
    const spendDatumCBOR = Data.to(spendDatum, SettlementDatum);

    const tx = lucid
      .newTx()
      .collectFrom([settlementInputUtxo.utxo],spendRedeemerCBOR)
      .pay.ToAddressWithData(
        params.settlementAddress,
        {
          kind: "inline",
          value: spendDatumCBOR,
        },
      )
    return tx;
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