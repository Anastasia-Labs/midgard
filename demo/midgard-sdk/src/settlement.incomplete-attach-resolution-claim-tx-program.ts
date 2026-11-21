import { type Assets, assetsEqual } from "@al-ft/midgard-core/assets";
import { asDataType } from "@al-ft/midgard-core/lucid-data";
import {
  type BuildTxWithRedeemer,
  Data,
  LucidEvolution,
  TxBuilder,
  type TxOutput,
} from "@lucid-evolution/lucid";
import { Data as EffectData, Effect } from "effect";

import {
  FetchActiveOperatorParams,
  fetchActiveOperatorUTxOs,
  requireActiveOperatorUTxO,
} from "./active-operators.js";
import {
  AuthenticatedValidator,
  DataCoercionError,
  GenericErrorFields,
  HashingError,
  LucidError,
  MerkleRootSchema,
  POSIXTimeSchema,
  VerificationKeyHashSchema,
} from "./common.js";
import { fetchHubOracleUTxOProgram, HubOracleError } from "./hub-oracle.js";
import { AuthenticUTxO } from "./internals.js";
import { WithdrawalValiditySchema } from "./ledger-state.js";
import { OperatorVerdictSchema } from "./rejection-reason.js";
import { fetchSchedulerUTxOProgram, SchedulerError } from "./scheduler.js";
import { EventSettlementMembershipProofSchema } from "./transition-trace.js";
import {
  requireInputIndex,
  requireOwnSpendPurpose,
  requireReferenceInputIndex,
  requireSpendRedeemerIndex,
  requireUniqueOutputIndex,
} from "./tx-context-redeemer.js";
import { outputDatumCborMatches } from "./tx-output-utils.js";

export const ResolutionClaimSchema = Data.Object({
  resolution_time: POSIXTimeSchema,
  operator: VerificationKeyHashSchema,
});

export type ResolutionClaim = Data.Static<typeof ResolutionClaimSchema>;

export const ResolutionClaim = asDataType<ResolutionClaim>(
  ResolutionClaimSchema,
);

export const SettlementDatumSchema = Data.Object({
  deposits_root: MerkleRootSchema,
  withdrawals_root: MerkleRootSchema,
  forced_transactions_root: MerkleRootSchema,
  transactions_root: MerkleRootSchema,
  resolution_claim: Data.Nullable(ResolutionClaimSchema),
});

export type SettlementDatum = Data.Static<typeof SettlementDatumSchema>;

export const SettlementDatum = asDataType<SettlementDatum>(
  SettlementDatumSchema,
);

export const EventTypeSchema = Data.Enum([
  Data.Literal("Deposit"),
  Data.Object({
    Withdrawal: Data.Object({
      validity_override: WithdrawalValiditySchema,
    }),
  }),
  Data.Object({
    TxOrder: Data.Object({
      validity_override: OperatorVerdictSchema,
    }),
  }),
]);

export type EventType = Data.Static<typeof EventTypeSchema>;

export const EventType = asDataType<EventType>(EventTypeSchema);

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

export const SettlementSpendRedeemer = asDataType<SettlementSpendRedeemer>(
  SettlementSpendRedeemerSchema,
);

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

export const SettlementMintRedeemer = asDataType<SettlementMintRedeemer>(
  SettlementMintRedeemerSchema,
);

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

export const outputMatches =
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

export class SettlementError extends EffectData.TaggedError(
  "SettlementError",
)<GenericErrorFields> {}

export class UnresolvedError extends EffectData.TaggedError(
  "UnresolvedError",
)<GenericErrorFields> {}
