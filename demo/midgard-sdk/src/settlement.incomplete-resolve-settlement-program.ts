import {
  type BuildTxWithRedeemer,
  Data,
  LucidEvolution,
  Script,
  toUnit,
  TxBuilder,
  TxSignBuilder,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  ActiveOperatorUTxO,
  fetchActiveOperatorUTxOs,
} from "./active-operators.js";
import {
  AssetError,
  DataCoercionError,
  findOperatorByPKH,
  HashingError,
  LucidError,
  makeReturn,
} from "./common.js";
import { fetchHubOracleUTxOProgram, HubOracleError } from "./hub-oracle.js";
import {
  fetchRetiredOperatorUTxOs,
  RetiredOperatorUTxO,
} from "./retired-operators.js";
import {
  SettlementError,
  SettlementMintRedeemer,
  SettlementSpendRedeemer,
  type SettlementUTxO,
} from "./settlement.incomplete-attach-resolution-claim-tx-program.js";
import {
  type DisproveResolutionClaimParams,
  fetchUserEventRefUTxO,
  type RemoveOperatorBadSettlementParams,
} from "./settlement.incomplete-update-bond-hold-new-settlement-tx-program.js";
import { completeTxWithLocalUPLCEvalProgram } from "./tx-completion.js";
import {
  requireInputIndex,
  requireOwnMintPurpose,
  requireSpendRedeemerIndex,
} from "./tx-context-redeemer.js";

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
    const operatorNFT = yield* getOperatorNFT(
      operatorInputUTxO,
      params.activeOperatorParams.activeOperatorPolicyId,
      params.retiredOperatorParams.retiredOperatorPolicyId,
    );

    const hubOracleRefUTxO = yield* fetchHubOracleUTxOProgram(lucid, {
      hubOracleAddress: params.hubOracleValidator.spendingScriptAddress,
      hubOraclePolicyId: params.hubOracleValidator.policyId,
    });

    if (params.slashingPenaltyLovelace <= 0n) {
      return yield* new SettlementError({
        message: "Bad-settlement slashing fee must be positive",
        cause: params.slashingPenaltyLovelace,
      });
    }

    const userEventRefUTxO = yield* fetchUserEventRefUTxO(
      params.eventType,
      params.eventAddress,
      params.eventPolicyId,
      lucid,
      params.eventHistory,
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
      .attach.MintingPolicy(params.activeOperatorMintingPolicy)
      .setMinFee(params.slashingPenaltyLovelace);
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
            message: `Failed to build the transaction: ${String(e)}`,
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

    const resolutionClaim = params.settlementUTxO.datum.resolution_claim;
    if (resolutionClaim === null) {
      throw new Error("Settlement resolution claim is required");
    }
    const resolutionTime = Number(resolutionClaim.resolution_time);
    const txLowerBound = resolutionTime + 1 * 60_000;
    const txSigner = resolutionClaim.operator;
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
