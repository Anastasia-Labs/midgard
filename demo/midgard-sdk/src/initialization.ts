import { Effect } from "effect";
import {
  LucidEvolution,
  TxBuilder,
  makeReturn,
  TxSignBuilder,
  UTxO,
} from "@lucid-evolution/lucid";
import {
  Bech32DeserializationError,
  LucidError,
  MidgardValidators,
  UnspecifiedNetworkError,
} from "@/common.js";
import { incompleteHubOracleInitTxProgram } from "@/hub-oracle.js";
import {
  INITIAL_SCHEDULER_DATUM,
  SchedulerDatum,
  incompleteSchedulerInitTxProgram,
} from "@/scheduler.js";
import { incompleteFraudProofCatalogueInitTxProgram } from "@/fraud-proof/catalogue.js";
import { incompleteInitStateQueueTxProgram } from "@/state-queue.js";
import { incompleteActiveOperatorInitTxProgram } from "@/active-operators.js";
import { incompleteRegisteredOperatorInitTxProgram } from "@/registered-operators.js";
import { incompleteRetiredOperatorInitTxProgram } from "@/retired-operators.js";

export const VALIDITY_RANGE_BUFFER = 5 * 60 * 1000;

export type HubAndSchedulerInitializationParams = {
  validators: MidgardValidators;
  oneShotNonceUTxO: UTxO;
  schedulerDatum?: SchedulerDatum;
  schedulerLovelace?: bigint;
};

export type InitializationParams = {
  midgardValidators: MidgardValidators;
  fraudProofCatalogueMerkleRoot: string;
};

export const incompleteHubAndSchedulerInitTxProgram = (
  lucid: LucidEvolution,
  params: HubAndSchedulerInitializationParams,
): Effect.Effect<
  TxBuilder,
  Bech32DeserializationError | UnspecifiedNetworkError
> =>
  Effect.gen(function* () {
    const hubOracleTx = yield* incompleteHubOracleInitTxProgram(lucid, {
      hubOracleMintValidator: params.validators.hubOracle,
      validators: params.validators,
      oneShotNonceUTxO: params.oneShotNonceUTxO,
    });
    const schedulerTx = incompleteSchedulerInitTxProgram(lucid, {
      validator: params.validators.scheduler,
      datum: params.schedulerDatum ?? INITIAL_SCHEDULER_DATUM,
      lovelace: params.schedulerLovelace,
    });

    return lucid.newTx().compose(hubOracleTx).compose(schedulerTx);
  });

export const incompleteInitializationTxProgram = (
  lucid: LucidEvolution,
  params: InitializationParams,
): Effect.Effect<
  TxBuilder,
  LucidError | Bech32DeserializationError | UnspecifiedNetworkError
> =>
  Effect.gen(function* () {
    const utxos = yield* Effect.tryPromise({
      try: () => lucid.wallet().getUtxos(),
      catch: (e) =>
        new LucidError({
          message: "Failed to fetch UTxOs to use as nonce for initialization",
          cause: e,
        }),
    });

    if (utxos.length === 0) {
      return yield* Effect.fail(
        new LucidError({
          message: "No UTxOs available for nonce of initialization",
          cause: "Wallet has no UTxOs",
        }),
      );
    }

    const nonceUtxo = utxos[0];
    const genesisTime = BigInt(Date.now() + VALIDITY_RANGE_BUFFER);
    const tx = lucid.newTx().validTo(Number(genesisTime));

    const hubAndSchedulerTx = yield* incompleteHubAndSchedulerInitTxProgram(
      lucid,
      {
        validators: params.midgardValidators,
        oneShotNonceUTxO: nonceUtxo,
      },
    );

    const stateQueueTx: TxBuilder = yield* incompleteInitStateQueueTxProgram(
      lucid,
      {
        validator: params.midgardValidators.stateQueue,
        genesisTime: genesisTime,
      },
    );

    const registeredOperatorsTx: TxBuilder =
      yield* incompleteRegisteredOperatorInitTxProgram(lucid, {
        validator: params.midgardValidators.registeredOperators,
      });

    const activeOperatorsTx: TxBuilder =
      yield* incompleteActiveOperatorInitTxProgram(lucid, {
        validator: params.midgardValidators.activeOperators,
      });

    const retiredOperatorsTx = yield* incompleteRetiredOperatorInitTxProgram(
      lucid,
      {
        validator: params.midgardValidators.retiredOperators,
      },
    );

    const fraudProofCatalogueTx: TxBuilder =
      yield* incompleteFraudProofCatalogueInitTxProgram(lucid, {
        validator: params.midgardValidators.fraudProofCatalogue,
        mptRootHash: params.fraudProofCatalogueMerkleRoot,
      });

    return tx
      .compose(hubAndSchedulerTx)
      .compose(stateQueueTx)
      .compose(registeredOperatorsTx)
      .compose(activeOperatorsTx)
      .compose(retiredOperatorsTx)
      .compose(fraudProofCatalogueTx);
  });

export const unsignedInitializationTxProgram = (
  lucid: LucidEvolution,
  initParams: InitializationParams,
): Effect.Effect<
  TxSignBuilder,
  LucidError | Bech32DeserializationError | UnspecifiedNetworkError
> =>
  Effect.gen(function* () {
    const commitTx = yield* incompleteInitializationTxProgram(
      lucid,
      initParams,
    );
    const completedTx: TxSignBuilder = yield* Effect.tryPromise({
      try: () => commitTx.complete({ localUPLCEval: true }),
      catch: (e) =>
        new LucidError({
          message: `Failed to build the init transaction: ${e}`,
          cause: e,
        }),
    });
    return completedTx;
  });

/**
 * Builds completed tx for initializing all Midgard contracts.
 *
 * @param lucid - The `LucidEvolution` API object.
 * @param initParams - Parameters for initializing all Midgard contracts.
 * @returns A promise that resolves to a `TxSignBuilder` instance.
 */
export const unsignedInitializationTx = (
  lucid: LucidEvolution,
  initParams: InitializationParams,
): Promise<TxSignBuilder> =>
  makeReturn(unsignedInitializationTxProgram(lucid, initParams)).unsafeRun();
