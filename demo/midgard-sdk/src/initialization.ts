import { Effect } from "effect";
import {
  LucidEvolution,
  TxBuilder,
  makeReturn,
  TxSignBuilder,
} from "@lucid-evolution/lucid";
import {
  Bech32DeserializationError,
  LucidError,
  MidgardValidators,
  UnspecifiedNetworkError,
} from "@/common.js";
import { incompleteHubOracleInitTxProgram } from "@/hub-oracle.js";
import { incompleteSchedulerInitTxProgram } from "@/scheduler.js";
import { incompleteFraudProofCatalogueInitTxProgram } from "@/fraud-proof/catalogue.js";
import { incompleteInitStateQueueTxProgram } from "@/state-queue.js";
import { incompleteActiveOperatorInitTxProgram } from "@/operator-directory/active-operators.js";
import { incompleteRegisteredOperatorInitTxProgram } from "@/operator-directory/registered-operators.js";
import { incompleteRetiredOperatorInitTxProgram } from "@/operator-directory/retired-operators.js";

export const VALIDITY_RANGE_BUFFER = 8 * 60 * 1000;

export const ATOMIC_INIT_OUTPUT_INDEXES = {
  hubOracle: 0n,
  scheduler: 1n,
  stateQueue: 2n,
  registeredOperators: 3n,
  activeOperators: 4n,
  retiredOperators: 5n,
  fraudProofCatalogue: 6n,
} as const;

export type InitializationParams = {
  midgardValidators: MidgardValidators;
  fraudProofCatalogueMerkleRoot: string;
};

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

    const hubOracleTx = yield* incompleteHubOracleInitTxProgram(lucid, {
      hubOracleMintValidator: params.midgardValidators.hubOracle,
      validators: params.midgardValidators,
      oneShotNonceUTxO: nonceUtxo,
    });

    const schedulerTx = incompleteSchedulerInitTxProgram(lucid, {
      validator: params.midgardValidators.scheduler,
    });

    const stateQueueTx: TxBuilder = yield* incompleteInitStateQueueTxProgram(
      lucid,
      {
        validator: params.midgardValidators.stateQueue,
        genesisTime: genesisTime,
        outputIndex: ATOMIC_INIT_OUTPUT_INDEXES.stateQueue,
      },
    );

    const registeredOperatorsTx: TxBuilder =
      yield* incompleteRegisteredOperatorInitTxProgram(lucid, {
        validator: params.midgardValidators.registeredOperators,
        outputIndex: ATOMIC_INIT_OUTPUT_INDEXES.registeredOperators,
      });

    const activeOperatorsTx: TxBuilder =
      yield* incompleteActiveOperatorInitTxProgram(lucid, {
        validator: params.midgardValidators.activeOperators,
        outputIndex: ATOMIC_INIT_OUTPUT_INDEXES.activeOperators,
      });

    const retiredOperatorsTx = yield* incompleteRetiredOperatorInitTxProgram(
      lucid,
      {
        validator: params.midgardValidators.retiredOperators,
        outputIndex: ATOMIC_INIT_OUTPUT_INDEXES.retiredOperators,
      },
    );

    const fraudProofCatalogueTx: TxBuilder =
      yield* incompleteFraudProofCatalogueInitTxProgram(lucid, {
        validator: params.midgardValidators.fraudProofCatalogue,
        mptRootHash: params.fraudProofCatalogueMerkleRoot,
      });

    return tx
      .compose(hubOracleTx)
      .compose(schedulerTx)
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
