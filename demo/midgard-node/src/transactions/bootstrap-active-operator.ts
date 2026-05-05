import * as SDK from "@al-ft/midgard-sdk";
import {
  Data as LucidData,
  LucidEvolution,
  UTxO,
  paymentCredentialOf,
  toUnit,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { handleSignSubmitNoConfirmation } from "@/transactions/utils.js";

/**
 * Startup-time bootstrap flow for the active-operators witness node.
 *
 * The node uses this helper to ensure an operator-specific witness UTxO exists
 * before later lifecycle transactions rely on it.
 */
const MIN_ACTIVE_OPERATOR_NODE_LOVELACE = 5_000_000n;
const BOOTSTRAP_CONFIRMATION_POLL_INTERVAL = "3 seconds";
const BOOTSTRAP_CONFIRMATION_MAX_POLLS = 40;
const BOOTSTRAP_AWAIT_TX_TIMEOUT_MS = 20_000;

/**
 * Picks the largest wallet UTxO, breaking ties deterministically by outref.
 *
 * Bootstrap transactions only need one funding input, so choosing the largest
 * candidate reduces the odds of accidental insufficiency.
 */
const selectLargestWalletUtxo = (
  utxos: readonly UTxO[],
): UTxO | undefined =>
  [...utxos].sort((a, b) => {
    const lovelaceA = a.assets.lovelace ?? 0n;
    const lovelaceB = b.assets.lovelace ?? 0n;
    if (lovelaceA === lovelaceB) {
      const txHashCompare = a.txHash.localeCompare(b.txHash);
      if (txHashCompare !== 0) {
        return txHashCompare;
      }
      return a.outputIndex - b.outputIndex;
    }
    return lovelaceA > lovelaceB ? -1 : 1;
  })[0];

/**
 * Resolves the operator wallet's key hash from the currently selected Lucid
 * wallet.
 */
const getOperatorKeyHash = (
  lucid: LucidEvolution,
): Effect.Effect<string, SDK.StateQueueError> =>
  Effect.gen(function* () {
    const operatorAddress = yield* Effect.tryPromise({
      try: () => lucid.wallet().address(),
      catch: (cause) =>
        new SDK.StateQueueError({
          message: "Failed to resolve operator wallet address",
          cause,
        }),
    });
    const paymentCredential = paymentCredentialOf(operatorAddress);
    if (paymentCredential?.type !== "Key") {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message: "Operator wallet does not have a key payment credential",
          cause: operatorAddress,
        }),
      );
    }
    return paymentCredential.hash;
  });

/**
 * Checks whether the active-operators set already contains this operator's
 * witness node.
 */
const hasOperatorNode = (
  activeOperatorUtxos: readonly UTxO[],
  operatorKeyHash: string,
  operatorNodeUnit: string,
): Effect.Effect<boolean, SDK.DataCoercionError | SDK.MissingDatumError> =>
  Effect.gen(function* () {
    for (const utxo of activeOperatorUtxos) {
      const nodeDatum = yield* SDK.getNodeDatumFromUTxO(utxo);
      if (
        nodeDatum.key !== "Empty" &&
        nodeDatum.key.Key.key === operatorKeyHash &&
        (utxo.assets[operatorNodeUnit] ?? 0n) === 1n
      ) {
        return true;
      }
    }
    return false;
  });

/**
 * Ensures the operator-specific active-operators witness node exists on-chain.
 *
 * The effect is idempotent: it first checks for the existing witness node and
 * only submits the bootstrap transaction when the node is missing.
 */
export const ensureActiveOperatorWitnessNodeProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
): Effect.Effect<
  void,
  | SDK.LucidError
  | SDK.StateQueueError
  | SDK.DataCoercionError
  | SDK.MissingDatumError
  | import("@/transactions/utils.js").TxSignError
  | import("@/transactions/utils.js").TxSubmitError
> =>
  Effect.gen(function* () {
    const stateQueueUtxos = yield* SDK.utxosAtByNFTPolicyId(
      lucid,
      contracts.stateQueue.spendingScriptAddress,
      contracts.stateQueue.policyId,
    ).pipe(
      Effect.mapError(
        (cause) =>
          new SDK.LucidError({
            message: "Failed to fetch state_queue UTxOs during startup check",
            cause,
          }),
      ),
    );
    if (stateQueueUtxos.length === 0) {
      yield* Effect.logInfo(
        "Skipping active-operator bootstrap: state_queue is not initialized yet.",
      );
      return;
    }

    const operatorKeyHash = yield* getOperatorKeyHash(lucid);
    const operatorNodeUnit = toUnit(
      contracts.activeOperators.policyId,
      SDK.ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX + operatorKeyHash,
    );

    const activeOperatorUtxos = yield* SDK.utxosAtByNFTPolicyId(
      lucid,
      contracts.activeOperators.spendingScriptAddress,
      contracts.activeOperators.policyId,
    ).pipe(
      Effect.mapError(
        (cause) =>
          new SDK.LucidError({
            message:
              "Failed to fetch active-operators UTxOs during startup check",
            cause,
          }),
      ),
    );

    const operatorNodeExists = yield* hasOperatorNode(
      activeOperatorUtxos.map((beacon) => beacon.utxo),
      operatorKeyHash,
      operatorNodeUnit,
    );
    if (operatorNodeExists) {
      yield* Effect.logInfo(
        `Active-operators witness node already present for operator ${operatorKeyHash}.`,
      );
      return;
    }

    yield* Effect.logWarning(
      `No active-operators witness node found for operator ${operatorKeyHash}; submitting bootstrap tx.`,
    );

    const walletUtxos = yield* Effect.tryPromise({
      try: () => lucid.wallet().getUtxos(),
      catch: (cause) =>
        new SDK.LucidError({
          message:
            "Failed to fetch wallet UTxOs for active-operator bootstrap tx",
          cause,
        }),
    });
    const feeInput = selectLargestWalletUtxo(walletUtxos);
    if (feeInput === undefined) {
      return yield* Effect.fail(
        new SDK.LucidError({
          message: "No wallet UTxO available to fund active-operator bootstrap tx",
          cause: "empty wallet",
        }),
      );
    }

    const bootstrapDatum: SDK.LinkedListNodeView = {
      key: { Key: { key: operatorKeyHash } },
      next: "Empty",
      data: SDK.castActiveOperatorDatumToData({
        bond_unlock_time: null,
        inactivity_strikes: 0n,
      }) as SDK.LinkedListNodeView["data"],
    };
    const bootstrapAssets = {
      lovelace: MIN_ACTIVE_OPERATOR_NODE_LOVELACE,
      [operatorNodeUnit]: 1n,
    };

    const unsignedTx = yield* Effect.tryPromise({
      try: () =>
        lucid
          .newTx()
          .collectFrom([feeInput])
          .mintAssets({ [operatorNodeUnit]: 1n }, LucidData.void())
          .pay.ToContract(
            contracts.activeOperators.spendingScriptAddress,
            {
              kind: "inline",
              value: SDK.encodeLinkedListNodeView(bootstrapDatum),
            },
            bootstrapAssets,
          )
          .attach.Script(contracts.activeOperators.mintingScript)
          .complete({ localUPLCEval: true }),
      catch: (cause) =>
        new SDK.LucidError({
          message: `Failed to build active-operator bootstrap tx: ${cause}`,
          cause,
        }),
    });

    const txHash = yield* handleSignSubmitNoConfirmation(lucid, unsignedTx);
    yield* Effect.logInfo(
      `Active-operators witness node bootstrap submitted successfully: ${txHash}`,
    );

    // Best effort await: providers differ on wait semantics, so time-box it and
    // continue with explicit UTxO polling.
    yield* Effect.tryPromise({
      try: () =>
        Promise.race([
          lucid.awaitTx(txHash),
          new Promise<void>((_, reject) =>
            setTimeout(
              () =>
                reject(
                  new Error(
                    `awaitTx timeout after ${BOOTSTRAP_AWAIT_TX_TIMEOUT_MS}ms`,
                  ),
                ),
              BOOTSTRAP_AWAIT_TX_TIMEOUT_MS,
            ),
          ),
        ]),
      catch: (cause) =>
        new SDK.LucidError({
          message:
            "Best-effort awaitTx failed for active-operator bootstrap; falling back to UTxO polling",
          cause,
        }),
    }).pipe(
      Effect.catchAll((e) =>
        Effect.logWarning(
          `${e.message}; txHash=${txHash}, continuing with UTxO polling.`,
        ),
      ),
    );

    let pollCount = 0;
    while (pollCount < BOOTSTRAP_CONFIRMATION_MAX_POLLS) {
      const operatorNodeUtxos = yield* Effect.tryPromise({
        try: () =>
          lucid.utxosAtWithUnit(
            contracts.activeOperators.spendingScriptAddress,
            operatorNodeUnit,
          ),
        catch: (cause) =>
          new SDK.LucidError({
            message:
              "Failed to fetch active-operators witness node while waiting for bootstrap confirmation",
            cause,
          }),
      });
      if (operatorNodeUtxos.length > 0) {
        yield* Effect.logInfo(
          `Active-operators witness node is now visible on-chain for operator ${operatorKeyHash}.`,
        );
        return;
      }

      pollCount += 1;
      yield* Effect.sleep(BOOTSTRAP_CONFIRMATION_POLL_INTERVAL);
    }

    return yield* Effect.fail(
      new SDK.LucidError({
        message:
          "Timed out waiting for active-operators witness node to become visible after bootstrap submission",
        cause: txHash,
      }),
    );
  });
