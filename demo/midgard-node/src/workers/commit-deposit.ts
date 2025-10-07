import {
  reexportedParentPort as parentPort,
  reexportedWorkerData as workerData,
} from "@/utils.js";
import * as SDK from "@al-ft/midgard-sdk";
import { Cause, Effect, Schema, pipe } from "effect";
import {
  AlwaysSucceedsContract,
  AuthenticatedValidator,
  Globals,
  Lucid,
  NodeConfig,
} from "@/services/index.js";
import {
  WorkerInput,
  WorkerOutput,
} from "@/workers/utils/commit-deposit.js";
import { serializeStateQueueUTxO } from "@/workers/utils/commit-block-header.js";
import { LucidEvolution, CML, Data, fromHex } from "@lucid-evolution/lucid";
import { TxConfirmError } from "@/transactions/utils.js";
import { keyValueMptRoot } from "./utils/mpt.js";
import * as ETH from "@ethereumjs/mpt";
import * as ETH_UTILS from "@ethereumjs/util";

const inputData = workerData as WorkerInput;

// TODO: rewrite it with `midgard-node/src/workers/confirm-block-commitments.ts` in mind

const fetchDepositUTxOs = (
  lucid: LucidEvolution,
  inclusionStartTime: bigint,
  inclusionEndTime: bigint,
): Effect.Effect<
  SDK.TxBuilder.Deposit.DepositUTxO[],
  | SDK.Utils.LucidError
  | SDK.Utils.DataCoercionError
  | SDK.Utils.AssetError
  | SDK.Utils.UnauthenticUtxoError,
  AlwaysSucceedsContract | NodeConfig
> =>
  Effect.gen(function* () {
    const { depositAuthValidator } = yield* AlwaysSucceedsContract;
    const fetchConfig: SDK.TxBuilder.Deposit.FetchConfig = {
      depositAddress: depositAuthValidator.spendScriptAddress,
      depositPolicyId: depositAuthValidator.policyId,
      inclusionStartTime: inclusionStartTime,
      inclusionEndTime: inclusionEndTime,
    };
    return yield* SDK.Endpoints.fetchDepositUTxOsProgram(lucid, fetchConfig);
  });

const wrapper = (
  workerInput: WorkerInput,
): Effect.Effect<
  WorkerOutput,
  Error,
  NodeConfig | Lucid | AlwaysSucceedsContract | Globals
> =>
  Effect.gen(function* () {
    const { api: lucid } = yield* Lucid;
    const globals = yield* Globals
    const startTime = yield* globals.UNCONFIRMED_SUBMITTED_BLOCK_TIME
    const endTime = BigInt(Date.now())

    yield* Effect.logInfo("  fetching DepositUTxOs...");
    const depositUTxOs = yield* fetchDepositUTxOs(lucid, startTime, endTime);

    const outRefs = depositUTxOs.map((utxo) => Data.to(utxo.datum.event.id, SDK.TxBuilder.Common.OutputReference));
    const depositInfos = depositUTxOs.map((utxo) => Data.to(utxo.datum.event.info, SDK.TxBuilder.Deposit.DepositInfo));

    const keys: Buffer[] = outRefs.map((ref) => Buffer.from(fromHex(ref)));
    const values: Buffer[] = depositInfos.map((ref) => Buffer.from(fromHex(ref)));

    const depositRoot = yield* keyValueMptRoot(keys, values);

    yield* globals.DEPOSIT_ROOTS_QUEUE.offer(depositRoot)

    return {
      type: "SuccessfulRootCalculationOutput",
      mptRoot: depositRoot,
      inclusionTime: endTime,
    };
  });

const program = pipe(
  wrapper(inputData),
  Effect.provide(Lucid.Default),
  Effect.provide(AlwaysSucceedsContract.Default),
  Effect.provide(NodeConfig.layer),
  Effect.provide(Globals.Default), // TODO: check if that creates a new layer, or references a global default
);

Effect.runPromise(
  program.pipe(
    Effect.catchAllCause((cause) =>
      Effect.succeed({
        type: "FailedRootCalculationOutput",
        error: `Root calculation worker failure: ${Cause.pretty(cause)}`,
      }),
    ),
  ),
).then((output) => {
  Effect.runSync(
    Effect.logInfo(
      `🔍 Root calculation work completed (${JSON.stringify(output)}).`,
    ),
  );
  parentPort?.postMessage(output);
});
