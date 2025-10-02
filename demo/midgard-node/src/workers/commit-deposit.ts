import {
  reexportedParentPort as parentPort,
  reexportedWorkerData as workerData,
} from "@/utils.js";
import * as SDK from "@al-ft/midgard-sdk";
import { Cause, Effect, Schema, pipe } from "effect";
import {
  AlwaysSucceedsContract,
  AuthenticatedValidator,
  Lucid,
  NodeConfig,
} from "@/services/index.js";
import {
  WorkerInput,
  WorkerOutput,
} from "@/workers/utils/confirm-block-commitments.js";
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
      inclusionStartTime: BigInt(0),
      inclusionEndTime: BigInt(0),
    };
    return yield* SDK.Endpoints.fetchDepositUTxOsProgram(lucid, fetchConfig);
  });

const wrapper = (
  workerInput: WorkerInput,
): Effect.Effect<
  WorkerOutput,
  Error,
  NodeConfig | Lucid | AlwaysSucceedsContract
> =>
  Effect.gen(function* () {
    const { api: lucid } = yield* Lucid;

    yield* Effect.logInfo("  fetching DepositUTxOs...");
    const depositUTxOs = yield* fetchDepositUTxOs(lucid);

    const outRefs = depositUTxOs.map((utxo) => Data.to(utxo.datum.event.id, SDK.TxBuilder.Common.OutputReference));
    const depositInfos = depositUTxOs.map((utxo) => Data.to(utxo.datum.event.info, SDK.TxBuilder.Deposit.DepositInfo));

    const keys: Buffer[] = outRefs.map((ref) => Buffer.from(fromHex(ref)));
    const values: Buffer[] = depositInfos.map((ref) => Buffer.from(fromHex(ref)));

    const depositRoot = yield* keyValueMptRoot(keys, values);

    return {
      type: "SuccessfulRootCalculationOutput",
      mptRoot: depositRoot,
    };
  });

const program = pipe(
  wrapper(inputData),
  Effect.provide(Lucid.Default),
  Effect.provide(AlwaysSucceedsContract.Default),
  Effect.provide(NodeConfig.layer),
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
