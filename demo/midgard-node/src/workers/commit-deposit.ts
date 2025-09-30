import {
  reexportedParentPort as parentPort,
  reexportedWorkerData as workerData,
} from "@/utils.js";
import * as SDK from "@al-ft/midgard-sdk";
import { Cause, Effect, Schedule, pipe } from "effect";
import { NodeConfig, NodeConfigDep, User } from "@/config.js";
import {
  WorkerInput,
  WorkerOutput,
} from "@/workers/utils/confirm-block-commitments.js";
import { serializeStateQueueUTxO } from "@/workers/utils/commit-block-header.js";
import {
  AlwaysSucceedsContract,
  AuthenticatedValidator,
} from "@/services/always-succeeds.js";
import { LucidEvolution } from "@lucid-evolution/lucid";
import { TxConfirmError } from "@/transactions/utils.js";
import { AlwaysSucceeds } from "@/services/index.js";
import * as ETH from "@ethereumjs/mpt";
import * as ETH_UTILS from "@ethereumjs/util";

const inputData = workerData as WorkerInput;

const fetchDepositUTxOs = (
  lucid: LucidEvolution,
  depositAuthValidator: AuthenticatedValidator,
): Effect.Effect<SDK.TxBuilder.Deposit.DepositUTxO[], Error> =>
  Effect.gen(function* () {
    const fetchConfig: SDK.TxBuilder.Deposit.FetchConfig = {
      depositAddress: depositAuthValidator.spendScriptAddress,
      depositPolicyId: depositAuthValidator.policyId,
    };
    return yield* SDK.Endpoints.fetchDepositUTxOsProgram(
      lucid,
      fetchConfig,
    );
  });

const mpt = (

) =>
  Effect.gen(function* () {
    const trie: ETH.MerklePatriciaTrie = yield* Effect.tryPromise({
    try: () =>
      ETH.createMPT({
        db: new ETH_UTILS.MapDB(),
        useRootPersistence: true,
        // valueEncoding: LEVELDB_ENCODING_OPTS.valueEncoding,
      }),
    catch: (e) => MptError.trieCreate("mempool", e),
  });

  await trie.put(utf8ToBytes('test'), utf8ToBytes('one'))
    const value = await trie.get(utf8ToBytes('test'))
    console.log(value ? bytesToUtf8(value) : 'not found') // 'one'
  })

const wrapper = (
  workerInput: WorkerInput,
): Effect.Effect<
  WorkerOutput,
  Error,
  NodeConfig | User | AlwaysSucceedsContract
> =>
  Effect.gen(function* () {
    const alwaysSucceeds = yield* AlwaysSucceeds.AlwaysSucceedsContract;
    const { user: lucid } = yield* User;

    yield* Effect.logInfo("  fetching DepositUTxOs...");
    const depositUTxOs = yield* fetchDepositUTxOs(
    lucid,
    alwaysSucceeds.stateQueueAuthValidator,
    );
    return {
    type: "SuccessfulConfirmationOutput",
    blocksUTxO: serializedUTxO,
    };
  });

const program = pipe(
  wrapper(inputData),
  Effect.provide(User.layer),
  Effect.provide(AlwaysSucceeds.AlwaysSucceedsContract.layer),
  Effect.provide(NodeConfig.layer),
);

Effect.runPromise(
  program.pipe(
    Effect.catchAllCause((cause) =>
      Effect.succeed({
        type: "FailedConfirmationOutput",
        error: `Tx confirmation worker failure: ${Cause.pretty(cause)}`,
      }),
    ),
  ),
).then((output) => {
  Effect.runSync(
    Effect.logInfo(
      `🔍 Confirmation work completed (${JSON.stringify(output)}).`,
    ),
  );
  parentPort?.postMessage(output);
});
