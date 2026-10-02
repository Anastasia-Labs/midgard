import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { getAddressDetails, toUnit } from "@lucid-evolution/lucid";
import { Effect, Ref } from "effect";
import { beforeEach, describe, expect, it } from "vitest";

import {
  blockConfirmationStep,
  confirmationTick,
} from "../src/fibers/block-confirmation.block-confirmation-fiber.js";
import type { ConfirmationWorkerRunner } from "../src/fibers/block-confirmation.record-confirmed-pending-block.js";
import { Database, Globals, NodeConfig } from "../src/services/index.js";
import {
  HaltSource,
  raiseLivenessIncident,
} from "../src/services/liveness-halt.js";
import { SIGNED_INTENT_UNDECIDED } from "../src/services/signed-intent-undecided.js";
import { serializeStateQueueUTxO } from "../src/workers/utils/commit-block-header.js";
import type { WorkerOutput } from "../src/workers/utils/confirm-block-commitments.js";
import {
  activeE,
  run,
  seed,
} from "./helpers/history-expired-intent-release-before-ttl.js";

/**
 * The confirmation fiber clears `signed_intent_undecided` only when its tick
 * reports that it reached the signed-intent decision. These drive the tick the
 * fiber runs (`confirmationTick` under `blockConfirmationStep`) with a stub
 * worker: a tick that decided nothing must leave the reason, and a tick that
 * reached the decision must clear it.
 */

const address =
  "addr_test1wzylc3gg4h37gt69yx057gkn4egefs5t9rsycmryecpsenswtdp58";
const bytes = (n: number, width = 32) => Buffer.alloc(width, n);

/** A real state-queue block UTxO, the queue tip the worker reports. */
const tipBlock = Effect.gen(function* () {
  const empty = SDK.EMPTY_MERKLE_TREE_ROOT;
  const header: SDK.Header = {
    utxosRoot: empty,
    forcedTransactionsRoot: empty,
    transactionsRoot: empty,
    depositsRoot: empty,
    withdrawalsRoot: empty,
    transitionTraceRoot: empty,
    eventToStepRoot: empty,
    validationTracesRoot: empty,
    withdrawalCount: 0n,
    forcedTransactionCount: 0n,
    l2TransactionCount: 0n,
    depositCount: 0n,
    totalEventCount: 0n,
    transitionStepCount: 0n,
    validationTraceCount: 0n,
    prevUtxosRoot: empty,
    startTime: 1_000_000n,
    endTime: 1_060_000n,
    blockSlot: 0n,
    expectedNetworkId: 0n,
    minFeeA: 0n,
    minFeeB: 0n,
    prevHeaderHash: bytes(75, 28).toString("hex"),
    operatorVkey: bytes(76, 28).toString("hex"),
    protocolVersion: 1n,
  };
  const headerHash = yield* SDK.hashBlockHeader(header);
  const assetName = SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + headerHash;
  const credential = getAddressDetails(address).paymentCredential!;
  const datum: SDK.LinkedListNodeView = {
    key: { Key: { key: headerHash } },
    next: "Empty",
    data: SDK.castStateQueueNodeToData({
      proven_fraud: null,
      header,
      da_attestation: SDK.NO_DA_ATTESTATION,
    }) as SDK.LinkedListNodeView["data"],
  };
  return yield* serializeStateQueueUTxO({
    utxo: {
      txHash: bytes(74).toString("hex"),
      outputIndex: 0,
      address,
      assets: {
        lovelace: 3_000_000n,
        [toUnit(credential.hash, assetName)]: 1n,
      },
      datum: SDK.encodeLinkedListNodeView(datum),
    },
    datum,
    assetName,
  });
});

/** The node's globals with the signed-intent reason raised, and a history
 * owner that runs producer work directly. */
const heldNode = Effect.gen(function* () {
  const globals = yield* Globals;
  yield* Ref.set(globals.EVENT_HISTORY_OWNER, {
    runProducer: (
      work: (
        token: never,
        assert: Effect.Effect<void>,
        coverage: never,
      ) => Effect.Effect<unknown, unknown, unknown>,
    ) => work("token" as never, Effect.void, {} as never),
  } as never);
  yield* raiseLivenessIncident(
    globals,
    HaltSource.blockConfirmationSignedIntent,
    SIGNED_INTENT_UNDECIDED,
    "replaced block holds its base's slot",
  );
  return globals;
});

const raised = (globals: Globals) =>
  Effect.map(Ref.get(globals.LIVENESS_REASONS), (reasons) =>
    reasons.get(HaltSource.blockConfirmationSignedIntent),
  );

/** Runs `effect` with the node's globals; `run` provides the database and
 * the node config. */
const runNode = <A, E>(
  effect: Effect.Effect<
    A,
    E,
    Globals | SqlClient.SqlClient | Database | NodeConfig
  >,
) =>
  run(
    effect.pipe(Effect.provide(Globals.Default)) as Effect.Effect<
      A,
      E,
      SqlClient.SqlClient
    >,
  );

describe("confirmation tick and the signed-intent reason", () => {
  beforeEach(() => run(seed));

  it("leaves the reason raised when the tick ran its worker with a journal active, or the journal changed under it", async () => {
    const outcome = await runNode(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        yield* activeE();
        // The fixture row's block window is empty; give it one minute.
        yield* sql`UPDATE pending_block_finalizations
          SET block_end_time = block_start_time + interval '1 minute'`;
        const globals = yield* heldNode;
        const latestBlocksUTxO = yield* tipBlock;
        let calls = 0;
        const reply =
          (output: WorkerOutput): ConfirmationWorkerRunner =>
          () =>
            Effect.sync(() => {
              calls += 1;
              return output;
            });
        const tick = (runner: ConfirmationWorkerRunner) =>
          blockConfirmationStep(confirmationTick(runner), globals);

        yield* tick(reply({ type: "NoTxForConfirmationOutput" }));
        const afterNoTx = yield* raised(globals);
        // The journal is retired while the worker runs: its output is stale.
        yield* tick(() =>
          sql`DELETE FROM pending_block_finalizations`.pipe(
            Effect.orDie,
            Effect.zipRight(
              reply({
                type: "SuccessfulConfirmationOutput",
                latestBlocksUTxO,
                matchedPendingBlocksUTxO: null,
                canonicalHeaders: [],
              })(undefined as never),
            ),
          ),
        );
        const afterChange = yield* raised(globals);
        return { calls, afterNoTx, afterChange };
      }),
    );
    expect(outcome.calls).toBe(2);
    expect(outcome.afterNoTx).toBe(SIGNED_INTENT_UNDECIDED);
    expect(outcome.afterChange).toBe(SIGNED_INTENT_UNDECIDED);
  });

  it("leaves the reason raised on a tick that returned before running its worker, and clears it once a tick reaches the decision", async () => {
    const outcome = await runNode(
      Effect.gen(function* () {
        const globals = yield* heldNode;
        const latestBlocksUTxO = yield* tipBlock;
        let calls = 0;
        const runner: ConfirmationWorkerRunner = () =>
          Effect.sync(() => {
            calls += 1;
            return {
              type: "SuccessfulConfirmationOutput",
              latestBlocksUTxO,
              matchedPendingBlocksUTxO: null,
              canonicalHeaders: [],
            };
          });
        const tick = blockConfirmationStep(confirmationTick(runner), globals);

        yield* Ref.set(globals.RESET_IN_PROGRESS, true);
        yield* tick;
        const afterEarlyReturn = yield* raised(globals);
        const callsAfterEarlyReturn = calls;
        yield* Ref.set(globals.RESET_IN_PROGRESS, false);
        yield* tick;
        const afterDecision = yield* raised(globals);
        return {
          afterEarlyReturn,
          callsAfterEarlyReturn,
          afterDecision,
          calls,
        };
      }),
    );
    expect(outcome.callsAfterEarlyReturn).toBe(0);
    expect(outcome.afterEarlyReturn).toBe(SIGNED_INTENT_UNDECIDED);
    expect(outcome.calls).toBe(1);
    expect(outcome.afterDecision).toBeUndefined();
  });
});
