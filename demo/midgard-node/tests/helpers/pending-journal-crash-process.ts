import { MIDGARD_CONSENSUS_PROFILE_V1_ID } from "@al-ft/midgard-core/consensus-profile-v1";
import { EMPTY_MERKLE_TREE_ROOT } from "@al-ft/midgard-sdk";
import { Effect } from "effect";

import { DepositsDB, PendingBlockFinalizationsDB } from "@/database/index.js";
import { Database } from "@/services/database.js";

const mode = process.argv[2] as "before" | "during" | "after" | undefined;
const depositIdHex = process.argv[3];
const headerHashHex = process.argv[4];

const killSelf = (): never => {
  process.kill(process.pid, "SIGKILL");
  throw new Error("SIGKILL did not terminate pending-journal crash probe");
};

const emptyRoots = {
  utxosRoot: EMPTY_MERKLE_TREE_ROOT,
  forcedTransactionsRoot: EMPTY_MERKLE_TREE_ROOT,
  transactionsRoot: EMPTY_MERKLE_TREE_ROOT,
  depositsRoot: EMPTY_MERKLE_TREE_ROOT,
  withdrawalsRoot: EMPTY_MERKLE_TREE_ROOT,
};

const program = Effect.gen(function* () {
  if (
    mode === undefined ||
    depositIdHex === undefined ||
    headerHashHex === undefined
  ) {
    throw new Error("Crash probe requires mode, deposit id, and header hash");
  }
  if (mode === "before") killSelf();

  const depositId = Buffer.from(depositIdHex, "hex");
  const deposit = (yield* DepositsDB.retrieveAllEntries()).find((entry) =>
    entry[DepositsDB.Columns.ID].equals(depositId),
  );
  if (deposit === undefined) {
    throw new Error(`Crash probe deposit not found: ${depositIdHex}`);
  }
  const input: PendingBlockFinalizationsDB.PrepareInput = {
    headerHash: Buffer.from(headerHashHex, "hex"),
    headerCbor: Buffer.from("00", "hex"),
    metadata: {
      consensusProfileId: MIDGARD_CONSENSUS_PROFILE_V1_ID,
      stateQueueLeaseToken: "crash-probe-lease",
      baseSnapshotId: "crash-probe-snapshot",
      baseTailOutRef: "crash-probe#0",
      baseTailHeaderHash: Buffer.alloc(28, 0x11),
      baseTailDatumCbor: "d87980",
      baseRoots: emptyRoots,
      blockStartTime: new Date("2026-04-13T18:00:00.000Z"),
      expectedRoots: {
        ...emptyRoots,
        transitionTraceRoot: EMPTY_MERKLE_TREE_ROOT,
        eventToStepRoot: EMPTY_MERKLE_TREE_ROOT,
        validationTracesRoot: EMPTY_MERKLE_TREE_ROOT,
      },
      expectedCounts: {
        withdrawalCount: 0n,
        forcedTransactionCount: 0n,
        l2TransactionCount: 0n,
        depositCount: 1n,
        totalEventCount: 1n,
        transitionStepCount: 1n,
        validationTraceCount: 0n,
      },
    },
    blockEndTime: new Date("2026-04-13T18:01:00.000Z"),
    depositEventIds: [depositId],
    depositEntries: [deposit],
    forcedTransactionEventIds: [],
    forcedTransactionEntries: [],
    withdrawalEventIds: [],
    withdrawalEntries: [],
    mempoolTxIds: [],
    mempoolTxs: [],
    mempoolTxSourceTable: "none",
    transitionTraceMembers: [],
    eventToStepMembers: [],
    validationTraceMembers: [],
    ledgerDelta: {
      spent: [],
      produced: [],
    },
  };
  yield* PendingBlockFinalizationsDB.preparePendingSubmission(input, {
    beforeJournalInsert: DepositsDB.markAwaitingAsProjected([depositId]).pipe(
      Effect.andThen(mode === "during" ? Effect.sync(killSelf) : Effect.void),
    ),
  });
  killSelf();
});

void Effect.runPromise(program.pipe(Effect.provide(Database.layer))).catch(
  (error: unknown) => {
    process.stderr.write(
      `${error instanceof Error ? error.stack : String(error)}\n`,
    );
    process.exitCode = 1;
  },
);
