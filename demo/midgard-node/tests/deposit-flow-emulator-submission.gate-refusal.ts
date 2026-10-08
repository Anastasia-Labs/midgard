/**
 * The commit worker's failure path when its intent journal refuses inside
 * the commit's pre-broadcast gate (I1-fix2): the real worker, run once
 * under a follower-backed journal over the flow's node database, whose
 * follower tables track none of the operator's wallet outputs.
 */
import { expect } from "vitest";

import { takeCommitWorkerOutput } from "../src/fibers/block-commitment.promote-or-recover-native-mpf.js";
import { COMMIT_WORKER_FAILED } from "../src/fibers/block-commitment.worker-readiness.js";
import {
  INTENT_INPUT_UNTRACKED,
  intentJournalOver,
} from "../src/services/intent-journal.js";
import { readProviderWalletView } from "../src/services/intent-journal.wallet-view.js";
import { activeLivenessReasons } from "../src/services/liveness-halt.js";
import { alignCommitSchedulerBeforeTestWorker } from "./deposit-flow-emulator-shared.commit-worker-program.js";
import {
  COMMIT_MINIMUM_FUTURE_BUFFER_MS,
  Effect,
  ingestEmulatorEventsUnowned,
  makeGlobalsService,
  makeLucidRuntimeService,
  makeNodeConfigForFixture,
  runCommitWorker,
  runNodeDatabaseEffect,
  SDK,
  SqlClient,
} from "./deposit-flow-emulator-shared.js";
import {
  type EmulatorFixture,
  fixtureDeploymentIdentity,
} from "./deposit-flow-emulator-shared.make-fixture.js";

/**
 * Runs one commit worker pass whose journal refuses inside the gate, and
 * expects a failure output, nothing sent, no signed intent on any pending
 * block, the refusal held under its reason, and the node's readiness
 * naming the worker failure.
 */
export const expectGateRefusedCommitNamed = async ({
  fixture,
  lucidService,
  latestBlock,
}: {
  readonly fixture: EmulatorFixture;
  readonly lucidService: Awaited<ReturnType<typeof makeLucidRuntimeService>>;
  readonly latestBlock: SDK.StateQueueUTxO;
}): Promise<void> => {
  // A commit whose intent journal refuses inside the commit's
  // pre-broadcast gate (a follower-backed journal, over follower tables
  // that track none of the operator's wallet outputs): the worker returns
  // a failure, sends nothing, and leaves the pending block without a
  // signed intent; the node's readiness names it (the commit worker
  // failure, and the refusal's hold) until a worker succeeds.
  await alignCommitSchedulerBeforeTestWorker({
    fixture,
    lucidService,
    targetEndTimeMs: Date.now() + COMMIT_MINIMUM_FUTURE_BUFFER_MS,
  });
  await runNodeDatabaseEffect(ingestEmulatorEventsUnowned(fixture));
  // Its wallet view is the provider's, as if the follower held the
  // operator's wallet outputs, so the build reaches the gate.
  const refusing = await runNodeDatabaseEffect(
    Effect.map(SqlClient.SqlClient, (sql) => ({
      ...intentJournalOver(sql, () => true),
      walletView: readProviderWalletView,
    })),
  );
  const refusedOutput = await runCommitWorker(
    fixture.contracts,
    lucidService,
    latestBlock,
    await makeNodeConfigForFixture(fixture),
    fixtureDeploymentIdentity(fixture),
    undefined,
    refusing,
  );
  expect(refusing.holds().map(({ reason }) => reason)).toEqual([
    INTENT_INPUT_UNTRACKED,
  ]);
  expect(refusedOutput).toMatchObject({
    type: "FailureOutput",
    error: expect.stringContaining(
      "not a tracked fact or an output of a journaled intent",
    ),
  });
  expect(Object.keys(fixture.emulator.mempool)).toEqual([]);
  const refusedPending = await runNodeDatabaseEffect(
    Effect.flatMap(
      SqlClient.SqlClient,
      (sql) => sql<{
        readonly status: string;
        readonly intended: boolean;
        readonly submitted: boolean;
      }>`SELECT status, intended_tx_hash IS NOT NULL AS intended,
          submitted_tx_hash IS NOT NULL AS submitted
        FROM pending_block_finalizations`,
    ),
  );
  expect(refusedPending.length).toBeGreaterThan(0);
  for (const row of refusedPending)
    expect(row).toMatchObject({ intended: false, submitted: false });
  const refusedGlobals = await makeGlobalsService();
  expect(takeCommitWorkerOutput(refusedGlobals, refusedOutput!, 0)).toEqual(
    refusedOutput,
  );
  expect(
    (await Effect.runPromise(activeLivenessReasons(refusedGlobals))).map(
      ({ reason }) => reason,
    ),
  ).toEqual([COMMIT_WORKER_FAILED]);
};
