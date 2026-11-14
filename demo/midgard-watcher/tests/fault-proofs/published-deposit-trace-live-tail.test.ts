import * as SDK from "@al-ft/midgard-sdk";
import { paymentCredentialOf } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import {
  createPublishedWorkflowDeploymentAccounts,
  publishWorkflowDeployment,
} from "midgard-node/tests/helpers/published-workflow-deployment";
import { expect, it, vi } from "vitest";

import {
  createPublishedWatcherBlockActor,
  type PublishedWatcherBlock,
} from "../support/published-block-actor.js";
import {
  DEPOSIT_BLOCK_INTERVAL_MS,
  stagePublishedDepositTrace,
} from "../support/published-deposit-trace.js";

/** Every state-queue entry in list order: the root's confirmed hash, then each header. */
const queueOrder = async (
  deployment: Awaited<ReturnType<typeof publishWorkflowDeployment>>,
) =>
  (
    await Effect.runPromise(
      SDK.fetchSortedStateQueueUTxOsProgram(deployment.operatorLucid, {
        stateQueueAddress:
          deployment.contracts.stateQueue.spendingScriptAddress,
        stateQueuePolicyId: deployment.contracts.stateQueue.policyId,
      }),
    )
  ).map(({ datum }) => (datum.key === "Empty" ? "root" : datum.key.Key.key));

/** A published deployment whose queue already holds an honest deposit trace. */
const liveQueue = async () => {
  const accounts = createPublishedWorkflowDeploymentAccounts();
  const deployment = await publishWorkflowDeployment({ accounts });
  vi.spyOn(Date, "now").mockImplementation(() => deployment.emulator.now());
  const history = SDK.eventHistoryDeploymentFromContracts(
    SDK.requireEventHistoryContracts(deployment.contracts).deposit,
  );
  const protectedUntil = SDK.authenticateHistoryNodes(
    await deployment.operatorLucid.utxosAt(history.address),
    history,
  ).reduce(
    (latest, { node }) =>
      node.protected_until > latest ? node.protected_until : latest,
    0n,
  );
  await deployment.chain.awaitLedgerTime(Number(protectedUntil) + 60_000);
  const daSignerConfig = {
    L1_OPERATOR_SEED_PHRASE: accounts.operator.seedPhrase,
    DA_COSIGNER_SEED_PHRASE: accounts.cosigner.seedPhrase,
    NETWORK: "Preprod",
  } as const;
  const tail = await stagePublishedDepositTrace(deployment, {
    daSignerConfig,
    honest: true,
  });
  return { deployment, daSignerConfig, tail };
};

// A live deployment's queue is not empty when the transition-trace fixture
// stages: it must extend the queue's tail from that tail's replayed ledger
// rather than restart from the confirmed state.
it("stages the transition trace after a live queue tail", async () => {
  const { deployment, daSignerConfig, tail } = await liveQueue();

  // A tail ledger that does not replay to the committed root, or none at
  // all, is refused before anything is committed.
  await expect(
    stagePublishedDepositTrace(deployment, {
      daSignerConfig,
      liveTail: { ledgerAt: async () => tail.predecessor.postLedgerEntries },
    }),
  ).rejects.toThrow(/differs from the predecessor root/u);
  await expect(
    stagePublishedDepositTrace(deployment, {
      daSignerConfig,
      liveTail: { ledgerAt: async () => undefined },
    }),
  ).rejects.toThrow(/has no independently replayed ledger/u);

  const staged = await stagePublishedDepositTrace(deployment, {
    daSignerConfig,
    liveTail: {
      ledgerAt: async (headerHash) =>
        headerHash === tail.current.headerHash
          ? tail.current.postLedgerEntries
          : undefined,
    },
  });

  expect(staged.predecessor.header.prevHeaderHash).toBe(
    tail.current.headerHash,
  );
  expect(staged.predecessor.header.prevUtxosRoot).toBe(
    tail.current.header.utxosRoot,
  );
  expect(staged.current.header.prevHeaderHash).toBe(
    staged.predecessor.headerHash,
  );
  expect(staged.commits).toHaveLength(2);
  expect(await queueOrder(deployment)).toEqual([
    "root",
    tail.predecessor.headerHash,
    tail.current.headerHash,
    staged.predecessor.headerHash,
    staged.current.headerHash,
  ]);
}, 1_800_000);

// On an attached stack the stack's own committee attests and the stack's own
// node builds the honest successor; the trace never signs as the committee.
it("stages the transition trace on an attached stack through its committee", async () => {
  const { deployment, daSignerConfig, tail } = await liveQueue();
  const committee = await createPublishedWatcherBlockActor({
    deployment,
    lucid: deployment.publisherLucid,
    daSignerConfig,
  });
  const answered: string[] = [];
  /** Each late-shift wait: for whom, the queue it saw, and when it ended. */
  const lateShift: {
    operator: string;
    queue: string[];
    waitedUntil: number;
  }[] = [];
  const attachedStack = {
    attest: async (block: PublishedWatcherBlock) => {
      answered.push(block.headerHash);
      return committee.attest(block);
    },
    // The trace's span under the emulator profile, and the wait below.
    shiftWorkMs:
      Number(SDK.MAX_EVENT_VALIDITY_RANGE_LENGTH_MS) +
      SDK.EVENT_WAIT_DURATION_MS +
      DEPOSIT_BLOCK_INTERVAL_MS +
      61_000,
    // The first wait outlasts the fault block's interval, as the wait for
    // the end of operator-2's shift does.
    beforeFaultCommit: async (operator: string) => {
      if (lateShift.length === 0)
        await deployment.chain.awaitLedgerTime(deployment.chain.now() + 61_000);
      lateShift.push({
        operator,
        queue: await queueOrder(deployment),
        waitedUntil: deployment.chain.now(),
      });
    },
    beforeTraceDeposit: async () => {},
  };
  const ledgerAt = async (headerHash: string) =>
    headerHash === tail.current.headerHash
      ? tail.current.postLedgerEntries
      : undefined;

  await expect(
    stagePublishedDepositTrace(deployment, { attachedStack }),
  ).rejects.toThrow(/extends its live tail/u);
  // Work no shift can hold is refused before the trace publishes anything.
  await expect(
    stagePublishedDepositTrace(deployment, {
      attachedStack: {
        ...attachedStack,
        shiftWorkMs: Number(SDK.SHIFT_DURATION_MS) + 1,
      },
      liveTail: { ledgerAt },
    }),
  ).rejects.toThrow(/shift covers/u);
  await expect(
    stagePublishedDepositTrace(deployment, {
      daSignerConfig,
      attachedStack,
      liveTail: { ledgerAt },
    }),
  ).rejects.toThrow(/own committee or an attached stack's/u);
  expect(answered).toEqual([]);

  const staged = await stagePublishedDepositTrace(deployment, {
    attachedStack,
    liveTail: { ledgerAt },
  });

  expect(staged.predecessor.header.prevHeaderHash).toBe(
    tail.current.headerHash,
  );
  expect(staged.commits).toHaveLength(2);
  // The fault's commit waits for its late-shift window once the empty block
  // is on the queue, and is rebuilt once its interval passed in that wait.
  expect(lateShift.length).toBeGreaterThanOrEqual(1);
  expect(new Set(lateShift.map(({ operator }) => operator))).toEqual(
    new Set([
      paymentCredentialOf(await deployment.operatorLucid.wallet().address())
        .hash,
    ]),
  );
  expect(lateShift[0]!.queue).toEqual([
    "root",
    tail.predecessor.headerHash,
    tail.current.headerHash,
    staged.predecessor.headerHash,
  ]);
  expect(Number(staged.current.header.endTime)).toBeGreaterThan(
    lateShift[0]!.waitedUntil,
  );
  expect([...new Set(answered)]).toEqual([
    staged.predecessor.headerHash,
    staged.current.headerHash,
  ]);
  expect(await queueOrder(deployment)).toEqual([
    "root",
    tail.predecessor.headerHash,
    tail.current.headerHash,
    staged.predecessor.headerHash,
    staged.current.headerHash,
  ]);
  await expect(staged.commitHonestSuccessor()).rejects.toThrow(
    /own node builds its honest successor/u,
  );
}, 1_800_000);
