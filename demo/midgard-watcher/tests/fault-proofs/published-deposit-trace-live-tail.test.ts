import * as SDK from "@al-ft/midgard-sdk";
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
import { stagePublishedDepositTrace } from "../support/published-deposit-trace.js";

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
  const attachedStack = {
    attest: async (block: PublishedWatcherBlock) => {
      answered.push(block.headerHash);
      return committee.attest(block);
    },
  };
  const ledgerAt = async (headerHash: string) =>
    headerHash === tail.current.headerHash
      ? tail.current.postLedgerEntries
      : undefined;

  await expect(
    stagePublishedDepositTrace(deployment, { attachedStack }),
  ).rejects.toThrow(/extends its live tail/u);
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
