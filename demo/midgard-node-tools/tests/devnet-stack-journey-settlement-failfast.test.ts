/**
 * A payout wait gives up early, long before its deadline, only on its own
 * settlement job failing without a break or on a crash-looping settlement
 * worker; another job's errors never end it.
 */
import { afterEach, describe, expect, it } from "vitest";

import {
  FatalJourneyError,
  JourneyDeadlineError,
  type WithdrawalRecord,
} from "../src/devnet-stack/journey.js";
import {
  exited,
  fakeContext,
  json,
  makeJourney,
  removeFakeContexts,
} from "./devnet-stack-journey.fixtures.js";

afterEach(removeFakeContexts);

const withdrawal: WithdrawalRecord = {
  user: "userC",
  l2OutRef: "cc#0",
  l1Address: "addr_test1",
  txHash: "tx-cc#0",
  withdrawalEventId: "event-cc#0",
  l2Value: { lovelace: "5000000" },
};

const idle = {
  durableAdmission: { backlog: "0" },
  localResidue: { mempoolTxCount: "0", processedMempoolTxCount: "0" },
  stateQueue: {
    queueLength: 0,
    unconfirmedSubmittedBlockTxHash: null,
    localFinalizationPending: false,
  },
  pendingBlockFinalizations: { oldestActive: null },
  localMutationJobs: { unfinished: "0" },
  settlement: { unfinishedJobs: "0", failingJobs: [] },
};
const unpayable = "No spendable reserve UTxO can fund the payout of event-cc#0";
/** The awaited withdrawal's job, as /pipeline-status lists a failing one. */
const failingJob = {
  kind: "withdrawal",
  eventId: "event-cc#0",
  phase: "fund",
  failures: 4,
  lastError: unpayable,
  dueAt: "2026-09-30T00:00:00.000Z",
};
const otherJob = {
  ...failingJob,
  kind: "deposit",
  eventId: "event-other",
  phase: "absorb",
  lastError: "Settlement journal output index is invalid",
};

type Health = {
  readonly state: string;
  readonly detail: string;
  readonly workerFailures?: object;
};
/** One /pipeline-status answer: the failing jobs it lists, or a 500. */
type Pipeline = readonly object[] | "down";

/** The node: /readyz answers `health` in turn (503 once unready),
 * /pipeline-status lists `pipeline` in turn, Kupo the payout; the last
 * answer of each repeats. */
const node = (
  health: readonly Health[],
  pipeline: readonly Pipeline[] = [[]],
) => {
  const healthAnswers = [...health];
  const pipelineAnswers = [...pipeline];
  const next = <T>(answers: T[]): T =>
    answers.length > 1 ? answers.shift()! : answers[0]!;
  return (async (input: string | URL) => {
    const url = String(input);
    if (url.includes("/readyz")) {
      const settlement = next(healthAnswers);
      return json(settlement.state === "error" ? 503 : 200, {
        ready: false,
        settlement,
      });
    }
    if (url.includes("/pipeline-status")) {
      const failingJobs = next(pipelineAnswers);
      return failingJobs === "down"
        ? json(500, { error: "database unavailable" })
        : json(200, {
            ...idle,
            settlement: {
              unfinishedJobs: String(failingJobs.length + 1),
              failingJobs,
            },
          });
    }
    return json(200, [{ value: { coins: 5_000_000 } }]);
  }) as unknown as typeof globalThis.fetch;
};
const phases = (...answers: string[]) => ({
  runCli: async () =>
    exited(0, {
      phase: answers.length > 1 ? answers.shift()! : answers[0]!,
    }),
});
const sleep = (ms: number) =>
  new Promise<void>((resolve) => setTimeout(resolve, Math.max(ms, 5)));
const fundThenConcluded = () =>
  phases(...Array.from({ length: 30 }, () => "fund"), "concluded");

/** The awaited withdrawal's pending body failing its reconcile, as the
 * node labels it; the job's record does not carry that failure. */
const awaitedFailure = {
  state: "error",
  detail: `settlement withdrawal event-cc#0 initialize reconcile: transactionStatus ab: socket hang up\n    at stack`,
};
/** Another event's job failing on every one of its turns. */
const otherFailure = {
  state: "error",
  detail:
    "settlement deferred until 2026-09-30T21:00:00.000Z: settlement deposit event-other absorb: Settlement journal output index is invalid",
};
const waiting = {
  state: "waiting",
  detail: "settlement eligibility wait until 2026-09-30T21:00:00.000Z",
};
const running = {
  state: "running",
  detail: "settlement progress checkpointed",
};
const crashDetail =
  "Worker terminated due to reaching memory limit: JS heap out of memory (last reported error: none)";
const crashLoop = {
  state: "error",
  detail: `${crashDetail}\n    at stack`,
  workerFailures: { count: 3, since: 0, last: crashDetail },
};
/** The worker's health between turns of the awaited job in its backoff:
 * other jobs' reports, never one naming the awaited event. */
const interleaved = Array.from(
  { length: 300 },
  (_, i) => [otherFailure, waiting, running][i % 3]!,
);

describe("a payout wait's settlement fail-fast", () => {
  it("gives up once its own job stays failing, however the worker's health interleaves other jobs' reports", async () => {
    const { journey } = makeJourney(fakeContext(), {
      ...phases("fund"),
      fetch: node(interleaved, [[otherJob, failingJob]]),
      sleep,
      deadlines: { payoutMs: 5_000, settlementErrorMs: 50 },
    });
    const started = Date.now();
    const failure = journey.waitPaidOut(withdrawal);
    await expect(failure).rejects.toThrow(FatalJourneyError);
    await expect(failure).rejects.toThrow(
      /the settlement of event-cc#0 has kept failing for \d+ s \(bound 0\.05 s\): settlement job withdrawal event-cc#0 is in phase fund after 4 failed attempts: No spendable reserve UTxO can fund the payout of event-cc#0$/u,
    );
    expect(Date.now() - started).toBeLessThan(5_000);
  });

  it("keeps its job's failing time through an unreadable pipeline status", async () => {
    const { journey } = makeJourney(fakeContext(), {
      ...phases("fund"),
      fetch: node(
        [waiting],
        Array.from({ length: 300 }, (_, i) =>
          i % 2 === 0 ? [failingJob] : "down",
        ),
      ),
      sleep,
      deadlines: { payoutMs: 5_000, settlementErrorMs: 50 },
    });
    await expect(journey.waitPaidOut(withdrawal)).rejects.toThrow(
      /the settlement of event-cc#0 has kept failing/u,
    );
  });

  it("restarts its job's failing time once the job leaves the failing list", async () => {
    const { journey, logs } = makeJourney(fakeContext(), {
      ...fundThenConcluded(),
      // Failing on one read in three, never for the whole bound.
      fetch: node(
        interleaved,
        Array.from({ length: 30 }, (_, i) => (i % 3 === 0 ? [failingJob] : [])),
      ),
      sleep,
      deadlines: { payoutMs: 60 * 60_000, settlementErrorMs: 50 },
    });
    await journey.waitPaidOut(withdrawal);
    expect(logs.at(-1)).toBe("withdrawal event-cc#0 paid out exactly");
  });

  it("gives up once settlement health stays 'error' naming it, the failure of a pending body its job does not record", async () => {
    const { journey } = makeJourney(fakeContext(), {
      ...phases("initialize"),
      fetch: node([awaitedFailure]),
      sleep,
      deadlines: { payoutMs: 60 * 60_000, settlementErrorMs: 50 },
    });
    const started = Date.now();
    const failure = journey.waitPaidOut(withdrawal);
    await expect(failure).rejects.toThrow(FatalJourneyError);
    await expect(failure).rejects.toThrow(
      /settlement health has been 'error' for event-cc#0 for \d+ s \(bound 0\.05 s\): settlement withdrawal event-cc#0 initialize reconcile: transactionStatus ab: socket hang up$/u,
    );
    expect(Date.now() - started).toBeLessThan(10_000);
  });

  it("gives up once the settlement worker keeps crashing, whatever its detail names", async () => {
    const { journey } = makeJourney(fakeContext(), {
      ...phases("fund"),
      fetch: node([crashLoop]),
      sleep,
      deadlines: { payoutMs: 60 * 60_000, settlementErrorMs: 50 },
    });
    await expect(journey.waitPaidOut(withdrawal)).rejects.toThrow(
      /the settlement worker has kept failing \(3 runs in a row\) for \d+ s \(bound 0\.05 s\): Worker terminated due to reaching memory limit/u,
    );
  });

  it("gives up once the settlement worker keeps dying waiting for its ownership lease", async () => {
    // A worker that never takes the lease reports 'starting' between deaths;
    // the node keeps the failure streak on every report.
    const workerFailures = {
      count: 4,
      since: 0,
      last: "Settlement wallet identity changed or another node owns settlement (worker exited 1)",
    };
    const leaseLoop = [
      {
        state: "starting",
        detail: "waiting for the previous settlement ownership lease",
        workerFailures,
      },
      { state: "error", detail: workerFailures.last, workerFailures },
    ];
    const { journey } = makeJourney(fakeContext(), {
      ...phases("fund"),
      fetch: node(Array.from({ length: 41 }, (_, i) => leaseLoop[i % 2]!)),
      sleep,
      deadlines: { payoutMs: 5_000, settlementErrorMs: 50 },
    });
    await expect(journey.waitPaidOut(withdrawal)).rejects.toThrow(
      /settlement worker has kept failing \(4 runs in a row\)/u,
    );
  });

  it("keeps waiting while another event's job stays failing, and names it", async () => {
    const { journey, logs } = makeJourney(fakeContext(), {
      ...fundThenConcluded(),
      // Every read, far beyond the bound, lists and reports the other job.
      fetch: node([otherFailure], [[otherJob]]),
      sleep,
      deadlines: { payoutMs: 60 * 60_000, settlementErrorMs: 50 },
    });
    await journey.waitPaidOut(withdrawal);
    expect(logs.at(-1)).toBe("withdrawal event-cc#0 paid out exactly");

    const timedOut = makeJourney(fakeContext(), {
      ...phases("fund"),
      fetch: node([otherFailure], [[otherJob]]),
      deadlines: { payoutMs: 0 },
    }).journey;
    const failure = timedOut.waitPaidOut(withdrawal);
    await expect(failure).rejects.toThrow(JourneyDeadlineError);
    await expect(failure).rejects.toThrow(
      `payout in phase fund; settlement error: ${otherFailure.detail}`,
    );
  });

  it("keeps waiting through healthy waits and interrupted errors, naming the health it saw", async () => {
    const { journey, logs } = makeJourney(fakeContext(), {
      ...fundThenConcluded(),
      // Errors come and go, but never for the whole bound.
      fetch: node([
        ...Array.from({ length: 30 }, (_, i) =>
          i % 3 === 0 ? awaitedFailure : waiting,
        ),
        waiting,
      ]),
      sleep,
      deadlines: { payoutMs: 60 * 60_000, settlementErrorMs: 50 },
    });
    await journey.waitPaidOut(withdrawal);
    expect(logs.at(-1)).toBe("withdrawal event-cc#0 paid out exactly");

    const timedOut = makeJourney(fakeContext(), {
      ...phases("fund"),
      fetch: node([waiting]),
      deadlines: { payoutMs: 0 },
    }).journey;
    await expect(timedOut.waitPaidOut(withdrawal)).rejects.toThrow(
      `payout in phase fund; settlement waiting: ${waiting.detail}`,
    );
  });
});
