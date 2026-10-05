import { afterEach, describe, expect, it } from "vitest";

import {
  definitiveCliFailure,
  drained,
  drainResidue,
  FatalJourneyError,
  type WithdrawalRecord,
} from "../src/devnet-stack/journey.js";
import {
  argAfter,
  crash,
  exited,
  fakeContext,
  flush,
  json,
  makeJourney,
  removeFakeContexts,
  scriptedCli,
} from "./devnet-stack-journey.fixtures.js";

afterEach(removeFakeContexts);

const withdrawal = (
  user: WithdrawalRecord["user"],
  l2OutRef: string,
): WithdrawalRecord => ({
  user,
  l2OutRef,
  l1Address: "addr_test1",
  txHash: `tx-${l2OutRef}`,
  withdrawalEventId: `event-${l2OutRef}`,
  l2Value: { lovelace: "5000000" },
});

const excludedIn = (args: readonly string[]) =>
  args.flatMap((arg, index) =>
    arg === "--exclude-out-ref" ? [args[index + 1]] : [],
  );

const transferResult = {
  txId: "t7",
  selectedInputs: ["ee#0"],
  status: "queued",
};

describe("journey transfers never spend an output their withdrawals claimed", () => {
  it("excludes the sender's withdrawn outputs, fixed at the first journaling", async () => {
    const context = fakeContext();
    const first = makeJourney(context, { runCli: scriptedCli(crash).runCli });
    first.journey.journal.set("withdrawal:W3", withdrawal("userC", "cc#0"));
    first.journey.journal.set("withdrawal:W1", withdrawal("userA", "aa#0"));
    // Journaled before withdrawal intents recorded their user.
    first.journey.journal.set("withdrawal:W0", {
      l2OutRef: "dd#1",
      l1Address: "addr_test1",
    });
    const firstCli = scriptedCli(crash);
    void makeJourney(context, { runCli: firstCli.runCli }).journey.transfer(
      "T7",
      "userC",
      "userB",
      { lovelace: 5_000_000n },
    );
    await flush();
    expect(firstCli.calls[0]!.args[0]).toBe("submit-l2-transfer");
    expect(excludedIn(firstCli.calls[0]!.args)).toEqual(["cc#0", "dd#1"]);

    // A withdrawal journaled after the intent does not change the intent:
    // the CLI refuses a submission id asked for with other exclusions.
    const second = scriptedCli(() => exited(0, transferResult));
    const { journey } = makeJourney(context, { runCli: second.runCli });
    journey.journal.set("withdrawal:W5", withdrawal("userC", "c5#0"));
    const record = await journey.transfer("T7", "userC", "userB", {
      lovelace: 5_000_000n,
    });
    expect(second.calls[0]!.args).toEqual(firstCli.calls[0]!.args);
    expect(record.excludeOutRefs).toEqual(["cc#0", "dd#1"]);

    // So does the resubmission of a transfer the node lost.
    const statuses = [
      json(404, { status: "not_found" }),
      json(200, { status: "committed" }),
    ];
    const resubmit = scriptedCli(() => exited(0, transferResult));
    const waiting = makeJourney(context, {
      runCli: resubmit.runCli,
      fetch: (async () => statuses.shift()!) as unknown as typeof fetch,
      resubmitAfterMs: 0,
    }).journey;
    await waiting.waitTransfer(record, "committed");
    expect(resubmit.calls[0]!.args).toEqual(firstCli.calls[0]!.args);
  });

  it("resumes a transfer journaled before exclusions with none", async () => {
    const context = fakeContext();
    const cli = scriptedCli(() => exited(0, transferResult));
    const { journey } = makeJourney(context, { runCli: cli.runCli });
    journey.journal.set("withdrawal:W3", withdrawal("userC", "cc#0"));
    journey.journal.set("transfer:T7", {
      submissionId: "run1-T7",
      from: "userC",
      to: "userB",
      value: { lovelace: "5000000" },
    });
    const record = await journey.transfer("T7", "userC", "userB", {
      lovelace: 5_000_000n,
    });
    expect(excludedIn(cli.calls[0]!.args)).toEqual([]);
    expect(record).not.toHaveProperty("excludeOutRefs");
  });
});

describe("journey submissions stop at a failure no retry can repair", () => {
  const failing = (stderr: string) => () => ({
    ...exited(1),
    stderr: `${stderr}\n`,
  });

  it.each([
    "error: unknown option '--exclude-out-ref'",
    "error: required option '--l2-address <address>' not specified",
    "error: unknown command 'submit-l2-transfer'",
    "error: missing required argument 'address'",
    "Error: Submission ID run1-T7 belongs to a different transfer (signer, destination, value or excluded outputs); use a new --submission-id for a new transfer.",
  ])("fails at once on: %s", async (stderr) => {
    const cli = scriptedCli(failing(stderr), () => exited(0, transferResult));
    const { journey } = makeJourney(fakeContext(), { runCli: cli.runCli });
    const failure = await journey
      .transfer("T7", "userC", "userB", { lovelace: 5_000_000n })
      .catch((error: unknown) => error);
    expect(failure).toBeInstanceOf(FatalJourneyError);
    expect(String(failure)).toContain(
      stderr.replace(/^Error: /u, "").slice(0, 40),
    );
    expect(String(failure)).toContain("transcript /transcripts/");
    expect(cli.calls).toHaveLength(1);
  });

  it.each([
    "Error: Insufficient Midgard L2 funds for requested transfer. Missing {}.",
    "Error: No unreserved plain wallet nonce is available",
    "Error: Failed to submit Midgard-native transfer: Error: Submit failed (503): capacity",
    "Error: logged by the node: error: unknown option is not at the start of this line",
  ])("keeps retrying on: %s", async (stderr) => {
    const cli = scriptedCli(failing(stderr), () => exited(0, transferResult));
    const { journey } = makeJourney(fakeContext(), { runCli: cli.runCli });
    await journey.transfer("T7", "userC", "userB", { lovelace: 5_000_000n });
    expect(cli.calls).toHaveLength(2);
    expect(definitiveCliFailure(stderr)).toBeUndefined();
  });

  it("fails at once, naming the tx and the reason, when a resumed transfer was rejected", async () => {
    const cli = scriptedCli(() =>
      exited(0, { ...transferResult, status: "rejected" }),
    );
    const urls: string[] = [];
    const fetch = (async (url: string) => {
      urls.push(url);
      return json(200, {
        txId: "t7",
        status: "rejected",
        reasonCode: "E_ADMISSION_PENDING_WITHDRAWAL_INPUT",
        reasonDetail: "input cc#0 is named by a pending withdrawal",
      });
    }) as unknown as typeof globalThis.fetch;
    const { journey } = makeJourney(fakeContext(), {
      runCli: cli.runCli,
      fetch,
    });
    const failure = await journey
      .transfer("T7", "userC", "userB", { lovelace: 5_000_000n })
      .catch((error: unknown) => error);
    expect(failure).toBeInstanceOf(FatalJourneyError);
    expect(String(failure)).toContain("L2 tx t7");
    expect(String(failure)).toContain(
      "E_ADMISSION_PENDING_WITHDRAWAL_INPUT: input cc#0 is named by a pending withdrawal",
    );
    expect(urls).toEqual(["http://127.0.0.1:3000/tx-status?tx_hash=t7"]);
    expect(cli.calls).toHaveLength(1);
    expect(journey.transfers()).toEqual([]);
  });
});

describe("journey waits know every status", () => {
  it("counts awaiting_local_recovery as past accepted and short of committed", async () => {
    const answers = [
      json(200, { status: "awaiting_local_recovery" }),
      json(200, { status: "awaiting_local_recovery" }),
      json(200, { status: "committed" }),
    ];
    const fetch = (async () =>
      answers.shift()!) as unknown as typeof globalThis.fetch;
    const { journey } = makeJourney(fakeContext(), { fetch });
    await expect(
      journey.waitTx("t1", "accepted", 1_000),
    ).resolves.toMatchObject({
      status: "awaiting_local_recovery",
    });
    await expect(
      journey.waitTx("t1", "committed", 1_000),
    ).resolves.toMatchObject({
      status: "committed",
    });
    expect(answers).toEqual([]);
  });
});

describe("journey drain and payout name the settlement worker's state", () => {
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
  const unpayable =
    "No spendable reserve UTxO can fund the payout of event-cc#0";
  const failingJob = {
    kind: "withdrawal",
    eventId: "event-cc#0",
    phase: "fund",
    failures: 4,
    lastError: unpayable,
    dueAt: "2026-09-30T00:00:00.000Z",
  };

  it("is drained only once no settlement job is unfinished", () => {
    expect(drainResidue(idle)).toEqual([]);
    expect(drained(idle)).toBe(true);
    const { settlement: _settlement, ...withoutSettlement } = idle;
    expect(drainResidue(withoutSettlement)).toEqual([
      "settlement.unfinishedJobs is missing",
    ]);
    const stuck = {
      ...idle,
      settlement: { unfinishedJobs: "2", failingJobs: [failingJob] },
    };
    expect(drained(stuck)).toBe(false);
    expect(drainResidue(stuck)).toEqual([
      `settlement.unfinishedJobs is "2"; withdrawal event-cc#0 fund failed 4 times: ${unpayable}`,
    ]);
    expect(
      drainResidue({
        ...idle,
        stateQueue: { ...idle.stateQueue, queueLength: 1 },
      }),
    ).toEqual(["stateQueue.queueLength is 1"]);
  });

  it("names the payout's failing settlement job when the payout wait times out", async () => {
    const cli = scriptedCli(() => exited(0, { phase: "fund" }));
    const fetch = (async () =>
      json(200, {
        ...idle,
        settlement: {
          unfinishedJobs: "2",
          failingJobs: [{ ...failingJob, eventId: "event-other" }, failingJob],
        },
      })) as unknown as typeof globalThis.fetch;
    const { journey } = makeJourney(fakeContext(), {
      runCli: cli.runCli,
      fetch,
      deadlines: { payoutMs: 0 },
    });
    await expect(
      journey.waitPaidOut(withdrawal("userC", "cc#0")),
    ).rejects.toThrow(
      `settlement job withdrawal event-cc#0 is in phase fund after 4 failed attempts: ${unpayable}`,
    );
    expect(argAfter(cli.calls[0]!, "--withdrawal-event-id")).toBe("event-cc#0");
  });
});
