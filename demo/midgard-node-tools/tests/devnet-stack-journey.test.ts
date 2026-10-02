import { spawn } from "node:child_process";
import { readFileSync } from "node:fs";
import { join } from "node:path";

import { afterEach, describe, expect, it } from "vitest";

import {
  awaitEarlierAttempts,
  earlierAttempts,
  FatalJourneyError,
  SUBMISSION_MARKER_ENV,
  SUBMISSION_RUN_ENV,
  type TransferRecord,
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

describe("journey submissions", () => {
  it("retries a failed deposit under the same submission id and parameters", async () => {
    const cli = scriptedCli(
      () => exited(1, "Ogmios: connection refused"),
      () => exited(1),
      () => exited(0, { txHash: "d1", metadata: { depositEventId: "e1" } }),
    );
    const { journey, logs } = makeJourney(fakeContext(), {
      runCli: cli.runCli,
    });
    const record = await journey.deposit("A1", "userA", {
      lovelace: 5_000_000n,
    });
    expect(record).toEqual({
      user: "userA",
      value: { lovelace: "5000000" },
      txHash: "d1",
      eventId: "e1",
    });
    expect(cli.calls).toHaveLength(3);
    for (const call of cli.calls) {
      expect(call.submissionId).toBe("run1-A1");
      expect(argAfter(call, "--submission-id")).toBe("run1-A1");
      expect(call.args).toEqual(cli.calls[0]!.args);
    }
    const retries = logs.filter((line) =>
      line.includes("retrying under submission id run1-A1"),
    );
    expect(retries).toHaveLength(2);
    expect(retries[0]).toContain("transcript /transcripts/");
    // Recorded: a second call returns the record without running the CLI.
    expect(
      await journey.deposit("A1", "userA", { lovelace: 5_000_000n }),
    ).toEqual(record);
    expect(cli.calls).toHaveLength(3);
  });

  it("gives up only once the submit deadline passes", async () => {
    const cli = scriptedCli(
      ...Array.from({ length: 50 }, () => () => exited(1)),
    );
    const { journey } = makeJourney(fakeContext(), {
      runCli: cli.runCli,
      deadlines: { submitMs: 0 },
    });
    await expect(
      journey.deposit("A1", "userA", { lovelace: 1n }),
    ).rejects.toThrow(
      /did not complete within 0 s under submission id run1-A1/u,
    );
  });

  it("resumes a deposit interrupted after its intent was journaled", async () => {
    const context = fakeContext();
    const first = scriptedCli(crash);
    void makeJourney(context, { runCli: first.runCli }).journey.deposit(
      "A1",
      "userA",
      {
        lovelace: 7_000_000n,
      },
    );
    await flush();
    expect(first.calls).toHaveLength(1);

    const second = scriptedCli(() =>
      exited(0, { txHash: "d1", metadata: { depositEventId: "e1" } }),
    );
    const { journey } = makeJourney(context, { runCli: second.runCli });
    const record = await journey.deposit("A1", "userA", {
      lovelace: 7_000_000n,
    });
    expect(record.txHash).toBe("d1");
    expect(second.calls[0]!.args).toEqual(first.calls[0]!.args);
    expect(second.calls[0]!.submissionId).toBe("run1-A1");
    expect(journey.deposits()).toEqual([record]);
  });

  it("refuses to resume an intent with different parameters", async () => {
    const context = fakeContext();
    void makeJourney(context, {
      runCli: scriptedCli(crash).runCli,
    }).journey.deposit("A1", "userA", { lovelace: 7_000_000n });
    await flush();
    const second = scriptedCli();
    const { journey } = makeJourney(context, { runCli: second.runCli });
    await expect(
      journey.deposit("A1", "userA", { lovelace: 8_000_000n }),
    ).rejects.toBeInstanceOf(FatalJourneyError);
    expect(second.calls).toHaveLength(0);
  });

  it("resumes an interrupted transfer with --submission-id instead of failing", async () => {
    const context = fakeContext();
    const first = scriptedCli(crash);
    void makeJourney(context, { runCli: first.runCli }).journey.transfer(
      "T1",
      "userA",
      "userB",
      {
        lovelace: 3_000_000n,
      },
    );
    await flush();
    expect(argAfter(first.calls[0]!, "--submission-id")).toBe("run1-T1");

    const result = {
      txId: "t1",
      selectedInputs: ["i#0"],
      requestedAssets: {},
      changeAssets: {},
    };
    const second = scriptedCli(() => exited(0, result));
    const { journey } = makeJourney(context, { runCli: second.runCli });
    const record = await journey.transfer("T1", "userA", "userB", {
      lovelace: 3_000_000n,
    });
    expect(second.calls[0]!.args).toEqual(first.calls[0]!.args);
    expect(second.calls[0]!.args[0]).toBe("submit-l2-transfer");
    expect(argAfter(second.calls[0]!, "--submission-id")).toBe("run1-T1");
    // The CLI's signed-transfer journal lives in this run, not the machine-wide default.
    expect(argAfter(second.calls[0]!, "--submission-journal-dir")).toBe(
      join(context.layout.journeyDir, "l2-transfer-submissions"),
    );
    expect(record).toMatchObject({
      txId: "t1",
      selectedInputs: ["i#0"],
      submissionId: "run1-T1",
    });
    expect(journey.transfers()).toEqual([record]);
  });

  it("keeps a transfer started without a submission id fatal", async () => {
    const context = fakeContext();
    const { journey } = makeJourney(context, { runCli: scriptedCli().runCli });
    journey.journal.set("transfer:T1", { intent: true });
    await expect(
      journey.transfer("T1", "userA", "userB", { lovelace: 1n }),
    ).rejects.toBeInstanceOf(FatalJourneyError);
  });

  it("resumes a withdrawal with the journaled output, address and submission id", async () => {
    const context = fakeContext();
    const utxos = {
      utxos: [
        { txHash: "aa", outputIndex: 0, assets: { lovelace: "9000000" } },
        { txHash: "bb", outputIndex: 1, assets: { lovelace: "4000000" } },
      ],
    };
    const first = scriptedCli(() => exited(0, utxos), crash);
    void makeJourney(context, { runCli: first.runCli }).journey.withdraw(
      "W1",
      "userA",
      1,
      (candidates) => candidates[0]?.outRef,
    );
    await flush();
    expect(first.calls).toHaveLength(2);

    const second = scriptedCli(() =>
      exited(0, {
        txHash: "w1",
        withdrawalEventId: "we1",
        l2Value: { lovelace: "9000000" },
      }),
    );
    const { journey } = makeJourney(context, { runCli: second.runCli });
    const record = await journey.withdraw("W1", "userA", 1, () => {
      throw new Error("the journaled choice must be reused");
    });
    expect(second.calls[0]!.args).toEqual(first.calls[1]!.args);
    expect(argAfter(second.calls[0]!, "--l2-out-ref")).toBe("aa#0");
    expect(argAfter(second.calls[0]!, "--submission-id")).toBe("run1-W1");
    expect(record.l2OutRef).toBe("aa#0");

    // A later withdrawal never picks an output an earlier one claimed.
    const third = scriptedCli(() => exited(0, utxos), crash);
    void makeJourney(context, { runCli: third.runCli }).journey.withdraw(
      "W4",
      "userA",
      2,
      (candidates) => candidates[0]?.outRef,
    );
    await flush();
    expect(argAfter(third.calls[1]!, "--l2-out-ref")).toBe("bb#1");
  });
});

describe("journey waits", () => {
  it("treats refused connections, 5xx and non-JSON bodies as not yet", async () => {
    const answers: (() => Response)[] = [
      () => {
        throw new TypeError("fetch failed", {
          cause: { code: "ECONNREFUSED" },
        });
      },
      () => json(503, { error: "database unavailable" }),
      () => json(200, "<html>proxy</html>"),
      () => json(404, { status: "not_found" }),
      () => json(200, { status: "pending_commit" }),
      () => json(200, { status: "committed" }),
    ];
    const urls: string[] = [];
    const fetch = (async (url: string, init?: RequestInit) => {
      urls.push(url);
      expect(init?.signal).toBeInstanceOf(AbortSignal);
      return answers.shift()!();
    }) as unknown as typeof globalThis.fetch;
    const { journey } = makeJourney(fakeContext(), { fetch });
    await expect(journey.waitTx("t1", "committed")).resolves.toMatchObject({
      status: "committed",
    });
    expect(urls).toHaveLength(6);
    expect(urls[0]).toBe("http://127.0.0.1:3000/tx-status?tx_hash=t1");
  });

  it("fails at once on a rejected transaction", async () => {
    let requests = 0;
    const fetch = (async () => {
      requests += 1;
      return json(200, { status: "rejected", rejectCode: "E_INPUT" });
    }) as unknown as typeof globalThis.fetch;
    const { journey } = makeJourney(fakeContext(), { fetch });
    await expect(journey.waitTx("t1", "committed")).rejects.toBeInstanceOf(
      FatalJourneyError,
    );
    expect(requests).toBe(1);
  });

  it("fails once a wait's deadline passes", async () => {
    const fetch = (async () =>
      json(503, {})) as unknown as typeof globalThis.fetch;
    const { journey } = makeJourney(fakeContext(), { fetch });
    await expect(journey.waitTx("t1", "committed", 0)).rejects.toThrow(
      /timed out after 0 s waiting for L2 tx t1 to be committed: GET \/tx-status\?tx_hash=t1 answered 503/u,
    );
  });

  it("resubmits a transfer the node does not know under its submission id", async () => {
    const statuses = [
      json(404, { status: "not_found" }),
      json(200, { status: "committed" }),
    ];
    const fetch = (async () =>
      statuses.shift()!) as unknown as typeof globalThis.fetch;
    const cli = scriptedCli(() =>
      exited(0, { txId: "t1", selectedInputs: [] }),
    );
    const { journey } = makeJourney(fakeContext(), {
      fetch,
      runCli: cli.runCli,
      resubmitAfterMs: 0,
    });
    const record: TransferRecord = {
      from: "userA",
      to: "userB",
      value: { lovelace: "3000000" },
      txId: "t1",
      selectedInputs: [],
      submissionId: "run1-T1",
    };
    await journey.waitTransfer(record, "committed");
    expect(cli.calls).toHaveLength(1);
    expect(cli.calls[0]!.submissionId).toBe("run1-T1");
    expect(argAfter(cli.calls[0]!, "--submission-id")).toBe("run1-T1");
    expect(argAfter(cli.calls[0]!, "--lovelace")).toBe("3000000");
  });

  it("retries payout-status and the Kupo read through outages", async () => {
    const cli = scriptedCli(
      () => exited(1, "ECONNREFUSED 127.0.0.1:5432"),
      () => exited(0, { phase: "concluded" }),
    );
    const kupo = [
      () => {
        throw new TypeError("fetch failed", { cause: { code: "ECONNRESET" } });
      },
      () => json(200, []),
      () => json(200, [{ value: { coins: 9_000_000 } }]),
    ];
    const fetch = (async () =>
      kupo.shift()!()) as unknown as typeof globalThis.fetch;
    const { journey, logs } = makeJourney(fakeContext(), {
      runCli: cli.runCli,
      fetch,
    });
    await journey.waitPaidOut({
      user: "userA",
      l2OutRef: "aa#0",
      l1Address: "addr_test1",
      txHash: "w1",
      withdrawalEventId: "we1",
      l2Value: { lovelace: "9000000" },
    });
    expect(logs.at(-1)).toBe("withdrawal we1 paid out exactly");
  });

  it("fails at once when a payout exceeds the withdrawn value", async () => {
    const cli = scriptedCli(() => exited(0, { phase: "concluded" }));
    const fetch = (async () =>
      json(200, [
        { value: { coins: 10_000_000 } },
      ])) as unknown as typeof globalThis.fetch;
    const { journey } = makeJourney(fakeContext(), {
      runCli: cli.runCli,
      fetch,
    });
    await expect(
      journey.waitPaidOut({
        user: "userA",
        l2OutRef: "aa#0",
        l1Address: "addr_test1",
        txHash: "w1",
        withdrawalEventId: "we1",
        l2Value: { lovelace: "9000000" },
      }),
    ).rejects.toThrow(/expected exactly/u);
  });
});

describe("journey phases", () => {
  it("skips a completed phase on a rerun", async () => {
    const context = fakeContext();
    let runs = 0;
    await makeJourney(context).journey.phase("deposits", async () => {
      runs += 1;
    });
    const { journey, logs } = makeJourney(context);
    await journey.phase("deposits", async () => {
      runs += 1;
    });
    expect(runs).toBe(1);
    expect(logs).toContain("phase deposits already done");
    const journal = JSON.parse(
      readFileSync(join(context.layout.journeyDir, "journey.json"), "utf8"),
    );
    expect(journal.entries["phase:deposits"]).toBe("done");
  });

  it("leaves a failed phase to run again", async () => {
    const context = fakeContext();
    await expect(
      makeJourney(context).journey.phase("drain", async () => {
        throw new Error("interrupted");
      }),
    ).rejects.toThrow("interrupted");
    let runs = 0;
    await makeJourney(context).journey.phase("drain", async () => {
      runs += 1;
    });
    expect(runs).toBe(1);
  });
});

describe("earlier CLI attempts", () => {
  const holdOpen = (env: Record<string, string>) =>
    spawn(process.execPath, ["-e", "setTimeout(() => {}, 60_000)"], {
      env: { PATH: process.env.PATH, ...env },
      stdio: "ignore",
    });

  it("finds a left-over attempt of a submission in this run and stops it", async () => {
    // Same submission id, another run (run ids repeat across run directories).
    const foreign = holdOpen({
      [SUBMISSION_RUN_ENV]: "/other/run/stack/journey",
      [SUBMISSION_MARKER_ENV]: "run1-test-orphan",
    });
    const child = holdOpen({
      [SUBMISSION_RUN_ENV]: "/this/run/stack/journey",
      [SUBMISSION_MARKER_ENV]: "run1-test-orphan",
    });
    try {
      const pid = child.pid!;
      const deadline = Date.now() + 5_000;
      while (
        !earlierAttempts(
          "/this/run/stack/journey",
          "run1-test-orphan",
        ).includes(pid)
      ) {
        if (Date.now() > deadline)
          throw new Error("the attempt was never found");
        await new Promise((resolve) => setTimeout(resolve, 25));
      }
      expect(
        earlierAttempts("/this/run/stack/journey", "run1-other"),
      ).not.toContain(pid);
      expect(
        earlierAttempts("/this/run/stack/journey", "run1-test-orphan"),
      ).not.toContain(foreign.pid);
      const logs: string[] = [];
      await awaitEarlierAttempts(
        "/this/run/stack/journey",
        "run1-test-orphan",
        0,
        (message) => logs.push(message),
        (ms) => new Promise((resolve) => setTimeout(resolve, Math.min(ms, 50))),
      );
      expect(
        earlierAttempts("/this/run/stack/journey", "run1-test-orphan"),
      ).toEqual([]);
      expect(logs.some((line) => line.includes("SIGTERM"))).toBe(true);
      // The other run's attempt was never signalled.
      expect(foreign.exitCode).toBeNull();
      expect(foreign.signalCode).toBeNull();
      expect(
        earlierAttempts("/other/run/stack/journey", "run1-test-orphan"),
      ).toContain(foreign.pid);
    } finally {
      child.kill("SIGKILL");
      foreign.kill("SIGKILL");
    }
  });
});
