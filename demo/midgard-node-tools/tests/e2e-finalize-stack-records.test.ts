import { mkdir, readFile, writeFile } from "node:fs/promises";
import { join } from "node:path";

import { describe, expect, it } from "vitest";

import {
  readStackAttempts,
  STACK_DEPLOYMENT_COMMAND_IDS,
  stackAttemptQualityGate,
  stackDatabaseGates,
  stackFreshDeploymentGate,
  stackSettlementTargets,
} from "../src/commands/e2e-finalize-summary.js";
import { parseJournal } from "../src/full-stack/journal.js";
import {
  DEPOSIT_EVENT,
  honestRun,
  INTENT_DIGEST,
  makeRunDirectory,
  MANIFEST_ID,
  RUN_ID,
  WITHDRAWAL_EVENT,
} from "./e2e-finalize-stack-run.fixture.js";
import { step } from "./e2e-summary.stress-summary.js";

const uuid = (n: number) =>
  `${n.toString(16).padStart(8, "0")}-0000-4000-8000-000000000000`;

/** Writes `attempts/` as StackProcesses.command does: a log, then its record. */
async function attemptsDirectory(
  attempts: readonly {
    id: string;
    status: Parameters<typeof step>[0]["status"];
    exitCode?: number;
    record?: boolean;
  }[],
) {
  const runDirectory = await makeRunDirectory();
  const directory = join(runDirectory, "attempts");
  await mkdir(directory);
  for (const [index, attempt] of attempts.entries()) {
    const name = `${attempt.id}-${uuid(index)}`;
    await writeFile(join(directory, `${name}.log`), "");
    if (attempt.record === false) continue;
    const summary = step({ id: attempt.id, status: attempt.status });
    await writeFile(
      join(directory, `${name}.json`),
      JSON.stringify(
        attempt.exitCode === undefined
          ? summary
          : { ...summary, exitCode: attempt.exitCode },
      ),
    );
  }
  return runDirectory;
}

const deploymentAttempts = STACK_DEPLOYMENT_COMMAND_IDS.map((id) => ({
  id,
  status: "success" as const,
}));

describe("e2e-finalize-summary stack attempts (process.ts)", () => {
  it("accepts a fresh run that ran every deployment-creating command cleanly", async () => {
    const attempts = await readStackAttempts(
      await attemptsDirectory([
        { id: "providers-kupo", status: "success" },
        ...deploymentAttempts,
      ]),
    );
    expect(stackFreshDeploymentGate(attempts)).toMatchObject({
      label: "stack_fresh_deployment",
      status: "satisfied",
      details: { missing: "" },
    });
    expect(stackAttemptQualityGate(attempts)).toMatchObject({
      label: "stack_attempt_quality",
      status: "satisfied",
    });
  });
  it("refuses fresh mode for a run that attached to an existing deployment", async () => {
    const attempts = await readStackAttempts(
      await attemptsDirectory(
        deploymentAttempts.filter(({ id }) => id !== "initialize-submit"),
      ),
    );
    expect(stackFreshDeploymentGate(attempts)).toMatchObject({
      status: "failed",
      details: { missing: "initialize-submit" },
    });
  });
  it("does not count a command the per-command lock refused as run", async () => {
    const attempts = await readStackAttempts(
      await attemptsDirectory([
        ...deploymentAttempts.filter(({ id }) => id !== "references-publish"),
        { id: "references-publish", status: "failed", exitCode: 75 },
      ]),
    );
    expect(stackFreshDeploymentGate(attempts)).toMatchObject({
      status: "failed",
      details: { missing: "references-publish" },
    });
    expect(stackAttemptQualityGate(attempts)).toMatchObject({
      status: "satisfied",
      details: { notStarted: "1", failed: "" },
    });
  });
  it("reads a log without its record as a command the controller stopped mid-run", async () => {
    const attempts = await readStackAttempts(
      await attemptsDirectory([
        ...deploymentAttempts,
        { id: "cycle-0-deposit-submit", status: "success", record: false },
      ]),
    );
    expect(attempts.unfinished).toEqual(["cycle-0-deposit-submit"]);
    expect(stackAttemptQualityGate(attempts)).toMatchObject({
      status: "interrupted",
      details: { unfinished: "cycle-0-deposit-submit" },
    });
  });
  it("marks a failed or unreadable attempt as a failed clean run", async () => {
    const failed = await readStackAttempts(
      await attemptsDirectory([
        ...deploymentAttempts,
        { id: "cycle-0-withdrawal-submit", status: "failed" },
      ]),
    );
    expect(stackAttemptQualityGate(failed)).toMatchObject({
      status: "failed",
      details: { failed: "cycle-0-withdrawal-submit:failed" },
    });
    const directory = await attemptsDirectory(deploymentAttempts);
    await writeFile(
      join(directory, "attempts", `nonce-create-or-resume-${uuid(99)}.json`),
      JSON.stringify(step({ id: "initialize-submit", status: "success" })),
    );
    expect(
      stackAttemptQualityGate(await readStackAttempts(directory)),
    ).toMatchObject({
      status: "failed",
      details: { malformed: `nonce-create-or-resume-${uuid(99)}.json` },
    });
  });
});

describe("e2e-finalize-summary stack database (storage.ts, payout-body.ts)", () => {
  const complete = [{ phase: "complete" }];
  async function database(
    change: (settlements: Map<string, any>) => void = () => {},
  ) {
    const run = await honestRun();
    const journal = parseJournal(
      JSON.parse(
        await readFile(
          join(run.expectation.runDirectory, "stack-journal.json"),
          "utf8",
        ),
      ),
      INTENT_DIGEST,
    );
    const settlements = new Map<string, any>([
      [`deposit:${DEPOSIT_EVENT}`, { jobs: complete, attempts: [] }],
      [
        `withdrawal:${WITHDRAWAL_EVENT}`,
        {
          jobs: complete,
          attempts: [
            {
              phase: "conclude",
              status: "final",
              txHash: run.payout.txHash,
              signedCbor: run.payout.cbor,
            },
          ],
        },
      ],
    ]);
    change(settlements);
    return { journal, settlements, payout: run.payout };
  }
  const gates = (
    value: Awaited<ReturnType<typeof database>>,
    marker: { runId: string; manifestId: string } | null = {
      runId: RUN_ID,
      manifestId: MANIFEST_ID,
    },
  ) =>
    stackDatabaseGates({
      journal: value.journal,
      cycles: 1,
      manifestId: MANIFEST_ID,
      observation: { marker, settlements: value.settlements },
    });

  it("names the journal's settlement events and accepts the honest rows", async () => {
    const value = await database();
    expect(stackSettlementTargets(value.journal, 1)).toEqual([
      { cycle: 0, kind: "deposit", eventId: DEPOSIT_EVENT },
      { cycle: 0, kind: "withdrawal", eventId: WITHDRAWAL_EVENT },
    ]);
    expect(gates(value).map(({ label, status }) => [label, status])).toEqual([
      ["stack_storage_identity", "satisfied"],
      ["stack_settlement", "satisfied"],
    ]);
  });
  it("refuses a database that belongs to another run or has no stack identity", async () => {
    const value = await database();
    expect(
      gates(value, { runId: "another-run", manifestId: MANIFEST_ID })[0],
    ).toMatchObject({
      status: "failed",
      details: { reason: "Storage identity differs from this deployment" },
    });
    expect(gates(value, null)[0]).toMatchObject({
      status: "failed",
      details: { reason: "the database has no stack identity" },
    });
  });
  it("refuses a withdrawal with no confirmed payout", async () => {
    const value = await database((settlements) => {
      settlements.get(`withdrawal:${WITHDRAWAL_EVENT}`).attempts[0].status =
        "expired";
    });
    expect(gates(value)[1]).toMatchObject({
      status: "failed",
      details: {
        "cycle-0":
          "withdrawal settlement has no confirmed payout of a complete job",
      },
    });
  });
  it("refuses a second confirmed payout for the same withdrawal", async () => {
    const value = await database((settlements) => {
      const attempts = settlements.get(
        `withdrawal:${WITHDRAWAL_EVENT}`,
      ).attempts;
      attempts.push({ ...attempts[0], txHash: "0f".repeat(32) });
    });
    expect(gates(value)[1].details["cycle-0"]).toBe(
      "More than one unexpired payout transaction for the same withdrawal",
    );
  });
  it("refuses a confirmed payout that is not the journal's", async () => {
    const value = await database((settlements) => {
      settlements.get(`withdrawal:${WITHDRAWAL_EVENT}`).attempts[0].txHash =
        "0f".repeat(32);
    });
    expect(gates(value)[1].details["cycle-0"]).toBe(
      "confirmed payout differs from the journal's payout",
    );
  });
  it("refuses a deposit whose settlement job is not complete", async () => {
    const value = await database((settlements) => {
      settlements.get(`deposit:${DEPOSIT_EVENT}`).jobs = [
        { phase: "absorbing" },
      ];
    });
    expect(gates(value)[1].details["cycle-0"]).toBe(
      "deposit settlement job is not complete",
    );
  });
});
