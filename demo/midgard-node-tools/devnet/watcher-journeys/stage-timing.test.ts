import { mkdtemp, readFile, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { expect, it } from "vitest";

import { measureJourneyStage } from "./stage-timing.js";

it("records failed startup duration and preserves the original error before diagnostics exist", async () => {
  const directory = await mkdtemp(join(tmpdir(), "journey-timing-"));
  try {
    const failure = new Error("operations endpoint did not bind");
    await expect(
      measureJourneyStage(directory, "normal watcher launcher", async () => {
        throw failure;
      }),
    ).rejects.toBe(failure);
    expect(
      await measureJourneyStage(directory, "next attempt", async () => 42),
    ).toBe(42);
    const records = (await readFile(join(directory, "timings.ndjson"), "utf8"))
      .trim()
      .split("\n")
      .map((line) => JSON.parse(line));
    expect(records).toHaveLength(2);
    expect(records[0]).toMatchObject({
      stage: "normal watcher launcher",
      outcome: "failed",
      error: { message: failure.message },
    });
    expect(records[1]).toMatchObject({
      stage: "next attempt",
      outcome: "completed",
    });
    expect(
      records.every(
        (record) =>
          record.seconds >= 0 &&
          Date.parse(record.completedAt) >= Date.parse(record.startedAt),
      ),
    ).toBe(true);
  } finally {
    await rm(directory, { recursive: true, force: true });
  }
});

it("records a failed stage's whole cause chain, Ogmios data included", async () => {
  const directory = await mkdtemp(join(tmpdir(), "journey-timing-"));
  try {
    const failure = new Error("Header submission x is unresolved", {
      cause: {
        message: "RejectTx",
        data: { reason: "ValueNotConserved", n: 1n },
      },
    });
    await expect(
      measureJourneyStage(directory, "s", async () => {
        throw failure;
      }),
    ).rejects.toBe(failure);
    const [entry] = (await readFile(join(directory, "timings.ndjson"), "utf8"))
      .trim()
      .split("\n")
      .map((line) => JSON.parse(line));
    expect(entry.error.message).toBe("Header submission x is unresolved");
    expect(entry.error.chain).toEqual([
      "Header submission x is unresolved",
      'RejectTx {"reason":"ValueNotConserved","n":"1"}',
    ]);
  } finally {
    await rm(directory, { recursive: true, force: true });
  }
});
