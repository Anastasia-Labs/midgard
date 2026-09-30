import "./missing-redeemer.missing-redeemer-v1.js";

import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { describe, expect, it } from "vitest";

import { createMissingRedeemerDirectoryJournal } from "../src/missing-redeemer/directory-journal.js";
import {
  type MissingRedeemerDurableState,
  reconcileMissingRedeemerState,
  runMissingRedeemerWorkflow,
} from "../src/missing-redeemer/workflow.js";
import { evidence } from "./missing-redeemer.frontier.js";

describe("missingRedeemer durable workflow", () => {
  it("persists compare-and-append journal state across instances", async () => {
    const directory = await mkdtemp(join(tmpdir(), "missing-redeemer-"));
    try {
      const state: MissingRedeemerDurableState = {
        stage: "step02a",
        scanCursor: 0,
        txHash: "ee".repeat(32),
        outputReference: `${"ee".repeat(32)}#0`,
      };
      const first = await createMissingRedeemerDirectoryJournal(directory);
      await first.append("identity", 0, state);
      const reopened = await createMissingRedeemerDirectoryJournal(directory);
      expect(await reopened.load("identity")).toEqual([state]);
      await expect(reopened.append("identity", 0, state)).rejects.toThrow(
        /compare-and-append conflict/u,
      );
    } finally {
      await rm(directory, { recursive: true });
    }
  });

  it("resumes from authenticated state and permanently removes after mint", async () => {
    const prepared = evidence(
      0,
      Array.from({ length: 33 }, (_, index) => [1, index] as const),
    );
    const entries: MissingRedeemerDurableState[] = [];
    const stages = [
      "step01",
      "step02",
      "step02a",
      "step02b",
      "step03",
      "step04",
      "step04",
      "step04",
      "step05",
      "proven",
      "removed",
    ] as const;
    let submitted = 0;
    let observed: MissingRedeemerDurableState = {
      stage: "none",
      scanCursor: 0,
      txHash: "00".repeat(32),
      outputReference: null,
    };
    const result = await runMissingRedeemerWorkflow({
      evidence: prepared,
      journal: {
        load: async () => entries,
        append: async (_identity, expectedLength, state) => {
          expect(expectedLength).toBe(entries.length);
          entries.push(state);
        },
      },
      actuator: {
        observe: async () => observed,
        submit: async ({ action }) => {
          expect(action).toBe(
            [
              "init",
              "bind",
              "authenticatePurpose",
              "authenticateTrace",
              "authenticateSelection",
              "openRedeemers",
              "scan",
              "scan",
              "scan",
              "finalize",
              "remove",
            ][submitted],
          );
          observed = {
            stage: stages[submitted]!,
            scanCursor:
              stages[submitted] === "step04"
                ? [0, 16, 32][submitted - 5]!
                : stages[submitted] === "step05"
                  ? 33
                  : 0,
            txHash: submitted.toString(16).padStart(64, "0"),
            outputReference:
              stages[submitted] === "removed" ? null : `${submitted}#0`,
          };
          submitted += 1;
          return observed;
        },
      },
    });
    expect(result).toBe("removed");
    expect(entries.map(({ stage }) => stage)).toEqual(stages);
  });

  it("fails closed on journal stage or scan regression", () => {
    const recorded: MissingRedeemerDurableState = {
      stage: "step04",
      scanCursor: 16,
      txHash: "11".repeat(32),
      outputReference: `${"11".repeat(32)}#0`,
    };
    expect(() =>
      reconcileMissingRedeemerState([recorded], {
        ...recorded,
        stage: "step03",
      }),
    ).toThrow(/chain regressed/u);
    expect(() =>
      reconcileMissingRedeemerState([recorded], {
        ...recorded,
        scanCursor: 15,
      }),
    ).toThrow(/checkpoint regressed/u);
  });

  it("treats an observed on-chain cancellation as terminal", async () => {
    const result = await runMissingRedeemerWorkflow({
      evidence: evidence(0, []),
      journal: {
        load: async () => [],
        append: async () => {
          throw new Error("terminal cancellation must not append");
        },
      },
      actuator: {
        observe: async () => ({
          stage: "cancelled",
          scanCursor: 0,
          txHash: "dd".repeat(32),
          outputReference: null,
        }),
        submit: async () => {
          throw new Error("terminal cancellation must not submit");
        },
      },
    });
    expect(result).toBe("cancelled");
  });
});
