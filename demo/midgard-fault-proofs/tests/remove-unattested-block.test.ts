import type { StateQueueUTxO } from "@al-ft/midgard-sdk";
import { describe, expect, it, vi } from "vitest";

import {
  parseTimeoutCorrectionJournal,
  planNextTimeoutCorrection,
  reconcileCompletedTimeoutCorrectionJournal,
  reconcileLastTimeoutCorrectionStep,
  releaseTimeoutCorrectionLeaseBeforeYield,
  type TimeoutCorrectionJournal,
} from "../src/remove-unattested-block.js";

const txHash = (byte: string): string => byte.repeat(64);
const headerHash = (byte: string): string => byte.repeat(56);
const outRef = (byte: string, outputIndex = 0): string =>
  `${txHash(byte)}#${outputIndex.toString()}`;

const root = (byte = "a"): StateQueueUTxO =>
  ({
    assetName: "",
    datum: { key: "Empty", next: "Empty", data: "d87980" },
    utxo: {
      txHash: txHash(byte),
      outputIndex: 0,
      address: "addr_test1root",
      assets: { lovelace: 2_000_000n },
    },
  }) as StateQueueUTxO;

const block = (
  byte: string,
  txByte = byte,
  outputIndex = 0,
): StateQueueUTxO => {
  const hash = headerHash(byte);
  return {
    assetName: `000de140${hash}`,
    datum: {
      key: { Key: { key: hash } },
      next: "Empty",
      data: "d87980",
    },
    utxo: {
      txHash: txHash(txByte),
      outputIndex,
      address: "addr_test1queue",
      assets: { lovelace: 2_000_000n },
    },
  } as StateQueueUTxO;
};

const completedJournal = (): TimeoutCorrectionJournal => ({
  version: 1,
  targetHeaderHash: headerHash("1"),
  targetDeadlineMs: "3600000",
  steps: [
    {
      kind: "remove-head",
      removedHeaderHash: headerHash("1"),
      inputOutRefs: [outRef("a"), outRef("1"), outRef("c")],
      txHash: txHash("f"),
      status: "confirmed",
    },
  ],
  completed: true,
});

describe("attestation-timeout correction recovery", () => {
  it("rejects malformed nested journal state instead of trusting the cast", () => {
    expect(() =>
      parseTimeoutCorrectionJournal({
        ...completedJournal(),
        steps: [{ kind: "remove-head", status: "confirmed" }],
      }),
    ).toThrow(/step 0 is invalid/);
    expect(() =>
      parseTimeoutCorrectionJournal({
        ...completedJournal(),
        steps: [
          completedJournal().steps[0],
          {
            ...completedJournal().steps[0],
            removedHeaderHash: headerHash("2"),
          },
        ],
      }),
    ).toThrow(/repeats transaction hash/);
    expect(() =>
      parseTimeoutCorrectionJournal({
        ...completedJournal(),
        steps: [{ ...completedJournal().steps[0], status: "submitted" }],
      }),
    ).toThrow(/Completed.*non-terminal/);
    expect(() =>
      parseTimeoutCorrectionJournal({
        ...completedJournal(),
        completedAuthority: true,
      }),
    ).toThrow(/Invalid.*journal/);
    expect(() =>
      parseTimeoutCorrectionJournal({
        ...completedJournal(),
        steps: [
          {
            ...completedJournal().steps[0],
            providerConfirmed: true,
          },
        ],
      }),
    ).toThrow(/step 0 is invalid/);
    expect(() =>
      parseTimeoutCorrectionJournal({
        ...completedJournal(),
        completed: false,
        steps: [{ ...completedJournal().steps[0], status: "accepted" }],
      }),
    ).toThrow(/non-canonical fields/);
  });

  it("reopens completion when a rollback restores the target head", () => {
    const reconciled = reconcileCompletedTimeoutCorrectionJournal(
      completedJournal(),
      [root(), block("1")],
    );
    expect(reconciled).toMatchObject({
      completed: false,
      steps: [{ status: "superseded" }],
    });
  });

  it("rotates a completed journal only after proving the old target absent", () => {
    expect(
      reconcileCompletedTimeoutCorrectionJournal(completedJournal(), [
        root(),
        block("2"),
      ]),
    ).toBeUndefined();
    expect(() =>
      reconcileCompletedTimeoutCorrectionJournal(completedJournal(), [
        root(),
        block("2"),
        block("1"),
      ]),
    ).toThrow(/outside the canonical head position/);
  });

  it("replans from fresh topology after concurrent append or stale UTxOs", () => {
    const target = headerHash("1");
    const first = planNextTimeoutCorrection(
      [root(), block("1", "1"), block("2", "2")],
      target,
    );
    const refreshed = planNextTimeoutCorrection(
      [root(), block("1", "3", 1), block("4", "4")],
      target,
    );
    expect(first?.kind).toBe("prune-descendant");
    expect(refreshed?.kind).toBe("prune-descendant");
    expect(refreshed?.inputOutRefs).not.toEqual(first?.inputOutRefs);
    expect(refreshed?.removed.assetName).toContain(headerHash("4"));
  });

  it("confirms an authenticated prune before deriving the next prune plan", () => {
    const target = headerHash("1");
    const pending: TimeoutCorrectionJournal = {
      version: 1,
      targetHeaderHash: target,
      targetDeadlineMs: "3600000",
      completed: false,
      steps: [
        {
          kind: "prune-descendant",
          removedHeaderHash: headerHash("2"),
          inputOutRefs: [outRef("1"), outRef("2"), outRef("c")],
          txHash: txHash("e"),
          status: "submitted",
        },
      ],
    };
    const refreshedQueue = [root("a"), block("1", "9"), block("3", "3")];
    const reconciled = reconcileLastTimeoutCorrectionStep(
      pending,
      refreshedQueue,
      "confirmed",
    );
    expect(reconciled.disposition).toBe("confirmed");
    expect(reconciled.journal.steps[0]?.status).toBe("confirmed");
    expect(planNextTimeoutCorrection(refreshedQueue, target)).toMatchObject({
      kind: "prune-descendant",
      removed: { assetName: `000de140${headerHash("3")}` },
    });
  });

  it("confirms terminal head removal before accepting an undefined plan", () => {
    const pending: TimeoutCorrectionJournal = {
      ...completedJournal(),
      completed: false,
      steps: [
        {
          ...completedJournal().steps[0]!,
          status: "submitted",
        },
      ],
    };
    const refreshedQueue = [root("9")];
    const reconciled = reconcileLastTimeoutCorrectionStep(
      pending,
      refreshedQueue,
      "confirmed",
    );
    expect(reconciled.disposition).toBe("confirmed");
    expect(planNextTimeoutCorrection(refreshedQueue, headerHash("1"))).toBe(
      undefined,
    );
  });

  it("does not confirm when concurrent topology still carries the recorded header", () => {
    const pending: TimeoutCorrectionJournal = {
      version: 1,
      targetHeaderHash: headerHash("1"),
      targetDeadlineMs: "3600000",
      completed: false,
      steps: [
        {
          kind: "prune-descendant",
          removedHeaderHash: headerHash("2"),
          inputOutRefs: [outRef("1"), outRef("2"), outRef("c")],
          txHash: txHash("e"),
          status: "submitted",
        },
      ],
    };
    const reconciled = reconcileLastTimeoutCorrectionStep(
      pending,
      [root("9"), block("1", "8"), block("2", "7")],
      "confirmed",
    );
    expect(reconciled.disposition).toBe("pending");
    expect(reconciled.journal.steps[0]?.status).toBe("submitted");
  });

  it.each(["failed", "not_found"] as const)(
    "always supersedes a %s transaction before replanning changed topology",
    (status) => {
      const target = headerHash("1");
      const pending: TimeoutCorrectionJournal = {
        version: 1,
        targetHeaderHash: target,
        targetDeadlineMs: "3600000",
        completed: false,
        steps: [
          {
            kind: "prune-descendant",
            removedHeaderHash: headerHash("2"),
            inputOutRefs: [outRef("1"), outRef("2"), outRef("c")],
            txHash: txHash("e"),
            status: "submitted",
          },
        ],
      };
      const concurrentlyChangedQueue = [
        root("9"),
        block("1", "8"),
        block("4", "4"),
      ];
      const reconciled = reconcileLastTimeoutCorrectionStep(
        pending,
        concurrentlyChangedQueue,
        status,
      );
      expect(reconciled.disposition).toBe("superseded");
      expect(reconciled.journal.steps[0]?.status).toBe("superseded");
      expect(
        planNextTimeoutCorrection(concurrentlyChangedQueue, target)?.removed
          .assetName,
      ).toContain(headerHash("4"));
    },
  );

  it("releases an acquired lease before yielding a resumable pending result", async () => {
    const release = vi.fn(async () => undefined);
    await expect(
      releaseTimeoutCorrectionLeaseBeforeYield({ release }),
    ).resolves.toBe(true);
    expect(release).toHaveBeenCalledOnce();
  });
});
