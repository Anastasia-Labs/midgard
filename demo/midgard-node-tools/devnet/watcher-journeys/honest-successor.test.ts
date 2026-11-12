import { existsSync } from "node:fs";
import { readFile, rm, writeFile } from "node:fs/promises";
import { join } from "node:path";
import { setTimeout as pause } from "node:timers/promises";

import { afterAll, afterEach, describe, expect, it } from "vitest";

import {
  drillDirectories,
  stackDrill,
} from "./honest-successor.stack-drill.test-support.js";
import {
  closeServers,
  faultHold,
  headerOn,
  honestOperator,
  operator2,
} from "./honest-successor.test-support.js";

afterAll(() =>
  Promise.all(
    drillDirectories.map((path) => rm(path, { recursive: true, force: true })),
  ),
);
afterEach(closeServers);

describe("stack successor", () => {
  it("gives our node work while the fault is the tail, then proves all three refusal observables", async () => {
    const drill = await stackDrill();
    const port = drill.port();
    await port.afterFaultLanded();
    expect(drill.sent).toHaveLength(1);
    const { txId } = drill.sent[0]!;
    drill.node.readyz = faultHold(drill.fault.headerHash);
    await drill.observedHold();
    // Our node's shift began while the fault was the tail (committed at
    // slot 10); then the watcher removed the fault and our node built on its
    // predecessor.
    drill.appoints(operator2, 9);
    drill.appoints(honestOperator, 14);
    drill.removed(20);
    drill.node.readyz = { ready: true, reasons: [] };
    const ours = await headerOn(
      drill.predecessor.headerHash,
      honestOperator,
      3n,
    );
    await drill.commits(ours, [txId]);
    const { successor, stage } = await port.commit(drill.correction);
    expect(stage).toEqual({ successorStage: "exercised" });
    expect(successor).toMatchObject({
      headerHash: ours.headerHash,
      commitTxHash: "03".padStart(64, "0"),
    });
    expect(
      JSON.parse(await readFile(join(drill.directory, "refusal.json"), "utf8")),
    ).toMatchObject({
      fault: drill.fault.headerHash,
      holdBinding: "header",
      transfer: txId,
      successor: ours.headerHash,
      faultTail: {
        faultCommittedMs: drill.at(10),
        toMs: drill.at(20),
        ourShifts: [{ startTime: drill.at(14), endTime: drill.at(19) }],
        // What the first live run measures the late-shift lead from.
        measured: {
          faultCommitToConvictionMs: 10_000,
          faultCommitToOurNextShiftMs: 4_000,
        },
      },
      // No DA attestation was applied to the fault before its removal.
      faultAttestation: {
        blockTimeMs: Number(drill.fault.header.endTime),
        apply: null,
        lagAfterBlockTimeMs: null,
      },
    });
    // A resumed run reads the proven successor back.
    const resumed = drill.port();
    await resumed.afterFaultLanded();
    expect(drill.sent).toHaveLength(1);
    await expect(resumed.commit(drill.correction)).resolves.toMatchObject({
      successor: { headerHash: ours.headerHash },
      stage: { successorStage: "exercised" },
      faultAttestation: { apply: null, lagAfterBlockTimeMs: null },
    });
  });

  it("sees our node's hold from the fault's commit, though the watcher corrected the fault before staging ended", async () => {
    const drill = await stackDrill();
    const watch = drill.holdWatch();
    watch.start(drill.fault.headerHash);
    drill.node.readyz = faultHold(drill.fault.headerHash);
    await drill.observedHold();
    // Corrected before the runner gives our node work, in our node's shift:
    // the hold is gone.
    drill.appoints(honestOperator, 11);
    drill.removed(12);
    drill.node.readyz = { ready: true, reasons: [] };
    const port = drill.port(watch);
    await port.afterFaultLanded();
    const ours = await headerOn(
      drill.predecessor.headerHash,
      honestOperator,
      3n,
    );
    await drill.commits(ours, [drill.sent[0]!.txId]);
    await expect(port.commit(drill.correction)).resolves.toMatchObject({
      successor: { headerHash: ours.headerHash },
    });
  });

  it("ignores hold observations of another fault and a saved transfer on another predecessor", async () => {
    const drill = await stackDrill();
    const other = await headerOn(drill.predecessor.headerHash, operator2, 9n);
    const observedAt = new Date().toISOString();
    await writeFile(
      join(drill.directory, "node-hold-observations.ndjson"),
      [
        {
          faultHeaderHash: other.headerHash,
          observedAt,
          hold: { state: "held", binding: "header", reasons: [] },
        },
        {
          observedAt,
          hold: { state: "held", binding: "reason_only", reasons: [] },
        },
      ]
        .map((entry) => JSON.stringify(entry))
        .join("\n") + "\n",
    );
    const first = drill.port();
    await first.afterFaultLanded();
    await first.close();
    const path = join(drill.directory, "successor-transfer.json");
    const saved = JSON.parse(await readFile(path, "utf8")) as Record<
      string,
      unknown
    >;
    expect(saved["predecessorHeaderHash"]).toBe(drill.predecessor.headerHash);
    // A transfer saved on another predecessor is not this fault's work.
    await writeFile(
      path,
      JSON.stringify({ ...saved, predecessorHeaderHash: other.headerHash }),
    );
    const port = drill.port();
    await port.afterFaultLanded();
    expect(drill.sent).toHaveLength(2);
    // The second attempt staged its own transfer, on this predecessor.
    expect(
      (JSON.parse(await readFile(path, "utf8")) as Record<string, unknown>)[
        "predecessorHeaderHash"
      ],
    ).toBe(drill.predecessor.headerHash);
    await pause(30);
    await expect(port.commit(drill.correction)).rejects.toThrow(
      /never reported landed_block_invalid/u,
    );
  });

  it("takes our node's commit_base_pending while the fault was the tail for its shift", async () => {
    const drill = await stackDrill();
    const port = drill.port();
    await port.afterFaultLanded();
    const hold = faultHold(drill.fault.headerHash);
    drill.node.readyz = {
      ...hold,
      reasons: [...hold.reasons, "commit_base_pending"],
    };
    await drill.observedHold();
    drill.removed(20);
    drill.node.readyz = { ready: true, reasons: [] };
    const ours = await headerOn(
      drill.predecessor.headerHash,
      honestOperator,
      3n,
    );
    await drill.commits(ours, [drill.sent[0]!.txId]);
    await port.commit(drill.correction);
    expect(
      JSON.parse(await readFile(join(drill.directory, "refusal.json"), "utf8")),
    ).toMatchObject({ faultTail: { ourShifts: [] } });
    expect(
      JSON.parse(await readFile(join(drill.directory, "refusal.json"), "utf8"))
        .faultTail.commitBasePendingAt,
    ).toEqual(expect.any(String));
  });

  it("passes a fault the watcher removed before our first shift as not reached, with its evidence", async () => {
    const drill = await stackDrill();
    const port = drill.port();
    await port.afterFaultLanded();
    drill.node.readyz = faultHold(drill.fault.headerHash);
    await drill.observedHold();
    // Operator-2's shift (slots 9 to 14) held the fault's commit at slot 10;
    // the watcher removed the fault at slot 13, before our node's next shift.
    drill.appoints(honestOperator, 1);
    drill.appoints(operator2, 9);
    drill.removed(13);
    drill.appoints(honestOperator, 14);
    drill.node.readyz = { ready: true, reasons: [] };
    const ours = await headerOn(
      drill.predecessor.headerHash,
      honestOperator,
      3n,
    );
    await drill.commits(ours, [drill.sent[0]!.txId]);
    const notReached = {
      successorStage: "not-reached-fault-removed-before-our-shift",
      successorStageEvidence: {
        removal: {
          txHash: drill.correction.removalTxHash,
          slot: 13,
          atMs: drill.at(13),
        },
        ourFirstShiftStartMs: drill.at(14),
        operator2ShiftEndMs: drill.at(14),
      },
    };
    await expect(port.commit(drill.correction)).resolves.toEqual({
      successor: expect.objectContaining({ headerHash: ours.headerHash }),
      stage: notReached,
      faultAttestation: {
        blockTimeMs: Number(drill.fault.header.endTime),
        apply: null,
        lagAfterBlockTimeMs: null,
      },
    });
    // The family's evidence records it with the timing it was decided on.
    expect(
      JSON.parse(await readFile(join(drill.directory, "refusal.json"), "utf8")),
    ).toMatchObject({ successor: ours.headerHash, stage: notReached });
    expect(
      JSON.parse(
        await readFile(join(drill.directory, "fault-tail.json"), "utf8"),
      ),
    ).toMatchObject({
      fault: drill.fault.headerHash,
      ourShifts: [],
      measured: {
        faultCommitToConvictionMs: 3_000,
        faultCommitToOurNextShiftMs: 4_000,
      },
    });
    // A resumed run reads the stage back with its successor.
    await expect(drill.port().commit(drill.correction)).resolves.toMatchObject({
      successor: { headerHash: ours.headerHash },
      stage: notReached,
    });
  });

  it.each([
    [
      "lands in the slot our first shift starts in",
      (drill: Awaited<ReturnType<typeof stackDrill>>) => {
        drill.appoints(operator2, 9);
        drill.removed(14);
        drill.appoints(honestOperator, 14);
      },
      /is not before our first shift's start/u,
    ],
    [
      "is not indexed yet",
      (drill: Awaited<ReturnType<typeof stackDrill>>) => {
        drill.appoints(operator2, 9);
        drill.appoints(honestOperator, 31);
      },
      /no output of the fault spent by the watcher's removal/u,
    ],
    [
      "is not named by Kupo",
      (drill: Awaited<ReturnType<typeof stackDrill>>) => {
        drill.appoints(operator2, 9);
        drill.removed(13, { by: null });
        drill.appoints(honestOperator, 14);
      },
      /no output of the fault spent by the watcher's removal/u,
    ],
    [
      "is another transaction than the watcher's removal",
      (drill: Awaited<ReturnType<typeof stackDrill>>) => {
        drill.appoints(operator2, 9);
        drill.removed(13, { by: "cd".repeat(32) });
        drill.appoints(honestOperator, 14);
      },
      /no output of the fault spent by the watcher's removal/u,
    ],
    [
      "has no shift of ours on L1 after it",
      (drill: Awaited<ReturnType<typeof stackDrill>>) => {
        drill.appoints(operator2, 9);
        drill.removed(13);
      },
      /no shift of ours after the fault's commit/u,
    ],
    [
      "has no shift of operator-2's holding the fault's commit",
      (drill: Awaited<ReturnType<typeof stackDrill>>) => {
        drill.appoints(operator2, 3);
        drill.removed(13);
        drill.appoints(honestOperator, 14);
      },
      /no shift of operator-2's that the fault's commit landed in/u,
    ],
  ] as const)(
    "fails the stage, naming why, when the fault's removal %s",
    async (_label, chain, reason) => {
      const drill = await stackDrill();
      const port = drill.port();
      await port.afterFaultLanded();
      drill.node.readyz = faultHold(drill.fault.headerHash);
      await drill.observedHold();
      chain(drill);
      drill.node.readyz = { ready: true, reasons: [] };
      await drill.commits(
        await headerOn(drill.predecessor.headerHash, honestOperator, 3n),
        [drill.sent[0]!.txId],
      );
      const failure = port.commit(drill.correction);
      await expect(failure).rejects.toThrow(
        /had no shift while the fault .* was the tail/u,
      );
      await expect(failure).rejects.toThrow(reason);
      expect(existsSync(join(drill.directory, "refusal.json"))).toBe(false);
    },
  );

  it("takes no shift of ours that ended before our node first held the fault", async () => {
    // Our node first reports the fault invalid at slot 20.
    const drill = await stackDrill({ startSlot: 20 });
    const port = drill.port();
    await port.afterFaultLanded();
    drill.node.readyz = faultHold(drill.fault.headerHash);
    await drill.observedHold();
    // Our shift began after the fault's commit (slot 10) and ended before
    // our node held it: no evidence it refused to build on the fault.
    drill.appoints(honestOperator, 11);
    drill.appoints(operator2, 16);
    drill.removed(25);
    drill.node.readyz = { ready: true, reasons: [] };
    await drill.commits(
      await headerOn(drill.predecessor.headerHash, honestOperator, 3n),
      [drill.sent[0]!.txId],
    );
    await expect(port.commit(drill.correction)).rejects.toThrow(
      /had no shift while the fault .* was the tail/u,
    );
    const tail = JSON.parse(
      await readFile(join(drill.directory, "fault-tail.json"), "utf8"),
    ) as { fromMs: number; ourShifts: unknown[] };
    expect(tail.ourShifts).toEqual([]);
    expect(tail.fromMs).toBeGreaterThanOrEqual(drill.at(20));
  });

  it("fails when our node never held the fault", async () => {
    const drill = await stackDrill();
    const port = drill.port();
    await port.afterFaultLanded();
    await pause(30);
    await expect(port.commit(drill.correction)).rejects.toThrow(
      /never reported landed_block_invalid/u,
    );
  });

  it("fails when our node holds another block than the fault", async () => {
    const drill = await stackDrill();
    const port = drill.port();
    await port.afterFaultLanded();
    const other = await headerOn(drill.predecessor.headerHash, operator2, 9n);
    drill.node.readyz = faultHold(other.headerHash);
    await pause(30);
    await expect(port.commit(drill.correction)).rejects.toThrow(
      /not on the fault/u,
    );
  });

  it.each([
    [
      "links to the fault",
      async (drill: Awaited<ReturnType<typeof stackDrill>>, txId: string) =>
        drill.commits(
          await headerOn(drill.fault.headerHash, honestOperator, 3n),
          [txId],
        ),
      /links to .* not to the fault's predecessor/u,
    ],
    [
      "does not carry the transfer",
      async (drill: Awaited<ReturnType<typeof stackDrill>>) =>
        drill.commits(
          await headerOn(drill.predecessor.headerHash, honestOperator, 3n),
          ["77".repeat(32)],
        ),
      /does not carry the drill's transfer/u,
    ],
    [
      "is not our operator's",
      async (drill: Awaited<ReturnType<typeof stackDrill>>, txId: string) =>
        drill.commits(
          await headerOn(drill.predecessor.headerHash, operator2, 3n),
          [txId],
        ),
      /is not our operator's/u,
    ],
  ] as const)(
    "fails when our node's next header %s",
    async (_label, commit, refusal) => {
      const drill = await stackDrill();
      const port = drill.port();
      await port.afterFaultLanded();
      drill.node.readyz = faultHold(drill.fault.headerHash);
      await drill.observedHold();
      drill.node.readyz = { ready: true, reasons: [] };
      await commit(drill, drill.sent[0]!.txId);
      await expect(port.commit(drill.correction)).rejects.toThrow(refusal);
    },
  );

  it("fails when an honest header on the fault reached L1, or the hold stays", async () => {
    const drill = await stackDrill();
    const port = drill.port();
    await port.afterFaultLanded();
    drill.node.readyz = faultHold(drill.fault.headerHash);
    await drill.observedHold();
    drill.indexed.push({
      block: await headerOn(drill.fault.headerHash, honestOperator, 8n),
      slot: 15,
    });
    await drill.commits(
      await headerOn(drill.predecessor.headerHash, honestOperator, 3n),
      [drill.sent[0]!.txId],
    );
    await expect(port.commit(drill.correction)).rejects.toThrow(
      /built header .* on the fault/u,
    );

    const held = await stackDrill();
    const stays = held.port();
    await stays.afterFaultLanded();
    held.node.readyz = faultHold(held.fault.headerHash);
    await held.observedHold();
    await held.commits(
      await headerOn(held.predecessor.headerHash, honestOperator, 3n),
      [held.sent[0]!.txId],
    );
    await expect(stays.commit(held.correction)).rejects.toThrow(
      /still holds the fault/u,
    );
  });

  it("names why our node refused the transfer's status", async () => {
    const drill = await stackDrill();
    drill.node.txStatusRefusal = {
      status: 500,
      body: {
        error: "Request could not be completed",
        error_code: "E_HTTP_INTERNAL",
      },
    };
    await expect(drill.port().afterFaultLanded()).rejects.toThrow(
      /\/tx-status answered 500\b.*E_HTTP_INTERNAL/u,
    );
    expect(drill.sent).toHaveLength(0);
  });

  it("never resends a transfer our node knows, and refuses a changed saved transfer", async () => {
    const drill = await stackDrill();
    const first = drill.port();
    await first.afterFaultLanded();
    await first.close();
    drill.node.status = { status: "queued" };
    const again = drill.port();
    await again.afterFaultLanded();
    await again.close();
    expect(drill.sent).toHaveLength(1);
    const path = join(drill.directory, "successor-transfer.json");
    const saved = JSON.parse(await readFile(path, "utf8")) as {
      fee: string;
    };
    await writeFile(
      path,
      JSON.stringify({ ...saved, fee: (BigInt(saved.fee) + 1n).toString() }),
    );
    await expect(drill.port().afterFaultLanded()).rejects.toThrow(
      /differs from its exact transaction identity or fee/u,
    );
  });
});
