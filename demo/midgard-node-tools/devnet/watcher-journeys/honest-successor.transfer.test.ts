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

describe("stack successor transfer", () => {
  it("hands our node its transfer before the fault's commit, once our node's ledger shows the predecessor, and proves the successor that carries it", async () => {
    const drill = await stackDrill();
    const intentPath = join(drill.directory, "successor-transfer.json");
    // Our node's ledger is still behind the predecessor: nothing is built.
    drill.ledgerLags(true);
    const readying = drill.transferBeforeFault()(drill.replayedPredecessor);
    await pause(30);
    expect(existsSync(intentPath)).toBe(false);
    drill.ledgerLags(false);
    const handOver = await readying;
    // Readied on the predecessor, and sent only at the commit attempt.
    expect(drill.sent).toEqual([]);
    expect(
      (
        JSON.parse(await readFile(intentPath, "utf8")) as Record<
          string,
          unknown
        >
      )["predecessorHeaderHash"],
    ).toBe(drill.predecessor.headerHash);
    await handOver();
    expect(drill.sent).toHaveLength(1);
    const { txId } = drill.sent[0]!;
    // Our node knows it: a second attempt, a resumed staging and the fault's
    // landing send nothing more.
    drill.node.status = { status: "queued" };
    await handOver();
    await (
      await drill.transferBeforeFault()(drill.replayedPredecessor)
    )();
    const port = drill.port();
    await port.afterFaultLanded();
    expect(drill.sent).toHaveLength(1);
    drill.node.readyz = faultHold(drill.fault.headerHash);
    await drill.observedHold();
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
    await expect(port.commit(drill.correction)).resolves.toEqual({
      successor: expect.objectContaining({ headerHash: ours.headerHash }),
      stage: { successorStage: "exercised" },
      faultAttestation: {
        blockTimeMs: Number(drill.fault.header.endTime),
        apply: null,
        lagAfterBlockTimeMs: null,
      },
    });
    expect(
      JSON.parse(await readFile(join(drill.directory, "refusal.json"), "utf8")),
    ).toMatchObject({ transfer: txId, successor: ours.headerHash });
  });

  it("hands nothing over while our node's ledger never shows the predecessor", async () => {
    const drill = await stackDrill();
    drill.ledgerLags(true);
    await expect(
      drill.transferBeforeFault(50)(drill.replayedPredecessor),
    ).rejects.toThrow(
      new RegExp(
        `did not show the drill's L2 owner's outputs at the fault's predecessor ${drill.predecessor.headerHash} within 50 ms`,
        "u",
      ),
    );
    expect(existsSync(join(drill.directory, "successor-transfer.json"))).toBe(
      false,
    );
    expect(drill.sent).toEqual([]);
  });

  it("leaves the transfer to the fault's landing once the late-shift window opens with our node's ledger still behind", async () => {
    const drill = await stackDrill();
    const intentPath = join(drill.directory, "successor-transfer.json");
    drill.ledgerLags(true);
    // The window opens long before the readiness wait's own limit.
    const readyByMs = Date.now() + 30;
    const handOver = await drill.transferBeforeFault(
      60_000,
      readyByMs,
    )(drill.replayedPredecessor);
    expect(Date.now()).toBeGreaterThanOrEqual(readyByMs);
    // Nothing is built or handed over before the fault's commit.
    expect(existsSync(intentPath)).toBe(false);
    await handOver();
    expect(drill.sent).toEqual([]);
    expect(drill.stages).toEqual([
      `Our node's ledger did not show the drill's L2 owner's outputs at the fault's predecessor ${drill.predecessor.headerHash} by ${readyByMs.toString()}, when the fault's late-shift window opens; its transfer is handed over once the fault lands`,
    ]);
    expect(
      JSON.parse(
        await readFile(
          join(drill.directory, "successor-transfer-deferred.json"),
          "utf8",
        ),
      ),
    ).toEqual({
      predecessorHeaderHash: drill.predecessor.headerHash,
      readyByMs,
    });
    // Once the fault lands, the transfer is built on the predecessor and
    // sent.
    drill.ledgerLags(false);
    const port = drill.port();
    await port.afterFaultLanded();
    await port.close();
    expect(drill.sent).toHaveLength(1);
    expect(
      (
        JSON.parse(await readFile(intentPath, "utf8")) as Record<
          string,
          unknown
        >
      )["predecessorHeaderHash"],
    ).toBe(drill.predecessor.headerHash);
  });

  it("takes a transfer an earlier run saved for this fault after it landed, and not one saved for another", async () => {
    const drill = await stackDrill();
    const other = await headerOn(drill.predecessor.headerHash, operator2, 9n);
    const path = join(drill.directory, "successor-transfer.json");
    const first = drill.port();
    await first.afterFaultLanded();
    await first.close();
    const { predecessorHeaderHash: _, ...transfer } = JSON.parse(
      await readFile(path, "utf8"),
    ) as Record<string, unknown>;
    await writeFile(
      path,
      JSON.stringify({ ...transfer, faultHeaderHash: drill.fault.headerHash }),
    );
    const resumed = drill.port();
    await resumed.afterFaultLanded();
    await resumed.close();
    // Our node lost it, so the same transfer is sent again, not rebuilt.
    expect(drill.sent.map(({ txId }) => txId)).toEqual([
      transfer["txId"],
      transfer["txId"],
    ]);
    expect(JSON.parse(await readFile(path, "utf8"))).toEqual({
      ...transfer,
      faultHeaderHash: drill.fault.headerHash,
    });
    await writeFile(
      path,
      JSON.stringify({ ...transfer, faultHeaderHash: other.headerHash }),
    );
    const port = drill.port();
    await port.afterFaultLanded();
    await port.close();
    expect(drill.sent).toHaveLength(3);
    expect(JSON.parse(await readFile(path, "utf8"))).toMatchObject({
      predecessorHeaderHash: drill.predecessor.headerHash,
    });
  });
});
