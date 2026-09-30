import { setTimeout as delay } from "node:timers/promises";

import { h32 } from "@al-ft/midgard-test-support/hex";
import { describe, expect, it } from "vitest";

import {
  assertWatcherUserEventRuntime,
  createWatcherUserEventRuntime,
} from "../../src/runtime/user-event-runtime.js";
import { createWatcherDurableRuntime } from "../../src/storage/durable-runtime.js";
import { setup } from "./user-event-runtime.setup.js";

describe("user-event runtime coverage and capture", () => {
  it("prefetches closed first observations without publishing ahead and discards them on rollback", async () => {
    const context = await setup();
    const { fixture, capturesOf } = context;
    let runtime: Awaited<
      ReturnType<typeof createWatcherUserEventRuntime>
    > | null = null;
    try {
      runtime = await createWatcherUserEventRuntime(context.input);
      const first = await context.touchedBlock();
      const second = await context.touchedBlock();
      const third = await context.touchedBlock();
      const before = (await fixture.readNativeQueries()).length;
      await runtime.advanceThrough(first.point);
      expect(runtime.read()).toMatchObject({
        currentPoint: first.point,
        headCursor: first.point,
      });
      let queries = (await fixture.readNativeQueries()).slice(before);
      // Each actual capture makes two native queries. Future blocks have only
      // their closed first capture; only the requested block has both captures.
      expect(capturesOf(queries, first.point.blockHash)).toHaveLength(4);
      expect(capturesOf(queries, second.point.blockHash)).toHaveLength(2);
      expect(capturesOf(queries, third.point.blockHash)).toHaveLength(2);
      expect(
        queries.findIndex(
          (query) => query.target.blockHash === third.point.blockHash,
        ),
      ).toBeLessThan(
        queries.lastIndexOf(capturesOf(queries, first.point.blockHash).at(-1)!),
      );

      await runtime.advanceThrough(second.point);
      expect(runtime.read()).toMatchObject({
        currentPoint: second.point,
        headCursor: second.point,
      });
      queries = (await fixture.readNativeQueries()).slice(before);
      expect(capturesOf(queries, second.point.blockHash)).toHaveLength(4);
      expect(capturesOf(queries, third.point.blockHash)).toHaveLength(2);

      await runtime.handleRollback({
        kind: "point",
        blockHash: second.point.blockHash,
        slot: second.point.slot,
      });
      await runtime.advanceThrough(third.point);
      expect(runtime.read()).toMatchObject({
        currentPoint: third.point,
        headCursor: third.point,
      });
      queries = (await fixture.readNativeQueries()).slice(before);
      // The old speculative first capture cannot cross a rollback generation.
      expect(capturesOf(queries, third.point.blockHash)).toHaveLength(6);
    } finally {
      await runtime?.close();
      await context.close();
    }
  }, 120_000);

  it("captures only touched blocks, covers the quiet stretch in place, rewinds coverage on rollback, and restores it on restart", async () => {
    const context = await setup();
    const { fixture, capturesOf } = context;
    let runtime: Awaited<
      ReturnType<typeof createWatcherUserEventRuntime>
    > | null = null;
    try {
      runtime = await createWatcherUserEventRuntime(context.input);
      const quietA = fixture.emptySuccessorBlock;
      const quietB = await fixture.makeBlock({ transactions: [] });
      const touchedA = await context.touchedBlock();
      const quietC = await fixture.makeBlock({ transactions: [] });
      const touchedB = await context.touchedBlock();
      const quietD = await fixture.makeBlock({ transactions: [] });
      let before = (await fixture.readNativeQueries()).length;
      await runtime.advanceThrough(quietD.point);
      expect(runtime.read()).toMatchObject({
        currentPoint: quietD.point,
        headCursor: touchedB.point,
      });
      let queries = (await fixture.readNativeQueries()).slice(before);
      for (const quiet of [quietA, quietB, quietC, quietD])
        expect(capturesOf(queries, quiet.point.blockHash)).toHaveLength(0);
      for (const touched of [touchedA, touchedB])
        expect(capturesOf(queries, touched.point.blockHash)).toHaveLength(4);

      // Rolling back into the quiet stretch above the head observation
      // rewinds the checkpoint in place; the head observation survives.
      const quietE = await fixture.makeBlock({ transactions: [] });
      const quietF = await fixture.makeBlock({ transactions: [] });
      await runtime.advanceThrough(quietF.point);
      expect(runtime.read()).toMatchObject({
        currentPoint: quietF.point,
        headCursor: touchedB.point,
      });
      const priorGeneration = runtime.read().generation;
      await runtime.handleRollback({
        kind: "point",
        blockHash: quietD.point.blockHash,
        slot: quietD.point.slot,
      });
      expect(runtime.read()).toMatchObject({
        status: "ready",
        generation: priorGeneration + 1,
        currentPoint: quietD.point,
        headCursor: touchedB.point,
      });
      before = (await fixture.readNativeQueries()).length;
      await runtime.advanceThrough(quietF.point);
      expect(runtime.read()).toMatchObject({
        currentPoint: quietF.point,
        headCursor: touchedB.point,
      });
      queries = (await fixture.readNativeQueries()).slice(before);
      for (const quiet of [quietE, quietF])
        expect(capturesOf(queries, quiet.point.blockHash)).toHaveLength(0);

      // A restart restores the saved coverage above the sealed head.
      await runtime.close();
      const reopened = await createWatcherDurableRuntime(
        context.durable.runtimeInput,
      );
      before = (await fixture.readNativeQueries()).length;
      runtime = await createWatcherUserEventRuntime({
        ...context.input,
        runtime: reopened,
      });
      expect(runtime.read()).toMatchObject({
        status: "ready",
        currentPoint: quietF.point,
        headCursor: touchedB.point,
      });
      queries = (await fixture.readNativeQueries()).slice(before);
      for (const quiet of [quietC, quietD, quietE, quietF])
        expect(capturesOf(queries, quiet.point.blockHash)).toHaveLength(0);
      const quietG = await fixture.makeBlock({ transactions: [] });
      await runtime.advanceThrough(quietG.point);
      expect(runtime.read()).toMatchObject({
        currentPoint: quietG.point,
        headCursor: touchedB.point,
      });

      // A real native rollback frame suspends the service between calls.
      await fixture.rollbackNativeStream({
        blockHash: quietF.point.blockHash,
        blockNo: quietF.point.blockNo,
        slot: quietF.point.slot,
      });
      await expect.poll(() => runtime!.read().status).toBe("suspended");
      expect(() => assertWatcherUserEventRuntime(runtime!)).toThrow();
      await runtime.handleRollback({
        kind: "point",
        blockHash: quietF.point.blockHash,
        slot: quietF.point.slot,
      });
      expect(runtime.read()).toMatchObject({
        status: "ready",
        currentPoint: quietF.point,
        headCursor: touchedB.point,
      });

      // A rollback hint cannot prune a head that fresh native evidence still
      // proves canonical. Reopen it without manufacturing a new generation.
      await runtime.handleRollback({
        kind: "point",
        blockHash: quietB.point.blockHash,
        slot: quietB.point.slot,
      });
      await runtime.advanceThrough(quietF.point);
      await runtime.close();
      runtime = await createWatcherUserEventRuntime({
        ...context.input,
        runtime: await createWatcherDurableRuntime(
          context.durable.runtimeInput,
        ),
      });
      expect(runtime.read()).toMatchObject({
        status: "ready",
        currentPoint: quietF.point,
        headCursor: touchedB.point,
      });

      // Below the release-final boundary the hash check is unnecessary: a
      // point there resolves as covered by height alone, with no request.
      const { computeFraudProofRawL1PointId } = await import(
        "@al-ft/midgard-fault-proofs"
      );
      const deep = { ...quietB.point, blockHash: h32(0xaa) };
      deep.pointId = computeFraudProofRawL1PointId(deep);
      before = (await fixture.readNativeQueries()).length;
      await runtime.advanceThrough(deep);
      expect((await fixture.readNativeQueries()).length).toBe(before);
      expect(runtime.read()).toMatchObject({
        status: "ready",
        currentPoint: quietF.point,
      });
      // A wrong hash at a height the runtime itself covered is refused
      // without any request; the caller asked for a point off the canonical
      // chain, so the runtime fails closed.
      const quietH = await fixture.makeBlock({ transactions: [] });
      await runtime.advanceThrough(quietH.point);
      const fork = { ...quietH.point, blockHash: h32(0xaa) };
      fork.pointId = computeFraudProofRawL1PointId(fork);
      before = (await fixture.readNativeQueries()).length;
      await expect(runtime.advanceThrough(fork)).rejects.toThrow(
        /exact accepted block/u,
      );
      expect((await fixture.readNativeQueries()).length).toBe(before);
      expect(runtime.read().status).toBe("failed");
      await expect(runtime.done).rejects.toThrow();
    } finally {
      await runtime?.close();
      await context.close();
    }
  }, 300_000);

  it("crosses 128 retained observations, rotates the sealed history, and restores it without replay", async () => {
    const context = await setup();
    const { fixture } = context;
    let runtime: Awaited<
      ReturnType<typeof createWatcherUserEventRuntime>
    > | null = null;
    try {
      runtime = await createWatcherUserEventRuntime(context.input);
      const blocks = [fixture.emptySuccessorBlock];
      for (let index = 1; index <= 130; index++)
        blocks.push(await context.touchedBlock());
      for (const index of [64, 127, 130]) {
        await runtime.advanceThrough(blocks[index]!.point);
        expect(runtime.read()).toMatchObject({
          currentPoint: blocks[index]!.point,
          headCursor: blocks[index]!.point,
        });
      }
      await runtime.advanceThrough(blocks[20]!.point);
      expect(runtime.read().currentPoint).toEqual(blocks[130]!.point);
      await runtime.close();
      const reopened = await createWatcherDurableRuntime(
        context.durable.runtimeInput,
      );
      runtime = await createWatcherUserEventRuntime({
        ...context.input,
        runtime: reopened,
      });
      expect(runtime.read()).toMatchObject({
        status: "ready",
        currentPoint: blocks[130]!.point,
        headCursor: blocks[130]!.point,
      });
      const successor = await fixture.makeBlock({ transactions: [] });
      await runtime.advanceThrough(successor.point);
      expect(runtime.read()).toMatchObject({
        currentPoint: successor.point,
        headCursor: blocks[130]!.point,
      });
    } finally {
      await runtime?.close();
      await context.close();
    }
  }, 600_000);
});

export const startControlledRuntime = async (): Promise<{
  context: Awaited<ReturnType<typeof setup>>;
  runtime: Awaited<ReturnType<typeof createWatcherUserEventRuntime>>;
}> => {
  const context = await setup("controlled");
  await context.fixture.setNativeTip(context.fixture.emptySuccessorBlock.point);
  await context.fixture.growNativeTip(100);
  let starting = true;
  const growth = (async () => {
    while (starting) {
      await context.fixture.growNativeTip();
      await delay(100);
    }
  })();
  try {
    const runtime = await createWatcherUserEventRuntime(context.input);
    return { context, runtime };
  } catch (error) {
    await context.close();
    throw error;
  } finally {
    starting = false;
    await growth;
  }
};
