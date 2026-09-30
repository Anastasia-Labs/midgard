import "./user-event-runtime.user-event-pending-native-growth.js";

import { describe, expect, it } from "vitest";

import { createWatcherUserEventRuntime } from "../../src/runtime/user-event-runtime.js";
import {
  createWatcherDurableRuntime,
  readWatcherProtectedUserEventCheckpoint,
  readWatcherProtectedUserEventCheckpointReceipt,
} from "../../src/storage/durable-runtime.js";
import { setup } from "./user-event-runtime.setup.js";

describe("semantic rollback runtime integration", () => {
  it.each(["live", "restart", "interrupted_cas", "retry", "bounded"] as const)(
    "recovers a native replacement branch through %s without double advancement",
    async (mode) => {
      const context = await setup();
      const { fixture, durable } = context;
      let runtime: Awaited<
        ReturnType<typeof createWatcherUserEventRuntime>
      > | null = null;
      try {
        runtime = await createWatcherUserEventRuntime(context.input);
        let fork = fixture.emptySuccessorBlock;
        if (mode === "bounded") {
          for (let index = 0; index < 130; index++) {
            fork = await fixture.makeBlock({ transactions: [] });
            // The query-counter fixture advances its native tip through captures.
            // Publish periodic real touched frames while constructing the prefix.
            if (index % 32 === 31) {
              fork = await context.touchedBlock();
              await runtime.advanceThrough(fork.point);
            }
          }
        }
        const old = await context.touchedBlock();
        await runtime.advanceThrough(old.point);
        const previous = readWatcherProtectedUserEventCheckpointReceipt(
          await readWatcherProtectedUserEventCheckpoint(durable.runtime),
        ).checkpoint!;
        if (mode === "restart") {
          await runtime.close();
          runtime = null;
        }
        const replacement = await fixture.makeBlock({
          parent: fork,
          transactions: [],
          slot: Number(old.point.slot) + 1,
        });
        const descendant = await fixture.makeBlock({
          parent: replacement,
          transactions: [],
        });
        await fixture.selectCanonicalBranch(descendant.point);
        if (mode !== "restart") {
          if (mode === "interrupted_cas") durable.interruptNextReadBack();
          if (mode === "retry")
            durable.setBeforePut(async () => {
              throw new Error("archive temporarily unavailable");
            });
          const operation = runtime!.handleRollback({
            kind: "point",
            blockHash: fork.point.blockHash,
            slot: fork.point.slot,
          });
          expect(runtime!.read().status).toBe("suspended");
          if (mode === "interrupted_cas")
            await expect(operation).rejects.toThrow(
              "fixture read-back interruption",
            );
          else if (mode === "retry") {
            await expect(operation).rejects.toThrow(
              "archive temporarily unavailable",
            );
            expect(runtime!.read().status).toBe("suspended");
            expect(
              readWatcherProtectedUserEventCheckpointReceipt(
                await readWatcherProtectedUserEventCheckpoint(durable.runtime),
              ).checkpoint,
            ).toEqual(previous);
            durable.setBeforePut(null);
            await runtime!.handleRollback({
              kind: "point",
              blockHash: fork.point.blockHash,
              slot: fork.point.slot,
            });
          } else await operation;
          await runtime!.close();
          runtime = null;
        }
        const reopened = await createWatcherDurableRuntime(
          durable.runtimeInput,
        );
        runtime = await createWatcherUserEventRuntime({
          ...context.input,
          runtime: reopened,
        });
        expect(runtime.read()).toMatchObject({
          status: "ready",
          headCursor: replacement.point,
          currentPoint: replacement.point,
        });
        const checkpoint = readWatcherProtectedUserEventCheckpointReceipt(
          await readWatcherProtectedUserEventCheckpoint(reopened),
        ).checkpoint!;
        expect(checkpoint).toMatchObject({
          rollbackGeneration: "1",
          predecessorCheckpointDigest: previous.checkpointDigest,
          checkpointSequence: (
            BigInt(previous.checkpointSequence) + 1n
          ).toString(),
        });
        await runtime.close();
        runtime = null;
        const again = await createWatcherDurableRuntime(durable.runtimeInput);
        runtime = await createWatcherUserEventRuntime({
          ...context.input,
          runtime: again,
        });
        expect(
          readWatcherProtectedUserEventCheckpointReceipt(
            await readWatcherProtectedUserEventCheckpoint(again),
          ).checkpoint,
        ).toEqual(checkpoint);
      } finally {
        await runtime?.close();
        await context.close();
      }
    },
    120_000,
  );
});
