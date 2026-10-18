/**
 * Phase A throwing on a foreign block's transaction is this node's failure,
 * not a verdict on the block: the import is `incomplete`, and the landed
 * block is held on a retried hold that raises the local-fault alarm and
 * counts once, never refused as invalid.
 */
import { Effect, HashMap, Logger, Metric, Option } from "effect";
import { describe, expect, it, vi } from "vitest";

const phaseA = vi.hoisted(() => ({ throwing: false, thrown: 0 }));
vi.mock("@al-ft/midgard-validation/phase-a", async (importOriginal) => {
  const actual =
    await importOriginal<typeof import("@al-ft/midgard-validation/phase-a")>();
  return {
    ...actual,
    validatePhaseASingle: (
      ...args: Parameters<typeof actual.validatePhaseASingle>
    ) => {
      if (phaseA.throwing) {
        phaseA.thrown += 1;
        throw new Error("Phase A fault injected by the test");
      }
      return actual.validatePhaseASingle(...args);
    },
  };
});

import { isRetriedHold } from "../src/l1-events/driver.js";
import {
  LANDED_BLOCK_LOCAL_FAULT,
  LANDED_BLOCK_REPLAY_INCOMPLETE,
} from "../src/landed-blocks/holds.js";
import {
  landedBlockLocalFaultCounter,
  replayOutcomeAlarm,
  replayOutcomeHold,
} from "../src/landed-blocks/replay-outcome-hold.js";
import { ForeignBlockVerificationError } from "../src/mpf/verified-block-import.js";
import { committedBlock, fundedBy } from "./import-check.fixture.js";

describe("foreign import when Phase A throws", () => {
  it("holds the block as incomplete on a retried hold that raises the local-fault alarm", async () => {
    const parent = [fundedBy(0x51, "accept")];
    const block = await committedBlock(parent, [
      { input: parent[0]!.outRef, body: "accept", next: "accept" },
    ]);

    phaseA.throwing = true;
    const result = await block.imported().finally(() => {
      phaseA.throwing = false;
    });
    expect(phaseA.thrown).toBe(1);
    if (result._tag === "Right") throw new Error("the import succeeded");
    const error = result.left;
    expect(error).toBeInstanceOf(ForeignBlockVerificationError);
    if (!(error instanceof ForeignBlockVerificationError)) return;
    expect(error.reason).toBe("incomplete");

    // The landed-block replay holds on the import's reason as its outcome.
    const kind = error.reason;
    const hold = replayOutcomeHold(kind, error.detail);
    expect([hold.reason, isRetriedHold(hold)]).toEqual([
      LANDED_BLOCK_REPLAY_INCOMPLETE,
      true,
    ]);

    const alarms: unknown[] = [];
    const counted = await Effect.runPromise(
      Effect.gen(function* () {
        const count = Metric.value(
          Metric.tagged(landedBlockLocalFaultCounter, "kind", kind),
        ).pipe(Effect.map((state) => state.count));
        const before = yield* count;
        yield* replayOutcomeAlarm(block.headerHash, kind, error.detail);
        return (yield* count) - before;
      }).pipe(
        Effect.provide(
          Logger.replace(
            Logger.defaultLogger,
            Logger.make(({ annotations }) => {
              alarms.push(
                Option.getOrUndefined(HashMap.get(annotations, "alarm")),
              );
            }),
          ),
        ),
      ),
    );
    expect(alarms).toEqual([LANDED_BLOCK_LOCAL_FAULT]);
    expect(counted).toBe(1n);
  });
});
