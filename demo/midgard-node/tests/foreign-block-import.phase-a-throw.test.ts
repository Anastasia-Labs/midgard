/**
 * Phase A throwing on a foreign block's transaction is this node's failure,
 * not a verdict on the block: the import is `incomplete`, and the landed
 * block is held on a retried hold that raises the local-fault alarm and
 * counts once, never refused as invalid. The hold is a local fault on the
 * block: retried with no progress past it, it reaches the coalesced
 * runner's local-fault bound and the node's typed exit.
 *
 * A fault of the Ed25519 verifier raised from Phase A or Phase B, directly
 * or as a cause, is `verifier_unavailable` whatever either phase's throw
 * would otherwise make of it.
 */
import { LedgerSignatureVerifierUnavailableError } from "@al-ft/midgard-core";
import { LOCAL_FAULT_RUN_LIMIT } from "@al-ft/midgard-fault-proofs";
import {
  Deferred,
  Effect,
  Exit,
  HashMap,
  Logger,
  Metric,
  Option,
} from "effect";
import { describe, expect, it, vi } from "vitest";

/** What each phase throws when the test arms it, and how often it did. */
const inject = vi.hoisted(() => ({
  phaseA: undefined as (() => unknown) | undefined,
  phaseB: undefined as (() => unknown) | undefined,
  thrown: 0,
}));
vi.mock("@al-ft/midgard-validation/phase-a", async (importOriginal) => {
  const actual =
    await importOriginal<typeof import("@al-ft/midgard-validation/phase-a")>();
  return {
    ...actual,
    validatePhaseASingle: (
      ...args: Parameters<typeof actual.validatePhaseASingle>
    ) => {
      if (inject.phaseA !== undefined) {
        inject.thrown += 1;
        throw inject.phaseA();
      }
      return actual.validatePhaseASingle(...args);
    },
  };
});
vi.mock("@al-ft/midgard-validation/phase-b", async (importOriginal) => {
  const actual =
    await importOriginal<typeof import("@al-ft/midgard-validation/phase-b")>();
  return {
    ...actual,
    runPhaseBValidationWithPatch: (
      ...args: Parameters<typeof actual.runPhaseBValidationWithPatch>
    ) => {
      if (inject.phaseB !== undefined) {
        inject.thrown += 1;
        throw inject.phaseB();
      }
      return actual.runPhaseBValidationWithPatch(...args);
    },
  };
});

import {
  type DriverHold,
  isRetriedHold,
  localFaultSubject,
} from "../src/l1-events/driver.js";
import {
  combineHolds,
  LANDED_BLOCK_LOCAL_FAULT,
  LANDED_BLOCK_REPLAY_INCOMPLETE,
  LANDED_BLOCK_VERIFIER_UNAVAILABLE,
} from "../src/landed-blocks/holds.js";
import {
  landedBlockLocalFaultCounter,
  replayOutcomeAlarm,
  replayOutcomeHold,
} from "../src/landed-blocks/replay-outcome-hold.js";
import { ForeignBlockVerificationError } from "../src/mpf/verified-block-import.js";
import {
  coalescedRunner,
  localFaultProgress,
} from "../src/services/l1-follower.coalesced-runner.js";
import {
  signalTransientExhausted,
  TransientBudgetExhaustedError,
} from "../src/services/transient-exhaustion.js";
import { committedBlock, fundedBy } from "./import-check.fixture.js";

type Block = Awaited<ReturnType<typeof committedBlock>>;

const oneSpend = async (byte: number): Promise<Block> => {
  const parent = [fundedBy(byte, "accept")];
  return committedBlock(parent, [
    { input: parent[0]!.outRef, body: "accept", next: "accept" },
  ]);
};

/** The import's failure with the armed phase throwing; never a success. */
const failedImport = async (block: Block) => {
  const result = await block.imported();
  if (result._tag === "Right") throw new Error("the import succeeded");
  const error = result.left;
  if (!(error instanceof ForeignBlockVerificationError))
    throw new Error(`the import failed outside its outcomes: ${String(error)}`);
  return error;
};

/** The hold processing records for `error`, as `process.ts` builds it. */
const holdOf = (block: Block, error: ForeignBlockVerificationError) =>
  combineHolds([
    replayOutcomeHold(
      error.reason as Parameters<typeof replayOutcomeHold>[0],
      `block ${block.headerHash}: ${error.detail}`,
      block.headerHash,
    ),
  ])!;

const phaseAFault = () => new Error("Phase A fault injected by the test");

describe("foreign import when Phase A throws", () => {
  it("holds the block as incomplete on a retried hold that raises the local-fault alarm", async () => {
    const block = await oneSpend(0x51);

    const thrownBefore = inject.thrown;
    inject.phaseA = phaseAFault;
    const error = await failedImport(block).finally(() => {
      inject.phaseA = undefined;
    });
    expect(inject.thrown - thrownBefore).toBe(1);
    expect(error.reason).toBe("incomplete");

    // The landed-block replay holds on the import's reason as its outcome,
    // a local fault on the block.
    const kind = error.reason;
    const hold = replayOutcomeHold(
      kind,
      `block ${block.headerHash}: ${error.detail}`,
      block.headerHash,
    );
    expect([hold.reason, isRetriedHold(hold), localFaultSubject(hold)]).toEqual(
      [LANDED_BLOCK_REPLAY_INCOMPLETE, true, block.headerHash],
    );

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

  it("retries the block until the local-fault bound, then ends in the node's typed exit", async () => {
    const block = await oneSpend(0x52);
    const exhaustion = Effect.runSync(
      Deferred.make<never, TransientBudgetExhaustedError>(),
    );
    const runs: DriverHold[] = [];
    let running = false;
    const abort = new AbortController();
    // Only the runner's backoff is faked; the import's own turns stay real.
    vi.useFakeTimers({ toFake: ["setTimeout", "clearTimeout"] });
    try {
      const trigger = coalescedRunner(
        async () => {
          running = true;
          inject.phaseA = phaseAFault;
          try {
            const hold = holdOf(block, await failedImport(block));
            runs.push(hold);
            return [hold];
          } finally {
            inject.phaseA = undefined;
            running = false;
          }
        },
        abort.signal,
        {
          // Processing commits nothing: no progress past the block.
          progress: localFaultProgress(),
          // As the follower wires the runner's exit.
          onExhausted: (holds) =>
            signalTransientExhausted(exhaustion, {
              source: "driver",
              reason: holds[0]?.reason ?? "l1_driver_transient_failure",
              detail: holds
                .map((hold) => `${hold.reason}: ${hold.detail}`)
                .join("; "),
            }),
        },
      );
      const exited = () =>
        Effect.runSync(Deferred.isDone(exhaustion)) && !running;
      const drive = async (steps: number, until: () => boolean) => {
        for (let step = 0; step < steps && !until(); step += 1) {
          await vi.advanceTimersByTimeAsync(1_000);
          await new Promise<void>((resolve) => setImmediate(resolve));
        }
      };
      trigger();
      // The backoff before the bound's last run is about a minute.
      await drive(600, exited);
      // Stopped: no further run on the backoff or a trigger.
      trigger();
      await drive(600, () => false);
      expect(runs).toHaveLength(LOCAL_FAULT_RUN_LIMIT);
      for (const hold of runs)
        expect([
          hold.reason,
          isRetriedHold(hold),
          localFaultSubject(hold),
        ]).toEqual([LANDED_BLOCK_REPLAY_INCOMPLETE, true, block.headerHash]);
    } finally {
      abort.abort();
      vi.useRealTimers();
    }
    const exit = await Effect.runPromiseExit(Deferred.await(exhaustion));
    expect(Exit.isFailure(exit)).toBe(true);
    if (!Exit.isFailure(exit) || exit.cause._tag !== "Fail")
      throw new Error("the runner did not exit on a typed failure");
    const failure = exit.cause.error;
    expect(failure).toBeInstanceOf(TransientBudgetExhaustedError);
    expect([failure.source, failure.reason]).toEqual([
      "driver",
      LANDED_BLOCK_REPLAY_INCOMPLETE,
    ]);
    expect(failure.message).toContain(`block ${block.headerHash}`);
  }, 120_000);
});

const verifierFault = () =>
  new LedgerSignatureVerifierUnavailableError(
    "Ed25519 verification is unavailable: fault injected by the test",
  );

describe("foreign import when a phase raises the verifier's fault", () => {
  it.each([
    ["Phase A", "directly", "phaseA", verifierFault],
    [
      "Phase A",
      "as a cause",
      "phaseA",
      () =>
        Object.assign(new Error("Phase A fault"), { cause: verifierFault() }),
    ],
    ["Phase B", "directly", "phaseB", verifierFault],
    [
      "Phase B",
      "as a cause",
      "phaseB",
      () =>
        Object.assign(new Error("Phase B fault"), { cause: verifierFault() }),
    ],
  ] as const)(
    "holds the block as verifier_unavailable when %s raises it %s",
    async (_phase, _how, phase, fault) => {
      const block = await oneSpend(0x53);
      const thrownBefore = inject.thrown;
      inject[phase] = fault;
      const error = await failedImport(block).finally(() => {
        inject[phase] = undefined;
      });
      expect(inject.thrown - thrownBefore).toBe(1);
      expect(error.reason).toBe("verifier_unavailable");
      expect(error.detail).toContain("LedgerSignatureVerifierUnavailableError");
      expect(error.detail).toContain("fault injected by the test");
      const hold = holdOf(block, error);
      expect([
        hold.reason,
        isRetriedHold(hold),
        localFaultSubject(hold),
      ]).toEqual([LANDED_BLOCK_VERIFIER_UNAVAILABLE, true, block.headerHash]);
    },
  );
});
