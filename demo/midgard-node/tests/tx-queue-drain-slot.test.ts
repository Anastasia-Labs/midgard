import { Effect, Exit, Ref } from "effect";
import { describe, expect, it } from "vitest";

import { txQueueProcessorDrainOnce } from "../src/fibers/tx-queue-processor.tx-queue-processor-drain-once.js";
import { Globals, NodeConfig } from "../src/services/index.js";

// Each case fails or stalls right after the slot claim, so the drain loop and
// the services only it uses never run.
const runDrain = <A, E>(
  effect: (globals: Globals) => Effect.Effect<A, E, Globals | NodeConfig>,
) =>
  Effect.runPromise(
    Effect.gen(function* () {
      const globals = yield* Globals;
      const result = yield* effect(globals);
      return {
        result,
        active: yield* Ref.get(globals.TX_QUEUE_PROCESSOR_ACTIVE),
      };
    }).pipe(
      Effect.provide(Globals.Default),
      Effect.provideService(NodeConfig, {
        VALIDATION_DRAIN_LOOPS: 2,
      } as unknown as NodeConfig["Type"]),
    ),
  );

const drainOnce = (afterSlotClaimed: () => Effect.Effect<void>) =>
  txQueueProcessorDrainOnce({ afterSlotClaimed }) as Effect.Effect<
    void,
    unknown,
    Globals | NodeConfig
  >;

describe("claiming a slot before the release is attached", () => {
  // The pre-fix shape: the release is attached only after the window.
  const claimThenEnsure = (
    slots: Ref.Ref<number>,
    window: Effect.Effect<void>,
  ) =>
    Effect.gen(function* () {
      yield* Ref.update(slots, (n) => n + 1);
      yield* window;
      yield* Effect.void.pipe(Effect.ensuring(Ref.update(slots, (n) => n - 1)));
    });

  it("leaks the slot when a defect strikes in the window", async () => {
    const leaked = await Effect.runPromise(
      Effect.gen(function* () {
        const slots = yield* Ref.make(0);
        yield* claimThenEnsure(slots, Effect.die("defect")).pipe(Effect.exit);
        return yield* Ref.get(slots);
      }),
    );
    expect(leaked).toBe(1);
  });
});

describe("tx-queue drain slot", () => {
  it("returns the slot when a defect strikes right after the claim", async () => {
    const { result, active } = await runDrain(() =>
      drainOnce(() => Effect.die("injected defect")).pipe(Effect.exit),
    );
    expect(Exit.isFailure(result)).toBe(true);
    expect(active).toBe(0);
  });

  it("returns every slot when a sibling's failure interrupts a drain inside the window", async () => {
    const { result, active } = await runDrain(() =>
      Effect.all(
        [
          drainOnce(() =>
            Effect.sleep(5).pipe(Effect.zipRight(Effect.die("x"))),
          ),
          drainOnce(() => Effect.never),
        ],
        { concurrency: "unbounded" },
      ).pipe(Effect.exit),
    );
    expect(Exit.isFailure(result)).toBe(true);
    expect(active).toBe(0);
  });

  it("claims nothing when every slot is taken, and leaves the count alone", async () => {
    const { result, active } = await runDrain((globals) =>
      Ref.set(globals.TX_QUEUE_PROCESSOR_ACTIVE, 2).pipe(
        Effect.zipRight(drainOnce(() => Effect.die("never reached"))),
        Effect.exit,
      ),
    );
    expect(Exit.isSuccess(result)).toBe(true);
    expect(active).toBe(2);
  });
});
