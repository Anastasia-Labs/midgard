import { Effect } from "effect";
import { describe, expectTypeOf, it } from "vitest";

import type {
  nodeFibers,
  ProcessEndingStartupFiber,
  runNodeFiberSet,
} from "../src/commands/listen.node-fibers.js";
import type { NodeConfigDep } from "../src/services/index.js";
import type { HeldFiber } from "../src/services/liveness-halt.js";

type NodeFibers = ReturnType<typeof nodeFibers>;

/** The names of the fibers in `Fibers` whose error channel is not `never`. */
type FailingFibers<Fibers> = {
  [K in keyof Fibers]: Fibers[K] extends Effect.Effect<
    unknown,
    infer E,
    unknown
  >
    ? [E] extends [never]
      ? never
      : K
    : K;
}[keyof Fibers];

// Never called: `tsc --noEmit` checks the startup fibers `runNodeFiberSet`
// admits. One that can fail is refused unless `ProcessEndingStartupFiber`
// names it.
export const startupFiberChecks = (
  run: typeof runNodeFiberSet,
  nodeConfig: NodeConfigDep,
) => {
  const roster = run({
    nodeConfig,
    withMonitoring: false,
    startupFibers: {
      historyOwnerStopped: Effect.fail(new Error("history owner stopped")),
      retainedPayloadServer: Effect.void,
    },
  });
  run({
    nodeConfig,
    withMonitoring: false,
    startupFibers: {
      // @ts-expect-error a startup fiber outside ProcessEndingStartupFiber cannot fail
      retainedPayloadServer: Effect.fail(new Error("server failed")),
    },
  });
  return roster;
};

// `runNode` runs this roster under `Effect.all(...).pipe(Effect.orDie)`, so a
// fiber that can fail stops the node. The check is in the type: `tsc --noEmit`
// rejects this file, naming the fiber, once one of them can fail.
describe("node fiber roster", () => {
  it("has no scheduled fiber that can fail", () => {
    expectTypeOf<FailingFibers<NodeFibers>>().toEqualTypeOf<never>();
  });

  it("has no startup fiber that can fail outside ProcessEndingStartupFiber", () => {
    expectTypeOf<
      Exclude<
        FailingFibers<ReturnType<typeof startupFiberChecks>>,
        ProcessEndingStartupFiber
      >
    >().toEqualTypeOf<never>();
  });

  it("holds only fibers of the roster", () => {
    expectTypeOf<Exclude<HeldFiber, keyof NodeFibers>>().toEqualTypeOf<never>();
  });
});
