import { SqlClient } from "@effect/sql";
import { Deferred, Effect, Fiber, Ref } from "effect";
import { describe, expect, it } from "vitest";

import {
  OPERATOR_MEMBERSHIP_HALT_REASONS,
  operatorMembershipTick,
  publishOperatorMembership,
  untilOperatorRemoved,
} from "../src/fibers/operator-membership.js";
import { withL1ControlPlane } from "../src/services/globals.globals.js";
import { Globals } from "../src/services/globals.js";
import {
  ContractDeploymentIdentity,
  Lucid,
  MidgardContracts,
  NodeConfig,
} from "../src/services/index.js";
import { HaltSource } from "../src/services/liveness-halt.js";

describe("operator membership holds duties", () => {
  it("holds duties only on authenticated removal evidence and keeps the last state on a failed check", async () => {
    await Effect.runPromise(
      Effect.gen(function* () {
        const globals = yield* Globals;
        const halt = Effect.map(Ref.get(globals.LIVENESS_REASONS), (reasons) =>
          reasons.get(HaltSource.operatorMembership),
        );
        // The startup default holds nothing: duties start before the first check.
        expect(yield* Ref.get(globals.OPERATOR_MEMBERSHIP)).toBe("unknown");
        expect(yield* halt).toBeUndefined();
        yield* publishOperatorMembership("removal_pending");
        expect(yield* halt).toBe(
          OPERATOR_MEMBERSHIP_HALT_REASONS.removal_pending,
        );
        // A check that cannot authenticate anything changes nothing.
        yield* operatorMembershipTick.pipe(
          Effect.provideService(NodeConfig, {} as never),
          Effect.provideService(MidgardContracts, {} as never),
          Effect.provideService(ContractDeploymentIdentity, {} as never),
          Effect.provideService(Lucid, {} as never),
          Effect.provideService(SqlClient.SqlClient, {} as never),
        );
        expect(yield* Ref.get(globals.OPERATOR_MEMBERSHIP)).toBe(
          "removal_pending",
        );
        expect(yield* halt).toBe(
          OPERATOR_MEMBERSHIP_HALT_REASONS.removal_pending,
        );
        yield* publishOperatorMembership("active");
        expect(yield* halt).toBeUndefined();
        yield* publishOperatorMembership("awaiting_activation");
        expect(yield* halt).toBeUndefined();
      }).pipe(Effect.provide(Globals.Default)),
    );
  });
  it("ends the node after confirmed removal, awaiting durable-work finalizers", async () => {
    const events: string[] = [];
    await Effect.runPromise(
      Effect.scoped(
        Effect.gen(function* () {
          const globals = yield* Globals;
          yield* Ref.set(globals.OPERATOR_MEMBERSHIP, "active");
          const started = yield* Deferred.make<void>();
          const duties = Effect.gen(function* () {
            events.push("started");
            yield* Deferred.succeed(started, undefined);
            yield* Effect.never;
          }).pipe(Effect.ensuring(Effect.sync(() => events.push("drained"))));
          const fiber = yield* Effect.forkScoped(untilOperatorRemoved(duties));
          yield* Deferred.await(started);
          yield* publishOperatorMembership("removed");
          expect(yield* Ref.get(globals.OPERATOR_MEMBERSHIP)).toBe("removed");
          yield* Fiber.join(fiber);
          events.push("shutdown");
        }),
      ).pipe(Effect.provide(Globals.Default)),
    );
    expect(events).toEqual(["started", "drained", "shutdown"]);
  });
  it("leaves non-duty L1 work to run while removal is pending", async () => {
    // The halt source holds the duties (see `FIBER_HALT_SOURCES`); block
    // confirmation, ingestion and the readiness refreshers keep the permit.
    await Effect.runPromise(
      Effect.gen(function* () {
        const globals = yield* Globals;
        yield* publishOperatorMembership("removal_pending");
        let ran = false;
        yield* withL1ControlPlane(
          globals,
          { scope: "block_confirmation" },
          Effect.sync(() => {
            ran = true;
          }),
        );
        expect(ran).toBe(true);
      }).pipe(Effect.provide(Globals.Default)),
    );
  });
});
