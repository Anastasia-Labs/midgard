import { Effect, Exit, Ref, Schedule } from "effect";
import { describe, expect, it } from "vitest";

import { nativeMpfOwnerSupervisorFiber } from "../src/fibers/native-mpf-owner-supervisor.js";
import { Globals } from "../src/services/globals.js";
import type { NativeMpfOwnerService } from "../src/services/mpf-native-owner/protocol.js";

const owner = (terminalFailure: () => Error | undefined) =>
  ({ terminalFailure }) as unknown as NativeMpfOwnerService;

// One tick per owner; the schedule installs the next owner between ticks, as
// a recovery flow replaces the live one.
const supervise = (owners: readonly (NativeMpfOwnerService | undefined)[]) =>
  Effect.runPromiseExit(
    Effect.gen(function* () {
      const globals = yield* Globals;
      yield* Ref.set(globals.NATIVE_MPF_OWNER, owners[0]);
      return yield* nativeMpfOwnerSupervisorFiber(
        Schedule.recurs(owners.length - 1).pipe(
          Schedule.tapOutput((tick) =>
            Ref.set(globals.NATIVE_MPF_OWNER, owners[tick + 1]),
          ),
        ),
      );
    }).pipe(Effect.provide(Globals.Default)),
  );

describe("native MPF owner supervisor", () => {
  it("keeps the node running while the live owner, or none, is healthy", async () => {
    let checks = 0;
    const healthy = () => {
      checks += 1;
      return undefined;
    };
    const exit = await supervise([undefined, owner(healthy), owner(healthy)]);
    expect(Exit.isSuccess(exit)).toBe(true);
    expect(checks).toBe(2);
  });

  it("fails with the terminal failure of the owner that replaced a healthy one", async () => {
    const terminal = new Error("Native MPF owner restart limit exhausted");
    const exit = await supervise([
      owner(() => undefined),
      owner(() => terminal),
      owner(() => undefined),
    ]);
    expect(exit).toEqual(Exit.fail(terminal));
  });
});
