import { Effect } from "effect";
import { expect, it } from "vitest";

import * as Leases from "../src/database/stateQueueMutationLeases.js";
import { initializeNodeRuntime } from "./deposit-flow-emulator-shared.js";
import { provideDatabaseLayers } from "./utils.js";

// A completed fixture may deliberately leave a crash-model lease active.
// No worker/fiber remains: the next fixture owns this disposable shard.
it("leaves the stopped crash-model fixture lease active until fixture cleanup", async () => {
  await initializeNodeRuntime();
  const acquired = await Effect.runPromise(
    provideDatabaseLayers(Leases.tryAcquire({ holder: "stopped-fixture" })),
  );
  expect(acquired._tag).toBe("Acquired");
});

it("lets the next fixture acquire the production mutation lease after cleanup", async () => {
  const result = await Effect.runPromise(
    provideDatabaseLayers(
      Leases.tryWithLease("next-fixture", () => Effect.succeed("ran")),
    ),
  );
  expect(result).toEqual({ _tag: "Ran", value: "ran" });
});
