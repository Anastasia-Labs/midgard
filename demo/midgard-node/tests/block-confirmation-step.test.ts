import { Effect, Exit } from "effect";
import { describe, expect, it } from "vitest";

import { blockConfirmationStep } from "../src/fibers/block-confirmation.block-confirmation-fiber.js";

describe("block confirmation step", () => {
  it("logs a failed or dead tick and succeeds, so the next tick retries", async () => {
    for (const action of [
      Effect.fail(new Error("Ogmios unavailable")),
      Effect.die(new Error("worker crashed")),
      Effect.succeed(true),
    ]) {
      const exit = await Effect.runPromiseExit(blockConfirmationStep(action));
      expect(Exit.isSuccess(exit)).toBe(true);
    }
  });
});
