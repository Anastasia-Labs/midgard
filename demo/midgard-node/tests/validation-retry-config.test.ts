import "./utils.js";

import { Effect } from "effect";
import { afterEach, describe, expect, it, vi } from "vitest";

import { NodeConfig } from "@/services/config.js";

const loadNodeConfig = () =>
  Effect.runPromise(
    Effect.gen(function* () {
      return yield* NodeConfig;
    }).pipe(Effect.provide(NodeConfig.layer)),
  );

describe("validation retry configuration", () => {
  afterEach(() => {
    vi.unstubAllEnvs();
  });

  it("rejects non-positive retry delays", async () => {
    vi.stubEnv("VALIDATION_RETRY_BACKOFF_BASE_MS", "0");
    vi.stubEnv("VALIDATION_RETRY_BACKOFF_MAX_MS", "1000");

    await expect(loadNodeConfig()).rejects.toThrow(
      "VALIDATION_RETRY_BACKOFF_BASE_MS must be a positive safe integer",
    );
  });

  it("rejects a retry cap below the base delay", async () => {
    vi.stubEnv("VALIDATION_RETRY_BACKOFF_BASE_MS", "1000");
    vi.stubEnv("VALIDATION_RETRY_BACKOFF_MAX_MS", "999");

    await expect(loadNodeConfig()).rejects.toThrow(
      "VALIDATION_RETRY_BACKOFF_MAX_MS must not be less than VALIDATION_RETRY_BACKOFF_BASE_MS",
    );
  });
});
