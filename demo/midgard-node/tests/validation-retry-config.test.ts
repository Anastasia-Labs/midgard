import "./utils.js";

import { MIDGARD_CONSENSUS_LIMITS_V1 } from "@al-ft/midgard-core/consensus-profile-v1";
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

describe("durable admission byte backlog configuration", () => {
  afterEach(() => {
    vi.unstubAllEnvs();
  });

  it("defaults to one full V1 DA envelope", async () => {
    await expect(loadNodeConfig()).resolves.toMatchObject({
      MAX_DURABLE_ADMISSION_BACKLOG_BYTES:
        MIDGARD_CONSENSUS_LIMITS_V1.maxDaPayloadBytes,
    });
  });

  it("accepts a positive operator override", async () => {
    vi.stubEnv("MAX_DURABLE_ADMISSION_BACKLOG_BYTES", "1048576");
    await expect(loadNodeConfig()).resolves.toMatchObject({
      MAX_DURABLE_ADMISSION_BACKLOG_BYTES: 1_048_576,
    });
  });

  it("rejects non-positive byte quotas", async () => {
    vi.stubEnv("MAX_DURABLE_ADMISSION_BACKLOG_BYTES", "0");
    await expect(loadNodeConfig()).rejects.toThrow(
      "MAX_DURABLE_ADMISSION_BACKLOG_BYTES must be a positive safe integer",
    );
  });
});
