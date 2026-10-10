import "./utils.js";

import { Effect } from "effect";
import { afterEach, describe, expect, it, vi } from "vitest";

import { NodeConfig } from "../src/services/config.js";

const loadConfig = () =>
  Effect.runPromise(
    Effect.gen(function* () {
      return yield* NodeConfig;
    }).pipe(Effect.provide(NodeConfig.layer)),
  );

afterEach(() => {
  vi.unstubAllEnvs();
});

const HASH = "ab".repeat(32);

/**
 * L1_ORIGIN is the operator-config form of the deployment origin until the
 * redeploy carries it in the manifest (l1-architecture-plan §5.3, §14).
 * Unset, it is not configured; malformed, the node refuses to load config,
 * before any readiness listener is bound.
 */
describe("L1_ORIGIN config", () => {
  it("is null when unset or empty", async () => {
    vi.stubEnv("L1_ORIGIN", undefined);
    expect((await loadConfig()).L1_ORIGIN).toBeNull();
    vi.stubEnv("L1_ORIGIN", "");
    expect((await loadConfig()).L1_ORIGIN).toBeNull();
  });

  it("parses <slot>.<block hash>", async () => {
    vi.stubEnv("L1_ORIGIN", `86400.${HASH}`);
    expect((await loadConfig()).L1_ORIGIN).toEqual({
      slot: 86_400,
      blockHash: HASH,
    });
  });

  it.each([`86400:${HASH}`, `86400.${HASH.toUpperCase()}`, `x.${HASH}`])(
    "refuses %s, naming the key",
    async (value) => {
      vi.stubEnv("L1_ORIGIN", value);
      await expect(loadConfig()).rejects.toThrow(/L1_ORIGIN/u);
    },
  );
});
