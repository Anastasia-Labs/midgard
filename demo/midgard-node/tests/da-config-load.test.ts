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

/**
 * `DA_COMMITTEE_HEX`, `DA_OWNERS_HEX` and `DA_THRESHOLD` are read by exactly
 * one consumer — the bootstrap in `deriveOperatorDaParams`. Config load is
 * shared by every subsystem, so it validates their encoding and leaves the Q63
 * governed floors to the consumer.
 *
 * The regression these pin: enforcing the floors at config load made a stale
 * pre-Q63 `DA_THRESHOLD=1` in a checkout's `.env` fail `NodeConfig` for the
 * whole process, surfacing as an opaque error inside tests and subsystems that
 * never touch DA.
 */
describe("DA config loading", () => {
  it("loads a pre-Q63 DA_THRESHOLD of one without failing the process config", async () => {
    vi.stubEnv("DA_THRESHOLD", "1");
    vi.stubEnv("DA_COMMITTEE_HEX", "01".repeat(32) + "02".repeat(32));

    const config = await loadConfig();

    expect(config.DA_THRESHOLD).toBe(1n);
    expect(config.DA_COMMITTEE_HEX).toBe("01".repeat(32) + "02".repeat(32));
  });

  it("loads a single-member DA_COMMITTEE_HEX without failing the process config", async () => {
    vi.stubEnv("DA_COMMITTEE_HEX", "01".repeat(32));

    await expect(loadConfig()).resolves.toMatchObject({
      DA_COMMITTEE_HEX: "01".repeat(32),
    });
  });

  it("normalizes case and leaves the DA fields empty by default", async () => {
    vi.stubEnv(
      "DA_COMMITTEE_HEX",
      ("01".repeat(32) + "02".repeat(32)).toUpperCase(),
    );

    const config = await loadConfig();

    expect(config.DA_COMMITTEE_HEX).toBe("01".repeat(32) + "02".repeat(32));
    expect(config.DA_OWNERS_HEX).toBe("");
    expect(config.DA_COSIGNER_SEED_PHRASE).toBe("");
  });

  // Encoding faults are unambiguous regardless of governance policy, so these
  // are still worth catching at load rather than at initialization.
  it("still rejects malformed and unsorted DA key sets", async () => {
    vi.stubEnv("DA_COMMITTEE_HEX", "nothex");
    await expect(loadConfig()).rejects.toThrow(/DA_COMMITTEE_HEX/);

    vi.unstubAllEnvs();
    vi.stubEnv("DA_COMMITTEE_HEX", "02".repeat(32) + "01".repeat(32));
    await expect(loadConfig()).rejects.toThrow(/sorted ascending/);

    vi.unstubAllEnvs();
    vi.stubEnv("DA_OWNERS_HEX", "11".repeat(28) + "11".repeat(28));
    await expect(loadConfig()).rejects.toThrow(/sorted ascending/);
  });

  it("rejects a non-positive DA_THRESHOLD", async () => {
    vi.stubEnv("DA_THRESHOLD", "0");
    await expect(loadConfig()).rejects.toThrow(/positive integer/);
  });

  it("rejects a malformed DA_COSIGNER_SEED_PHRASE", async () => {
    vi.stubEnv("DA_COSIGNER_SEED_PHRASE", "not a real mnemonic");
    await expect(loadConfig()).rejects.toThrow(/DA_COSIGNER_SEED_PHRASE/);
  });
});
