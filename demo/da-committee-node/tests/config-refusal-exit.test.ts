/**
 * The committee entry's exit on a configuration that refuses to load. No
 * port is known before the configuration is read, so no `/readyz` can name
 * a refusal there: a role L1 refusal (`role_non_follower_l1_config`) exits
 * 78 (EX_CONFIG, which a supervisor does not restart on) with one named
 * line, and every other configuration error keeps exit 1 and its stack.
 */
import { NON_FOLLOWER_L1_ENV_KEYS } from "@al-ft/midgard-l1-follower";
import { afterEach, describe, expect, it, vi } from "vitest";

import { loadCommitteeConfig } from "../src/config.js";
import {
  COMMITTEE_CONFIG_REFUSAL_EXIT_CODE,
  COMMITTEE_CONFIG_REFUSED,
  committeeFailureExit,
} from "../src/config-refusal-exit.js";

/** Runs the committee entry as its process would, returning how it exited. */
const runEntry = async () => {
  const exited = new Promise<number>((resolve) => {
    vi.spyOn(process, "exit").mockImplementation(((code?: number) => {
      resolve(code ?? 0);
      return undefined as never;
    }) as typeof process.exit);
  });
  const written: string[] = [];
  vi.spyOn(process.stderr, "write").mockImplementation((chunk) => {
    written.push(String(chunk));
    return true;
  });
  vi.resetModules();
  await import("../src/index.js");
  const code = await exited;
  return { code, stderr: written.join("") };
};

const followerOnlyEnv = () => {
  vi.stubEnv("MIDGARD_CONFIG_MODE", "disabled");
  for (const key of NON_FOLLOWER_L1_ENV_KEYS) vi.stubEnv(key, "");
  vi.stubEnv("MIDGARD_DEPLOYMENT_MANIFEST_PATH", "");
};

describe("the committee entry's exit on a refused configuration", () => {
  afterEach(() => {
    vi.unstubAllEnvs();
    vi.restoreAllMocks();
  });

  it("exits 78 naming role_non_follower_l1_config for a Kupmios setting", async () => {
    followerOnlyEnv();
    vi.stubEnv("L1_KUPO_URL", "http://kupo:1442");
    const { code, stderr } = await runEntry();
    expect(code).toBe(COMMITTEE_CONFIG_REFUSAL_EXIT_CODE);
    expect(code).toBe(78);
    const line = JSON.parse(stderr.trim()) as Record<string, unknown>;
    expect(line).toMatchObject({
      event: COMMITTEE_CONFIG_REFUSED,
      reason: "role_non_follower_l1_config",
      keys: ["L1_KUPO_URL"],
    });
    expect(line.detail).toMatch(/da-committee-node reads L1 only through/u);
  });

  it("keeps exit 1 and the stack for any other configuration error", async () => {
    followerOnlyEnv();
    const { code, stderr } = await runEntry();
    expect(code).toBe(1);
    expect(stderr).toMatch(/MIDGARD_DEPLOYMENT_MANIFEST_PATH/u);
    expect(stderr).not.toContain(COMMITTEE_CONFIG_REFUSED);
  });

  it("finds the refusal on the cause chain, and maps nothing else to 78", async () => {
    const refusal = await loadCommitteeConfig({ L1_ACCESS: "kupmios" }).catch(
      (error: unknown) => error,
    );
    expect(
      committeeFailureExit(new Error("wrapped", { cause: refusal })).code,
    ).toBe(78);
    expect(committeeFailureExit(new Error("other")).code).toBe(1);
    expect(committeeFailureExit("not an error")).toEqual({
      code: 1,
      line: "not an error\n",
    });
  });
});
