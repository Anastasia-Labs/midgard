import "./utils.js";

import { Effect } from "effect";
import { afterEach, describe, expect, it, vi } from "vitest";

import { shouldRunMpfPayloadAudit } from "../src/fibers/mpf-payload-audit.js";
import {
  configureCommitMpfRuntime,
  getMpfScratchBuild,
  setMpfScratchBuild,
} from "../src/mpf/index.js";
import { NodeConfig } from "../src/services/config.js";

describe("commit MPF runtime configuration", () => {
  afterEach(() => {
    setMpfScratchBuild("insert");
    vi.unstubAllEnvs();
  });

  it("applies scratch configuration for Architecture G paths that do not call makeMpfs", async () => {
    await Effect.runPromise(
      configureCommitMpfRuntime({
        MPF_SCRATCH_BUILD: "fromlist",
        MPF_PATH_HYDRATION_MODE: "whole_block",
        MPF_HYDRATION_CHUNK_OPS: 512,
        MPF_RETAIN_HYDRATED_DEPTH: 2,
        MPF_PARALLEL_ROOTS: false,
        MPF_ROOT_WORKERS: 1,
        MPF_PARALLEL_ROOT_MIN_ENTRIES: 5_000,
      }),
    );
    expect(getMpfScratchBuild()).toBe("fromlist");
  });

  it("disables the background payload audit only when root checks are off", () => {
    expect(shouldRunMpfPayloadAudit("every_block")).toBe(true);
    expect(shouldRunMpfPayloadAudit("periodic")).toBe(true);
    expect(shouldRunMpfPayloadAudit("off")).toBe(false);
  });

  it("rejects an unknown MPF engine identifier", async () => {
    vi.stubEnv("MPF_ENGINE", "architecture_h");

    await expect(
      Effect.runPromise(
        Effect.gen(function* () {
          return yield* NodeConfig;
        }).pipe(Effect.provide(NodeConfig.layer)),
      ),
    ).rejects.toThrow(/MPF_ENGINE/);
  });
});
