import { Effect } from "effect";
import { afterEach, describe, expect, it } from "vitest";

import { shouldRunMpfPayloadAudit } from "@/fibers/mpf-payload-audit.js";
import {
  configureCommitMpfRuntime,
  getMpfScratchBuild,
  setMpfScratchBuild,
} from "@/workers/utils/mpf.js";

describe("commit MPF runtime configuration", () => {
  afterEach(() => setMpfScratchBuild("insert"));

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
});
