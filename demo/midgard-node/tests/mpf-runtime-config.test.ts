import "./utils.js";

import { Effect } from "effect";
import { afterEach, describe, expect, it } from "vitest";

import { shouldRunMpfPayloadAudit } from "../src/fibers/mpf-payload-audit.js";
import {
  configureCommitMpfRuntime,
  getMpfScratchBuild,
  setMpfScratchBuild,
} from "../src/mpf/index.js";
import { requirePinnedNativeOwnerBinary } from "../src/services/native-mpf-startup.js";

describe("commit MPF runtime configuration", () => {
  afterEach(() => {
    setMpfScratchBuild("insert");
  });

  it("applies the scratch build configuration", async () => {
    await Effect.runPromise(
      configureCommitMpfRuntime({
        MPF_SCRATCH_BUILD: "fromlist",
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

  const pinned = {
    MPF_NATIVE_OWNER_BINARY_PATH: "/app/native/architecture-g-owner",
    MPF_NATIVE_OWNER_BINARY_SHA256: "ab".repeat(32),
    MPF_NATIVE_OWNER_SIDECAR_PATH: "/app/db/ledger.architecture-g.sidecar",
  };

  it("accepts a native owner pinned by path, SHA-256 and sidecar", async () => {
    await expect(
      Effect.runPromise(requirePinnedNativeOwnerBinary(pinned)),
    ).resolves.toBeUndefined();
  });

  it.each([
    ["an unset SHA-256", { MPF_NATIVE_OWNER_BINARY_SHA256: "" }, /SHA256/],
    [
      "an uppercase SHA-256",
      { MPF_NATIVE_OWNER_BINARY_SHA256: "AB".repeat(32) },
      /SHA256/,
    ],
    [
      "a truncated SHA-256",
      { MPF_NATIVE_OWNER_BINARY_SHA256: "ab".repeat(31) },
      /SHA256/,
    ],
    ["an empty binary path", { MPF_NATIVE_OWNER_BINARY_PATH: " " }, /PATH/],
    [
      "an empty sidecar path",
      { MPF_NATIVE_OWNER_SIDECAR_PATH: "" },
      /SIDECAR_PATH/,
    ],
  ])(
    "refuses to start the native owner from %s",
    async (_, override, error) => {
      await expect(
        Effect.runPromise(
          requirePinnedNativeOwnerBinary({ ...pinned, ...override }),
        ),
      ).rejects.toThrow(error);
    },
  );
});
