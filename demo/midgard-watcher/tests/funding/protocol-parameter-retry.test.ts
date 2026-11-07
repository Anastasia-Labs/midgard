import { expect, it, vi } from "vitest";

import { buildWithWatcherProtocolParameterRefresh } from "../../src/funding/protocol-parameter-retry.js";

it("rebuilds an unsigned transaction once after changed live parameters", async () => {
  const build = vi
    .fn()
    .mockRejectedValueOnce(new Error("fee changed"))
    .mockResolvedValue("rebuilt");
  const refresh = vi.fn(async () => true);
  const assertCurrent = vi.fn();
  await expect(
    buildWithWatcherProtocolParameterRefresh({ build, refresh, assertCurrent }),
  ).resolves.toBe("rebuilt");
  expect(build).toHaveBeenCalledTimes(2);
  expect(refresh).toHaveBeenCalledTimes(1);
});

it("preserves the original error if the live parameters have not changed", async () => {
  const error = new Error("script refuses");
  const build = vi.fn(async () => {
    throw error;
  });
  await expect(
    buildWithWatcherProtocolParameterRefresh({
      build,
      refresh: async () => false,
      assertCurrent() {},
    }),
  ).rejects.toBe(error);
  expect(build).toHaveBeenCalledTimes(1);
});

it("bounds repeated invalidations and never retries a revoked generation", async () => {
  const build = vi.fn(async () => {
    throw new Error("fee changed again");
  });
  const refresh = vi.fn(async () => true);
  await expect(
    buildWithWatcherProtocolParameterRefresh({
      build,
      refresh,
      assertCurrent() {},
    }),
  ).rejects.toThrow("fee changed again");
  expect(build).toHaveBeenCalledTimes(2);
  expect(refresh).toHaveBeenCalledTimes(1);
  let current = true;
  build.mockClear();
  await expect(
    buildWithWatcherProtocolParameterRefresh({
      build,
      refresh: async () => {
        current = false;
        return true;
      },
      assertCurrent() {
        if (!current) throw new Error("revoked");
      },
    }),
  ).rejects.toThrow("revoked");
  expect(build).toHaveBeenCalledTimes(1);
});
