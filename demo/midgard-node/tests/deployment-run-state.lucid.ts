import { createTrackedTempDirFactory } from "@al-ft/midgard-test-support/temp-files";
import {
  credentialToAddress,
  type LucidEvolution,
} from "@lucid-evolution/lucid";
import { afterEach, vi } from "vitest";

export const lucid = {
  wallet: () => ({
    address: async () =>
      credentialToAddress("Preprod", { type: "Key", hash: "ab".repeat(28) }),
  }),
  unixTimeToSlot: (unixTime: number) => Math.floor(unixTime / 1_000),
} as unknown as LucidEvolution;

export const makeTempDir = createTrackedTempDirFactory("midgard-run-state-");

afterEach(() => {
  vi.restoreAllMocks();
});
