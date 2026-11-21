/**
 * The interactive-emulator project's files run against the 72 h emulator
 * profile, not the checkout's selected one. The project's Vite plugin selects
 * it in midgard-core's generated profiles module, and when the workspace is
 * bundled the bundle applies the same rewrite (its source transform), since
 * this file then loads midgard-core from the bundle and the plugin never sees
 * it. Either way this file sees the emulator profile, or fails.
 */
import {
  DEPLOYMENT_PROFILE_DIGESTS,
  DEPLOYMENT_PROFILES,
  SELECTED_DEPLOYMENT_PROFILE,
  SELECTED_DEPLOYMENT_PROFILE_DIGEST,
} from "@al-ft/midgard-core/deployment-profile";
import { describe, expect, it } from "vitest";

const EMULATOR_PROFILE = "preprod-emulator-testing";

describe("interactive-emulator project", () => {
  it("selects the emulator profile in the midgard-core its files load", () => {
    expect(SELECTED_DEPLOYMENT_PROFILE).toBe(
      DEPLOYMENT_PROFILES[EMULATOR_PROFILE],
    );
    expect(SELECTED_DEPLOYMENT_PROFILE_DIGEST).toBe(
      DEPLOYMENT_PROFILE_DIGESTS[EMULATOR_PROFILE],
    );
  });
});
