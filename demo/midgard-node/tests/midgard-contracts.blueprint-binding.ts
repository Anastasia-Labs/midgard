import { createHash } from "node:crypto";
import { mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import {
  SELECTED_DEPLOYMENT_PROFILE,
  SELECTED_DEPLOYMENT_PROFILE_DIGEST,
} from "@al-ft/midgard-core/deployment-profile";
import { Effect } from "effect";
import { expect, it as unitIt, vi } from "vitest";

import { loadRealBlueprintSha256 } from "../src/services/midgard-contracts.js";

export const registerBlueprintProfileBindingTests = () => {
  unitIt.each(["missing", "digest", "bytes"])(
    "rejects a %s blueprint profile binding",
    async (mutation) => {
      const dir = await mkdtemp(join(tmpdir(), "midgard-profile-blueprint-"));
      const blueprintPath = join(dir, "plutus.json");
      const raw = await readFile(
        new URL("../../../onchain/aiken/plutus.json", import.meta.url),
      );
      try {
        await writeFile(blueprintPath, raw);
        if (mutation !== "missing") {
          await writeFile(
            `${blueprintPath}.deployment.json`,
            JSON.stringify({
              profile: SELECTED_DEPLOYMENT_PROFILE,
              profileDigest:
                mutation === "digest"
                  ? "00".repeat(32)
                  : SELECTED_DEPLOYMENT_PROFILE_DIGEST,
              blueprintHash:
                mutation === "bytes"
                  ? "00".repeat(32)
                  : createHash("sha256").update(raw).digest("hex"),
            }),
          );
        }
        vi.stubEnv("MIDGARD_REAL_BLUEPRINT_PATH", blueprintPath);
        await expect(
          Effect.runPromise(loadRealBlueprintSha256()),
        ).rejects.toThrow(/Failed to hash canonical real blueprint/u);
      } finally {
        vi.unstubAllEnvs();
        await rm(dir, { recursive: true });
      }
    },
  );
};
