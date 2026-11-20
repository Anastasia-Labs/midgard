import { readFile } from "node:fs/promises";
import { fileURLToPath } from "node:url";

import { runDaZstdStartupSelfTest } from "@al-ft/midgard-core/da-compression";
import { describe, expect, it } from "vitest";

describe("committee runtime capability", () => {
  it("pins Node >=22.15 and passes the zstd decoder self-test", async () => {
    const packagePath = fileURLToPath(new URL("../package.json", import.meta.url));
    const manifest = JSON.parse(await readFile(packagePath, "utf8")) as {
      readonly engines?: { readonly node?: string };
    };
    expect(manifest.engines?.node).toBe(">=22.15.0");
    await expect(runDaZstdStartupSelfTest()).resolves.toBeUndefined();
  });
});
