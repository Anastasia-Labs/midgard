import { writeFile } from "node:fs/promises";
import { join } from "node:path";

import { createTrackedTempDirFactory } from "@al-ft/midgard-test-support/temp-files";
import { describe, expect, it } from "vitest";

import {
  buildE2EProcessEnv,
  loadDotenvFile,
  parseEnvOverride,
} from "../src/e2e/env.js";

const makeTempDir = createTrackedTempDirFactory("midgard-e2e-env-");

describe("e2e env helper", () => {
  it("parses dotenv quoting without shell syntax leaking into values", async () => {
    const dir = await makeTempDir();
    const envFile = join(dir, ".env");
    await writeFile(
      envFile,
      [
        'WALLET_SEED_PHRASE="alpha beta gamma"',
        "NETWORK=Preprod",
        "# ignored comment",
      ].join("\n"),
      "utf8",
    );

    await expect(loadDotenvFile(envFile)).resolves.toMatchObject({
      WALLET_SEED_PHRASE: "alpha beta gamma",
      NETWORK: "Preprod",
    });
  });

  it("applies env files before explicit overrides and records redacted provenance", async () => {
    const dir = await makeTempDir();
    await writeFile(
      join(dir, ".env"),
      ['WALLET_SEED_PHRASE="alpha beta"', "NETWORK=Preview"].join("\n"),
      "utf8",
    );

    const built = await buildE2EProcessEnv({
      cwd: dir,
      envFiles: [".env"],
      overrides: {
        NETWORK: "Preprod",
        L1_PROVIDER_API_KEY: "secret",
      },
      inherit: "none",
    });

    expect(built.env).toMatchObject({
      WALLET_SEED_PHRASE: "alpha beta",
      NETWORK: "Preprod",
      L1_PROVIDER_API_KEY: "secret",
    });
    expect(built.provenance).toMatchObject({
      inheritance: "none",
      envFiles: [
        {
          path: join(dir, ".env"),
          keys: ["NETWORK", "WALLET_SEED_PHRASE=<redacted>"],
        },
      ],
      overrideKeys: ["L1_PROVIDER_API_KEY=<redacted>", "NETWORK"],
      explicitEnvKeys: [
        "L1_PROVIDER_API_KEY=<redacted>",
        "NETWORK",
        "WALLET_SEED_PHRASE=<redacted>",
      ],
    });
  });

  it("rejects malformed explicit env overrides", () => {
    expect(() => parseEnvOverride("NO_EQUALS")).toThrow(/KEY=VALUE/);
    expect(() => parseEnvOverride("1BAD=value")).toThrow(/invalid/);
  });
});
