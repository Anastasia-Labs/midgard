import { mkdtemp, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { afterEach, describe, expect, it } from "vitest";

import {
  buildE2EProcessEnv,
  loadDotenvFile,
  parseEnvOverride,
} from "@/e2e/env.js";

let tempDirs: string[] = [];

const makeTempDir = async (): Promise<string> => {
  const dir = await mkdtemp(join(tmpdir(), "midgard-e2e-env-"));
  tempDirs.push(dir);
  return dir;
};

afterEach(async () => {
  await Promise.all(
    tempDirs.map((dir) => rm(dir, { recursive: true, force: true })),
  );
  tempDirs = [];
});

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
