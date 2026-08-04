import { execFile } from "node:child_process";
import { mkdir, mkdtemp, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { promisify } from "node:util";

import { describe, expect, it } from "vitest";

const execFileAsync = promisify(execFile);
const packageRoot = resolve(dirname(fileURLToPath(import.meta.url)), "..");
const scriptPath = join(packageRoot, "scripts/check-no-http-da-transport.mjs");

describe("no HTTP DA transport guardrail", () => {
  it("passes for the migrated DA libp2p target set", async () => {
    const { stderr, stdout } = await execFileAsync(
      process.execPath,
      [scriptPath],
      {
        cwd: packageRoot,
      },
    );

    expect(stderr).toBe("");
    expect(stdout).toContain("No forbidden HTTP/URL tokens found");
    expect(stdout).toContain("migrated DA libp2p target file");
  });

  it("fails on an injected forbidden token in a new libp2p DA module", async () => {
    const dir = await mkdtemp(join(tmpdir(), "midgard-da-guardrail-"));
    const targetDir = join(dir, "src/da/libp2p");
    await mkdir(targetDir, { recursive: true });
    const badFile = join(targetDir, "BadTransport.ts");
    await writeFile(
      badFile,
      [
        "export const retrievePayload = async () => {",
        '  return fetch("http://da.example/payload");',
        "};",
        "",
      ].join("\n"),
    );

    await expect(
      execFileAsync(process.execPath, [scriptPath, targetDir], {
        cwd: packageRoot,
      }),
    ).rejects.toMatchObject({
      code: 1,
      stderr: expect.stringContaining("Forbidden HTTP/URL"),
    });
  });
});
