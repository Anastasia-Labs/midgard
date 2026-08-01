import { spawnSync } from "node:child_process";
import {
  copyFileSync,
  existsSync,
  mkdirSync,
  mkdtempSync,
  readFileSync,
  rmSync,
  symlinkSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import path from "node:path";
import { fileURLToPath } from "node:url";

import { describe, expect, it } from "vitest";

const packageRoot = path.resolve(
  path.dirname(fileURLToPath(import.meta.url)),
  "..",
);
const repositoryRoot = path.resolve(packageRoot, "../..");
const generatorRelativePath =
  "demo/lucid-midgard/scripts/generate-native-compact-aiken-goldens.mjs";
const generatorPath = path.join(repositoryRoot, generatorRelativePath);
const generatedRelativePaths = [
  "demo/lucid-midgard/tests/fixtures/native-high-cardinality.json",
  "demo/lucid-midgard/tests/fixtures/native-size-balanced-15_5k.json",
  "onchain/aiken/lib/midgard/fraud-proofs/native-tx.high-cardinality.test.ak",
  "onchain/aiken/lib/midgard/fraud-proofs/native-tx.size-balanced.test.ak",
  "onchain/aiken/lib/midgard/fraud-proofs/native-tx.test.ak",
];

const runGenerator = (scriptPath: string, cwd: string) =>
  spawnSync(process.execPath, [scriptPath, "--check"], {
    cwd,
    encoding: "utf8",
  });

const snapshotFiles = (root: string): Map<string, string> =>
  new Map(
    generatedRelativePaths.map((relativePath) => [
      relativePath,
      readFileSync(path.join(root, relativePath), "utf8"),
    ]),
  );

const copyGeneratorMirror = (root: string): string => {
  for (const relativePath of [
    generatorRelativePath,
    ...generatedRelativePaths,
  ]) {
    const destination = path.join(root, relativePath);
    mkdirSync(path.dirname(destination), { recursive: true });
    copyFileSync(path.join(repositoryRoot, relativePath), destination);
  }

  const coreDestination = path.join(root, "demo/midgard-core");
  mkdirSync(path.dirname(coreDestination), { recursive: true });
  symlinkSync(
    path.join(repositoryRoot, "demo/midgard-core"),
    coreDestination,
    "dir",
  );
  return path.join(root, generatorRelativePath);
};

describe("native compact golden generator", () => {
  it("checks checked-in goldens without writing", () => {
    expect(
      existsSync(path.join(repositoryRoot, "demo/midgard-core/dist/index.js")),
      "build demo/midgard-core before running this check",
    ).toBe(true);

    const before = snapshotFiles(repositoryRoot);
    const result = runGenerator(generatorPath, repositoryRoot);

    expect(result.error).toBeUndefined();
    expect(result.status).toBe(0);
    expect(result.stdout).toContain("checked ");
    expect(result.stdout).not.toContain("wrote ");
    expect(snapshotFiles(repositoryRoot)).toEqual(before);
  });

  it("rejects an isolated stale fixture without repairing it", () => {
    expect(
      existsSync(path.join(repositoryRoot, "demo/midgard-core/dist/index.js")),
      "build demo/midgard-core before running this check",
    ).toBe(true);

    const mirrorRoot = mkdtempSync(path.join(tmpdir(), "midgard-rf076-"));
    try {
      const mirrorGeneratorPath = copyGeneratorMirror(mirrorRoot);
      const fixtureRelativePath =
        "demo/lucid-midgard/tests/fixtures/native-size-balanced-15_5k.json";
      const fixturePath = path.join(mirrorRoot, fixtureRelativePath);
      const fixture = JSON.parse(readFileSync(fixturePath, "utf8")) as {
        compactBodyCborHex: string;
      };
      fixture.compactBodyCborHex = "00";
      const tamperedFixture = `${JSON.stringify(fixture, null, 2)}\n`;
      writeFileSync(fixturePath, tamperedFixture, "utf8");

      const result = runGenerator(mirrorGeneratorPath, mirrorRoot);

      expect(result.error).toBeUndefined();
      expect(result.status).not.toBe(0);
      expect(`${result.stdout}${result.stderr}`).toContain(
        "stale generated artifact",
      );
      expect(readFileSync(fixturePath, "utf8")).toBe(tamperedFixture);
    } finally {
      rmSync(mirrorRoot, { recursive: true, force: true });
    }
  });
});
