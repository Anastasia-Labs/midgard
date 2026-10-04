import { execFileSync } from "node:child_process";
import { mkdirSync, mkdtempSync, writeFileSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { resolve } from "node:path";

export const fixture = (test) => {
  const root = mkdtempSync(resolve(tmpdir(), "midgard-contrib-test-"));
  test.after(() => rmSync(root, { recursive: true, force: true }));
  mkdirSync(resolve(root, "demo/example/src"), { recursive: true });
  mkdirSync(resolve(root, "demo/example/dist"));
  writeFileSync(
    resolve(root, "demo/example/package.json"),
    JSON.stringify({
      name: "example",
      type: "module",
      scripts: {
        build: "tsup src/index.ts",
        "build:contrib-raw": "tsup src/index.ts",
      },
    }),
  );
  writeFileSync(
    resolve(root, "demo/example/src/index.ts"),
    "export const x = 1;\n",
  );
  writeFileSync(
    resolve(root, "demo/example/dist/index.js"),
    "export const x = 1;\n",
  );
  execFileSync("git", ["init", "-q"], { cwd: root });
  execFileSync(
    "git",
    [
      "add",
      "demo/example/package.json",
      "demo/example/src/index.ts",
      "demo/example/dist/index.js",
    ],
    { cwd: root },
  );
  execFileSync(
    "git",
    [
      "-c",
      "user.name=Fixture",
      "-c",
      "user.email=fixture@example.invalid",
      "-c",
      "core.hooksPath=/dev/null",
      "commit",
      "-qm",
      "Fixture",
    ],
    { cwd: root },
  );
  return root;
};
