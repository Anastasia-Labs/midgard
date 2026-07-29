import { readdirSync, readFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

import { describe, expect, it } from "vitest";

import {
  parseWatcherArguments,
  WATCHER_NOT_READY_EXIT_CODE,
} from "../src/cli.js";
import {
  runWatcherCommand,
  WATCHER_FOUNDATION_STATUS,
} from "../src/scaffold.js";

const packageRoot = dirname(dirname(fileURLToPath(import.meta.url)));

const readJson = (path: string): Record<string, unknown> =>
  JSON.parse(readFileSync(path, "utf8")) as Record<string, unknown>;

describe("W00 watcher package identity", () => {
  it("declares an independent command package with all required scripts", () => {
    const watcherPackage = readJson(join(packageRoot, "package.json"));
    const committeePackage = readJson(
      join(packageRoot, "..", "da-committee-node", "package.json"),
    );

    expect(watcherPackage.name).toBe("midgard-watcher");
    expect(watcherPackage.bin).toEqual({
      "midgard-watcher": "./dist/cli.js",
    });
    expect(watcherPackage.dependencies).toEqual({
      "@al-ft/midgard-sdk": "workspace:*",
      "@lucid-evolution/lucid": "0.6.0",
    });
    expect(watcherPackage.scripts).toMatchObject({
      build: expect.any(String),
      lint: expect.any(String),
      replay: "node dist/cli.js replay",
      start: "node dist/cli.js start",
      test: expect.any(String),
      typecheck: "tsc --noEmit",
    });

    expect(committeePackage.name).toBe("da-committee-node");
    expect(committeePackage.bin).toEqual({
      "da-committee-node": "./dist/index.js",
    });
    expect(committeePackage.name).not.toBe(watcherPackage.name);
  });

  it("does not alias the committee or import operator-private services", () => {
    const sourceDirectory = join(packageRoot, "src");
    const source = readdirSync(sourceDirectory)
      .filter((path) => path.endsWith(".ts"))
      .map((path) => readFileSync(join(sourceDirectory, path), "utf8"))
      .join("\n");

    expect(source).not.toMatch(/da-committee-node/);
    expect(source).not.toMatch(/midgard-node/);
    expect(source).not.toMatch(
      /(?:^|[/.-])(?:admin|database|postgres)(?:[/.-]|$)/,
    );
    expect(source).not.toMatch(/export\s+\*\s+from/);
  });
});

describe("W00 fail-closed command behavior", () => {
  it.each(["start", "replay"] as const)(
    "%s cannot report production readiness",
    (command) => {
      const errors: string[] = [];
      const exitCode = runWatcherCommand(command, {
        writeError: (text) => errors.push(text),
      });
      const result = JSON.parse(errors.join("")) as Record<string, unknown>;

      expect(exitCode).toBe(WATCHER_NOT_READY_EXIT_CODE);
      expect(exitCode).not.toBe(0);
      expect(result).toMatchObject({
        packageName: "midgard-watcher",
        state: "foundation_incomplete",
        productionReady: false,
        implementedThrough: "W00",
        requiredBeforeReadiness: ["W01-W46", "WG1", "WG2"],
        command,
      });
    },
  );

  it("defaults to fail-closed start and exposes replay as a distinct command", () => {
    expect(parseWatcherArguments([])).toEqual({
      kind: "command",
      command: "start",
    });
    expect(parseWatcherArguments(["replay"])).toEqual({
      kind: "command",
      command: "replay",
    });
    expect(WATCHER_FOUNDATION_STATUS.productionReady).toBe(false);
  });
});
