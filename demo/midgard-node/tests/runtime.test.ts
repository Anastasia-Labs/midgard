import { readFile } from "node:fs/promises";
import { describe, expect, it } from "vitest";

const listenSourceUrl = new URL("../src/commands/listen.ts", import.meta.url);
const dockerignoreUrl = new URL("../.dockerignore", import.meta.url);

describe("Midgard node runtime ownership", () => {
  it("returns the node Effect to NodeRuntime instead of launching a detached Promise", async () => {
    const source = await readFile(listenSourceUrl, "utf8");
    const runNodeSource = source.slice(source.indexOf("export const runNode"));

    expect(runNodeSource).not.toContain("Effect.runPromise");
  });

  it("keeps host dependencies out of the Linux Docker build context", async () => {
    const ignoredPaths = (await readFile(dockerignoreUrl, "utf8"))
      .split("\n")
      .map((line) => line.trim());

    expect(ignoredPaths).toContain("node_modules");
    expect(ignoredPaths).toContain("dist");
  });
});
