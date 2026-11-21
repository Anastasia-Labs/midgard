import { execFileSync } from "node:child_process";
import { mkdirSync } from "node:fs";
import { fileURLToPath } from "node:url";

import type { TestProject } from "vitest/node";

declare module "vitest" {
  export interface ProvidedContext {
    sidecarBinary: string;
    mockNodeBinary: string;
  }
}

const packageDirectory = fileURLToPath(new URL("..", import.meta.url));
const nativeDirectory = `${packageDirectory}native`;
const outputDirectory = `${packageDirectory}node_modules/.cache/l1-node-transport-tests`;

const goBuild = (target: string, output: string): void => {
  execFileSync(
    "go",
    [
      "-C",
      nativeDirectory,
      "build",
      "-buildvcs=false",
      "-trimpath",
      "-o",
      output,
      target,
    ],
    { stdio: ["ignore", "inherit", "inherit"], timeout: 300_000 },
  );
};

/**
 * Compiles the sidecar (or takes the guarded `contrib native` binary named by
 * MIDGARD_L1_NODE_TRANSPORT_BINARY) and the mock N2C node.
 */
export default function setup(project: TestProject): void {
  mkdirSync(outputDirectory, { recursive: true });
  const override = process.env.MIDGARD_L1_NODE_TRANSPORT_BINARY?.trim();
  const sidecarBinary =
    override === undefined || override === ""
      ? `${outputDirectory}/midgard-l1-node-transport`
      : override;
  if (sidecarBinary !== override) goBuild(".", sidecarBinary);
  const mockNodeBinary = `${outputDirectory}/l1-mock-node`;
  goBuild("./cmd/l1-mock-node", mockNodeBinary);
  project.provide("sidecarBinary", sidecarBinary);
  project.provide("mockNodeBinary", mockNodeBinary);
}
