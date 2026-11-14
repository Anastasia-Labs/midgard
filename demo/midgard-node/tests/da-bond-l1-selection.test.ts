/**
 * `da-bond` under the CLI's `--l1` selection: the pool, its params and the
 * payer are not reads the node's ledger serves, so `--l1 node` is refused
 * before the manifest is read, naming `--l1 kupmios|blockfrost`. Under
 * `--l1 kupmios` the command goes on to read the manifest (here a missing
 * file, so it stops there).
 */
import { afterEach, describe, expect, it, vi } from "vitest";

import { program } from "../src/index.registration.js";

const MANIFEST = "/da-bond-l1-selection/missing-manifest.json";

const status = async (access: string) => {
  const errors = vi.spyOn(console, "error").mockImplementation(() => {});
  await program.parseAsync([
    "node",
    "midgard-node",
    "--l1",
    access,
    "da-bond",
    "status",
    "--manifest",
    MANIFEST,
  ]);
  return errors.mock.calls.map((call) => call.map(String).join(" "));
};

describe("da-bond under the CLI's --l1 selection", () => {
  afterEach(() => {
    vi.restoreAllMocks();
    process.exitCode = undefined;
  });

  it("refuses --l1 node before reading the manifest", async () => {
    const errors = await status("node");
    expect(errors).toHaveLength(1);
    expect(errors[0]).toMatch(
      /^da-bond status: .*pass --l1 kupmios\|blockfrost/,
    );
    expect(errors[0]).not.toContain(MANIFEST);
    expect(process.exitCode).toBe(1);
  });

  it("goes on to read the manifest under --l1 kupmios", async () => {
    const errors = await status("kupmios");
    expect(errors).toHaveLength(1);
    expect(errors[0]).toContain(MANIFEST);
  });
});
