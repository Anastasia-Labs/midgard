/**
 * The availability command under the access the CLI's own `--l1` option
 * selects: it reads chain history, which only Kupo and Ogmios serve, so
 * under `--l1 node` or `--l1 blockfrost` every action, `status` included,
 * is refused before the manifest is read, naming `--l1 kupmios`. Under
 * `--l1 kupmios` the same action goes on to read the manifest (here a
 * missing file, so it stops there).
 */
import { generateSeedPhrase } from "@lucid-evolution/lucid";
import { afterEach, describe, expect, it, vi } from "vitest";

import { program } from "../src/index.registration.js";

const MANIFEST = "/availability-l1-selection/missing-manifest.json";

const run = async (access: string, action: string) => {
  const errors = vi.spyOn(console, "error").mockImplementation(() => {});
  await program.parseAsync([
    "node",
    "midgard-node",
    "--l1",
    access,
    "availability-challenge",
    action,
    "--manifest",
    MANIFEST,
    "--journal",
    "/tmp/availability.sqlite",
    "--header-hash",
    "11".repeat(28),
    "--wallet-seed-env",
    "AVAILABILITY_ACTOR_SEED",
  ]);
  return errors.mock.calls.map((call) => call.map(String).join(" "));
};

describe("availability under the CLI's --l1 selection", () => {
  afterEach(() => {
    vi.unstubAllEnvs();
    vi.restoreAllMocks();
    process.exitCode = undefined;
  });

  const seed = generateSeedPhrase();

  for (const access of ["node", "blockfrost"])
    for (const action of ["status", "open", "recover"])
      it(`refuses ${action} under --l1 ${access} before reading the manifest`, async () => {
        vi.stubEnv("AVAILABILITY_ACTOR_SEED", seed);
        const errors = await run(access, action);
        expect(errors).toHaveLength(1);
        expect(errors[0]).toMatch(
          new RegExp(
            `^availability-challenge ${action}: availability under --l1 ${access} cannot read .*: run it with --l1 kupmios`,
          ),
        );
        expect(errors[0]).not.toContain(MANIFEST);
        expect(process.exitCode).toBe(1);
      });

  it("goes on to read the manifest under --l1 kupmios", async () => {
    vi.stubEnv("AVAILABILITY_ACTOR_SEED", seed);
    const errors = await run("kupmios", "open");
    expect(errors).toHaveLength(1);
    expect(errors[0]).toContain(MANIFEST);
    expect(errors[0]).not.toMatch(/--l1 kupmios/);
  });
});
