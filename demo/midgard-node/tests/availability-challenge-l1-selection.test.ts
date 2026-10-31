/**
 * The availability command under the access the CLI's own `--l1` option
 * selects: on the node ledger every action but `status` observes its
 * operations' inclusion, which the ledger cannot resolve, so the command
 * refuses it before reading the manifest or opening the access it would
 * build and submit through, naming `--l1 kupmios`. Under `--l1 kupmios` the
 * same action proceeds to that access, and `status` proceeds on node.
 */
import { generateSeedPhrase } from "@lucid-evolution/lucid";
import { afterEach, describe, expect, it, vi } from "vitest";

const seen = vi.hoisted(() => ({
  manifestReads: 0,
  opened: [] as string[],
}));

vi.mock("../src/runtime-env.js", () => ({ loadRuntimeDotenv: () => {} }));
vi.mock(
  "../src/commands/contract-deployment-info.js",
  async (importOriginal) => {
    const original =
      await importOriginal<
        typeof import("../src/commands/contract-deployment-info.js")
      >();
    const Lucid = await import("@lucid-evolution/lucid");
    return {
      ...original,
      readDeploymentManifestFile: () => {
        seen.manifestReads += 1;
        return {
          network: "Preview",
          referenceScriptDeployAddress: Lucid.walletFromSeed(
            Lucid.generateSeedPhrase(),
            { network: "Preview", addressType: "Enterprise" },
          ).address,
        };
      },
    };
  },
);
vi.mock(
  "@al-ft/midgard-core/deployment-manifest-identity",
  async (importOriginal) => ({
    ...(await importOriginal<
      typeof import("@al-ft/midgard-core/deployment-manifest-identity")
    >()),
    verifyFinalizedDeploymentManifest: () => undefined,
  }),
);
vi.mock("../src/commands/l1-command-access.js", async (importOriginal) => {
  const original =
    await importOriginal<
      typeof import("../src/commands/l1-command-access.js")
    >();
  return {
    ...original,
    // The access the command builds and submits through: recorded, never
    // opened, so reaching it is the proof the action was not refused.
    withCommandL1Access: async (input: { env?: NodeJS.ProcessEnv }) => {
      seen.opened.push(original.selectToolL1Access(input.env ?? process.env));
      return { reachedAccess: true };
    },
  };
});

import { program } from "../src/index.registration.js";

const run = async (access: string, action: string) => {
  seen.manifestReads = 0;
  seen.opened = [];
  const errors = vi.spyOn(console, "error").mockImplementation(() => {});
  const stdout = vi
    .spyOn(process.stdout, "write")
    .mockImplementation(() => true);
  await program.parseAsync([
    "node",
    "midgard-node",
    "--l1",
    access,
    "availability-challenge",
    action,
    "--manifest",
    "/unread-manifest.json",
    "--journal",
    "/tmp/availability.sqlite",
    "--header-hash",
    "11".repeat(28),
    "--wallet-seed-env",
    "AVAILABILITY_ACTOR_SEED",
  ]);
  return {
    errors: errors.mock.calls.map((call) => call.map(String).join(" ")),
    stdout: stdout.mock.calls.map((call) => String(call[0])).join(""),
  };
};

describe("availability under the CLI's --l1 selection", () => {
  afterEach(() => {
    vi.unstubAllEnvs();
    vi.restoreAllMocks();
    process.exitCode = undefined;
  });

  const seed = generateSeedPhrase();
  const stubbed = () => {
    vi.stubEnv("L1_ACCESS", "");
    vi.stubEnv("AVAILABILITY_ACTOR_SEED", seed);
  };

  for (const action of [
    "open",
    "respond",
    "settle",
    "close",
    "timeout",
    "recover",
  ]) {
    it(`refuses ${action} under --l1 node before reading or opening anything`, async () => {
      stubbed();
      const result = await run("node", action);
      expect(result.errors).toHaveLength(1);
      expect(result.errors[0]).toMatch(
        new RegExp(
          `^availability-challenge ${action}: availability under --l1 node cannot read the inclusion of .*: run it with --l1 kupmios`,
        ),
      );
      expect(process.exitCode).toBe(1);
      expect(seen.manifestReads).toBe(0);
      expect(seen.opened).toEqual([]);
    });
  }

  it("proceeds with open under --l1 kupmios to the access it submits through", async () => {
    stubbed();
    const result = await run("kupmios", "open");
    expect(result.errors).toEqual([]);
    expect(seen.manifestReads).toBe(1);
    expect(seen.opened).toEqual(["kupmios"]);
    expect(JSON.parse(result.stdout)).toEqual({ reachedAccess: true });
  });

  it("proceeds with status under --l1 node", async () => {
    stubbed();
    const result = await run("node", "status");
    expect(result.errors).toEqual([]);
    expect(seen.opened).toEqual(["node"]);
  });
});
