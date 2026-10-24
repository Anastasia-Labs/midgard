/**
 * The stress-wallet consolidation and terminal-drain commands with a ledger
 * Ed25519 verifier (libsodium) that never initialises: each waits for it up
 * to its bound, before touching any wallet file, then fails through the
 * CLI's own command dispatch: exit code 1 with a line naming the verifier
 * error. The service layers are empty stand-ins (a NodeConfig with fees for
 * the drain), since no service is reached before the verifier.
 */
import "../src/index.registration-4.js";
import "../src/index.registration-5.js";

import { mkdtemp, readdir, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { LEDGER_ED25519_READY_TIMEOUT_MS } from "@al-ft/midgard-core";
import { afterEach, describe, expect, it, vi } from "vitest";

import { program } from "../src/index.registration.js";

vi.mock(
  "../../midgard-core/node_modules/libsodium-wrappers-sumo",
  async (importOriginal) => {
    const actual = (
      await importOriginal<{ default: { ready: Promise<void> } }>()
    ).default;
    const ready = new Promise<void>(() => undefined);
    return {
      default: new Proxy(actual, {
        get: (target, key, receiver) =>
          key === "ready"
            ? ready
            : (Reflect.get(target, key, receiver) as unknown),
      }),
    };
  },
);

vi.mock("midgard-node/commands/cli-runtime", async (importOriginal) => {
  const { Layer } = await import("effect");
  return {
    ...(await importOriginal<object>()),
    ToolLucidLive: Layer.empty,
    ToolMidgardContractServices: Layer.empty,
  };
});

vi.mock("midgard-node/services/index", async (importOriginal) => {
  const { Layer } = await import("effect");
  const actual = await importOriginal<{
    NodeConfig: object;
    Database: object;
  }>();
  Object.defineProperty(actual.NodeConfig, "layer", {
    value: Layer.succeed(
      actual.NodeConfig as never,
      {
        MIN_FEE_A: 44,
        MIN_FEE_B: 155381,
      } as never,
    ),
  });
  Object.defineProperty(actual.Database, "layer", { value: Layer.empty });
  return { ...actual, WriteBehindLive: Layer.empty };
});

const SEED_ENV = "MIDGARD_TEST_VERIFIER_TREASURY_SEED";
const directories: string[] = [];
const exitCode = process.exitCode;

afterEach(async () => {
  vi.useRealTimers();
  delete process.env[SEED_ENV];
  process.exitCode = exitCode;
  await Promise.all(
    directories.splice(0).map((path) => rm(path, { recursive: true })),
  );
});

describe("stress-wallet commands with a verifier that never initialises", () => {
  it.each(["stress-wallets:consolidate", "stress-wallets:terminal-drain"])(
    "%s exits 1 at the bound with a line naming the verifier error",
    async (label) => {
      const outDir = await mkdtemp(join(tmpdir(), "midgard-stress-wallets-"));
      directories.push(outDir);
      process.env[SEED_ENV] = Array(24).fill("abandon").join(" ");
      const errors: string[] = [];
      vi.spyOn(console, "error").mockImplementation((line: unknown) => {
        errors.push(String(line));
      });
      vi.useFakeTimers();
      const run = program.parseAsync(
        [
          label,
          "--count",
          "1",
          "--out-dir",
          outDir,
          "--treasury-wallet-seed-phrase-env",
          SEED_ENV,
          "--network",
          "Preprod",
        ],
        { from: "user" },
      );
      await vi.advanceTimersByTimeAsync(LEDGER_ED25519_READY_TIMEOUT_MS);
      await run;
      expect(process.exitCode).toBe(1);
      expect(errors).toHaveLength(1);
      expect(errors[0]).toContain(
        `${label}: Ed25519 verification is unavailable: libsodium did not initialise within ${LEDGER_ED25519_READY_TIMEOUT_MS.toString()} ms`,
      );
      expect(await readdir(outDir)).toEqual([]);
    },
  );
});
