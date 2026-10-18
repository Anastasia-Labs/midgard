/**
 * The stress-wallet consolidation and terminal-drain commands with a ledger
 * Ed25519 verifier (libsodium) that never initialises: each waits for it up
 * to its bound, before touching any wallet file, then fails as the CLI fails
 * a command, exit code 1 with a line naming the verifier error.
 */
import { mkdtemp, readdir, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { LEDGER_ED25519_READY_TIMEOUT_MS } from "@al-ft/midgard-core";
import { Effect } from "effect";
import { failCli } from "midgard-node/commands/cli-runtime";
import { afterEach, describe, expect, it, vi } from "vitest";

import * as StressWalletsCommand from "../src/commands/stress-wallets/index.js";

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

const directories: string[] = [];
const exitCode = process.exitCode;

afterEach(async () => {
  vi.useRealTimers();
  process.exitCode = exitCode;
  await Promise.all(
    directories.splice(0).map((path) => rm(path, { recursive: true })),
  );
});

describe("stress-wallet commands with a verifier that never initialises", () => {
  it.each([
    [
      "stress-wallets:consolidate",
      StressWalletsCommand.consolidateStressWallets,
    ],
    [
      "stress-wallets:terminal-drain",
      StressWalletsCommand.terminalDrainStressWallets,
    ],
  ] as const)(
    "%s exits 1 at the bound with a line naming the verifier error",
    async (label, command) => {
      const outDir = await mkdtemp(join(tmpdir(), "midgard-stress-wallets-"));
      directories.push(outDir);
      const errors: string[] = [];
      vi.spyOn(console, "error").mockImplementation((line: unknown) => {
        errors.push(String(line));
      });
      vi.useFakeTimers();
      const run = Effect.runPromise(
        StressWalletsCommand.runWithSharedFanoutContext<unknown, never>(() =>
          command(
            { count: 1, treasurySeedPhrase: "", outDir } as never,
            {} as never,
          ),
        ),
      ).then(
        () => failCli(label, new Error("the command completed")),
        (error: unknown) => failCli(label, error),
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
