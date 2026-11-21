/**
 * The watcher with a ledger Ed25519 verifier (libsodium) that never
 * initialises: its command's first wait runs out at the bound, before the
 * configuration is read, and the command fails with the verifier-unavailable
 * error under the restartable exit code.
 */
import { join } from "node:path";

import {
  findLedgerSignatureVerifierUnavailable,
  LEDGER_ED25519_READY_TIMEOUT_MS,
} from "@al-ft/midgard-core";
import { afterEach, describe, expect, it, vi } from "vitest";

import {
  WATCHER_COMMAND_FAILURE_EXIT_CODE,
  watcherFailureExitCode,
} from "../../src/cli.js";
import { isWatcherPermanentRefusal } from "../../src/runtime/permanent-refusal.js";
import { runWatcherCommand } from "../../src/runtime/scaffold.js";

vi.mock(
  "../../../midgard-core/node_modules/libsodium-wrappers-sumo",
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

afterEach(() => {
  vi.useRealTimers();
});

describe("a watcher command with a verifier that never initialises", () => {
  it("fails at the bound with the verifier-unavailable error, restartable, before reading its configuration", async () => {
    vi.useFakeTimers();
    const written: string[] = [];
    const run = runWatcherCommand(
      "start",
      join("/var/tmp", "midgard-watcher-absent", "process.json"),
      {
        writeOutput: (text) => written.push(text),
        writeError: (text) => written.push(text),
      },
    ).then(
      (code) => ({ code }),
      (error: unknown) => error,
    );
    await vi.advanceTimersByTimeAsync(LEDGER_ED25519_READY_TIMEOUT_MS);
    const failure = await run;
    expect(findLedgerSignatureVerifierUnavailable(failure)).toBe(failure);
    expect(isWatcherPermanentRefusal(failure)).toBe(false);
    expect(watcherFailureExitCode(failure)).toBe(
      WATCHER_COMMAND_FAILURE_EXIT_CODE,
    );
    expect(written).toEqual([]);
  });
});
