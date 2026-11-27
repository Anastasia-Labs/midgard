/**
 * The fault-proofs CLI with a ledger Ed25519 verifier (libsodium) that never
 * initialises: its first wait runs out at the bound, before any command runs,
 * and the CLI exits 1 with one line naming the verifier error.
 */
import { LEDGER_ED25519_READY_TIMEOUT_MS } from "@al-ft/midgard-core";
import { afterEach, describe, expect, it, vi } from "vitest";

import { runFaultProofsCli } from "../src/bin.run-cli.js";

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

afterEach(() => {
  vi.useRealTimers();
});

describe("the fault-proofs CLI with a verifier that never initialises", () => {
  it("exits 1 at the bound with one line naming the verifier error", async () => {
    vi.useFakeTimers();
    const lines: string[] = [];
    const run = runFaultProofsCli((line) => lines.push(line));
    await vi.advanceTimersByTimeAsync(LEDGER_ED25519_READY_TIMEOUT_MS);
    expect(await run).toBe(1);
    expect(lines).toEqual([
      `midgard-fault-proofs: LedgerSignatureVerifierUnavailableError: Ed25519 verification is unavailable: libsodium did not initialise within ${LEDGER_ED25519_READY_TIMEOUT_MS.toString()} ms\n`,
    ]);
  });
});
