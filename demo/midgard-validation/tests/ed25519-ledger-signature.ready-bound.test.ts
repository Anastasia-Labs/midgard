/**
 * The wait for the shared Ed25519 verifier is bounded where it is defined:
 * `ledgerEd25519SignatureReady` rejects with the verifier's local fault once
 * libsodium has not initialised within `LEDGER_ED25519_READY_TIMEOUT_MS`, so
 * no caller that awaits it can hang. The rejection is the promise's own:
 * every later await gets the same error at once, with no second wait, and the
 * verifier stays unavailable.
 */
import {
  LEDGER_ED25519_READY_TIMEOUT_MS,
  ledgerEd25519SignatureReady,
  verifyLedgerEd25519Signature,
} from "@al-ft/midgard-core";
import { describe, expect, it, vi } from "vitest";

import {
  honestSignature,
  signerPublicKey,
  signerSeed,
} from "./ed25519-ledger-vectors.js";

vi.hoisted(() => {
  // Hoisted above the imports: installed before the verifier module loads,
  // so its bound runs on them.
  vi.useFakeTimers();
});

vi.mock(
  "../../midgard-core/node_modules/libsodium-wrappers-sumo",
  async (importOriginal) => {
    const actual = (await importOriginal<{ default: object }>()).default;
    return {
      default: new Proxy(actual, {
        get: (target, key, receiver) =>
          key === "ready"
            ? new Promise<never>(() => undefined)
            : (Reflect.get(target, key, receiver) as unknown),
      }),
    };
  },
);

const PENDING = Symbol("pending");

/** The readiness promise's outcome so far: `PENDING`, or what it settled to. */
const observe = () => {
  const seen: { outcome: unknown } = { outcome: PENDING };
  void ledgerEd25519SignatureReady.then(
    () => {
      seen.outcome = "ready";
    },
    (error: unknown) => {
      seen.outcome = error;
    },
  );
  return seen;
};

describe("the wait for an Ed25519 verifier whose initialisation never settles", () => {
  it("rejects with the verifier's local fault at the bound, and not before", async () => {
    expect(LEDGER_ED25519_READY_TIMEOUT_MS).toBe(10_000);
    const first = observe();
    await vi.advanceTimersByTimeAsync(LEDGER_ED25519_READY_TIMEOUT_MS - 1);
    expect(first.outcome).toBe(PENDING);
    await vi.advanceTimersByTimeAsync(1);
    expect(first.outcome).toMatchObject({
      name: "LedgerSignatureVerifierUnavailableError",
      message: expect.stringContaining(
        "did not initialise within 10000 ms",
      ) as unknown,
    });

    // A later await: the same rejection at once, with no wait armed.
    expect(vi.getTimerCount()).toBe(0);
    const second = observe();
    await vi.advanceTimersByTimeAsync(0);
    expect(second.outcome).toBe(first.outcome);
    expect(vi.getTimerCount()).toBe(0);

    // The verifier still has no answer.
    const seed = signerSeed(0);
    const message = Buffer.alloc(32, 0x42);
    expect(() =>
      verifyLedgerEd25519Signature(
        signerPublicKey(seed),
        message,
        honestSignature(seed, message),
      ),
    ).toThrow(
      expect.objectContaining({
        name: "LedgerSignatureVerifierUnavailableError",
      }),
    );
  });
});
