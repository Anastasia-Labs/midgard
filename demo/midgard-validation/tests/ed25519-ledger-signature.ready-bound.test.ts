/**
 * The wait for the shared Ed25519 verifier is bounded where it is defined:
 * `ledgerEd25519SignatureReady()` rejects with the verifier's local fault once
 * libsodium has not initialised within `LEDGER_ED25519_READY_TIMEOUT_MS` of the
 * first wait, so no caller that waits on it can hang. The bound starts at the
 * first wait, not when the module loads: time spent before it does not count.
 * The outcome is settled once: every later wait gets the same result at once,
 * with no second wait.
 *
 * Each row loads the verifier afresh, with libsodium's initialisation as the
 * row sets it, under a clock the row advances.
 */
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import {
  honestSignature,
  signerPublicKey,
  signerSeed,
} from "./ed25519-ledger-vectors.js";

/**
 * libsodium's initialisation, as the next load of the verifier sees it. Until
 * a row sets it, that is libsodium's own: the file's first load (through the
 * vectors' imports) hashes at module load, so it must not see libsodium as
 * initialised before it is.
 */
const sodiumInit = vi.hoisted(() => ({
  ready: undefined as Promise<void> | undefined,
  own: Promise.resolve() as Promise<void>,
  release: (): void => undefined,
}));

vi.mock(
  "../../midgard-core/node_modules/libsodium-wrappers-sumo",
  async (importOriginal) => {
    const actual = (
      await importOriginal<{ default: { ready: Promise<void> } }>()
    ).default;
    sodiumInit.own = actual.ready;
    return {
      default: new Proxy(actual, {
        get: (target, key, receiver) =>
          key === "ready"
            ? (sodiumInit.ready ?? sodiumInit.own)
            : (Reflect.get(target, key, receiver) as unknown),
      }),
    };
  },
);

/** A fresh load of the verifier, whose libsodium initialisation is
 * `released` (libsodium's own, delivered when the row releases it), `never`
 * settles, or `fails`. */
const loadVerifier = async (mode: "released" | "never" | "fails") => {
  if (mode === "never") {
    sodiumInit.ready = new Promise<never>(() => undefined);
  } else if (mode === "fails") {
    sodiumInit.ready = Promise.reject(
      new Error("injected initialisation failure"),
    );
    sodiumInit.ready.catch(() => undefined);
  } else {
    // libsodium's own initialisation is read once released, by when loading
    // the verifier has loaded the mock.
    sodiumInit.ready = new Promise<void>((resolve) => {
      sodiumInit.release = resolve;
    }).then(() => sodiumInit.own);
  }
  vi.resetModules();
  return import("@al-ft/midgard-core");
};

const PENDING = Symbol("pending");

/** A wait's outcome so far: `PENDING`, `"ready"`, or what it rejected with. */
const observe = (wait: Promise<void>) => {
  const seen: { outcome: unknown } = { outcome: PENDING };
  void wait.then(
    () => {
      seen.outcome = "ready";
    },
    (error: unknown) => {
      seen.outcome = error;
    },
  );
  return seen;
};

const seed = signerSeed(0);
const message = Buffer.alloc(32, 0x42);
const verifyHonest = (
  verify: (key: Uint8Array, msg: Uint8Array, sig: Uint8Array) => boolean,
) => verify(signerPublicKey(seed), message, honestSignature(seed, message));

const UNAVAILABLE = expect.objectContaining({
  name: "LedgerSignatureVerifierUnavailableError",
}) as unknown;

/** Longer than the bound: time a process spends before its first wait. */
const BEFORE_FIRST_WAIT_MS = 25_000;

beforeEach(() => {
  vi.useFakeTimers({ toFake: ["setTimeout", "clearTimeout", "Date"] });
});

afterEach(() => {
  vi.useRealTimers();
});

describe("the wait for the Ed25519 verifier", () => {
  it("reaches ready after synchronous work longer than the bound before the first wait", async () => {
    const core = await loadVerifier("released");
    expect(core.LEDGER_ED25519_READY_TIMEOUT_MS).toBe(10_000);
    // Synchronous work before the first wait: the clock moves past the bound
    // and whatever timers were due run when it ends.
    vi.advanceTimersByTime(BEFORE_FIRST_WAIT_MS);
    sodiumInit.release();

    await expect(core.ledgerEd25519SignatureReady()).resolves.toBeUndefined();
    expect(vi.getTimerCount()).toBe(0);
    expect(verifyHonest(core.verifyLedgerEd25519Signature)).toBe(true);
  });

  it("rejects an initialisation that never settles at the bound from the first wait, and not before", async () => {
    const core = await loadVerifier("never");
    vi.advanceTimersByTime(BEFORE_FIRST_WAIT_MS);
    expect(vi.getTimerCount()).toBe(0);

    const first = observe(core.ledgerEd25519SignatureReady());
    await vi.advanceTimersByTimeAsync(core.LEDGER_ED25519_READY_TIMEOUT_MS - 1);
    expect(first.outcome).toBe(PENDING);
    await vi.advanceTimersByTimeAsync(1);
    expect(first.outcome).toMatchObject({
      name: "LedgerSignatureVerifierUnavailableError",
      message: expect.stringContaining(
        "did not initialise within 10000 ms",
      ) as unknown,
    });

    // A later wait: the same rejection at once, with no wait armed.
    expect(vi.getTimerCount()).toBe(0);
    const second = observe(core.ledgerEd25519SignatureReady());
    await vi.advanceTimersByTimeAsync(0);
    expect(second.outcome).toBe(first.outcome);
    expect(vi.getTimerCount()).toBe(0);
    expect(() => verifyHonest(core.verifyLedgerEd25519Signature)).toThrow(
      UNAVAILABLE,
    );
  });

  it("stays unavailable when libsodium initialises only after the bound", async () => {
    const core = await loadVerifier("released");
    const first = observe(core.ledgerEd25519SignatureReady());
    await vi.advanceTimersByTimeAsync(core.LEDGER_ED25519_READY_TIMEOUT_MS);
    expect(first.outcome).toEqual(UNAVAILABLE);

    sodiumInit.release();
    await sodiumInit.ready;
    const second = observe(core.ledgerEd25519SignatureReady());
    await vi.advanceTimersByTimeAsync(0);
    expect(second.outcome).toBe(first.outcome);
    expect(() => verifyHonest(core.verifyLedgerEd25519Signature)).toThrow(
      UNAVAILABLE,
    );
  });

  it("rejects a failed initialisation at once, and every later wait the same", async () => {
    const core = await loadVerifier("fails");
    const first = observe(core.ledgerEd25519SignatureReady());
    await vi.advanceTimersByTimeAsync(0);
    expect(first.outcome).toMatchObject({
      name: "LedgerSignatureVerifierUnavailableError",
      message: expect.stringContaining(
        "injected initialisation failure",
      ) as unknown,
    });
    expect(vi.getTimerCount()).toBe(0);

    const second = observe(core.ledgerEd25519SignatureReady());
    await vi.advanceTimersByTimeAsync(0);
    expect(second.outcome).toBe(first.outcome);
    expect(vi.getTimerCount()).toBe(0);
    expect(() => verifyHonest(core.verifyLedgerEd25519Signature)).toThrow(
      UNAVAILABLE,
    );
  });
});
