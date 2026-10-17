/**
 * A fault of this node's Ed25519 verifier while it imports a foreign block
 * (libsodium raising, after it initialised) is a local fault, never a
 * verdict on the block: the import ends `verifier_unavailable` with the
 * original error's text, the hold it makes is retried on the driver's
 * backoff, and the import succeeds once the fault clears, with no follower
 * change. The same holds when the fault reaches the import as a defect (a
 * check the import calls back raises it inside `Effect.sync`). A wrong
 * signature with no fault is still the block's fault.
 */
import {
  ledgerEd25519SignatureReady,
  verifyLedgerEd25519Signature,
} from "@al-ft/midgard-core";
import { Effect } from "effect";
import { afterEach, beforeAll, describe, expect, it, vi } from "vitest";

import {
  type DriverHold,
  isRetriedHold,
  isTransientFailureHold,
  localFaultSubject,
} from "../src/l1-events/driver.js";
import { combineHolds, replayOutcomeHold } from "../src/landed-blocks/holds.js";
import { Verdict } from "../src/landed-blocks/replay-foreign.verdict.js";
import { ForeignBlockVerificationError } from "../src/mpf/verified-block-import.js";
import { coalescedRunner } from "../src/services/l1-follower.coalesced-runner.js";
import { normalFixture } from "./foreign-block-import.normal-fixture.js";
import { withdrawalFixture } from "./foreign-block-import.withdrawal-fixture.js";

const injected = vi.hoisted(() => ({ fault: undefined as Error | undefined }));

vi.mock(
  "../../midgard-core/node_modules/libsodium-wrappers-sumo",
  async (importOriginal) => {
    const actual = (await importOriginal<{ default: object }>()).default;
    return {
      default: new Proxy(actual, {
        get: (target, key, receiver) =>
          key === "crypto_sign_verify_detached" && injected.fault !== undefined
            ? () => {
                throw injected.fault;
              }
            : (Reflect.get(target, key, receiver) as unknown),
      }),
    };
  },
);

beforeAll(() => ledgerEd25519SignatureReady);

afterEach(() => {
  injected.fault = undefined;
});

type Imported = Awaited<
  ReturnType<Awaited<ReturnType<typeof withdrawalFixture>>["imported"]>
>;

/** The replay outcome the foreign replayer makes of a failed import. */
const outcomeOf = (left: unknown) => {
  if (left instanceof ForeignBlockVerificationError)
    return { kind: left.reason, detail: left.detail };
  if (left instanceof Verdict) return { kind: left.kind, detail: left.detail };
  throw new Error(`the import failed outside its outcomes: ${String(left)}`);
};

/** The hold processing makes of `imported`, as `process.ts` records it. */
const holdOf = (imported: Imported, headerHash: string) => {
  if (imported._tag === "Right") return undefined;
  const outcome = outcomeOf(imported.left);
  return combineHolds([
    replayOutcomeHold(
      outcome.kind as Parameters<typeof replayOutcomeHold>[0],
      `block ${headerHash}: ${outcome.detail}`,
      headerHash,
    ),
  ]);
};

const expectLocalFaultHold = (
  imported: Imported,
  headerHash: string,
  fault: Error,
) => {
  expect(imported._tag).toBe("Left");
  if (imported._tag !== "Left") return;
  const outcome = outcomeOf(imported.left);
  expect(outcome.kind).toBe("verifier_unavailable");
  expect(outcome.detail).toContain(fault.message);
  expect(outcome.detail).toContain("LedgerSignatureVerifierUnavailableError");
  expect(outcome.detail).not.toContain("UnknownException");
  const hold = holdOf(imported, headerHash)!;
  expect(hold.reason).toBe("landed_block_verifier_unavailable");
  expect(hold.detail).toContain(fault.message);
  expect(isRetriedHold(hold)).toBe(true);
};

describe("a verifier fault during a foreign-block import", () => {
  it("holds a withdrawal block on a retried local fault, then imports it", async () => {
    const fixture = await withdrawalFixture();
    expect((await fixture.imported())._tag).toBe("Right");
    const fault = new Error("injected verifier fault on a withdrawal");
    injected.fault = fault;
    expectLocalFaultHold(await fixture.imported(), fixture.headerHash, fault);
    injected.fault = undefined;
    expect((await fixture.imported())._tag).toBe("Right");
  }, 120_000);

  it("holds a normal-transaction block on a retried local fault, then imports it", async () => {
    const fixture = await normalFixture();
    const headerHash = fixture.payload.block_body.header_hash;
    expect((await fixture.imported())._tag).toBe("Right");
    const fault = new Error("injected verifier fault on a transaction");
    injected.fault = fault;
    expectLocalFaultHold(await fixture.imported(), headerHash, fault);
    injected.fault = undefined;
    expect((await fixture.imported())._tag).toBe("Right");
  }, 120_000);

  it("holds a block on a retried local fault when the fault reaches the import as a defect", async () => {
    // The user event's replay first runs a signature check inside
    // `Effect.sync`: a fault there is a defect, not a typed failure.
    const fixture = await withdrawalFixture({
      userEvent: (replay) => (event) =>
        Effect.zipRight(
          Effect.sync(() =>
            verifyLedgerEd25519Signature(
              new Uint8Array(32),
              new Uint8Array(0),
              new Uint8Array(64),
            ),
          ),
          replay(event),
        ),
    });
    expect((await fixture.imported())._tag).toBe("Right");
    const fault = new Error("injected verifier fault inside a callback");
    injected.fault = fault;
    const imported = await fixture.imported();
    expectLocalFaultHold(imported, fixture.headerHash, fault);
    expect(localFaultSubject(holdOf(imported, fixture.headerHash)!)).toBe(
      fixture.headerHash,
    );
    injected.fault = undefined;
    expect((await fixture.imported())._tag).toBe("Right");
  }, 120_000);

  it("still holds a block whose withdrawal signature is wrong as invalid, not retried", async () => {
    const fixture = await withdrawalFixture({ wrongSignature: true });
    const imported = await fixture.imported();
    expect(imported._tag).toBe("Left");
    if (imported._tag !== "Left") return;
    expect(outcomeOf(imported.left).kind).toBe("invalid");
    const hold = holdOf(imported, fixture.headerHash)!;
    expect(hold.reason).toBe("landed_block_invalid");
    expect(isRetriedHold(hold)).toBe(false);
    expect(isTransientFailureHold(hold)).toBe(false);
  }, 120_000);

  it("imports the block on the runner's own retry once the fault clears", async () => {
    const fixture = await withdrawalFixture();
    const fault = new Error("injected verifier fault, cleared after one run");
    injected.fault = fault;
    const runs: (DriverHold | undefined)[] = [];
    const exhausted: (readonly DriverHold[])[] = [];
    const abort = new AbortController();
    let settle = () => {};
    const done = new Promise<void>((resolve) => {
      settle = resolve;
    });
    const trigger = coalescedRunner(
      async () => {
        const hold = holdOf(await fixture.imported(), fixture.headerHash);
        runs.push(hold);
        // The fault clears on its own; nothing triggers the runner again.
        injected.fault = undefined;
        if (hold === undefined) settle();
        return hold === undefined ? [] : [hold];
      },
      abort.signal,
      { onExhausted: (holds) => exhausted.push(holds) },
    );
    try {
      trigger();
      await Promise.race([
        done,
        new Promise((_, reject) =>
          setTimeout(() => reject(new Error("no retry imported it")), 60_000),
        ),
      ]);
    } finally {
      abort.abort();
    }
    expect(runs).toHaveLength(2);
    expect(runs[0]?.reason).toBe("landed_block_verifier_unavailable");
    expect(runs[0]?.detail).toContain(fault.message);
    expect(runs[1]).toBeUndefined();
    expect(exhausted).toEqual([]);
  }, 120_000);
});
