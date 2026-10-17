/**
 * The watcher's Phase A verifier under a fault of its Ed25519 verifier: the
 * evaluation ends `error`, with no rejection and no acceptance, so no verdict
 * is published on the block. With no fault, a wrong witness is still a
 * Phase A rejection.
 */
import { ledgerEd25519SignatureReady } from "@al-ft/midgard-core";
import { makeNativeTx } from "@al-ft/midgard-validation/tests/validation-fixtures";
import { RejectCodes } from "@al-ft/midgard-validation/types";
import { afterEach, beforeAll, describe, expect, it, vi } from "vitest";

import { evaluateWatcherPhaseAQueuedTxs } from "../../src/verification/phase-a-verifier.js";
import { CONFIG, KEY, queuedTx } from "./phase-a-verifier.base-header.js";

const injected = vi.hoisted(() => ({ fault: false }));

vi.mock(
  "../../../midgard-core/node_modules/libsodium-wrappers-sumo",
  async (importOriginal) => {
    const actual = (await importOriginal<{ default: object }>()).default;
    return {
      default: new Proxy(actual, {
        get: (target, key, receiver) =>
          key === "crypto_sign_verify_detached" && injected.fault
            ? () => {
                throw new Error("injected verifier fault in the watcher");
              }
            : (Reflect.get(target, key, receiver) as unknown),
      }),
    };
  },
);

beforeAll(() => ledgerEd25519SignatureReady);

afterEach(() => {
  injected.fault = false;
});

const evaluate = (invalidVkeyWitness: boolean) => {
  const tx = makeNativeTx({
    privateKey: KEY,
    ...(invalidVkeyWitness ? { invalidVkeyWitness: true as const } : {}),
  });
  return evaluateWatcherPhaseAQueuedTxs({
    queuedTxs: [queuedTx(tx.txId, tx.txCbor)],
    config: CONFIG,
  });
};

describe("watcher Phase A under a verifier fault", () => {
  it("ends in error with no verdict, for an honest and a wrong witness", () => {
    injected.fault = true;
    for (const invalidVkeyWitness of [false, true]) {
      const result = evaluate(invalidVkeyWitness);
      expect(result.action).toBe("error");
      expect(result.reasonCodes).toContain("canonical_validation_threw");
      expect(result.rejections).toEqual([]);
      expect(result.acceptedTxIds).toEqual([]);
    }
    injected.fault = false;
    expect(evaluate(false).action).toBe("accept");
  });

  it("still rejects a wrong witness with no fault", () => {
    const result = evaluate(true);
    expect(result.action).toBe("reject");
    expect(result.rejections).toMatchObject([
      { code: RejectCodes.InvalidSignature },
    ]);
  });
});
