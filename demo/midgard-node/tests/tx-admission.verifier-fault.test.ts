/**
 * L2 transaction admission under a fault of this node's Ed25519 verifier:
 * the Phase A batch fails with the verifier's local fault (its original
 * text kept), the claimed admissions are released for a retry, and nothing
 * is rejected as an invalid signature. With no fault, a wrong witness is
 * still rejected as one.
 */
import {
  ledgerEd25519SignatureReady,
  MIDGARD_CONSENSUS_PROFILE,
} from "@al-ft/midgard-core";
import { RejectCodes } from "@al-ft/midgard-validation";
import {
  makeNativeTx,
  makeQueued,
} from "@al-ft/midgard-validation/tests/validation-fixtures";
import { Cause, Effect, Exit } from "effect";
import { afterEach, beforeAll, describe, expect, it, vi } from "vitest";

import {
  runPhaseAForBatch,
  withAdmissionLeaseRecovery,
} from "../src/fibers/tx-queue-processor.run-phase-afor-batch.js";
import type {
  NodeConfigDep,
  ValidationPoolService,
} from "../src/services/index.js";

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

beforeAll(() => ledgerEd25519SignatureReady());

afterEach(() => {
  injected.fault = undefined;
});

const config = {
  NETWORK: "Preprod",
  MIN_FEE_A: 0n,
  MIN_FEE_B: 0n,
  VALIDATION_PHASE_A_CONCURRENCY: 1,
  VALIDATION_STRICTNESS_PROFILE: "phase1_midgard",
  VALIDATION_WORKER_INLINE_THRESHOLD: 1_000,
  VALIDATION_WORKER_CHUNK_SIZE: 1_000,
} as unknown as NodeConfigDep;

const pool = {
  poolSize: 0,
  consensusProfile: MIDGARD_CONSENSUS_PROFILE,
  runPhaseAChunk: () => Effect.die("the inline path never reaches a worker"),
} as unknown as ValidationPoolService;

const queued = (invalidVkeyWitness: boolean) => {
  const tx = makeNativeTx(
    invalidVkeyWitness ? { invalidVkeyWitness: true as const } : {},
  );
  return [makeQueued(tx.txId, tx.txCbor)];
};

describe("L2 admission under a verifier fault", () => {
  it("fails the batch with the local fault and releases it for a retry", async () => {
    const fault = new Error("injected verifier fault during admission");
    injected.fault = fault;
    let released = 0;
    const exit = await Effect.runPromiseExit(
      withAdmissionLeaseRecovery(
        runPhaseAForBatch(queued(false), config, pool),
        Effect.sync(() => {
          released += 1;
        }),
      ),
    );
    expect(Exit.isFailure(exit)).toBe(true);
    if (!Exit.isFailure(exit)) return;
    const raised = [
      ...Cause.failures(exit.cause),
      ...Cause.defects(exit.cause),
    ] as Error[];
    expect(raised).toHaveLength(1);
    expect(raised[0]!.name).toBe("LedgerSignatureVerifierUnavailableError");
    expect(raised[0]!.cause).toBe(fault);
    expect(raised[0]!.message).toContain(fault.message);
    expect(released).toBe(1);

    injected.fault = undefined;
    const retried = await Effect.runPromise(
      runPhaseAForBatch(queued(false), config, pool),
    );
    expect(retried.accepted).toHaveLength(1);
    expect(retried.rejected).toEqual([]);
  });

  it("still rejects a wrong witness as an invalid signature", async () => {
    const result = await Effect.runPromise(
      runPhaseAForBatch(queued(true), config, pool),
    );
    expect(result.accepted).toEqual([]);
    expect(result.rejected).toMatchObject([
      { code: RejectCodes.InvalidSignature },
    ]);
  });
});
