import {
  ledgerEd25519SignatureReady,
  MIDGARD_CONSENSUS_PROFILE,
  verifyLedgerEd25519Signature,
} from "@al-ft/midgard-core";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import {
  classifyWithdrawalFromLedger,
  signWithdrawalBodyCbor,
  WithdrawalBody,
  WithdrawalInfo,
} from "@al-ft/midgard-sdk";
import { CML, Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it, vi } from "vitest";

import {
  replayValidationVerdictEvent,
  runPhaseAValidation,
} from "../src/index.js";
import { prepareValidationTrace } from "../src/validation-machine/trace-builder-prepare.js";
import {
  honestSignature,
  signerPublicKey,
  signerSeed,
} from "./ed25519-ledger-vectors.js";
import { makeNativeTx } from "./validation-fixtures.js";

/**
 * libsodium failing to initialise is a fault of this process: the readiness
 * promise rejects with the verifier's local fault, keeping the original
 * error, and every asynchronous entry point that waits on it (event replay,
 * Phase A, trace preparation, withdrawal classification) fails with that
 * typed error rather than a defect or a verdict.
 */
vi.mock(
  "../../midgard-core/node_modules/libsodium-wrappers-sumo",
  async (importOriginal) => {
    const actual = (await importOriginal<{ default: object }>()).default;
    const failed = Promise.reject(new Error("injected initialisation failure"));
    failed.catch(() => undefined);
    return {
      default: new Proxy(actual, {
        get: (target, key, receiver) =>
          key === "ready"
            ? failed
            : (Reflect.get(target, key, receiver) as unknown),
      }),
    };
  },
);

const expectLocalFault = (error: unknown) => {
  expect((error as Error).name).toBe("LedgerSignatureVerifierUnavailableError");
  expect((error as Error).message).toContain("injected initialisation failure");
  expect(((error as Error).cause as Error).message).toBe(
    "injected initialisation failure",
  );
};

const leftOf = async <A, E>(effect: Effect.Effect<A, E>) => {
  const result = await Effect.runPromise(Effect.either(effect));
  if (result._tag !== "Left") throw new Error("the entry point answered");
  return result.left;
};

describe("Ed25519 verification when libsodium fails to initialise", () => {
  it("rejects readiness with the local fault, and the verifier raises it", async () => {
    expectLocalFault(
      await ledgerEd25519SignatureReady.then(
        () => undefined,
        (error: unknown) => error,
      ),
    );
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

  it("fails event replay with the local fault", async () => {
    const tx = makeNativeTx();
    expectLocalFault(
      await leftOf(
        replayValidationVerdictEvent({
          consensusProfile: MIDGARD_CONSENSUS_PROFILE,
          canonicalTransactionCbor: tx.txCbor,
          ledgerWitnessEntries: [],
          blockTimeMs: 2,
          expectedNetworkId: 0n,
          minFeeA: 0n,
          minFeeB: 0n,
          blockSlot: 0n,
          l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
          sourceKind: "normal",
        }),
      ),
    );
  });

  it("fails Phase A with the local fault", async () => {
    expectLocalFault(
      await leftOf(
        runPhaseAValidation([], {
          expectedNetworkId: 0n,
          minFeeA: 0n,
          minFeeB: 0n,
          concurrency: 1,
          strictnessProfile: "phase1_midgard",
          consensusProfile: MIDGARD_CONSENSUS_PROFILE,
        }),
      ),
    );
  });

  it("fails trace preparation with the local fault", async () => {
    // The wait comes first: nothing of the input is read before it.
    expectLocalFault(
      await leftOf(
        prepareValidationTrace(
          {} as Parameters<typeof prepareValidationTrace>[0],
        ),
      ),
    );
  });

  it("fails withdrawal classification with the local fault", async () => {
    const key = CML.PrivateKey.from_normal_bytes(signerSeed(0));
    const owner = key.to_public().hash().to_hex();
    const body = {
      l2_outref: { transactionId: "aa".repeat(32), outputIndex: 0n },
      l2_owner: owner,
      l2_value: new Map([["", new Map([["", 20_000_000n]])]]),
      l1_address: {
        paymentCredential: { PublicKeyCredential: [owner] },
        stakeCredential: null,
      },
      l1_datum: "NoDatum",
    } satisfies WithdrawalBody;
    const signature = signWithdrawalBodyCbor(
      key,
      Data.to(body, WithdrawalBody),
    );
    expectLocalFault(
      await leftOf(
        classifyWithdrawalFromLedger({
          l2Owner: owner,
          // No ledger output: classification never reads the value.
          l2ValueCbor: "a0",
          eventInfoCbor: Data.to(
            { body, signature, validity: "WithdrawalIsValid" },
            WithdrawalInfo,
          ),
          ledgerOutRef: Buffer.alloc(36),
          ledgerOutput: null,
        }),
      ),
    );
  });
});
