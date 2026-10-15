import {
  ledgerEd25519SignatureReady,
  verifyLedgerEd25519Signature,
} from "@al-ft/midgard-core";
import { verifyAddressWitness } from "@al-ft/midgard-sdk";
import { afterEach, beforeAll, describe, expect, it, vi } from "vitest";

import { RejectCodes } from "../src/index.js";
import {
  phaseAVerdict as phaseA,
  signatureRejectionTrace as rejectedTrace,
  transactionWithWitnesses,
} from "./ed25519-ledger-transactions.js";
import {
  honestSignature,
  ownKeyIdentityRVkeyWitness,
  signerPublicKey,
  signerSeed,
  TEST_SIGNER_SEED,
} from "./ed25519-ledger-vectors.js";
import { makeNativeTx } from "./validation-fixtures.js";

/**
 * A failure inside libsodium is a fault of this process, not a verdict on a
 * witness: the shared verifier raises it, so Phase A and the trace builder
 * raise it, and no witness is named invalid because of it.
 */
const injected = vi.hoisted(() => ({ verifyFault: false }));

vi.mock(
  "../../midgard-core/node_modules/libsodium-wrappers-sumo",
  async (importOriginal) => {
    const actual = (await importOriginal<{ default: object }>()).default;
    return {
      default: new Proxy(actual, {
        get: (target, key, receiver) =>
          key === "crypto_sign_verify_detached" && injected.verifyFault
            ? () => {
                throw new Error("injected libsodium fault");
              }
            : (Reflect.get(target, key, receiver) as unknown),
      }),
    };
  },
);

const message = Buffer.alloc(32, 0x42);
const seed = signerSeed(0);

const identityRTransaction = () =>
  transactionWithWitnesses([ownKeyIdentityRVkeyWitness(TEST_SIGNER_SEED)]);

beforeAll(() => ledgerEd25519SignatureReady);

afterEach(() => {
  injected.verifyFault = false;
});

describe("Ed25519 verification faults", { timeout: 120_000 }, () => {
  it("raises a libsodium fault on a valid signature instead of answering false", () => {
    const vkey = signerPublicKey(seed);
    const signature = honestSignature(seed, message);
    expect(verifyLedgerEd25519Signature(vkey, message, signature)).toBe(true);
    injected.verifyFault = true;
    expect(() =>
      verifyLedgerEd25519Signature(vkey, message, signature),
    ).toThrow("injected libsodium fault");
    expect(() =>
      verifyAddressWitness({
        txId: message.toString("hex"),
        witness: {
          verification_key: vkey.toString("hex"),
          signature: signature.toString("hex"),
        },
      }),
    ).toThrow("injected libsodium fault");
  });

  it("makes Phase A raise the fault for a validly signed transaction, never InvalidSignature", () => {
    const transaction = makeNativeTx();
    injected.verifyFault = true;
    expect(() => phaseA(transaction)).toThrow("injected libsodium fault");
    injected.verifyFault = false;
    expect("ledgerTx" in phaseA(transaction)).toBe(true);
  });

  it("makes Phase A raise the fault for a witness the ledger rejects too", () => {
    const transaction = identityRTransaction();
    injected.verifyFault = true;
    expect(() => phaseA(transaction)).toThrow("injected libsodium fault");
    injected.verifyFault = false;
    expect(phaseA(transaction)).toMatchObject({
      code: RejectCodes.InvalidSignature,
    });
  });

  it("stops the trace build on the fault instead of building a rejection", async () => {
    const transaction = identityRTransaction();
    injected.verifyFault = true;
    await expect(rejectedTrace(transaction)).rejects.toThrow(
      "injected libsodium fault",
    );
    injected.verifyFault = false;
    const trace = await rejectedTrace(transaction);
    expect(trace.tree.descriptor.verdict).toBe("rejected");
  });
});
