import {
  type MidgardLedgerVKeyWitness,
  RejectCodes,
  validatePhaseASingle,
} from "@al-ft/midgard-validation";
import { afterEach, describe, expect, it, vi } from "vitest";

import {
  makeNativeTx,
  makeQueued,
} from "../../midgard-validation/tests/validation-fixtures.js";
import { NodeEd25519Verifier } from "../src/workers/utils/ed25519-verifier.js";

/**
 * A failure inside the signature library is a fault of this node, not a
 * verdict on the witness: the verifier raises it, so Phase A raises it (the
 * worker reports the job as failed and the batch is retried), and no witness
 * is named invalid because of it.
 */
const injected = vi.hoisted(() => ({
  importFault: false,
  verifyFault: false,
}));

vi.mock("node:crypto", async (importOriginal) => {
  const actual = await importOriginal<typeof import("node:crypto")>();
  return {
    ...actual,
    createPublicKey: (...args: Parameters<typeof actual.createPublicKey>) => {
      if (injected.importFault) throw new Error("injected key import fault");
      return actual.createPublicKey(...args);
    },
    verify: (...args: Parameters<typeof actual.verify>) => {
      if (injected.verifyFault) throw new Error("injected verify fault");
      return actual.verify(...args);
    },
  };
});

const phaseAConfig = {
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  concurrency: 1,
  strictnessProfile: "phase1_midgard",
} as const;

const RFC_8032_EMPTY_MESSAGE_WITNESS: MidgardLedgerVKeyWitness = {
  index: 0,
  keyHash: Buffer.alloc(28),
  vkey: Buffer.from(
    "d75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a",
    "hex",
  ),
  signature: Buffer.from(
    "e5564300c360ac729086e2cc806e828a84877f1eb8e5d974d873e065224901555fb8821590a33bacc61e39701cf9b46bd25bf5f0595bbe24655141438e7a100b",
    "hex",
  ),
};

const validateWithNodeVerifier = (
  verifier: NodeEd25519Verifier,
  fixture: ReturnType<typeof makeNativeTx>,
) =>
  validatePhaseASingle(makeQueued(fixture.txId, fixture.txCbor), phaseAConfig, {
    verifyVKeyWitnessSignature: verifier.verify,
  });

afterEach(() => {
  injected.importFault = false;
  injected.verifyFault = false;
});

describe("Node Ed25519 verifier faults", () => {
  it("raises a verification fault on a valid witness instead of answering false", () => {
    const verifier = new NodeEd25519Verifier();
    expect(
      verifier.verify(Buffer.alloc(0), RFC_8032_EMPTY_MESSAGE_WITNESS),
    ).toBe(true);
    injected.verifyFault = true;
    expect(() =>
      verifier.verify(Buffer.alloc(0), RFC_8032_EMPTY_MESSAGE_WITNESS),
    ).toThrow("injected verify fault");
  });

  it("raises a key import fault on a valid witness instead of answering false", () => {
    const verifier = new NodeEd25519Verifier();
    injected.importFault = true;
    expect(() =>
      verifier.verify(Buffer.alloc(0), RFC_8032_EMPTY_MESSAGE_WITNESS),
    ).toThrow("injected key import fault");
    injected.importFault = false;
    expect(
      verifier.verify(Buffer.alloc(0), RFC_8032_EMPTY_MESSAGE_WITNESS),
    ).toBe(true);
  });

  it("makes Phase A raise the fault for a validly signed transaction, never InvalidSignature", () => {
    const verifier = new NodeEd25519Verifier();
    const fixture = makeNativeTx();
    injected.verifyFault = true;
    expect(() => validateWithNodeVerifier(verifier, fixture)).toThrow(
      "injected verify fault",
    );
    injected.verifyFault = false;
    expect("ledgerTx" in validateWithNodeVerifier(verifier, fixture)).toBe(
      true,
    );
  });

  it("answers false without raising for well-sized keys and signatures that do not verify", () => {
    const verifier = new NodeEd25519Verifier();
    const message = Buffer.alloc(0);
    const sEncoding = (s: bigint): Buffer =>
      Buffer.from(s.toString(16).padStart(64, "0"), "hex").reverse();
    const ED25519_P = 2n ** 255n - 19n;
    const ED25519_L = 2n ** 252n + 27742317777372353535851937790883648493n;
    const keys = [
      Buffer.from(`02${"00".repeat(31)}`, "hex"), // y = 2 is not on the curve
      sEncoding(ED25519_P), // y = p
      sEncoding(ED25519_P + 1n), // y = p + 1
      Buffer.alloc(32, 0x00),
      Buffer.alloc(32, 0xff),
    ];
    for (const vkey of keys) {
      expect(
        verifier.verify(message, { ...RFC_8032_EMPTY_MESSAGE_WITNESS, vkey }),
      ).toBe(false);
    }
    const signature = Buffer.from(RFC_8032_EMPTY_MESSAGE_WITNESS.signature);
    const s = BigInt(
      `0x${Buffer.from(signature.subarray(32)).reverse().toString("hex")}`,
    );
    sEncoding(s + ED25519_L).copy(signature, 32);
    expect(
      verifier.verify(message, {
        ...RFC_8032_EMPTY_MESSAGE_WITNESS,
        signature,
      }),
    ).toBe(false);
  });

  it("still rejects an invalidly signed transaction with InvalidSignature", () => {
    const result = validateWithNodeVerifier(
      new NodeEd25519Verifier(),
      makeNativeTx({ invalidVkeyWitness: true }),
    );
    expect("ledgerTx" in result).toBe(false);
    if (!("ledgerTx" in result)) {
      expect(result.code).toBe(RejectCodes.InvalidSignature);
    }
  });
});
