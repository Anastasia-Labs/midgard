import { ledgerEd25519SignatureReady } from "@al-ft/midgard-core";
import { CML, Data } from "@lucid-evolution/lucid";
import { afterEach, beforeAll, describe, expect, it, vi } from "vitest";

import {
  WithdrawalBody,
  type WithdrawalSignature,
} from "../src/ledger-state.js";
import {
  signWithdrawalBodyCbor,
  verifyWithdrawalSignatureCbor,
} from "../src/withdrawal-signature.js";

/**
 * Once the body has read as Data, every failure in checking it is a fault of
 * this process: building the domain preimage, hashing it, or the ledger's
 * verifier itself. Each raises the verifier's local fault, with the original
 * error as its cause; none is reported as a signature that does not verify.
 * Bytes that do not read as Data are refused.
 */
const injected = vi.hoisted(() => ({
  verify: undefined as Error | undefined,
  hash: undefined as Error | undefined,
  verifyCalls: 0,
}));

vi.mock(
  "../../midgard-core/node_modules/libsodium-wrappers-sumo",
  async (importOriginal) => {
    const actual = (await importOriginal<{ default: object }>()).default;
    return {
      default: new Proxy(actual, {
        get: (target, key, receiver) => {
          const value = Reflect.get(target, key, receiver) as unknown;
          if (key !== "crypto_sign_verify_detached") return value;
          return (...args: unknown[]) => {
            injected.verifyCalls += 1;
            if (injected.verify !== undefined) throw injected.verify;
            return (value as (...a: unknown[]) => boolean)(...args);
          };
        },
      }),
    };
  },
);

vi.mock("@noble/hashes/blake2.js", async (importOriginal) => {
  const actual =
    await importOriginal<typeof import("@noble/hashes/blake2.js")>();
  return {
    ...actual,
    blake2b: (...args: Parameters<typeof actual.blake2b>) => {
      // Only the signed message's hash: the owner's key hash is 28 bytes.
      if (injected.hash !== undefined && args[1]?.dkLen === 32)
        throw injected.hash;
      return actual.blake2b(...args);
    },
  };
});

beforeAll(() => ledgerEd25519SignatureReady());

afterEach(() => {
  injected.verify = undefined;
  injected.hash = undefined;
  injected.verifyCalls = 0;
  vi.restoreAllMocks();
});

const key = CML.PrivateKey.from_normal_bytes(new Uint8Array(32).fill(7));
const publicKey = key.to_public().to_raw_bytes();
const owner = key.to_public().hash().to_hex();
const domain = Buffer.from("MidgardWithdrawalV1", "utf8");
const body = Data.to(
  {
    l2_outref: { transactionId: "aa".repeat(32), outputIndex: 0n },
    l2_owner: owner,
    l2_value: new Map([["", new Map([["", 20_000_000n]])]]),
    l1_address: {
      paymentCredential: { PublicKeyCredential: [owner] },
      stakeCredential: null,
    },
    l1_datum: { InlineDatum: { data: 0n } },
  },
  WithdrawalBody,
);
const honest = signWithdrawalBodyCbor(key, body);
const wrong: WithdrawalSignature = [
  Buffer.from(publicKey).toString("hex"),
  key.sign(new Uint8Array(32).fill(0xa5)).to_hex(),
];

/** What `verify` raised; fails the row if it returned instead. */
const raised = (verify: () => unknown): unknown => {
  try {
    verify();
  } catch (error) {
    return error;
  }
  throw new Error("the check returned instead of raising");
};

const expectLocalFault = (error: unknown, cause: Error) => {
  expect(error).toBeInstanceOf(Error);
  expect((error as Error).name).toBe("LedgerSignatureVerifierUnavailableError");
  expect((error as { cause?: unknown }).cause).toBe(cause);
  expect((error as Error).message).toContain(cause.message);
};

describe("withdrawal signature local faults", () => {
  it("accepts the honest body", () => {
    expect(verifyWithdrawalSignatureCbor(body, honest, owner)).toEqual({
      valid: true,
      publicKeyHash: owner,
    });
  });

  it("refuses truncated CBOR without reaching the verifier", () => {
    const result = verifyWithdrawalSignatureCbor("d8799f", honest, owner);
    expect(result.valid).toBe(false);
    expect(injected.verifyCalls).toBe(0);
  });

  it.each([
    { name: "honest", signature: honest, restored: true },
    { name: "invalid", signature: wrong, restored: false },
  ])(
    "raises a verifier fault on the $name signature as the local fault",
    ({ name, signature, restored }) => {
      const fault = new Error(`injected verifier fault: ${name}`);
      injected.verify = fault;
      expectLocalFault(
        raised(() => verifyWithdrawalSignatureCbor(body, signature, owner)),
        fault,
      );
      expect(injected.verifyCalls).toBe(1);
      injected.verify = undefined;
      expect(verifyWithdrawalSignatureCbor(body, signature, owner).valid).toBe(
        restored,
      );
    },
  );

  it("raises a domain-preimage construction fault as the local fault", () => {
    const fault = new Error("injected preimage construction fault");
    const concat = Buffer.concat;
    let hits = 0;
    vi.spyOn(Buffer, "concat").mockImplementation((list, totalLength) => {
      if (list.length === 2 && Buffer.from(list[0]!).equals(domain)) {
        hits += 1;
        throw fault;
      }
      return concat(list, totalLength);
    });
    expectLocalFault(
      raised(() => verifyWithdrawalSignatureCbor(body, honest, owner)),
      fault,
    );
    expect(hits).toBe(1);
    expect(injected.verifyCalls).toBe(0);
    vi.restoreAllMocks();
    expect(verifyWithdrawalSignatureCbor(body, honest, owner).valid).toBe(true);
  });

  it("raises a message-hash fault as the local fault", () => {
    const fault = new Error("injected message hash fault");
    injected.hash = fault;
    expectLocalFault(
      raised(() => verifyWithdrawalSignatureCbor(body, honest, owner)),
      fault,
    );
    expect(injected.verifyCalls).toBe(0);
    injected.hash = undefined;
    expect(verifyWithdrawalSignatureCbor(body, honest, owner).valid).toBe(true);
  });
});
