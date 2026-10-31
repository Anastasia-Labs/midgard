import { LedgerSignatureVerifierUnavailableError } from "@al-ft/midgard-core";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import { CML } from "@lucid-evolution/lucid";
import { Effect, Either } from "effect";
import { afterEach, describe, expect, it, vi } from "vitest";

import {
  commitValidationTraceV1,
  RejectCodes,
  ValidationTraceStopped,
} from "../src/index.js";
import {
  FUNDED_OUTPUT_LOVELACE,
  makeNativeTx,
  makeOutput,
  outRefFromByte,
} from "./validation-fixtures.js";
import { context } from "./validation-machine.validate-boundary-abi-and-collect-auxiliary-kinds.js";

/**
 * The check faults for `faultKey` once it has answered `answeredChecks` of
 * that key's checks.
 */
const injected = vi.hoisted(() => ({
  faultKey: null as Buffer | null,
  answeredChecks: 0,
  fault: null as unknown,
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
          const verify = value as (...args: Uint8Array[]) => boolean;
          return (
            signature: Uint8Array,
            message: Uint8Array,
            publicKey: Uint8Array,
          ): boolean => {
            if (injected.faultKey?.equals(publicKey) === true) {
              if (injected.answeredChecks === 0) throw injected.fault;
              injected.answeredChecks -= 1;
            }
            return verify(signature, message, publicKey);
          };
        },
      }),
    };
  },
);

/**
 * E8 fault injection: the trace builder checks each address-witness
 * signature itself. When that check fails (injected here for one key), the
 * build has no verdict: it stops as unavailable with the fault as its cause,
 * never reading the witness's signature as invalid.
 */
const keyHash = (key: CML.PrivateKey): Buffer =>
  Buffer.from(key.to_public().hash().to_raw_bytes());

// The valid witness sorts first by signer hash, so it comes first in the
// canonical witness list and the builder checks it before the invalid one.
const invalidSigner = CML.PrivateKey.generate_ed25519();
let validSigner = CML.PrivateKey.generate_ed25519();
while (Buffer.compare(keyHash(validSigner), keyHash(invalidSigner)) >= 0)
  validSigner = CML.PrivateKey.generate_ed25519();
const validVerificationKey = Buffer.from(
  validSigner.to_public().to_raw_bytes(),
);

const spent = outRefFromByte(0x11);
const output = makeOutput(FUNDED_OUTPUT_LOVELACE);
const vkeyWitness = (key: CML.PrivateKey, message: Buffer): Buffer =>
  Buffer.from(
    CML.make_vkey_witness(
      CML.TransactionHash.from_raw_bytes(message),
      key,
    ).to_cbor_bytes(),
  );
const transaction = makeNativeTx({
  version: 1n,
  vkeyWitnesses: (message) => [
    vkeyWitness(validSigner, message),
    vkeyWitness(invalidSigner, Buffer.alloc(32, 0x7f)),
  ],
  spendInputs: [spent],
  outputs: [output],
});

const commitClaimedInvalidSignature = () =>
  Effect.runPromise(
    Effect.either(
      commitValidationTraceV1({
        l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
        ...context,
        transactionId: transaction.txId,
        canonicalTransactionCbor: transaction.txCbor,
        priorUtxosRoot: "03".repeat(32),
        postUtxosRoot: "03".repeat(32),
        ledgerWitnessEntries: [{ outRef: spent, output }],
        expectedLedgerOps: [],
        expectedVerdict: "rejected",
        expectedRejectionCode: RejectCodes.InvalidSignature,
      }),
    ),
  );

describe("trace builder signature check fault", { timeout: 60_000 }, () => {
  afterEach(() => {
    injected.faultKey = null;
    injected.answeredChecks = 0;
  });

  it("builds the claimed rejection while the check works", async () => {
    const result = await commitClaimedInvalidSignature();
    expect(Either.isRight(result)).toBe(true);
    if (Either.isRight(result))
      expect(result.right.descriptor.verdict).toBe("rejected");
  });

  it("stops as unavailable, never building a rejection, when the check fails", async () => {
    const fault = new WebAssembly.RuntimeError("unreachable");
    injected.fault = fault;
    injected.faultKey = validVerificationKey;
    // The build's Phase A replay checks the key first; that check answers,
    // so the fault reaches the trace builder's own signature scan.
    injected.answeredChecks = 1;
    const result = await commitClaimedInvalidSignature();
    expect(Either.isLeft(result)).toBe(true);
    if (Either.isRight(result)) return;
    expect(result.left).toBeInstanceOf(ValidationTraceStopped);
    expect((result.left as ValidationTraceStopped).reason).toBe("unavailable");
    // The verifier's typed fault is the cause, carrying the fault itself, so
    // a caller can tell a node fault apart.
    const cause = (result.left as ValidationTraceStopped).cause;
    expect(cause).toBeInstanceOf(LedgerSignatureVerifierUnavailableError);
    expect((cause as Error).cause).toBe(fault);
  });
});
