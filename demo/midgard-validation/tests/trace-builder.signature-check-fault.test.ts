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
 * E8 fault injection: the trace builder checks each address-witness
 * signature itself. When that check fails (injected here for one key), the
 * build has no verdict: it stops as unavailable with the fault as its cause,
 * never reading the witness's signature as invalid.
 */
const keyHash = (key: CML.PrivateKey): Buffer =>
  Buffer.from(key.to_public().hash().to_raw_bytes());

// The invalid witness comes first in the transaction; the valid one sorts
// first by signer hash, so the builder checks it first.
const invalidSigner = CML.PrivateKey.generate_ed25519();
let validSigner = CML.PrivateKey.generate_ed25519();
while (Buffer.compare(keyHash(validSigner), keyHash(invalidSigner)) >= 0)
  validSigner = CML.PrivateKey.generate_ed25519();
const validVerificationKey = Buffer.from(
  validSigner.to_public().to_raw_bytes(),
);

const spent = outRefFromByte(0x11);
const output = makeOutput(FUNDED_OUTPUT_LOVELACE);
const transaction = makeNativeTx({
  version: 1n,
  invalidVkeyWitness: true,
  privateKey: invalidSigner,
  extraVkeyWitnessKeys: [validSigner],
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
    vi.restoreAllMocks();
  });

  it("builds the claimed rejection while the check works", async () => {
    const result = await commitClaimedInvalidSignature();
    expect(Either.isRight(result)).toBe(true);
    if (Either.isRight(result))
      expect(result.right.descriptor.verdict).toBe("rejected");
  });

  it("stops as unavailable, never building a rejection, when the check fails", async () => {
    const fault = new WebAssembly.RuntimeError("unreachable");
    const fromBytes = CML.PublicKey.from_bytes.bind(CML.PublicKey);
    vi.spyOn(CML.PublicKey, "from_bytes").mockImplementation((bytes) => {
      if (Buffer.from(bytes).equals(validVerificationKey)) throw fault;
      return fromBytes(bytes);
    });
    const result = await commitClaimedInvalidSignature();
    expect(Either.isLeft(result)).toBe(true);
    if (Either.isRight(result)) return;
    expect(result.left).toBeInstanceOf(ValidationTraceStopped);
    expect((result.left as ValidationTraceStopped).reason).toBe("unavailable");
    // The fault itself is the cause, so a caller can tell a node fault apart.
    expect((result.left as ValidationTraceStopped).cause).toBe(fault);
  });
});
