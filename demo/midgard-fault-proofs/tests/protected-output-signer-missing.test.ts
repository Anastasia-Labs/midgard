import { createPrivateKey, createPublicKey, sign } from "node:crypto";

import {
  computeMidgardNativeTxId,
  encodeCbor,
  encodeMidgardAddressWitnessItem,
  encodeMidgardNativeTxCanonical,
  encodeMidgardTxOutput,
} from "@al-ft/midgard-core";
import {
  acceptedVerdictSubject,
  buildProtectedOutputSignerMissingFaultProofContracts,
  forcedVerdictSubject,
  missingSignatureVkeyHash,
} from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  applyProtectedOutputSignerMissingScripts,
  PROTECTED_OUTPUT_SIGNER_MISSING_BLUEPRINT_TITLES,
} from "../src/protected-output-signer-missing/contracts.js";
import {
  classifyProtectedOutputSignerMissingFinding,
  detectProtectedOutputSignerMissingCompleteReplay,
  prepareProtectedOutputSignerMissingEvidence,
  PROTECTED_OUTPUT_SIGNER_MAX_WITNESSES,
  PROTECTED_OUTPUT_SIGNER_MISSING_ID,
} from "../src/protected-output-signer-missing/index.js";
import { buildRegisteredChainFixture } from "./support/emulator/registered-chain.js";
import { makeNativeTx } from "./support/submit-init-emulator-shared.js";

const txId = "11".repeat(32);
const seed = Buffer.alloc(32, 11);
const privateKey = createPrivateKey({
  key: Buffer.concat([
    Buffer.from("302e020100300506032b657004220420", "hex"),
    seed,
  ]),
  format: "der",
  type: "pkcs8",
});
const verificationKey = createPublicKey(privateKey)
  .export({ format: "der", type: "spki" })
  .subarray(-32);
const paymentCredential = missingSignatureVkeyHash(
  verificationKey.toString("hex"),
);
const protectedOutput = encodeMidgardTxOutput({
  address: Buffer.concat([
    Buffer.from([0x68]),
    Buffer.from(paymentCredential, "hex"),
  ]),
  value: { lovelace: 2_000_000n, assets: new Map() },
});

const transactionWithWitness = (kind: "empty" | "valid" | "invalid") => {
  const empty = makeNativeTx({
    spendInputCbors: [],
    fee: 7n,
    outputCbor: protectedOutput,
  });
  const id = computeMidgardNativeTxId(empty);
  const signature =
    kind === "valid" ? sign(null, id, privateKey) : Buffer.alloc(64, 0xff);
  return makeNativeTx({
    spendInputCbors: [],
    fee: 7n,
    outputCbor: protectedOutput,
    addrTxWitsPreimageCbor:
      kind === "empty"
        ? encodeCbor([])
        : encodeCbor([
            encodeMidgardAddressWitnessItem({
              verificationKey,
              signature,
            }),
          ]),
  });
};

const transactionWithWitnessCount = (count: number) =>
  makeNativeTx({
    spendInputCbors: [],
    fee: 7n,
    outputCbor: protectedOutput,
    addrTxWitsPreimageCbor: encodeCbor(
      Array.from({ length: count }, (_unused, index) => {
        const key = Buffer.alloc(32);
        key.writeUInt32BE(index + 1, 28);
        return encodeMidgardAddressWitnessItem({
          verificationKey: key,
          signature: Buffer.alloc(64, 0xff),
        });
      }),
    ),
  });

describe("protectedOutputSignerMissing V1 binding", () => {
  it("freezes the plan identity and accepts the accepted coordinate", () => {
    expect(PROTECTED_OUTPUT_SIGNER_MISSING_ID).toBe("0000002b");
    expect(() =>
      classifyProtectedOutputSignerMissingFinding({
        subject: acceptedVerdictSubject(txId),
        outputIndex: 3,
      }),
    ).not.toThrow();
  });

  it("binds the exact forced reason and output coordinate", () => {
    const subject = forcedVerdictSubject({
      transactionId: txId,
      sourceKey: { transactionId: "22".repeat(32), outputIndex: 0n },
      rejectionReason: {
        ProtectedOutputSignerMissing: { output_index: 3n },
      },
    });
    expect(() =>
      classifyProtectedOutputSignerMissingFinding({
        subject,
        outputIndex: 3,
      }),
    ).not.toThrow();
    expect(() =>
      classifyProtectedOutputSignerMissingFinding({
        subject,
        outputIndex: 2,
      }),
    ).toThrow(/coordinate was substituted/u);
  });

  it("refuses another missing-witness reason", () => {
    const subject = forcedVerdictSubject({
      transactionId: txId,
      sourceKey: { transactionId: "22".repeat(32), outputIndex: 0n },
      rejectionReason: { RequiredSignerUnsigned: { signer_index: 3n } },
    });
    expect(() =>
      classifyProtectedOutputSignerMissingFinding({
        subject,
        outputIndex: 3,
      }),
    ).toThrow(/wrong typed rejection reason/u);
  });

  it("admits only Ed25519-valid witnesses to the signer frontier in both directions", () => {
    for (const kind of ["empty", "invalid"] as const) {
      const transaction = transactionWithWitness(kind);
      const id = computeMidgardNativeTxId(transaction).toString("hex");
      const evidence = prepareProtectedOutputSignerMissingEvidence({
        subject: acceptedVerdictSubject(id),
        outputIndex: 0,
        canonicalTransactionCbor: encodeMidgardNativeTxCanonical(transaction),
      });
      expect(evidence.signerPresent).toBe(false);
      expect(evidence.validSignerHashes).toEqual([]);
    }

    const valid = transactionWithWitness("valid");
    const validId = computeMidgardNativeTxId(valid).toString("hex");
    expect(() =>
      prepareProtectedOutputSignerMissingEvidence({
        subject: acceptedVerdictSubject(validId),
        outputIndex: 0,
        canonicalTransactionCbor: encodeMidgardNativeTxCanonical(valid),
      }),
    ).toThrow(/agrees with the operator verdict/u);
    const forced = prepareProtectedOutputSignerMissingEvidence({
      subject: forcedVerdictSubject({
        transactionId: validId,
        sourceKey: { transactionId: "55".repeat(32), outputIndex: 0n },
        rejectionReason: {
          ProtectedOutputSignerMissing: { output_index: 0n },
        },
      }),
      outputIndex: 0,
      canonicalTransactionCbor: encodeMidgardNativeTxCanonical(valid),
    });
    expect(forced.signerPresent).toBe(true);
    expect(forced.validSignerHashes).toEqual([paymentCredential]);

    const missing = transactionWithWitness("empty");
    const missingId = computeMidgardNativeTxId(missing).toString("hex");
    expect(() =>
      prepareProtectedOutputSignerMissingEvidence({
        subject: forcedVerdictSubject({
          transactionId: missingId,
          sourceKey: { transactionId: "66".repeat(32), outputIndex: 0n },
          rejectionReason: {
            ProtectedOutputSignerMissing: { output_index: 0n },
          },
        }),
        outputIndex: 0,
        canonicalTransactionCbor: encodeMidgardNativeTxCanonical(missing),
      }),
    ).toThrow(/agrees with the operator verdict/u);
  });

  it("accepts the maximum witness frontier and refuses its adjacent overflow", () => {
    expect(PROTECTED_OUTPUT_SIGNER_MAX_WITNESSES).toBe(318);
    const maximum = transactionWithWitnessCount(318);
    const maximumId = computeMidgardNativeTxId(maximum).toString("hex");
    const evidence = prepareProtectedOutputSignerMissingEvidence({
      subject: acceptedVerdictSubject(maximumId),
      outputIndex: 0,
      canonicalTransactionCbor: encodeMidgardNativeTxCanonical(maximum),
    });
    expect(evidence.checkpoints.at(-1)).toEqual({
      cursor: 318,
      signerPresent: false,
    });
    expect(evidence.witnessCarriage).toBe("Certified");

    const overflow = transactionWithWitnessCount(319);
    const overflowId = computeMidgardNativeTxId(overflow).toString("hex");
    expect(() =>
      prepareProtectedOutputSignerMissingEvidence({
        subject: acceptedVerdictSubject(overflowId),
        outputIndex: 0,
        canonicalTransactionCbor: encodeMidgardNativeTxCanonical(overflow),
      }),
    ).toThrow(/frontier exceeds the canonical maximum/u);
  });

  it("complete replay scans accepted output coordinates and omits honest transactions", () => {
    const missing = transactionWithWitness("empty");
    const valid = transactionWithWitness("valid");
    const block = (transactions: readonly unknown[]) =>
      ({
        transactions,
        reconstruction: { forcedTransactions: [] },
      }) as never;
    expect(
      detectProtectedOutputSignerMissingCompleteReplay(
        block([
          {
            txCbor: encodeMidgardNativeTxCanonical(missing).toString("hex"),
          },
        ]),
      ),
    ).toHaveLength(1);
    expect(
      detectProtectedOutputSignerMissingCompleteReplay(
        block([
          {
            txCbor: encodeMidgardNativeTxCanonical(valid).toString("hex"),
          },
        ]),
      ),
    ).toEqual([]);
  });

  it("routes a forced unprotected, script-locked or out-of-range coordinate to the direct terminal and refuses it for an accepted subject", () => {
    // Canonical validation consults the signer frontier only for a
    // protected pub-key output at a position its cursor visits; every other
    // coordinate is authorized with no signer, so the forced rejection is
    // wrong without a scan.
    const outputAt = (header: number) =>
      encodeMidgardTxOutput({
        address: Buffer.concat([
          Buffer.from([header]),
          Buffer.from(paymentCredential, "hex"),
        ]),
        value: { lovelace: 2_000_000n, assets: new Map() },
      });
    const transactionWithOutput = (outputCbor: Buffer) =>
      makeNativeTx({
        spendInputCbors: [],
        fee: 9n,
        outputCbor,
        addrTxWitsPreimageCbor: encodeCbor([]),
      });
    const prepare = (
      transaction: ReturnType<typeof makeNativeTx>,
      outputIndex: number,
      direction: "forced" | "accepted",
    ) => {
      const id = computeMidgardNativeTxId(transaction).toString("hex");
      return prepareProtectedOutputSignerMissingEvidence({
        subject:
          direction === "forced"
            ? forcedVerdictSubject({
                transactionId: id,
                sourceKey: { transactionId: "22".repeat(32), outputIndex: 0n },
                rejectionReason: {
                  ProtectedOutputSignerMissing: {
                    output_index: BigInt(outputIndex),
                  },
                },
              })
            : acceptedVerdictSubject(id),
        outputIndex,
        canonicalTransactionCbor: encodeMidgardNativeTxCanonical(transaction),
      });
    };
    const unprotected = prepare(
      transactionWithOutput(outputAt(0x60)),
      0,
      "forced",
    );
    expect(unprotected.route).toBe("unprotected_output");
    expect(unprotected.signerRequired).toBe(false);
    expect(unprotected.signerPresent).toBe(false);
    expect(unprotected.paymentCredentialHex).toBeUndefined();
    expect(unprotected.outputCborHex).toBe(outputAt(0x60).toString("hex"));
    const scriptLocked = prepare(
      transactionWithOutput(outputAt(0x78)),
      0,
      "forced",
    );
    expect(scriptLocked.route).toBe("script_credential");
    expect(scriptLocked.signerRequired).toBe(false);
    const outOfRange = prepare(
      transactionWithOutput(outputAt(0x68)),
      1,
      "forced",
    );
    expect(outOfRange.route).toBe("coordinate_out_of_range");
    expect(outOfRange.signerRequired).toBe(false);
    expect(outOfRange.outputCborHex).toBeUndefined();
    expect(outOfRange.checkpoints).toHaveLength(0);
    // An accepted subject has no fault at any of these coordinates.
    expect(() =>
      prepare(transactionWithOutput(outputAt(0x60)), 0, "accepted"),
    ).toThrow(/not protected/u);
    expect(() =>
      prepare(transactionWithOutput(outputAt(0x78)), 0, "accepted"),
    ).toThrow(/does not use a key credential/u);
    expect(() =>
      prepare(transactionWithOutput(outputAt(0x68)), 1, "accepted"),
    ).toThrow(/out of range/u);
    // The witness-scan route is unchanged for a protected pub-key output.
    const scan = prepare(transactionWithOutput(outputAt(0x68)), 0, "accepted");
    expect(scan.route).toBe("witness_scan");
    expect(scan.signerRequired).toBe(true);
    expect(scan.signerPresent).toBe(false);
  });
});

describe("protectedOutputSignerMissing registered-chain parity", () => {
  it("applies the same chain the SDK registers for the same shared policies", async () => {
    const fixture = await buildRegisteredChainFixture(
      buildProtectedOutputSignerMissingFaultProofContracts,
    );
    const registered = fixture.contracts.protectedOutputSignerMissing;
    const applied = applyProtectedOutputSignerMissingScripts(
      fixture.applyParams,
    );
    expect(applied.map((step) => step.spendingScriptHash)).toStrictEqual(
      registered.steps.map((step) => step.spendingScriptHash),
    );
    expect(registered.firstStep.spendingScriptHash).toBe(
      applied[0].spendingScriptHash,
    );
    expect(applied.map((step) => step.blueprintTitle)).toStrictEqual([
      ...PROTECTED_OUTPUT_SIGNER_MISSING_BLUEPRINT_TITLES,
    ]);
  });
});
