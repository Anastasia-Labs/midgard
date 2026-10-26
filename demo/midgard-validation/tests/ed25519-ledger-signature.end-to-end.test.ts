import {
  hashMidgardValidationRejectionReasonV1,
  ledgerEd25519SignatureReady,
} from "@al-ft/midgard-core";
import { CML } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { beforeAll, describe, expect, it } from "vitest";

import {
  buildDeterministicValidationMachineTrace,
  buildValidationMachineLedgerInsertOp,
  buildValidationMachineLedgerMutationSteps,
  RejectCodes,
} from "../src/index.js";
import {
  phaseAVerdict,
  producedOutput,
  signatureRejectionTrace,
  spentOutRef,
  traceContext,
  transactionWithWitnesses,
} from "./ed25519-ledger-transactions.js";
import {
  honestVkeyWitness,
  ownKeyIdentityRVkeyWitness,
  signerPublicKey,
  signerSeed,
  TEST_SIGNER_SEED,
} from "./ed25519-ledger-vectors.js";
import { outRefFromTxId } from "./validation-fixtures.js";

/**
 * Whole-transaction rows: Phase A and the trace builder verify each address
 * witness exactly as the ledger does, and name the same witness when one does
 * not verify.
 */
const witnessRejectionHash = (index: number): string =>
  hashMidgardValidationRejectionReasonV1({
    AddressWitnessSignatureInvalid: { witness_index: BigInt(index) },
  }).toString("hex");

const descriptorRejectionHash = (
  trace: Awaited<ReturnType<typeof signatureRejectionTrace>>,
): string =>
  Buffer.from(trace.tree.descriptor.rejectionCodeHash).toString("hex");

const owner = honestVkeyWitness(TEST_SIGNER_SEED);
const ownerIdentityR = ownKeyIdentityRVkeyWitness(TEST_SIGNER_SEED);

const keyHash = (seed: Uint8Array): Buffer =>
  Buffer.from(
    CML.PublicKey.from_bytes(signerPublicKey(seed)).hash().to_raw_bytes(),
  );

/**
 * An identity-R witness by the first deterministic signer whose key hash sorts
 * on `side` of the owner's, so the two witnesses can be laid out in canonical
 * order with the invalid one at either position.
 */
const otherIdentityRSorting = (side: "before" | "after") => {
  const ownerHash = keyHash(TEST_SIGNER_SEED);
  for (let index = 1; ; index += 1) {
    const order = Buffer.compare(keyHash(signerSeed(index)), ownerHash);
    if (side === "before" ? order < 0 : order > 0) {
      return ownKeyIdentityRVkeyWitness(signerSeed(index));
    }
  }
};

describe("Ed25519 witness verdicts end to end", { timeout: 120_000 }, () => {
  beforeAll(() => ledgerEd25519SignatureReady());

  it("rejects the signer's own key with R the identity and S h·a at that witness", async () => {
    const transaction = transactionWithWitnesses([ownerIdentityR]);
    expect(phaseAVerdict(transaction)).toMatchObject({
      code: RejectCodes.InvalidSignature,
      subject: { arm: "AddressWitnessSignatureInvalid", index: 0n },
    });

    const trace = await signatureRejectionTrace(transaction);
    expect(trace.tree.descriptor.verdict).toBe("rejected");
    expect(descriptorRejectionHash(trace)).toBe(witnessRejectionHash(0));
  });

  it("accepts the honestly signed twin", async () => {
    const transaction = transactionWithWitnesses([owner]);
    expect("ledgerTx" in phaseAVerdict(transaction)).toBe(true);

    const expectedLedgerOps = [
      { type: "delete" as const, key: spentOutRef },
      buildValidationMachineLedgerInsertOp({
        key: outRefFromTxId(transaction.txId),
        outputCbor: producedOutput,
      }),
    ];
    const ledgerMutationSteps = await buildValidationMachineLedgerMutationSteps(
      {
        initialEntries: [{ outRef: spentOutRef, output: producedOutput }],
        operations: expectedLedgerOps,
      },
    );
    const trace = await Effect.runPromise(
      buildDeterministicValidationMachineTrace({
        ...traceContext,
        transactionId: transaction.txId,
        canonicalTransactionCbor: transaction.txCbor,
        priorUtxosRoot: ledgerMutationSteps[0]!.preRoot.toString("hex"),
        postUtxosRoot: ledgerMutationSteps.at(-1)!.postRoot.toString("hex"),
        ledgerWitnessEntries: [{ outRef: spentOutRef, output: producedOutput }],
        expectedLedgerOps,
        ledgerMutationSteps,
        expectedVerdict: "accepted",
        expectedRejectionCode: null,
      }),
    );
    expect(trace.tree.descriptor.verdict).toBe("accepted");
  });

  it.each([
    ["second", [owner, otherIdentityRSorting("after")], 1],
    ["first", [otherIdentityRSorting("before"), owner], 0],
  ] as const)(
    "names the same witness in Phase A and the trace when it is %s of two",
    async (_position, witnesses, invalidIndex) => {
      const transaction = transactionWithWitnesses(witnesses);
      expect(phaseAVerdict(transaction)).toMatchObject({
        code: RejectCodes.InvalidSignature,
        subject: {
          arm: "AddressWitnessSignatureInvalid",
          index: BigInt(invalidIndex),
        },
      });

      const trace = await signatureRejectionTrace(transaction);
      expect(descriptorRejectionHash(trace)).toBe(
        witnessRejectionHash(invalidIndex),
      );
    },
  );
});
