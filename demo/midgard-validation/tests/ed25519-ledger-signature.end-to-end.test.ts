import {
  hashMidgardValidationRejectionReasonV1,
  ledgerEd25519SignatureReady,
} from "@al-ft/midgard-core";
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
const otherIdentityR = ownKeyIdentityRVkeyWitness(signerSeed(1));

describe("Ed25519 witness verdicts end to end", { timeout: 120_000 }, () => {
  beforeAll(() => ledgerEd25519SignatureReady);

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
    ["second", [owner, otherIdentityR], 1],
    ["first", [otherIdentityR, owner], 0],
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
