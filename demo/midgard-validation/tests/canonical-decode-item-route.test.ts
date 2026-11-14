import {
  encodeMidgardNativeScript,
  encodeMidgardVersionedScript,
  type MidgardNativeScript,
} from "@al-ft/midgard-core";
import { decodeSingleCbor } from "@al-ft/midgard-core/codec";
import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core/consensus-profile";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import { CML } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  buildDeterministicValidationMachineTrace,
  buildValidationMachineLedgerInsertOp,
  buildValidationMachineLedgerMutationSteps,
  type DeterministicValidationMachineTrace,
  validationSemanticResolverIndex,
} from "../src/index.js";
import {
  FUNDED_OUTPUT_LOVELACE,
  fundingLovelaceForOutputs,
  hashScriptWitness,
  makeMinAdaFundedExactSizeOutputItem,
  makeNativeTx,
  makeOutput,
  type NativeTxFixture,
  outRefFromByte,
  outRefFromTxId,
  TEST_SIGNER_HASH,
} from "./validation-fixtures.js";
import { context } from "./validation-machine.validate-boundary-abi-and-collect-auxiliary-kinds.js";

/**
 * CanonicalDecode reads every non-empty field item through the complete-item
 * route, whatever the field and whatever the item's size. The canonical-decode
 * chunk route was deleted on-chain in 2e52d2ef4; these traces pin that the
 * producer emits only steps the deployed prepare accepts (semantic resolver 0
 * for an empty field, 1 for an item) and never a field-chunk auxiliary there.
 */

/** The work-witness controls of every canonicalDecode step, in trace order. */
const canonicalDecodeSteps = (trace: DeterministicValidationMachineTrace) =>
  trace.witnesses
    .filter((witness) => witness.phase === "canonicalDecode")
    .map((witness) => {
      const control = decodeSingleCbor(witness.cbor);
      if (!Array.isArray(control) || control.length !== 9) {
        throw new Error("canonicalDecode control must contain nine fields");
      }
      return {
        fieldIndex: Number(control[4] as bigint),
        itemIndex: Number(control[5] as bigint),
        chunkIndex: Number(control[6] as bigint),
        auxiliaryKind: witness.auxiliary?.kind ?? null,
        semanticResolverIndex: validationSemanticResolverIndex(witness),
      };
    });

const expectItemRouteOnly = (
  trace: DeterministicValidationMachineTrace,
  fieldIndex: number,
) => {
  const steps = canonicalDecodeSteps(trace);
  expect(steps.length).toBeGreaterThanOrEqual(9);
  for (const step of steps) {
    expect(step.auxiliaryKind === null ? 0 : 1).toBe(
      step.semanticResolverIndex,
    );
    expect([null, "transactionFieldItem"]).toContain(step.auxiliaryKind);
    expect(step.chunkIndex).toBe(0);
  }
  // The field under test is read in exactly one item step.
  expect(steps.filter((step) => step.fieldIndex === fieldIndex)).toEqual([
    {
      fieldIndex,
      itemIndex: 0,
      chunkIndex: 0,
      auxiliaryKind: "transactionFieldItem",
      semanticResolverIndex: 1,
    },
  ]);
};

const signature: MidgardNativeScript = {
  type: "sig",
  keyHash: Buffer.from(TEST_SIGNER_HASH, "hex"),
};

/**
 * An accepted spend from a native-script address whose single field-6 script
 * item is `all` of `signatureCount` copies of the transaction's own signer.
 */
const buildScriptSpendTrace = async (signatureCount: number) => {
  const nativeScript: MidgardNativeScript = {
    type: "all",
    scripts: Array.from({ length: signatureCount }, () => signature),
  };
  const script = {
    language: "NativeCardano" as const,
    scriptBytes: encodeMidgardNativeScript(nativeScript),
    nativeScript,
  };
  const scriptAddress = Buffer.from(
    CML.EnterpriseAddress.new(
      0,
      CML.Credential.new_script(
        CML.ScriptHash.from_hex(hashScriptWitness(script)),
      ),
    )
      .to_address()
      .to_raw_bytes(),
  );
  const spent = outRefFromByte(0x51);
  const spentOutput = makeOutput(FUNDED_OUTPUT_LOVELACE, scriptAddress);
  const output = makeOutput(FUNDED_OUTPUT_LOVELACE);
  const transaction = makeNativeTx({
    version: 1n,
    spendInputs: [spent],
    outputs: [output],
    scriptWitnesses: [script],
  });
  const trace = await buildAcceptedTrace(transaction, spent, spentOutput, [
    output,
  ]);
  return { trace, itemBytes: encodeMidgardVersionedScript(script).length };
};

const buildAcceptedTrace = async (
  transaction: NativeTxFixture,
  spent: Buffer,
  spentOutput: Buffer,
  outputs: readonly Buffer[],
) => {
  const expectedLedgerOps = [
    { type: "delete" as const, key: spent },
    ...outputs.map((outputCbor, index) =>
      buildValidationMachineLedgerInsertOp({
        key: outRefFromTxId(transaction.txId, BigInt(index)),
        outputCbor,
      }),
    ),
  ];
  const ledgerMutationSteps = await buildValidationMachineLedgerMutationSteps({
    initialEntries: [{ outRef: spent, output: spentOutput }],
    operations: expectedLedgerOps,
  });
  return Effect.runPromise(
    buildDeterministicValidationMachineTrace({
      l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
      ...context,
      transactionId: transaction.txId,
      canonicalTransactionCbor: transaction.txCbor,
      priorUtxosRoot: ledgerMutationSteps[0]!.preRoot.toString("hex"),
      postUtxosRoot: ledgerMutationSteps.at(-1)!.postRoot.toString("hex"),
      ledgerWitnessEntries: [{ outRef: spent, output: spentOutput }],
      expectedLedgerOps,
      ledgerMutationSteps,
      expectedVerdict: "accepted",
      expectedRejectionCode: null,
    }),
  );
};

const signatureBytes = encodeMidgardNativeScript(signature).length;

describe("canonicalDecode item route at every item size", () => {
  it("reads a ~20 KB field-6 script item in one item step", async () => {
    const { trace, itemBytes } = await buildScriptSpendTrace(
      Math.floor(20_000 / signatureBytes),
    );
    expect(itemBytes).toBeGreaterThan(
      MIDGARD_CONSENSUS_LIMITS.maxSinglePublicationCompleteItemBytes,
    );
    expect(itemBytes).toBeGreaterThan(19_000);
    expect(trace.verdict).toBe("accepted");
    expectItemRouteOnly(trace, 6);
  }, 300_000);

  it("reads a field-6 script item near the field cap in one item step", async () => {
    // One item in a §5.1 field preimage costs a one-byte array header and a
    // three-byte definite-bytes header, so the item fills the field to within
    // one signature clause of the aggregate cap.
    const cap = MIDGARD_CONSENSUS_LIMITS.maxTransactionAggregateFieldBytes;
    const { trace, itemBytes } = await buildScriptSpendTrace(
      Math.floor((cap - 4 - 8) / signatureBytes),
    );
    expect(itemBytes + 4).toBeLessThanOrEqual(cap);
    expect(itemBytes + 4).toBeGreaterThan(cap - signatureBytes - 8);
    expect(trace.verdict).toBe("accepted");
    expectItemRouteOnly(trace, 6);
  }, 600_000);

  it("reads a field-2 output at the output width cap in one item step", async () => {
    const outputBytes = MIDGARD_CONSENSUS_LIMITS.maxLedgerOutputPreimageBytes;
    const output = makeMinAdaFundedExactSizeOutputItem(outputBytes);
    const spent = outRefFromByte(0x52);
    const spentOutput = makeOutput(fundingLovelaceForOutputs([output]));
    const transaction = makeNativeTx({
      version: 1n,
      spendInputs: [spent],
      outputs: [output],
    });
    const trace = await buildAcceptedTrace(transaction, spent, spentOutput, [
      output,
    ]);
    expect(output.length).toBe(outputBytes);
    expect(trace.verdict).toBe("accepted");
    expectItemRouteOnly(trace, 2);
  }, 300_000);
});
