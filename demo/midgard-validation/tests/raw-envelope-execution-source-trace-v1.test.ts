import {
  encodeCbor,
  MIDGARD_CONSENSUS_PROFILE_V1,
  verifyMidgardValidationTraceProofV1,
} from "@al-ft/midgard-core";
import { blake2b } from "@noble/hashes/blake2.js";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  buildDeterministicValidationMachineTrace,
  buildValidationMachineLedgerInsertOpV1,
  buildValidationMachineLedgerMutationSteps,
} from "../src/index.js";
import {
  encodeRecomputedNativeTx,
  FUNDED_OUTPUT_LOVELACE_V1,
  makeMintPreimageCbor,
  makeNativeTx,
  makeOutput,
  nativeScriptWitness,
  outRefFromByte,
  outRefFromTxId,
} from "./validation-fixtures.js";

describe("raw-envelope execution-source validation trace", () => {
  it("retains malformed field-6 bytes and deterministically proves the NativeScripts state", async () => {
    const spent = outRefFromByte(0x79);
    const spentOutput = makeOutput(FUNDED_OUTPUT_LOVELACE_V1);
    const payload = Buffer.from("820700", "hex");
    const item = Buffer.from("820043820700", "hex");
    const policyId = Buffer.from(
      blake2b(Buffer.concat([Buffer.from([0]), payload]), { dkLen: 28 }),
    );
    const assetName = Buffer.from("31", "hex");
    const output = makeOutput(
      FUNDED_OUTPUT_LOVELACE_V1,
      undefined,
      new Map([
        [policyId.toString("hex"), new Map([[assetName.toString("hex"), 1n]])],
      ]),
    );
    const baseline = makeNativeTx({
      spendInputs: [spent],
      outputs: [output],
      scriptWitnesses: [nativeScriptWitness({ type: "all", scripts: [] })],
      mintPreimageCbor: makeMintPreimageCbor(
        new Map([[policyId, new Map([[assetName, 1n]])]]),
      ),
    });
    const malformed = encodeRecomputedNativeTx({
      ...baseline.tx,
      witnessSet: {
        ...baseline.tx.witnessSet,
        scriptTxWitsPreimageCbor: encodeCbor([item]),
      },
    });
    const acceptedOps = [
      { type: "delete" as const, key: spent },
      buildValidationMachineLedgerInsertOpV1({
        key: outRefFromTxId(malformed.txId),
        outputCbor: output,
      }),
    ];
    const mutations = await buildValidationMachineLedgerMutationSteps({
      initialEntries: [{ outRef: spent, output: spentOutput }],
      operations: acceptedOps,
    });
    const root = mutations[0]!.preRoot.toString("hex");
    const input = {
      consensusProfile: MIDGARD_CONSENSUS_PROFILE_V1,
      eventKeyCbor: encodeCbor([2n, malformed.txId]),
      sourceKind: "normal" as const,
      blockEndTimeMs: 1_750_000_000_000,
      expectedNetworkId: 0n,
      minFeeA: 0n,
      minFeeB: 0n,
      blockSlot: 100n,
      transactionId: malformed.txId,
      canonicalTransactionCbor: malformed.txCbor,
      priorUtxosRoot: root,
      postUtxosRoot: root,
      ledgerWitnessEntries: [{ outRef: spent, output: spentOutput }],
      expectedLedgerOps: [],
      ledgerMutationSteps: [],
      expectedVerdict: "rejected" as const,
      expectedRejectionCode: "E_INVALID_FIELD_TYPE" as const,
    };
    const first = await Effect.runPromise(
      buildDeterministicValidationMachineTrace(input),
    );
    const second = await Effect.runPromise(
      buildDeterministicValidationMachineTrace(input),
    );
    const index = first.witnesses.findIndex(
      ({ phase, auxiliary }) =>
        phase === "nativeScripts" &&
        auxiliary?.kind === "nativeExecutionDescriptor",
    );
    expect(index).toBeGreaterThanOrEqual(0);
    expect(first.witnesses[index]?.auxiliary).toMatchObject({
      kind: "nativeExecutionDescriptor",
      languageTag: 0,
      source: { scriptTotalLength: item.length },
    });
    expect(
      first.witnesses.some(
        ({ auxiliary }) => auxiliary?.kind === "nativeScriptToken",
      ),
    ).toBe(true);
    expect(first.tree.descriptor).toEqual(second.tree.descriptor);
    expect(first.tree.proofs[index]).toEqual(second.tree.proofs[index]);
    expect(
      verifyMidgardValidationTraceProofV1({
        descriptor: first.tree.descriptor,
        proof: first.tree.proofs[index]!,
      }),
    ).toBe(true);
  });
});
