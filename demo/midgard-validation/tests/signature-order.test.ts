import {
  encodeCbor,
  encodeMidgardForcedTxCanonical,
  encodeMidgardValidationMachineState,
  MIDGARD_CONSENSUS_LIMITS,
  MIDGARD_CONSENSUS_PROFILE,
  MidgardValidationPhase,
} from "@al-ft/midgard-core";
import { decodeSingleCbor } from "@al-ft/midgard-core/codec";
import { MIDGARD_ADDRESS_WITNESS_ITEM_BYTES } from "@al-ft/midgard-core/codec/native-tx-field-access";
import {
  validateMidgardConsensusForcedTxCbor,
  validateMidgardConsensusTxCbor,
} from "@al-ft/midgard-core/consensus-validation";
import {
  daPayloadEntryEncodedSize,
  deriveTxOrderMaterial,
  encodeRetainedValidationWitness,
  encodeRetainedValidationWitnessKey,
  type RetainedValidationAuxiliaryWitness,
  RetainedValidationAuxiliaryWitnessSchema,
  retainedValidationStateCoordinate,
  validationMachineStateDataFromCore,
  validationTraceProofDataFromCore,
} from "@al-ft/midgard-sdk";
import { CML, Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  buildDeterministicValidationMachineTrace,
  buildValidationMachineLedgerInsertOp,
  buildValidationMachineLedgerMutationSteps,
} from "../src/index.js";
import { retainedValidationAuxiliaryWitnessData } from "../src/validation-machine-data.js";
import {
  encodeByteList,
  encodeRecomputedNativeTx,
  FUNDED_OUTPUT_LOVELACE,
  makeNativeTx,
  makeOutput,
  outRefFromByte,
  outRefFromTxId,
} from "./validation-fixtures.js";
import { TEST_PRIVATE_KEY } from "./validation-fixtures.make-min-ada-funded-exact-size-output-item.js";
const context = {
  consensusProfile: MIDGARD_CONSENSUS_PROFILE,
  eventKeyCbor: encodeCbor([2n, Buffer.alloc(32, 0x41)]),
  sourceKind: "normal" as const,
  blockEndTimeMs: 1_750_000_000_000,
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  blockSlot: 100n,
};
// This is a pure trace producer check. Existing machine ABI tests eagerly load
// a blueprint, so the schedule/runtime case has its own source-only setup.
describe("deterministic signature consume order", { timeout: 60_000 }, () => {
  it.each([
    2,
    16,
    124,
    Math.floor(
      (MIDGARD_CONSENSUS_LIMITS.maxAddressWitnessesPreimageBytes - 3) /
        (MIDGARD_ADDRESS_WITNESS_ITEM_BYTES + 2),
    ),
  ])(
    "consumes %i valid address witnesses in arbitrary physical order with one successor schedule",
    async (count) => {
      const spent = outRefFromByte(0x11);
      const output = makeOutput(FUNDED_OUTPUT_LOVELACE);
      const original = makeNativeTx({
        version: 1n,
        spendInputs: [spent],
        outputs: [output],
      });
      const keys = [
        TEST_PRIVATE_KEY,
        ...Array.from({ length: count - 1 }, (_, index) =>
          CML.PrivateKey.from_normal_bytes(
            Buffer.concat([
              Buffer.alloc(28),
              Buffer.from([
                (index + 1) >>> 24,
                (index + 1) >>> 16,
                (index + 1) >>> 8,
                index + 1,
              ]),
            ]),
          ),
        ),
      ];
      const physical = keys
        .map((key) => ({
          signerHash: Buffer.from(key.to_public().hash().to_raw_bytes()),
          bytes: Buffer.from(
            CML.make_vkey_witness(
              CML.TransactionHash.from_raw_bytes(original.txId),
              key,
            ).to_cbor_bytes(),
          ),
        }))
        .sort((left, right) =>
          Buffer.compare(right.signerHash, left.signerHash),
        );
      const transaction = encodeRecomputedNativeTx({
        ...original.tx,
        witnessSet: {
          ...original.tx.witnessSet,
          addrTxWitsPreimageCbor: encodeByteList(
            physical.map(({ bytes }) => bytes),
          ),
        },
      });
      expect(validateMidgardConsensusTxCbor(transaction.txCbor)).toBeNull();
      const forcedCbor = encodeMidgardForcedTxCanonical(transaction.tx);
      expect(validateMidgardConsensusForcedTxCbor(forcedCbor)).toBeNull();
      const order = deriveTxOrderMaterial({
        submittedTxCbor: forcedCbor,
        owner: Buffer.alloc(28),
      });
      expect(order.transactionId).toBe(transaction.txId.toString("hex"));
      expect(
        order.carriage.find((field) => field.fieldIndex === 7)?.plan.tier,
      ).toBe(count > 124 ? "Certified" : "Inline");
      const expectedLedgerOps = [
        { type: "delete" as const, key: spent },
        buildValidationMachineLedgerInsertOp({
          key: outRefFromTxId(transaction.txId),
          outputCbor: output,
        }),
      ];
      const ledgerMutationSteps =
        await buildValidationMachineLedgerMutationSteps({
          initialEntries: [{ outRef: spent, output }],
          operations: expectedLedgerOps,
        });
      const started = performance.now();
      const trace = await Effect.runPromise(
        buildDeterministicValidationMachineTrace({
          ...context,
          transactionId: transaction.txId,
          canonicalTransactionCbor: transaction.txCbor,
          priorUtxosRoot: ledgerMutationSteps[0]!.preRoot.toString("hex"),
          postUtxosRoot: ledgerMutationSteps.at(-1)!.postRoot.toString("hex"),
          ledgerWitnessEntries: [{ outRef: spent, output }],
          expectedLedgerOps,
          ledgerMutationSteps,
          expectedVerdict: "accepted",
          expectedRejectionCode: null,
        }),
      );
      const durationMs = performance.now() - started;
      const signatures = trace.witnesses.filter(
        (witness) => witness.phase === "signatures",
      );
      expect(signatures).toHaveLength(count + 2);
      const controls = signatures.map(
        (witness) => decodeSingleCbor(witness.cbor) as readonly unknown[],
      );
      for (let selected = 0; selected < count; selected += 1) {
        expect(Number(controls[selected]?.[5])).toBe(0);
        expect(Number(controls[selected]?.[8])).toBe(selected);
        expect(signatures[selected]?.auxiliary).toMatchObject({
          kind: "transactionFieldChunk",
          fieldIndex: 7,
          itemIndex: count - selected - 1,
        });
      }
      expect(trace.verdict).toBe("accepted");
      let retainedEncodedBytes = 0;
      let retainedDaTupleBytes = 0;
      let retainedFailure: string | null = null;
      let rawFieldCarriageBytes = 0;
      for (const [index, witness] of trace.witnesses.entries()) {
        if (witness.auxiliary && "fieldPreimage" in witness.auxiliary)
          rawFieldCarriageBytes += witness.auxiliary.fieldPreimage.length;
        if (retainedFailure !== null) continue;
        try {
          const auxiliary = Data.from(
            Data.to(
              retainedValidationAuxiliaryWitnessData(
                witness.auxiliary,
              ) as never,
            ),
            RetainedValidationAuxiliaryWitnessSchema,
          ) as unknown as RetainedValidationAuxiliaryWitness;
          const retainedKey = encodeRetainedValidationWitnessKey({
            event_key: {
              L2TransactionEventKey: {
                tx_id: transaction.txId.toString("hex"),
              },
            },
            execution_index: retainedValidationStateCoordinate(
              BigInt(trace.tree.descriptor.stepCount),
              BigInt(index),
            ),
          });
          const retainedWitness = encodeRetainedValidationWitness({
            machine_state: validationMachineStateDataFromCore(
              trace.states[index]!,
            ),
            trace_proof: validationTraceProofDataFromCore(
              trace.tree.proofs[index]!,
            ),
            phase: BigInt(MidgardValidationPhase[witness.phase]),
            program_counter: BigInt(witness.programCounter),
            witness_cbor: witness.cbor.toString("hex"),
            auxiliary,
          });
          retainedEncodedBytes += retainedKey.length + retainedWitness.length;
          retainedDaTupleBytes += daPayloadEntryEncodedSize([
            retainedKey.toString("hex"),
            retainedWitness.toString("hex"),
          ]);
        } catch (error) {
          retainedFailure =
            error instanceof Error
              ? `${error.name}: ${error.message}`
              : String(error);
        }
      }
      expect(retainedFailure).toBeNull();
      expect(retainedEncodedBytes).toBeLessThan(64 * 1024 * 1024);
      expect(retainedDaTupleBytes).toBeLessThan(64 * 1024 * 1024);
      if (process.env.MIDGARD_PRINT_PROOF_FIT === "1") {
        console.info(
          JSON.stringify({
            signatureScan: {
              count,
              retainedEncodedBytes,
              retainedDaTupleBytes,
              retainedFailure,
              rawFieldCarriageBytes,
              durationMs,
              maxRssKiB: process.resourceUsage().maxRSS,
              heapUsedBytes: process.memoryUsage().heapUsed,
              sourceCanonicalBytes: transaction.txCbor.length,
              signatureSteps: signatures.length,
              totalSteps: trace.witnesses.length,
              stateCborBytes: trace.states.reduce(
                (total, state) =>
                  total + encodeMidgardValidationMachineState(state).length,
                0,
              ),
              workWitnessBytes: signatures.reduce(
                (total, witness) => total + witness.cbor.length,
                0,
              ),
              maxWorkWitnessBytes: Math.max(
                ...signatures.map((witness) => witness.cbor.length),
              ),
            },
          }),
        );
      }
    },
  );
});
