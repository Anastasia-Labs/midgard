import {
  decodeMidgardNativeTxFullFromCanonicalCbor,
  deriveMidgardNativeTxFaultEvidenceMaterial,
  encodeCbor,
  encodeMidgardForcedTxCanonical,
  encodeMidgardValidationMachineState,
  locateMidgardTransactionFieldItemFault,
  MIDGARD_CONSENSUS_LIMITS,
  MIDGARD_CONSENSUS_PROFILE,
} from "@al-ft/midgard-core";
import { decodeSingleCbor } from "@al-ft/midgard-core/codec";
import { MIDGARD_ADDRESS_WITNESS_ITEM_BYTES } from "@al-ft/midgard-core/codec/native-tx-field-access";
import {
  screenMidgardForcedTxAdmission,
  validateMidgardConsensusTxCbor,
} from "@al-ft/midgard-core/consensus-validation";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import { deriveTxOrderMaterial } from "@al-ft/midgard-sdk";
import { CML } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  buildValidationMachineLedgerInsertOp,
  buildValidationMachineLedgerMutationSteps,
} from "../src/index.js";
import { buildDeterministicValidationMachineTrace } from "./helpers/trace-commit-golden.js";
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
  blockTimeMs: 1_750_000_000_000,
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
      expect(screenMidgardForcedTxAdmission(transaction.tx)).toBeNull();
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
          l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
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
      let rawFieldCarriageBytes = 0;
      for (const witness of trace.witnesses) {
        if (witness.auxiliary && "fieldPreimage" in witness.auxiliary)
          rawFieldCarriageBytes += witness.auxiliary.fieldPreimage.length;
      }
      if (process.env.MIDGARD_PRINT_PROOF_FIT === "1") {
        console.info(
          JSON.stringify({
            signatureScan: {
              count,
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

// Fixed public seed vectors encoded independently with definite CBOR and
// signed over MidgardNativeTxBodyV1 || 01 || bodyCompact using OpenSSL.
// The same full transaction bytes feed FIWI's real accepted-source chain.
describe("source-bound FIWI witness-order oracle", () => {
  it.each([
    [
      "descending",
      "84018c582981582682582011111111111111111111111111111111111111111111111111111111111111111900004180582c815829a200581d600d6a577e9441ad8ed9663931906e4d43ece8f82c712b1d0235affb0601821a004c4b40a0002020418041804180582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53008358cf8258658258208a88e3dd7409f195fd52db2d3cba5d72ca6709bf1d94121bf3748801b40f6f5c584094da6e57c26fd188c52a4b23b2801f9c2fad870b15b2d289a5189141d8e18ada511c79dc9f7b32351c2a90aaafccb85bc9785189433ebc38c5ad8482d7b79e0158658258208139770ea87d175f56a35466c34c7ecccb8d8a91b4ee37a25df60f5b8fc9b3945840aa08f5f3164fbe3da499efa9bc02d9ccb302c34e05b0089e82f2ffb30cbbb4732e9beaf9ce196bb62959de0f05e7c73de24f8d5d266758a74956d011b88e1a0f4180418000",
    ],
    [
      "duplicate",
      "84018c582981582682582011111111111111111111111111111111111111111111111111111111111111111900004180582c815829a200581d600d6a577e9441ad8ed9663931906e4d43ece8f82c712b1d0235affb0601821a004c4b40a0002020418041804180582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53008358cf8258658258208a88e3dd7409f195fd52db2d3cba5d72ca6709bf1d94121bf3748801b40f6f5c584094da6e57c26fd188c52a4b23b2801f9c2fad870b15b2d289a5189141d8e18ada511c79dc9f7b32351c2a90aaafccb85bc9785189433ebc38c5ad8482d7b79e0158658258208a88e3dd7409f195fd52db2d3cba5d72ca6709bf1d94121bf3748801b40f6f5c584094da6e57c26fd188c52a4b23b2801f9c2fad870b15b2d289a5189141d8e18ada511c79dc9f7b32351c2a90aaafccb85bc9785189433ebc38c5ad8482d7b79e014180418000",
    ],
  ])(
    "accepts independently signed %s physical witnesses",
    async (_shape, hex) => {
      const bytes = Buffer.from(hex, "hex");
      const transaction = decodeMidgardNativeTxFullFromCanonicalCbor(bytes);
      const { transactionId } =
        deriveMidgardNativeTxFaultEvidenceMaterial(bytes);
      const spent = Buffer.from(
        "8258201111111111111111111111111111111111111111111111111111111111111111190000",
        "hex",
      );
      const output = Buffer.from(
        "a200581d600d6a577e9441ad8ed9663931906e4d43ece8f82c712b1d0235affb0601821a004c4b40a0",
        "hex",
      );
      expect(validateMidgardConsensusTxCbor(bytes)).toBeNull();
      expect(screenMidgardForcedTxAdmission(transaction)).toBeNull();
      const expectedLedgerOps = [
        { type: "delete" as const, key: spent },
        buildValidationMachineLedgerInsertOp({
          key: outRefFromTxId(transactionId),
          outputCbor: output,
        }),
      ];
      const ledgerMutationSteps =
        await buildValidationMachineLedgerMutationSteps({
          initialEntries: [{ outRef: spent, output }],
          operations: expectedLedgerOps,
        });
      const trace = await Effect.runPromise(
        buildDeterministicValidationMachineTrace({
          l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
          ...context,
          transactionId,
          canonicalTransactionCbor: bytes,
          priorUtxosRoot: ledgerMutationSteps[0]!.preRoot.toString("hex"),
          postUtxosRoot: ledgerMutationSteps.at(-1)!.postRoot.toString("hex"),
          ledgerWitnessEntries: [{ outRef: spent, output }],
          expectedLedgerOps,
          ledgerMutationSteps,
          expectedVerdict: "accepted",
          expectedRejectionCode: null,
        }),
      );
      expect(trace.verdict).toBe("accepted");
      const scans = trace.witnesses.filter(
        (witness) => witness.phase === "signatures",
      );
      expect(scans).toHaveLength(4);
      // This assertion is deliberately after actual admission and signature replay.
      expect(locateMidgardTransactionFieldItemFault(transaction)).toBeNull();
    },
  );
});
