import { computeHash28 } from "@al-ft/midgard-core";
import { encodeMidgardCekProgramMaterialSidecar } from "@al-ft/midgard-core/cek-proof";
import {
  decodeSingleCbor,
  deriveMidgardForcedTxFaultEvidenceMaterial,
  encodeCbor,
  encodeMidgardForcedTxCanonical,
  encodeMidgardTxOutput,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core/codec";
import {
  MIDGARD_CONSENSUS_LIMITS,
  MIDGARD_CONSENSUS_PROFILE,
} from "@al-ft/midgard-core/consensus-profile";
import {
  screenMidgardForcedTxAdmission,
  validateMidgardConsensusTxCbor,
} from "@al-ft/midgard-core/consensus-validation";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import { hashMidgardValidationRejectionCode } from "@al-ft/midgard-core/validation-trace";
import * as SDK from "@al-ft/midgard-sdk";
import {
  RejectCodes,
  replayValidationVerdictEvent,
} from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";
import { decodeDaPayloadStrict } from "da-committee-node/da/payload";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { ForcedTransactionsDB } from "../src/database/index.js";
import {
  buildPlaceholderValidationTraceMembers,
  classifyForcedTransactions,
  computeUtxoPayloadRoot,
} from "../src/mpf/index.js";
import {
  canonicalTransaction,
  forcedEntry,
  makeInlineDatumOutput,
  makeOutput,
  makeSignedEffectfulTransaction,
  outputReferenceFromHash,
} from "./forced-transactions.make-signed-effectful-transaction.js";

const sidecar = encodeMidgardCekProgramMaterialSidecar([]);
const time = new Date("2026-07-23T12:01:00.000Z");
const validation = {
  l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  bucketConcurrency: 1,
  slotForUnixTime: () => 100n,
};
const forced = (tx: ReturnType<typeof canonicalTransaction>) =>
  encodeMidgardForcedTxCanonical(materializeMidgardForcedTxFromCanonical(tx));
const oversizedOutput = makeInlineDatumOutput(
  100_000_000n,
  "ab".repeat(17_000),
);
const input = outputReferenceFromHash(Buffer.alloc(32, 0x31));
// Individually legal outputs require Certified aggregate carriage, funded by input.
const legalLargeOutput = makeInlineDatumOutput(50_000_000n, "ab".repeat(8_500));
const fundedInputOutput = makeOutput(100_000_000n);

/** This composition test needs the ingest encoder, node classification, the
 * shared verdict replay and committee decoder. Codec unit tests cannot catch a
 * consumer retaining the removed forced screen or committing a different
 * verdict. */
describe("forced admission screens", () => {
  it.each([
    ["large", true],
    ["large", false],
    ["malformed native", true],
    ["oversized output", true],
  ] as const)(
    "replays the genuine %s machine verdict with input present=%s",
    async (shape, present) => {
      expect(oversizedOutput.length).toBeGreaterThan(
        MIDGARD_CONSENSUS_LIMITS.maxLedgerOutputPreimageBytes,
      );
      const output =
        shape === "large"
          ? legalLargeOutput
          : shape === "oversized output"
            ? oversizedOutput
            : fundedInputOutput;
      const accepts = shape === "large" && present;
      const signed = makeSignedEffectfulTransaction(input, output, {
        additionalOutputs: shape === "large" ? [output] : [],
      });
      const transaction =
        shape !== "malformed native"
          ? signed
          : {
              ...signed,
              canonicalCbor: forced({
                ...signed.transaction,
                witnessSet: {
                  ...signed.transaction.witnessSet,
                  scriptTxWitsPreimageCbor: encodeCbor([
                    Buffer.from("820043820700", "hex"),
                  ]),
                },
              }),
            };
      expect(
        screenMidgardForcedTxAdmission(
          deriveMidgardForcedTxFaultEvidenceMaterial(transaction.canonicalCbor)
            .canonical,
        ),
      ).toBeNull();
      const orderMaterial = SDK.deriveTxOrderMaterial({
        submittedTxCbor: transaction.canonicalCbor,
        owner: Buffer.alloc(28),
      });
      expect(orderMaterial.transactionId).toBe(
        transaction.transactionId.toString("hex"),
      );
      if (shape === "large") {
        expect(output.length).toBeLessThanOrEqual(
          MIDGARD_CONSENSUS_LIMITS.maxLedgerOutputPreimageBytes,
        );
        expect(
          signed.transaction.body.outputsPreimageCbor.length,
        ).toBeGreaterThan(
          MIDGARD_CONSENSUS_LIMITS.maxLedgerOutputPreimageBytes,
        );
        expect(
          orderMaterial.carriage.find((field) => field.fieldIndex === 2)?.plan
            .tier,
        ).toBe("Certified");
      } else if (shape === "malformed native")
        expect(() =>
          validateMidgardConsensusTxCbor(
            encodeCbor([
              ...(decodeSingleCbor(transaction.canonicalCbor) as unknown[]),
              0n,
            ]),
          ),
        ).toThrow();
      const spentOutput =
        shape !== "malformed native"
          ? fundedInputOutput
          : encodeMidgardTxOutput({
              address: Buffer.concat([
                Buffer.from([0x70]),
                computeHash28(
                  Buffer.concat([
                    Buffer.from([0]),
                    Buffer.from("820700", "hex"),
                  ]),
                ),
              ]),
              value: { lovelace: 100_000_000n, assets: new Map() },
            });
      const initialState = new Map(
        present ? [[input.toString("hex"), spentOutput]] : [],
      );
      const [classified] = await Effect.runPromise(
        classifyForcedTransactions({
          entries: [await forcedEntry({ label: 8, transaction })],
          initialState,
          blockTime: time,
          consensusProfile: MIDGARD_CONSENSUS_PROFILE,
          validation,
        }),
      );
      expect(classified).toBeDefined();
      const result = classified!;
      const leaf = Data.from(
        result.entry[
          ForcedTransactionsDB.Columns.FORCED_INCLUSION_VALUE
        ].toString("hex"),
        SDK.ForcedInclusionTxV1,
      );
      const expectedReason: SDK.RejectionReason =
        shape === "large"
          ? { InputNotFound: { source_kind: 0n, input_index: 0n } }
          : shape === "malformed native"
            ? { WitnessNativeScriptMalformed: { script_index: 0n } }
            : { FieldItemWidthIllegal: { field_index: 2n, item_index: 0n } };
      const expectedCode = accepts
        ? null
        : shape === "large"
          ? RejectCodes.InputNotFound
          : RejectCodes.InvalidFieldType;
      expect(leaf.verdict).toEqual(
        accepts
          ? "ForcedTxValid"
          : { ForcedTxInvalid: { reason: expectedReason } },
      );
      expect(result.rejectionCode).toBe(expectedCode);
      const orderKey =
        result.entry[ForcedTransactionsDB.Columns.TX_ORDER_ID].toString("hex");
      const eventKey: SDK.EventKey = {
        ForcedTransactionEventKey: {
          tx_order_id: Data.from(orderKey, SDK.OutputReference),
        },
      };
      const priorRoot =
        result.ledgerMutationSteps[0]?.preRoot.toString("hex") ??
        (await Effect.runPromise(
          computeUtxoPayloadRoot(
            [...initialState.entries()].map(([key, value]) => ({
              outref: Buffer.from(key, "hex"),
              output: value,
            })),
          ),
        ));
      const postRoot =
        result.ledgerMutationSteps.at(-1)?.postRoot.toString("hex") ??
        priorRoot;
      // The shared Phase A/B verdict replay agrees with the classification.
      const replay = await Effect.runPromise(
        replayValidationVerdictEvent({
          l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
          consensusProfile: MIDGARD_CONSENSUS_PROFILE,
          canonicalTransactionCbor: transaction.canonicalCbor,
          programMaterialSidecarCbor: sidecar,
          sourceKind: "forced",
          ledgerWitnessEntries: result.ledgerWitnessEntries,
          blockTimeMs: time.getTime(),
          expectedNetworkId: 0n,
          minFeeA: 0n,
          minFeeB: 0n,
          blockSlot: 100n,
        }),
      );
      expect(replay.verdict).toBe(accepts ? "accepted" : "rejected");
      expect(replay.rejection?.code ?? null).toBe(expectedCode);
      const [member] = await Effect.runPromise(
        buildPlaceholderValidationTraceMembers({
          transactions: [
            {
              eventKey,
              canonicalTransactionCbor: transaction.canonicalCbor,
              sourceKind: "forced",
              verdict: accepts ? "accepted" : "rejected",
              rejectionCode: expectedCode,
            },
          ],
        }),
      );
      expect(member!.value.verdict).toBe(accepts ? "Accepted" : "Rejected");
      expect(member!.value.rejection_code_hash).toBe(
        accepts
          ? "00".repeat(32)
          : hashMidgardValidationRejectionCode(expectedCode!).toString("hex"),
      );
      const eventCbor = Data.to(eventKey, SDK.EventKey);
      const counts = {
        withdrawalCount: 0n,
        forcedTransactionCount: 1n,
        l2TransactionCount: 0n,
        depositCount: 0n,
        totalEventCount: 1n,
        transitionStepCount: 1n,
        validationTraceCount: 1n,
      };
      const header: SDK.Header = {
        blockSlot: 100n,
        expectedNetworkId: 0n,
        minFeeA: 0n,
        minFeeB: 0n,
        prevHeaderHash: "00".repeat(28),
        operatorVkey: "00".repeat(28),
        startTime: BigInt(time.getTime() - 1),
        endTime: BigInt(time.getTime()),
        prevUtxosRoot: priorRoot,
        utxosRoot: postRoot,
        transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
        depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
        withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
        forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
        transitionTraceRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
        eventToStepRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
        validationTracesRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
        protocolVersion: 1n,
        ...counts,
      };
      const payload: SDK.DaPayload = {
        version: 1n,
        block_body: {
          header,
          header_hash: await Effect.runPromise(SDK.hashBlockHeader(header)),
          counts,
          transactions: [],
          transaction_preimages: [],
          forced_transactions: [
            [
              orderKey,
              result.entry[
                ForcedTransactionsDB.Columns.FORCED_INCLUSION_VALUE
              ].toString("hex"),
            ],
          ],
          forced_transaction_preimages: [
            [orderKey, transaction.canonicalCbor.toString("hex")],
          ],
          deposits: [],
          withdrawals: [],
          transition_trace: [
            [
              Data.to(0n),
              Data.to(
                {
                  schema_version: 1n,
                  step_index: 0n,
                  event_key: eventKey,
                  phase: "ForcedTransaction",
                  pre_utxos_root: priorRoot,
                  post_utxos_root: postRoot,
                },
                SDK.TransitionStep,
              ),
            ],
          ],
          event_to_step: [
            [
              eventCbor,
              Data.to(
                { step_index: 0n, phase: "ForcedTransaction" },
                SDK.EventToStepValue,
              ),
            ],
          ],
          validation_traces: [
            [
              member!.keyCbor.toString("hex"),
              member!.valueCbor.toString("hex"),
            ],
          ],
          validation_trace_witnesses: [],
          cek_program_material: [],
        },
      };
      const decoded = decodeDaPayloadStrict(SDK.encodeDaPayload(payload));
      expect(decoded.block_body.forced_transactions).toEqual(
        payload.block_body.forced_transactions,
      );
      expect(decoded.block_body.validation_traces).toEqual(
        payload.block_body.validation_traces,
      );
      payload.block_body.forced_transactions = [
        [
          orderKey,
          Data.to(
            {
              ...leaf,
              verdict: accepts
                ? { ForcedTxInvalid: { reason: "EmptyInputs" } }
                : "ForcedTxValid",
            },
            SDK.ForcedInclusionTxV1,
          ),
        ],
      ];
      expect(() => decodeDaPayloadStrict(SDK.encodeDaPayload(payload))).toThrow(
        /verdict/,
      );
      payload.block_body.forced_transactions = [
        [orderKey, Data.to(leaf, SDK.ForcedInclusionTxV1)],
      ];
      payload.block_body.forced_transaction_preimages = [
        [
          orderKey,
          Buffer.concat([transaction.canonicalCbor, Buffer.from([0])]).toString(
            "hex",
          ),
        ],
      ];
      expect(() =>
        decodeDaPayloadStrict(SDK.encodeDaPayload(payload)),
      ).toThrow();
    },
  );
});
