import { computeHash28 } from "@al-ft/midgard-core";
import { encodeMidgardCekProgramMaterialSidecar } from "@al-ft/midgard-core/cek-proof";
import {
  decodeSingleCbor,
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
  validateMidgardConsensusForcedTxCbor,
  validateMidgardConsensusTxCbor,
} from "@al-ft/midgard-core/consensus-validation";
import { aikenSerialisedPlutusDataCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import {
  hashMidgardValidationRejectionCode,
  MidgardValidationPhase,
} from "@al-ft/midgard-core/validation-trace";
import * as SDK from "@al-ft/midgard-sdk";
import {
  buildDeterministicValidationMachineTrace,
  RejectCodes,
} from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";
import {
  computeDaPayloadUtxosRoot,
  decodeDaPayloadStrict,
  validateDaPayloadEventProgramCoverage,
} from "da-committee-node/da/payload";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { ForcedTransactionsDB } from "../src/database/index.js";
import {
  buildDeterministicValidationTraceMembers,
  classifyForcedTransactions,
} from "../src/mpf/index.js";
import {
  canonicalTransaction,
  forcedEntry,
  makeSignedEffectfulTransaction,
  outputReferenceFromHash,
  TEST_ADDRESS,
} from "./forced-transactions.make-signed-effectful-transaction.js";

const sidecar = encodeMidgardCekProgramMaterialSidecar([]);
const time = new Date("2026-07-23T12:01:00.000Z");
const validation = {
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  bucketConcurrency: 1,
  slotForUnixTime: () => 100n,
};
const forced = (tx: ReturnType<typeof canonicalTransaction>) =>
  encodeMidgardForcedTxCanonical(materializeMidgardForcedTxFromCanonical(tx));
const oversizedOutput = encodeMidgardTxOutput({
  address: TEST_ADDRESS,
  value: { lovelace: 100_000_000n, assets: new Map() },
  datum: {
    kind: "inline",
    cbor: Buffer.from(
      aikenSerialisedPlutusDataCbor(Data.to("ab".repeat(17_000))),
      "hex",
    ),
  },
});
const input = outputReferenceFromHash(Buffer.alloc(32, 0x31));

/** This composition test needs the ingest encoder, node classification, real
 * trace builder and committee decoder. Codec unit tests cannot catch a consumer
 * retaining the removed forced screen or fabricating a different terminal.
 * Full node retention above the Inline tier is a separately recorded transport
 * gap: this test retains the machine's genuine endpoints, never fake carriage. */
describe("forced admission screens", () => {
  it.each([
    ["large", true],
    ["large", false],
    ["malformed native", true],
  ] as const)(
    "replays the genuine %s machine verdict with input present=%s",
    async (shape, present) => {
      expect(oversizedOutput.length).toBeGreaterThan(
        MIDGARD_CONSENSUS_LIMITS.maxLedgerOutputPreimageBytes,
      );
      const output =
        shape === "large"
          ? oversizedOutput
          : encodeMidgardTxOutput({
              address: TEST_ADDRESS,
              value: { lovelace: 100_000_000n, assets: new Map() },
            });
      const accepts = shape === "large" && present;
      const signed = makeSignedEffectfulTransaction(input, output);
      const transaction =
        shape === "large"
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
        validateMidgardConsensusForcedTxCbor(transaction.canonicalCbor),
      ).toBeNull();
      const orderMaterial = SDK.deriveTxOrderMaterial({
        submittedTxCbor: transaction.canonicalCbor,
        owner: Buffer.alloc(28),
      });
      expect(orderMaterial.transactionId).toBe(
        transaction.transactionId.toString("hex"),
      );
      if (shape === "large")
        expect(
          orderMaterial.carriage.find((field) => field.fieldIndex === 2)?.plan
            .tier,
        ).toBe("Certified");
      else
        expect(() =>
          validateMidgardConsensusTxCbor(
            encodeCbor([
              ...(decodeSingleCbor(transaction.canonicalCbor) as unknown[]),
              0n,
            ]),
          ),
        ).toThrow();
      const spentOutput =
        shape === "large"
          ? output
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
          effectiveEndTime: time,
          consensusProfile: MIDGARD_CONSENSUS_PROFILE,
          validation,
          resolveProgramMaterialSidecar: () => Effect.succeed(sidecar),
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
          : { WitnessNativeScriptMalformed: { script_index: 0n } };
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
        (await computeDaPayloadUtxosRoot(
          [...initialState.entries()].map(([key, value]) => [
            key,
            value.toString("hex"),
          ]),
        ));
      const postRoot =
        result.ledgerMutationSteps.at(-1)?.postRoot.toString("hex") ??
        priorRoot;
      const trace = await Effect.runPromise(
        buildDeterministicValidationMachineTrace({
          consensusProfile: MIDGARD_CONSENSUS_PROFILE,
          eventKeyCbor: Buffer.from(Data.to(eventKey, SDK.EventKey), "hex"),
          transactionId: transaction.transactionId,
          canonicalTransactionCbor: transaction.canonicalCbor,
          programMaterialSidecarCbor: sidecar,
          sourceKind: "forced",
          priorUtxosRoot: priorRoot,
          postUtxosRoot: postRoot,
          ledgerWitnessEntries: result.ledgerWitnessEntries,
          ledgerMutationSteps: result.ledgerMutationSteps,
          expectedLedgerOps: result.ledgerOps,
          expectedVerdict: accepts ? "accepted" : "rejected",
          expectedRejectionCode: result.rejectionCode,
          scriptEvaluations: result.scriptEvaluations,
          blockEndTimeMs: time.getTime(),
          expectedNetworkId: 0n,
          minFeeA: 0n,
          minFeeB: 0n,
          blockSlot: 100n,
        }),
      );
      const descriptor = SDK.validationTraceDescriptorDataFromCore(
        trace.tree.descriptor,
      );
      const witnesses = (["initial", "terminal"] as const).map((endpoint) => {
        const stateIndex = endpoint === "initial" ? 0 : trace.states.length - 1;
        const source = trace.witnesses[stateIndex]!;
        return [
          SDK.encodeRetainedValidationWitnessKey({
            event_key: eventKey,
            execution_index: SDK.retainedValidationEndpointCoordinate(
              descriptor.step_count,
              endpoint,
            ),
          }).toString("hex"),
          SDK.encodeRetainedValidationWitness({
            machine_state: SDK.validationMachineStateDataFromCore(
              trace.states[stateIndex]!,
            ),
            trace_proof: SDK.validationTraceProofDataFromCore(
              trace.tree.proofs[stateIndex]!,
            ),
            phase:
              endpoint === "initial"
                ? -1n
                : BigInt(MidgardValidationPhase[source.phase]),
            program_counter: BigInt(source.programCounter),
            witness_cbor: (endpoint === "initial"
              ? trace.validationContextCbor
              : source.cbor
            ).toString("hex"),
            auxiliary: "NoAuxiliaryWitness",
          }).toString("hex"),
        ] satisfies SDK.DaPayloadEntry;
      });
      let member = {
        eventKey,
        value: descriptor,
        keyCbor: Buffer.from(Data.to(eventKey, SDK.EventKey), "hex"),
        valueCbor: Buffer.from(
          Data.to(descriptor, SDK.ValidationTraceDescriptor),
          "hex",
        ),
        witnesses,
      };
      if (shape === "malformed native") {
        const [retained] = await Effect.runPromise(
          buildDeterministicValidationTraceMembers({
            consensusProfile: MIDGARD_CONSENSUS_PROFILE,
            blockEndTime: time,
            expectedNetworkId: 0n,
            minFeeA: 0n,
            minFeeB: 0n,
            blockSlot: 100n,
            transactions: [
              {
                eventKey,
                transactionId: transaction.transactionId,
                canonicalTransactionCbor: transaction.canonicalCbor,
                programMaterialSidecarCbor: sidecar,
                sourceKind: "forced",
                priorUtxosRoot: priorRoot,
                postUtxosRoot: postRoot,
                ledgerOps: result.ledgerOps,
                ledgerWitnessEntries: result.ledgerWitnessEntries,
                ledgerMutationSteps: result.ledgerMutationSteps,
                verdict: "rejected",
                rejectionCode: expectedCode,
                scriptEvaluations: result.scriptEvaluations,
              },
            ],
          }),
        );
        member = {
          ...retained!,
          eventKey,
          witnesses: [...retained!.witnesses],
        };
      }
      const mismatched = await Effect.runPromise(
        Effect.either(
          buildDeterministicValidationMachineTrace({
            consensusProfile: MIDGARD_CONSENSUS_PROFILE,
            eventKeyCbor: Buffer.from(Data.to(eventKey, SDK.EventKey), "hex"),
            transactionId: transaction.transactionId,
            canonicalTransactionCbor: transaction.canonicalCbor,
            programMaterialSidecarCbor: sidecar,
            sourceKind: "forced",
            priorUtxosRoot: priorRoot,
            postUtxosRoot: postRoot,
            ledgerWitnessEntries: result.ledgerWitnessEntries,
            ledgerMutationSteps: result.ledgerMutationSteps,
            expectedLedgerOps: result.ledgerOps,
            expectedVerdict: accepts ? "rejected" : "accepted",
            expectedRejectionCode: accepts ? RejectCodes.InputNotFound : null,
            scriptEvaluations: result.scriptEvaluations,
            blockEndTimeMs: time.getTime(),
            expectedNetworkId: 0n,
            minFeeA: 0n,
            minFeeB: 0n,
            blockSlot: 100n,
          }),
        ),
      );
      expect(mismatched._tag).toBe("Left");
      expect(member!.value.verdict).toBe(accepts ? "Accepted" : "Rejected");
      expect(member!.value.rejection_code_hash).toBe(
        accepts
          ? "00".repeat(32)
          : hashMidgardValidationRejectionCode(expectedCode!).toString("hex"),
      );
      const endpoints = SDK.readRetainedValidationEndpoints({
        entries: member!.witnesses,
        eventKey,
        descriptor: member!.value,
      });
      expect(endpoints.initial.trace_proof.state_index).toBe(0n);
      expect(endpoints.terminal.trace_proof.state_index).toBe(
        member!.value.step_count,
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
          utxos: [],
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
          validation_trace_witnesses: [...member!.witnesses].sort(([a], [b]) =>
            a < b ? -1 : a > b ? 1 : 0,
          ),
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
      validateDaPayloadEventProgramCoverage(payload.block_body, [
        ...initialState.entries(),
      ]);
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
