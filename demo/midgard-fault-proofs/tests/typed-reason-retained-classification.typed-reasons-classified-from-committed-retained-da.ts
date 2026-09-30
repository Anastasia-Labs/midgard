import {
  MIDGARD_CONSENSUS_LIMITS,
  MIDGARD_CONSENSUS_PROFILE,
  MIDGARD_NATIVE_SCRIPT_SCAN_MAX_DEPTH,
  MIDGARD_NATIVE_SCRIPT_SCAN_MAX_NODES,
} from "@al-ft/midgard-core";
import {
  decodeMidgardNativeTxFullFromCanonicalCbor,
  decodeMidgardNativeTxProofFieldLengths,
  encodeMidgardNativeTxProofFieldLengths,
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
} from "@al-ft/midgard-core/codec";
import {
  encodeMidgardForcedTxCanonical as forcedTraceBytes,
  materializeMidgardForcedTxFromCanonical as forcedTraceView,
} from "@al-ft/midgard-core/codec/forced";
import { asLucidSchema } from "@al-ft/midgard-core/lucid-data";
import {
  EventKeySchema,
  GENESIS_HEADER_HASH,
  L2TransactionSource,
  type RejectionReason,
} from "@al-ft/midgard-sdk";
import {
  buildDeterministicValidationMachineTrace,
  buildValidationMachineLedgerInsertOp,
  buildValidationMachineLedgerMutationSteps,
} from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  DISTINCT_ASSET_ACCUMULATION_LIMIT_COMPLETE_CANONICAL_REPLAY,
  MISSING_REDEEMER_COMPLETE_CANONICAL_REPLAY,
  MISSING_SCRIPT_SOURCE_COMPLETE_CANONICAL_REPLAY,
  UNUSED_SCRIPT_WITNESS_COMPLETE_CANONICAL_REPLAY,
} from "../src/workflow/complete-replay.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
  outRefCbor,
} from "./helpers/canonical-block-evidence-fixture.js";
import { buildWidthForcedFixture } from "./support/field-item-width-illegal-shapes.js";
import {
  honestAddressWitness,
  invalidAddressWitness,
} from "./support/invalid-signature-emulator.js";
import { buildMissingRedeemerFixture } from "./support/missing-redeemer-emulator.js";
import { buildMissingScriptSourceFixture } from "./support/missing-script-source-emulator.js";
import {
  buildRetainedValidationBlockFixture,
  classifyRetainedReasonFixture,
  retainRejectedValidationTrace,
} from "./support/retained-reason-classifier.js";
import { buildUnusedScriptWitnessFixture } from "./support/unused-script-witness-emulator.js";
import { cases } from "./typed-reason-retained-classification.cases.js";
import { ordinaryMachineCases } from "./typed-reason-retained-classification.ordinary-machine-cases.js";
import {
  base,
  deploymentFingerprint,
  output,
  releaseFinalityAuthority,
  signerHash,
} from "./typed-reason-retained-classification.reason-case.js";

describe("typed reasons classified from committed retained DA", () => {
  it("InputAssetAccumulationLimit: wrongful operator verdict with one input asset", async () => {
    const assetOutput = encodeMidgardTxOutput({
      address: Buffer.concat([Buffer.from([0x60]), signerHash]),
      value: {
        lovelace: 2_000_000n,
        assets: new Map([["a1".repeat(28), new Map([["01", 1n]])]]),
      },
    });
    const input = outRefCbor(71, 0n);
    const unsigned = buildFixtureTransaction({
      spendInputs: [input],
      outputs: [assetOutput],
      fee: 0n,
    });
    const transaction = buildFixtureTransaction({
      spendInputs: [input],
      outputs: [assetOutput],
      fee: 0n,
      addressWitnesses: [
        honestAddressWitness({ index: 0, txId: unsigned.txId }),
      ],
    });
    const operations = [
      { type: "delete" as const, key: input },
      buildValidationMachineLedgerInsertOp({
        key: encodeMidgardSpendInputItem({
          txId: Buffer.from(transaction.txId, "hex"),
          outputIndex: 0,
        }),
        outputCbor: assetOutput,
      }),
    ];
    const ledgerWitnessEntries = [{ outRef: input, output: assetOutput }];
    const mutations = await buildValidationMachineLedgerMutationSteps({
      initialEntries: ledgerWitnessEntries,
      operations,
    });
    const orderKey = { transactionId: "52".repeat(32), outputIndex: 0n };
    const eventKey = {
      ForcedTransactionEventKey: { tx_order_id: orderKey },
    } as const;
    const reason = {
      InputAssetAccumulationLimit: { input_index: 0n, asset_index: 0n },
    } as const;
    const priorLedgerRoot = mutations[0]!.preRoot.toString("hex");
    const trace = await Effect.runPromise(
      buildDeterministicValidationMachineTrace({
        consensusProfile: MIDGARD_CONSENSUS_PROFILE,
        eventKeyCbor: Buffer.from(
          Data.to(eventKey, asLucidSchema(EventKeySchema)),
          "hex",
        ),
        sourceKind: "forced",

        blockEndTimeMs: 1_800_000_000_000,
        expectedNetworkId: 0n,
        minFeeA: 0n,
        minFeeB: 0n,
        blockSlot: 100n,
        transactionId: Buffer.from(transaction.txId, "hex"),
        canonicalTransactionCbor: forcedTraceBytes(
          forcedTraceView(
            decodeMidgardNativeTxFullFromCanonicalCbor(
              transaction.canonicalCbor,
            ),
          ),
        ),
        priorUtxosRoot: priorLedgerRoot,
        postUtxosRoot: mutations.at(-1)!.postRoot.toString("hex"),
        ledgerWitnessEntries,
        expectedLedgerOps: operations,
        ledgerMutationSteps: mutations,
        expectedVerdict: "accepted",
        expectedRejectionCode: null,
      }),
    );
    const fixture = await buildRetainedValidationBlockFixture({
      subject: {
        kind: "forced",
        nativeTx: decodeMidgardNativeTxFullFromCanonicalCbor(
          transaction.canonicalCbor,
        ),
        orderKey,
        verdict: { ForcedTxInvalid: { reason } },
      },
      priorLedgerRoot,
      ...retainRejectedValidationTrace({ trace, eventKey, reason }),
      blockEndTimeMs: 1_800_000_000_000,
    });
    const { decision } = await classifyRetainedReasonFixture({
      observation: authenticatedHeaderObservation(fixture),
      payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
      deploymentFingerprint,
      releaseFinalityAuthority,
      replayer: DISTINCT_ASSET_ACCUMULATION_LIMIT_COMPLETE_CANONICAL_REPLAY,
    });
    expect(decision).toMatchObject({
      decision: "fault_detected",
      category: "distinctAssetAccumulationLimit",
      headerHash: fixture.headerHash,
    });
  });
  for (const scenario of ordinaryMachineCases) {
    it(`${scenario.arm}: wrongful operator verdict from an ordinary complete machine trace`, async () => {
      const retained = await buildUnusedScriptWitnessFixture({
        direction: "forced",
        claimedVerdict: "rejected",
        accusedUnused: false,
        sourceCount: 4,
        allPurposeKinds: true,
        inputByte: 0x71,
        operatorVkey: "b1".repeat(28),
        startTime: 1_749_999_941_000n,
      });
      let reason: RejectionReason = scenario.reason;
      if (scenario.arm === "ReceivePurposePlutusV3Forbidden") {
        const receive = retained.trace.witnesses.find(
          ({ auxiliary }) =>
            auxiliary?.kind === "nativeExecutionDescriptor" &&
            auxiliary.purpose.purposeKind === 3,
        )?.auxiliary;
        if (receive?.kind !== "nativeExecutionDescriptor")
          throw new Error(
            "ordinary fixture omitted its native receive execution",
          );
        reason = {
          ReceivePurposePlutusV3Forbidden: {
            execution_index: BigInt(receive.executionIndex),
          },
        };
      }
      const fixture = await buildRetainedValidationBlockFixture({
        subject: {
          kind: "forced",
          nativeTx: retained.transaction.tx,
          orderKey: retained.orderKey,
          verdict: { ForcedTxInvalid: { reason } },
        },
        priorLedgerRoot:
          retained.block.reconstruction.traceByStepIndex.get(0n)!.value
            .pre_utxos_root,
        ...retainRejectedValidationTrace({
          trace: retained.trace,
          eventKey: retained.eventKey,
          reason,
        }),
        blockEndTimeMs: 1_750_000_001_000,
        blockSlot: 0n,
      });
      const { decision } = await classifyRetainedReasonFixture({
        observation: authenticatedHeaderObservation(fixture),
        payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
        deploymentFingerprint,
        releaseFinalityAuthority,
        replayer: scenario.replayer,
      });
      expect(decision).toMatchObject({
        decision: "fault_detected",
        category: scenario.category,
        headerHash: fixture.headerHash,
      });
    });
  }
  it.each(["accepted", "wrongful", "honest"] as const)(
    "UnusedScriptWitness: %s operator verdict under unusedScriptWitness",
    async (direction) => {
      const retained = await buildUnusedScriptWitnessFixture({
        direction: direction === "accepted" ? "accepted" : "forced",
        claimedVerdict: direction === "accepted" ? "accepted" : "rejected",
        accusedUnused: direction !== "wrongful",
        sourceCount: 2,
        inputByte: 0x71,
        operatorVkey: "b1".repeat(28),
        startTime: 1_749_999_941_000n,
      });
      const fixture = await buildRetainedValidationBlockFixture({
        subject:
          direction === "accepted"
            ? { kind: "normal", nativeTx: retained.transaction.tx }
            : {
                kind: "forced",
                nativeTx: retained.transaction.tx,
                orderKey: retained.orderKey,
                verdict: {
                  ForcedTxInvalid: {
                    reason: {
                      UnusedScriptWitness: {
                        script_index: BigInt(retained.scriptIndex),
                      },
                    },
                  },
                },
              },
        priorLedgerRoot:
          retained.block.reconstruction.traceByStepIndex.get(0n)!.value
            .pre_utxos_root,
        descriptorEntries:
          retained.retainedEntries.authenticatedValidationTraceEntries,
        retainedEntries:
          retained.retainedEntries.retainedValidationWitnessEntries,
        blockEndTimeMs: 1_750_000_001_000,
        blockSlot: 0n,
      });
      const { decision } = await classifyRetainedReasonFixture({
        observation: authenticatedHeaderObservation(fixture),
        payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
        deploymentFingerprint,
        releaseFinalityAuthority,
        replayer: UNUSED_SCRIPT_WITNESS_COMPLETE_CANONICAL_REPLAY,
      });
      expect(decision).toMatchObject(
        direction === "honest"
          ? { decision: "healthy", headerHash: fixture.headerHash }
          : {
              decision: "fault_detected",
              category: "unusedScriptWitness",
              headerHash: fixture.headerHash,
            },
      );
    },
  );
  it.each(["accepted", "wrongful", "honest"] as const)(
    "RedeemerMissing: %s operator verdict under missingRedeemer",
    async (direction) => {
      const retained = await buildMissingRedeemerFixture({
        direction: direction === "accepted" ? "accepted" : "forced",
        purposeKind: 0,
        sourceLocation: "inline",
        targetRedeemerPresent: direction === "wrongful",
      });
      const fixture = await buildRetainedValidationBlockFixture({
        subject:
          direction === "accepted"
            ? { kind: "normal", nativeTx: retained.transaction.tx }
            : {
                kind: "forced",
                nativeTx: retained.transaction.tx,
                orderKey: retained.orderKey,
                verdict: {
                  ForcedTxInvalid: { reason: retained.rejectionReason },
                },
              },
        priorLedgerRoot: "00".repeat(32),
        descriptorEntries: retained.descriptorEntries,
        retainedEntries: retained.retainedEntries,
        blockEndTimeMs: 1_800_000_000_000,
      });
      const { decision } = await classifyRetainedReasonFixture({
        observation: authenticatedHeaderObservation(fixture),
        payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
        deploymentFingerprint,
        releaseFinalityAuthority,
        replayer: MISSING_REDEEMER_COMPLETE_CANONICAL_REPLAY,
      });
      expect(decision).toMatchObject(
        direction === "honest"
          ? { decision: "healthy", headerHash: fixture.headerHash }
          : {
              decision: "fault_detected",
              category: "missingRedeemer",
              headerHash: fixture.headerHash,
            },
      );
    },
  );
  it.each(["accepted", "wrongful", "honest"] as const)(
    "ScriptSourceMissing: %s operator verdict under missingScriptSource",
    async (direction) => {
      const retained = await buildMissingScriptSourceFixture({
        direction: direction === "accepted" ? "accepted" : "forced",
        purposeKind: 0,
        presentAt: direction === "wrongful" ? "inline" : "absent",
        inlineDecoys: direction === "wrongful" ? 1 : 0,
        referenceDecoys: 0,
      });
      const fixture = await buildRetainedValidationBlockFixture({
        subject:
          direction === "accepted"
            ? { kind: "normal", nativeTx: retained.transaction.tx }
            : {
                kind: "forced",
                nativeTx: retained.transaction.tx,
                orderKey: retained.orderKey,
                verdict: { ForcedTxInvalid: { reason: retained.reason } },
              },
        priorLedgerRoot: retained.priorLedgerRoot,
        descriptorEntries: retained.descriptorEntries,
        retainedEntries: retained.retainedEntries,
        blockEndTimeMs: 1_750_000_000_000,
      });
      const { decision } = await classifyRetainedReasonFixture({
        observation: authenticatedHeaderObservation(fixture),
        payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
        deploymentFingerprint,
        releaseFinalityAuthority,
        replayer: MISSING_SCRIPT_SOURCE_COMPLETE_CANONICAL_REPLAY,
      });
      expect(decision).toMatchObject(
        direction === "honest"
          ? { decision: "healthy", headerHash: fixture.headerHash }
          : {
              decision: "fault_detected",
              category: "missingScriptSource",
              headerHash: fixture.headerHash,
            },
      );
    },
  );
  it("proves admitted script bytes cannot reach the node/depth rejection limits", () => {
    // Every native-script node uses at least three CBOR bytes; depth also
    // requires at least one distinct node per level, before outer wrappers.
    for (const scanLimit of [
      MIDGARD_NATIVE_SCRIPT_SCAN_MAX_NODES,
      MIDGARD_NATIVE_SCRIPT_SCAN_MAX_DEPTH,
    ]) {
      expect((scanLimit + 1) * 3).toBeGreaterThan(
        MIDGARD_CONSENSUS_LIMITS.maxScriptWitnessesPreimageBytes,
      );
      expect((scanLimit + 1) * 3).toBeGreaterThan(
        MIDGARD_CONSENSUS_LIMITS.maxLedgerOutputPreimageBytes,
      );
    }
  });
  for (const scenario of cases) {
    it.each(
      scenario.acceptedApplicability !== undefined
        ? (["wrongful"] as const)
        : scenario.mismatchFieldIndex !== undefined ||
            scenario.minFeeB !== undefined
          ? (["accepted", "wrongful"] as const)
          : (["accepted", "wrongful", "honest"] as const),
    )(
      `${scenario.arm}: %s operator verdict under ${scenario.category}`,
      async (direction) => {
        let transaction = buildFixtureTransaction({
          ...base,
          ...scenario[direction === "wrongful" ? "wrongful" : "accepted"],
        });
        const witness =
          direction === "wrongful"
            ? scenario.wrongfulWitness
            : scenario.acceptedWitness;
        if (witness !== undefined)
          transaction = buildFixtureTransaction({
            ...base,
            ...scenario[direction === "wrongful" ? "wrongful" : "accepted"],
            addressWitnesses: [
              witness === "valid"
                ? honestAddressWitness({ index: 0, txId: transaction.txId })
                : invalidAddressWitness(0),
            ],
          });
        if (
          direction === "accepted" &&
          scenario.mismatchFieldIndex !== undefined
        ) {
          const lengths = [
            ...decodeMidgardNativeTxProofFieldLengths(
              Buffer.from(
                transaction.source.source.field_preimage_lengths_cbor,
                "hex",
              ),
            ),
          ];
          lengths[scenario.mismatchFieldIndex] =
            lengths[scenario.mismatchFieldIndex]! + 1;
          const source = {
            ...transaction.source,
            source: {
              ...transaction.source.source,
              field_preimage_lengths_cbor:
                encodeMidgardNativeTxProofFieldLengths(lengths).toString("hex"),
            },
          };
          transaction = {
            ...transaction,
            source,
            sourceValueBytes: Buffer.from(
              Data.to(source, L2TransactionSource),
              "hex",
            ),
          };
        }
        const predecessor = scenario.priorLedger
          ? await buildCanonicalBlockFixture({
              transactions: [],
              prevHeaderHash: GENESIS_HEADER_HASH,
              utxos: [
                {
                  key: outRefCbor(71, 0n),
                  value: scenario.priorOutput ?? output(),
                },
              ],
            })
          : undefined;
        const fixture =
          direction === "accepted" && scenario.arm === "ValueNotPreserved"
            ? await buildRetainedValidationBlockFixture({
                subject: {
                  kind: "normal",
                  nativeTx: decodeMidgardNativeTxFullFromCanonicalCbor(
                    transaction.canonicalCbor,
                  ),
                },
                priorLedgerRoot: predecessor!.header.utxosRoot,
                prevHeaderHash: predecessor!.headerHash,
                blockEndTimeMs: 1_900_000_000_000,
                blockSlot: 0n,
              })
            : direction === "accepted"
              ? await buildCanonicalBlockFixture({
                  transactions: [transaction],
                  minFeeB: scenario.minFeeB,
                  prevHeaderHash: predecessor?.headerHash,
                  prevUtxosRoot: predecessor?.header.utxosRoot,
                })
              : await buildWidthForcedFixture({
                  operatorVkey: "b1".repeat(28),
                  now: 1_900_000_000_000,
                  nativeTx: decodeMidgardNativeTxFullFromCanonicalCbor(
                    transaction.canonicalCbor,
                  ),
                  rejectionReason: scenario.reason,
                  finalOutputCbor: output(),
                  priorUtxosRoot: predecessor?.header.utxosRoot,
                  prevHeaderHash: predecessor?.headerHash,
                });
        const { decision } = await classifyRetainedReasonFixture({
          observation: authenticatedHeaderObservation(fixture),
          payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
          deploymentFingerprint,
          releaseFinalityAuthority,
          replayer: scenario.replayer,
          ...(predecessor === undefined
            ? {}
            : {
                predecessor: {
                  observation: authenticatedHeaderObservation(predecessor),
                  payloadEnvelopeCbor: predecessor.payloadEnvelopeCbor,
                },
              }),
        });
        expect(decision).toMatchObject(
          direction === "honest"
            ? { decision: "healthy", headerHash: fixture.headerHash }
            : {
                decision: "fault_detected",
                category: scenario.category,
                headerHash: fixture.headerHash,
              },
        );
        expect(decision.decisionDigest).toMatch(/^[0-9a-f]{64}$/u);
      },
    );
  }
});
