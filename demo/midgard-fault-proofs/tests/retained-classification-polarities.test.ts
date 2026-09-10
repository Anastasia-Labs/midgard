import {
  computeMidgardNativeTxId,
  deriveMidgardNativeTxBodyCompact,
  MIDGARD_CONSENSUS_PROFILE,
} from "@al-ft/midgard-core";
import {
  decodeMidgardNativeTxFullFromCanonicalCbor,
  decodeMidgardNativeTxProofFieldLengths,
  encodeMidgardNativeTxProofFieldLengths,
} from "@al-ft/midgard-core/codec";
import { asLucidSchema } from "@al-ft/midgard-core/lucid-data";
import {
  decodeRetainedValidationWitnessKey,
  EventKeySchema,
} from "@al-ft/midgard-sdk";
import {
  buildDeterministicValidationMachineTrace,
  buildValidationMachineLedgerInsertOp,
  buildValidationMachineLedgerMutationSteps,
} from "@al-ft/midgard-validation";
import {
  encodeByteList,
  encodeRecomputedNativeTx,
  FUNDED_OUTPUT_LOVELACE,
  makeNativeTx,
  makeOutput,
  outRefFromByte,
  outRefFromTxId,
} from "@al-ft/midgard-validation/tests/validation-fixtures";
import { CML, Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  EXECUTION_SOURCE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
  FIELD_PREIMAGE_LENGTH_MISMATCH_COMPLETE_CANONICAL_REPLAY,
  MIN_FEE_COMPLETE_CANONICAL_REPLAY,
  NATIVE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
  RECEIVE_PURPOSE_LANGUAGE_COMPLETE_CANONICAL_REPLAY,
  RESOLVED_OUTPUT_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY,
  SCRIPT_INTEGRITY_HASH_MISMATCH_COMPLETE_CANONICAL_REPLAY,
} from "../src/workflow/complete-replay.js";
import {
  computeFraudProofReleaseFinalityPolicyDigest,
  FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
} from "../src/workflow/release-finality-policy.js";
import {
  authenticatedHeaderObservation,
  buildFixtureTransaction,
  outRefCbor,
  reencodeFixturePayload,
} from "./helpers/canonical-block-evidence-fixture.js";
import {
  buildSubjectFixture,
  emptyAllScript,
  rawNativeItem,
} from "./support/execution-source-script-decoding-emulator.js";
import { buildWidthForcedFixture } from "./support/field-item-width-illegal-shapes.js";
import { nativeDecodingFixture } from "./support/native-script-decoding-retained.js";
import {
  buildReceivePurposeFixture,
  receivePurposeReason,
} from "./support/receive-purpose-language-emulator.js";
import {
  buildRetainedValidationBlockFixture,
  classifyRetainedReasonFixture,
  retainValidationTrace,
} from "./support/retained-reason-classifier.js";

const deploymentFingerprint = "d1".repeat(32);
const policy = {
  confirmationDepth: 30,
  automaticRecoveryMaxDepth: 2160,
  deepRollbackPolicy: "automated_rewind_replay_incident-v1",
} as const;
const releaseFinalityAuthority = {
  authorityVersion: FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
  verifyForWorkflow: async () => ({
    schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
    deploymentIdentityDigest: deploymentFingerprint,
    blueprintHash: "e1".repeat(32),
    policyDigest: computeFraudProofReleaseFinalityPolicyDigest(policy),
    policy,
  }),
};

// These are the same small transaction inputs used by the original retained
// reason suite. Only the committed fee context or field-length declaration
// changes; no boundary-sized transaction or detector mock is needed.
const ordinaryTransaction = () =>
  buildFixtureTransaction({
    spendInputs: [outRefCbor(71, 0n)],
    fee: 7n,
    networkId: 0n,
  });

describe("ordinary retained classification polarities", () => {
  it.each(["honest", "wrongful"] as const)(
    "FeeBelowMinimum: %s rejection uses the committed header fee context",
    async (direction) => {
      const transaction = ordinaryTransaction();
      const minFeeB = direction === "honest" ? 8n : 0n;
      const fixture = await buildWidthForcedFixture({
        operatorVkey: "b1".repeat(28),
        now: 1_900_000_000_000,
        nativeTx: decodeMidgardNativeTxFullFromCanonicalCbor(
          transaction.canonicalCbor,
        ),
        rejectionReason: "FeeBelowMinimum",
        minFeeB,
      });
      expect(fixture.reconstruction.payload.block_body.header.minFeeB).toBe(
        minFeeB,
      );
      expect(fixture.nativeTx.body.fee).toBe(7n);
      const { decision } = await classifyRetainedReasonFixture({
        observation: authenticatedHeaderObservation(fixture),
        payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
        deploymentFingerprint,
        releaseFinalityAuthority,
        replayer: MIN_FEE_COMPLETE_CANONICAL_REPLAY,
      });
      expect(decision).toMatchObject(
        direction === "honest"
          ? { decision: "healthy", headerHash: fixture.headerHash }
          : {
              decision: "fault_detected",
              category: "minFee",
              headerHash: fixture.headerHash,
            },
      );
    },
  );

  it.each(["honest", "wrongful"] as const)(
    "FieldPreimageLengthMismatch: %s rejection respects the canonical-preimage admission boundary",
    async (direction) => {
      const transaction = ordinaryTransaction();
      const nativeTx = decodeMidgardNativeTxFullFromCanonicalCbor(
        transaction.canonicalCbor,
      );
      const lengths = [
        ...decodeMidgardNativeTxProofFieldLengths(
          Buffer.from(
            transaction.source.source.field_preimage_lengths_cbor,
            "hex",
          ),
        ),
      ];
      // Reuse the existing field-0 mismatch from the retained reason suite.
      if (direction === "honest") lengths[0] = lengths[0]! + 1;
      const fixturePromise = buildWidthForcedFixture({
        operatorVkey: "b1".repeat(28),
        now: 1_900_000_000_000,
        nativeTx,
        rejectionReason: {
          FieldPreimageLengthMismatch: { field_index: 0n },
        },
        fieldPreimageLengthsCbor:
          encodeMidgardNativeTxProofFieldLengths(lengths),
      });
      if (direction === "honest") {
        // Reconstruction requires the entire forced source, including its
        // declared lengths, to equal the canonical preimage. This cell is
        // excluded from canonical retained classification, not a healthy
        // classifier result. Preserve that guard rather than bypassing it.
        await expect(fixturePromise).rejects.toThrow(
          "Failed to authenticate forced_transactions[0] against its canonical preimage.",
        );
        return;
      }
      const fixture = await fixturePromise;
      expect(
        decodeMidgardNativeTxProofFieldLengths(
          Buffer.from(
            fixture.transaction.source.field_preimage_lengths_cbor,
            "hex",
          ),
        )[0],
      ).toBe(nativeTx.body.spendInputsPreimageCbor.length);
      const { decision } = await classifyRetainedReasonFixture({
        observation: authenticatedHeaderObservation(fixture),
        payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
        deploymentFingerprint,
        releaseFinalityAuthority,
        replayer: FIELD_PREIMAGE_LENGTH_MISMATCH_COMPLETE_CANONICAL_REPLAY,
      });
      expect(decision).toMatchObject({
        decision: "fault_detected",
        category: "fieldPreimageLengthMismatch",
        headerHash: fixture.headerHash,
      });
    },
  );

  it.each(["accepted", "honest", "wrongful"] as const)(
    "ReceivePurposePlutusV3Forbidden: %s operator verdict with one ordinary receive purpose",
    async (direction) => {
      const retained = await buildReceivePurposeFixture({
        direction: direction === "accepted" ? "accepted" : "forced",
        language: direction === "wrongful" ? "native" : "plutusV3",
        claimedVerdict: direction === "accepted" ? "accepted" : "rejected",
        purposeCount: 1,
        inputByte: 0x77,
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
                    reason: receivePurposeReason(retained.executionIndex),
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
      expect(retained.executionIndex).toBe(0);
      expect(retained.languageTag).toBe(direction === "wrongful" ? 0 : 3);
      const { decision } = await classifyRetainedReasonFixture({
        observation: authenticatedHeaderObservation(fixture),
        payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
        deploymentFingerprint,
        releaseFinalityAuthority,
        replayer: RECEIVE_PURPOSE_LANGUAGE_COMPLETE_CANONICAL_REPLAY,
      });
      expect(decision).toMatchObject(
        direction === "honest"
          ? { decision: "healthy", headerHash: fixture.headerHash }
          : {
              decision: "fault_detected",
              category: "receivePurposeLanguage",
              headerHash: fixture.headerHash,
            },
      );
    },
  );

  it.each(["accepted", "honest", "wrongful"] as const)(
    "ResolvedReferenceScriptMalformed: %s operator verdict from the existing retained predecessor",
    async (direction) => {
      const retained = await nativeDecodingFixture({
        direction: direction === "accepted" ? 0 : 1,
        // Exact ordinary case from native-script-decoding-retained-replay.
        ...(direction === "wrongful"
          ? {}
          : { item: Buffer.from("8200428109", "hex") }),
      });
      const { decision } = await classifyRetainedReasonFixture({
        observation: retained.evidence.observation,
        payloadEnvelopeCbor: await reencodeFixturePayload(
          retained.evidence.reconstruction.payload,
        ),
        predecessor: {
          observation: retained.predecessor.observation,
          payloadEnvelopeCbor: await reencodeFixturePayload(
            retained.predecessor.reconstruction.payload,
          ),
        },
        deploymentFingerprint,
        releaseFinalityAuthority,
        replayer: NATIVE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
      });
      expect(decision).toMatchObject(
        direction === "honest"
          ? { decision: "healthy", headerHash: retained.evidence.headerHash }
          : {
              decision: "fault_detected",
              category: "nativeScriptDecoding",
              headerHash: retained.evidence.headerHash,
            },
      );
    },
  );

  it.each(["accepted", "honest", "wrongful"] as const)(
    "ExecutionNativeScriptMalformed: %s operator verdict from the ordinary source fixture",
    async (direction) => {
      const reason = {
        ExecutionNativeScriptMalformed: { execution_index: 0n },
      } as const;
      const retained = await buildSubjectFixture({
        blockContext: {
          operatorVkey: "b1".repeat(28),
          startTime: 1_749_999_941_000n,
        },
        direction: direction === "accepted" ? "accepted" : "forced",
        item:
          direction === "wrongful"
            ? { kind: "script", script: emptyAllScript() }
            : {
                kind: "raw",
                // Exact small case from the existing honest-rejection lifecycle.
                item: rawNativeItem(Buffer.from("820700", "hex")),
              },
        ...(direction === "accepted" ? {} : { reason }),
      });
      expect(retained.canonicalVerdict).toBe(
        direction === "wrongful" ? "accepted" : "rejected",
      );
      const traceEntries = retainValidationTrace({
        trace: retained.trace,
        eventKey: retained.eventKey,
        claim:
          direction === "accepted"
            ? { verdict: "accepted" }
            : { verdict: "rejected", reason },
      });
      // Match the existing family's retained-DA fixture: its one execution
      // witness opens the real complete trace's committed state/proof.
      const executionEntries = traceEntries.retainedEntries.filter(
        ({ key }) =>
          decodeRetainedValidationWitnessKey(key).execution_index === 0n,
      );
      expect(executionEntries).toHaveLength(1);
      const fixture = await buildRetainedValidationBlockFixture({
        subject:
          direction === "accepted"
            ? { kind: "normal", nativeTx: retained.transaction.tx }
            : {
                kind: "forced",
                nativeTx: retained.transaction.tx,
                orderKey: retained.orderKey,
                verdict: { ForcedTxInvalid: { reason } },
              },
        priorLedgerRoot:
          retained.block.reconstruction.traceByStepIndex.get(0n)!.value
            .pre_utxos_root,
        descriptorEntries: traceEntries.descriptorEntries,
        retainedEntries: executionEntries,
        blockEndTimeMs: 1_750_000_001_000,
        blockSlot: 0n,
      });
      const { decision } = await classifyRetainedReasonFixture({
        observation: authenticatedHeaderObservation(fixture),
        payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
        deploymentFingerprint,
        releaseFinalityAuthority,
        replayer: EXECUTION_SOURCE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
      });
      expect(decision).toMatchObject(
        direction === "honest"
          ? { decision: "healthy", headerHash: fixture.headerHash }
          : {
              decision: "fault_detected",
              category: "executionSourceScriptDecoding",
              headerHash: fixture.headerHash,
            },
      );
    },
  );

  it.each(["accepted", "honest", "wrongful"] as const)(
    "ScriptIntegrityHashMismatch: %s operator verdict with the ordinary zero-language fixture",
    async (direction) => {
      // Reuse the bitmap-0 case from the existing integrity lifecycle: one
      // key input/output, no scripts or redeemers, and its exact body signing.
      const mismatch = direction !== "wrongful";
      const spent = outRefFromByte(0x61);
      const privateKey = CML.PrivateKey.from_normal_bytes(
        new Uint8Array(32).fill(7),
      );
      const spentOutput = makeOutput(
        FUNDED_OUTPUT_LOVELACE,
        Buffer.from(
          CML.EnterpriseAddress.new(
            0,
            CML.Credential.new_pub_key(privateKey.to_public().hash()),
          )
            .to_address()
            .to_raw_bytes(),
        ),
      );
      const output = makeOutput(FUNDED_OUTPUT_LOVELACE);
      const correct = makeNativeTx({
        spendInputs: [spent],
        outputs: [output],
        scriptWitnesses: [],
        scriptLanguages: [],
        privateKey,
      });
      const body = {
        ...correct.tx.body,
        scriptIntegrityHash: mismatch
          ? Buffer.alloc(32, 0xff)
          : correct.tx.body.scriptIntegrityHash,
      };
      const bodyHash = computeMidgardNativeTxId({
        version: correct.tx.version,
        transactionBody: deriveMidgardNativeTxBodyCompact(body),
        transactionWitnessSetHash: Buffer.alloc(32),
        validity: correct.tx.validity,
      });
      const transaction = encodeRecomputedNativeTx({
        ...correct.tx,
        body,
        witnessSet: {
          ...correct.tx.witnessSet,
          addrTxWitsPreimageCbor: encodeByteList([
            Buffer.from(
              CML.make_vkey_witness(
                CML.TransactionHash.from_raw_bytes(bodyHash),
                privateKey,
              ).to_cbor_bytes(),
            ),
          ]),
        },
      });
      const operations = [
        { type: "delete" as const, key: spent },
        buildValidationMachineLedgerInsertOp({
          key: outRefFromTxId(transaction.txId),
          outputCbor: output,
        }),
      ];
      const ledgerWitnessEntries = [{ outRef: spent, output: spentOutput }];
      const mutations = await buildValidationMachineLedgerMutationSteps({
        initialEntries: ledgerWitnessEntries,
        operations,
      });
      const orderKey = { transactionId: "cd".repeat(32), outputIndex: 0n };
      const eventKey =
        direction === "accepted"
          ? {
              L2TransactionEventKey: {
                tx_id: transaction.txId.toString("hex"),
              },
            }
          : { ForcedTransactionEventKey: { tx_order_id: orderKey } };
      const priorLedgerRoot = mutations[0]!.preRoot.toString("hex");
      const trace = await Effect.runPromise(
        buildDeterministicValidationMachineTrace({
          consensusProfile: MIDGARD_CONSENSUS_PROFILE,
          eventKeyCbor: Buffer.from(
            Data.to(eventKey, asLucidSchema(EventKeySchema)),
            "hex",
          ),
          sourceKind: direction === "accepted" ? "normal" : "forced",
          ...(direction === "accepted"
            ? {}
            : { committedForcedVerdict: "rejected" as const }),
          blockEndTimeMs: 1_800_000_000_000,
          expectedNetworkId: 0n,
          minFeeA: 0n,
          minFeeB: 0n,
          blockSlot: 100n,
          transactionId: transaction.txId,
          canonicalTransactionCbor: transaction.txCbor,
          priorUtxosRoot: priorLedgerRoot,
          postUtxosRoot: mismatch
            ? priorLedgerRoot
            : mutations.at(-1)!.postRoot.toString("hex"),
          ledgerWitnessEntries,
          expectedLedgerOps: mismatch ? [] : operations,
          ledgerMutationSteps: mismatch ? [] : mutations,
          expectedVerdict: mismatch ? "rejected" : "accepted",
          expectedRejectionCode: mismatch ? "E_INVALID_FIELD_TYPE" : null,
        }),
      );
      const reason = "ScriptIntegrityHashMismatch" as const;
      const fixture = await buildRetainedValidationBlockFixture({
        subject:
          direction === "accepted"
            ? { kind: "normal", nativeTx: transaction.tx }
            : {
                kind: "forced",
                nativeTx: transaction.tx,
                orderKey,
                verdict: { ForcedTxInvalid: { reason } },
              },
        priorLedgerRoot,
        ...retainValidationTrace({
          trace,
          eventKey,
          claim:
            direction === "accepted"
              ? { verdict: "accepted" }
              : { verdict: "rejected", reason },
        }),
        blockEndTimeMs: 1_800_000_000_000,
        blockSlot: 100n,
      });
      const { decision } = await classifyRetainedReasonFixture({
        observation: authenticatedHeaderObservation(fixture),
        payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
        deploymentFingerprint,
        releaseFinalityAuthority,
        replayer: SCRIPT_INTEGRITY_HASH_MISMATCH_COMPLETE_CANONICAL_REPLAY,
      });
      expect(decision).toMatchObject(
        direction === "honest"
          ? { decision: "healthy", headerHash: fixture.headerHash }
          : {
              decision: "fault_detected",
              category: "scriptIntegrityHashMismatch",
              headerHash: fixture.headerHash,
            },
      );
    },
  );

  it.each(["accepted", "honest", "wrongful"] as const)(
    "InputSpentOutputNonCanonical: %s operator verdict with the existing resolved native-script output",
    async (direction) => {
      const retained = await nativeDecodingFixture({
        direction: direction === "accepted" ? 0 : 1,
        sourceKind: 0,
        reasonName: "InputSpentOutputNonCanonical",
        ...(direction === "wrongful"
          ? {}
          : { item: Buffer.from("8200428109", "hex") }),
      });
      const source = retained.evidence.reconstruction.forcedTransactions[0]!;
      const nativeTx = decodeMidgardNativeTxFullFromCanonicalCbor(
        source.fullTransactionCbor,
      );
      const fixture = await buildRetainedValidationBlockFixture({
        subject:
          direction === "accepted"
            ? { kind: "normal", nativeTx }
            : {
                kind: "forced",
                nativeTx,
                orderKey: source.key,
                verdict: {
                  ForcedTxInvalid: {
                    reason: {
                      InputSpentOutputNonCanonical: {
                        source_kind: 0n,
                        input_index: 0n,
                      },
                    },
                  },
                },
              },
        priorLedgerRoot: retained.predecessor.header.utxosRoot,
        prevHeaderHash: retained.predecessor.headerHash,
        blockEndTimeMs: 1_750_000_001_000,
        blockSlot: 100n,
      });
      const { decision } = await classifyRetainedReasonFixture({
        observation: authenticatedHeaderObservation(fixture),
        payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
        predecessor: {
          observation: retained.predecessor.observation,
          payloadEnvelopeCbor: await reencodeFixturePayload(
            retained.predecessor.reconstruction.payload,
          ),
        },
        deploymentFingerprint,
        releaseFinalityAuthority,
        replayer: RESOLVED_OUTPUT_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY,
      });
      expect(decision).toMatchObject(
        direction === "honest"
          ? { decision: "healthy", headerHash: fixture.headerHash }
          : {
              decision: "fault_detected",
              category: "resolvedOutputNonCanonical",
              headerHash: fixture.headerHash,
            },
      );
    },
  );
});
