import {
  decodeMidgardNativeTxFullFromCanonicalCbor,
  MIDGARD_CONSENSUS_PROFILE,
} from "@al-ft/midgard-core";
import { asLucidSchema } from "@al-ft/midgard-core/lucid-data";
import { EventKeySchema, forcedVerdictSubject } from "@al-ft/midgard-sdk";
import { buildDeterministicValidationMachineTrace } from "@al-ft/midgard-validation";
import {
  FUNDED_OUTPUT_LOVELACE,
  makeProtectedScriptOutput,
  outRefFromByte,
} from "@al-ft/midgard-validation/tests/validation-fixtures";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { beforeAll, describe, expect, it } from "vitest";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../src/evidence/canonical-block-evidence.js";
import {
  buildUnusedRedeemerMaterialFromRetainedDa,
  buildUnusedRedeemerObservationFromRetainedDa,
  detectUnusedRedeemerCanonicalViolations,
} from "../src/unused-redeemer/replay.js";
import { UNUSED_REDEEMER_COMPLETE_CANONICAL_REPLAY } from "../src/workflow/complete-replay.js";
import {
  computeFraudProofReleaseFinalityPolicyDigest,
  FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
} from "../src/workflow/release-finality-policy.js";
import { authenticatedHeaderObservation } from "./helpers/canonical-block-evidence-fixture.js";
import { buildMissingRedeemerFixture } from "./support/missing-redeemer-emulator.js";
import {
  buildRetainedValidationBlockFixture,
  classifyRetainedReasonFixture,
  retainRejectedValidationTrace,
} from "./support/retained-reason-classifier.js";

/** Reuses the existing two-pointer transaction under an exact claimed coordinate. */
const retainedRejection = async (redeemerIndex: 0 | 1 = 1) => {
  const source = await buildMissingRedeemerFixture({
    direction: "forced",
    purposeKind: 0,
    sourceLocation: "inline",
  });
  const reason = {
    UnusedRedeemer: { redeemer_index: BigInt(redeemerIndex) },
  } as const;
  const priorLedgerRoot = "00".repeat(32);
  const trace = await Effect.runPromise(
    buildDeterministicValidationMachineTrace({
      consensusProfile: MIDGARD_CONSENSUS_PROFILE,
      eventKeyCbor: Buffer.from(
        Data.to(source.eventKey, asLucidSchema(EventKeySchema)),
        "hex",
      ),
      sourceKind: "forced",
      committedForcedVerdict: "rejected",
      blockEndTimeMs: 1_800_000_000_000,
      expectedNetworkId: 0n,
      minFeeA: 0n,
      minFeeB: 0n,
      blockSlot: 100n,
      transactionId: source.transaction.txId,
      canonicalTransactionCbor: source.transaction.txCbor,
      programMaterialSidecarCbor: Buffer.from(
        "82018282582072c078cab22fca41a65b75e6dfcff21d6258a743068e190836bd227ad35dd99d47830100438200008258207d068efad94d2953eefe63951671327af75e08c963cd1f232b08966e6026bf5e582983010058248202582072c078cab22fca41a65b75e6dfcff21d6258a743068e190836bd227ad35dd99d",
        "hex",
      ),
      priorUtxosRoot: priorLedgerRoot,
      postUtxosRoot: priorLedgerRoot,
      ledgerWitnessEntries: [
        {
          outRef: outRefFromByte(0x71),
          output: makeProtectedScriptOutput(
            source.scriptHashHex,
            FUNDED_OUTPUT_LOVELACE,
          ),
        },
      ],
      expectedLedgerOps: [],
      ledgerMutationSteps: [],
      expectedVerdict: "rejected",
      expectedRejectionCode: "E_INVALID_FIELD_TYPE",
    }),
  );
  expect(
    trace.witnesses.some(
      ({ auxiliary }) => auxiliary?.kind === "nativeExecutionDescriptor",
    ),
  ).toBe(false);
  const fixture = await buildRetainedValidationBlockFixture({
    subject: {
      kind: "forced",
      nativeTx: decodeMidgardNativeTxFullFromCanonicalCbor(
        source.transaction.txCbor,
      ),
      orderKey: source.orderKey,
      verdict: { ForcedTxInvalid: { reason } },
    },
    priorLedgerRoot,
    ...retainRejectedValidationTrace({
      trace,
      eventKey: source.eventKey,
      reason,
    }),
    blockEndTimeMs: 1_800_000_000_000,
  });
  const block = await canonicalBlockEvidenceFromVerifiedPayload({
    observation: authenticatedHeaderObservation(fixture),
    payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
    daProvenance: {
      trustClass: "public_or_permissionless_da",
      sourceId: "unused-redeemer-retained-fixture",
      grade: "security",
    },
  });
  return { source, block, reason, committed: fixture };
};

describe("unused-redeemer authenticated observation", () => {
  let fixture: Awaited<ReturnType<typeof retainedRejection>>;
  beforeAll(async () => {
    fixture = await retainedRejection();
  });

  it("reconstructs selected and unused pointers before native execution begins", async () => {
    for (const redeemerIndex of [0, 1]) {
      const { observation, base } =
        await buildUnusedRedeemerObservationFromRetainedDa({
          block: fixture.block,
          eventKey: fixture.source.eventKey,
          transactionId:
            fixture.block.reconstruction.forcedTransactions[0]!.value.tx_id,
          redeemerIndex,
          txCbor: fixture.source.transaction.txCbor,
        });
      expect(observation.unused).toBe(redeemerIndex === 1);
      expect(base.selectedBit).toBe(redeemerIndex === 0 ? 1n : 0n);
      expect(observation.selections).toHaveLength(1);
    }
  });

  it("does not challenge an honest unused-redeemer rejection", async () => {
    await expect(
      detectUnusedRedeemerCanonicalViolations(fixture.block),
    ).resolves.toEqual([]);
  });

  it.each([
    { label: "honest", redeemerIndex: 1 as const, expected: "healthy" },
    {
      label: "wrongful",
      redeemerIndex: 0 as const,
      expected: "fault_detected",
    },
  ])(
    "classifies the $label retained rejection as $expected",
    async (scenario) => {
      const retained =
        scenario.redeemerIndex === 1 ? fixture : await retainedRejection(0);
      const deploymentFingerprint = "d1".repeat(32);
      const policy = {
        confirmationDepth: 30,
        automaticRecoveryMaxDepth: 2160,
        deepRollbackPolicy: "automated_rewind_replay_incident-v1",
      } as const;
      const { decision } = await classifyRetainedReasonFixture({
        observation: authenticatedHeaderObservation(retained.committed),
        payloadEnvelopeCbor: retained.committed.payloadEnvelopeCbor,
        deploymentFingerprint,
        releaseFinalityAuthority: {
          authorityVersion: FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
          verifyForWorkflow: async () => ({
            schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
            deploymentIdentityDigest: deploymentFingerprint,
            blueprintHash: "e1".repeat(32),
            policyDigest: computeFraudProofReleaseFinalityPolicyDigest(policy),
            policy,
          }),
        },
        replayer: UNUSED_REDEEMER_COMPLETE_CANONICAL_REPLAY,
      });
      expect(decision).toMatchObject({
        decision: scenario.expected,
        headerHash: retained.committed.headerHash,
        ...(scenario.expected === "fault_detected"
          ? { category: "unusedRedeemer", violationId: "unused-redeemer" }
          : {}),
      });
    },
  );

  it("keeps artifact preparation strict when an honest rejection has no contradiction", async () => {
    await expect(
      buildUnusedRedeemerMaterialFromRetainedDa({
        block: fixture.block,
        eventKey: fixture.source.eventKey,
        subject: forcedVerdictSubject({
          transactionId:
            fixture.block.reconstruction.forcedTransactions[0]!.value.tx_id,
          sourceKey: fixture.source.orderKey,
          rejectionReason: fixture.reason,
        }),
        redeemerIndex: 1,
        txCbor: fixture.source.transaction.txCbor,
      }),
    ).rejects.toThrow("selection frontier contradicts proof direction");
  });
});
