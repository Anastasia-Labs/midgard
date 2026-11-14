import {
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
} from "@al-ft/midgard-core/codec";
import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import { describe, expect, it } from "vitest";

import { MIN_ADA_COMPLETE_CANONICAL_REPLAY } from "../src/workflow/complete-replay.js";
import {
  authenticatedStateQueueObservationDigest,
  classifyHeader,
  createHeaderClassifier,
  headerDecisionReplayContext,
} from "../src/workflow/header-classifier.js";
import {
  computeFraudProofReleaseFinalityPolicyDigest,
  FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
} from "../src/workflow/release-finality-policy.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
  outRefCbor,
} from "./helpers/canonical-block-evidence-fixture.js";
import { retainedDaSource } from "./helpers/header-classifier.retained-da-source.js";

const deploymentFingerprint = "d1".repeat(32);
const policy = { ...DEPLOYMENT_MANIFEST_L1_FINALITY };
const output = (lovelace: bigint) =>
  encodeMidgardTxOutput({
    address: Buffer.concat([Buffer.from([0x60]), Buffer.alloc(28, 0x44)]),
    value: { lovelace, assets: new Map() },
  });

const classify = async (emptyPredecessor: boolean, lovelace: bigint) => {
  const spend = outRefCbor(88, 0n);
  const predecessor = await buildCanonicalBlockFixture({
    transactions: [],
    utxos: emptyPredecessor ? [] : [{ key: spend, value: output(2_000_000n) }],
  });
  const transaction = buildFixtureTransaction({
    spendInputs: [spend],
    outputs: [output(lovelace)],
    fee: 7n,
  });
  const current = await buildCanonicalBlockFixture({
    transactions: [transaction],
    prevHeaderHash: predecessor.headerHash,
    prevUtxosRoot: predecessor.header.utxosRoot,
    utxos: [
      {
        key: encodeMidgardSpendInputItem({
          txId: Buffer.from(transaction.txId, "hex"),
          outputIndex: 0,
        }),
        value: output(lovelace),
      },
    ],
  });
  const observation = authenticatedHeaderObservation(current);
  const classifier = await createHeaderClassifier({
    deploymentFingerprint,
    replayer: MIN_ADA_COMPLETE_CANONICAL_REPLAY,
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
  });
  const fetched = { count: 0 };
  const decision = await classifyHeader({
    classifier,
    observation,
    authenticatedObservationDigest:
      await authenticatedStateQueueObservationDigest({
        observation,
        minimumConfirmationDepth: policy.confirmationDepth,
      }),
    sources: [
      retainedDaSource({
        payloadEnvelopeCbor: current.payloadEnvelopeCbor,
        fetched,
      }),
    ],
  });
  return { decision, fetched, transaction };
};

describe("transaction-only minimum-Ada classifier", () => {
  it.each([true, false])(
    "detects an authenticated underfunded normal output without predecessor access (empty predecessor: %s)",
    async (emptyPredecessor) => {
      const { decision, fetched, transaction } = await classify(
        emptyPredecessor,
        1n,
      );
      expect(decision).toMatchObject({
        decision: "fault_detected",
        category: "minAda",
        violationId: "min-ada",
        detectionId: `min-ada:tx:${transaction.txId}:0`,
      });
      expect(
        headerDecisionReplayContext(decision)?.predecessor,
      ).toBeUndefined();
      expect(fetched.count).toBe(1);
    },
  );

  it.each([true, false])(
    "keeps a funded normal output healthy without predecessor access (empty predecessor: %s)",
    async (emptyPredecessor) => {
      const { decision, fetched } = await classify(
        emptyPredecessor,
        2_000_000n,
      );
      expect(decision.decision).toBe("healthy");
      expect(
        headerDecisionReplayContext(decision)?.predecessor,
      ).toBeUndefined();
      expect(fetched.count).toBe(1);
    },
  );
});
