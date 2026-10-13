import { decodeMidgardTxOutput } from "@al-ft/midgard-core";
import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  MIDGARD_COINS_PER_UTXO_BYTE,
  outputMeetsMinAda,
} from "@al-ft/midgard-validation";
import { describe, expect, it } from "vitest";

import {
  createCompleteCanonicalReplayUnion,
  MIN_ADA_COMPLETE_CANONICAL_REPLAY,
  NETWORK_ID_COMPLETE_CANONICAL_REPLAY,
} from "../src/workflow/complete-replay.js";
import {
  authenticatedStateQueueObservationDigest,
  classifyHeader,
  createHeaderClassifier,
} from "../src/workflow/header-classifier.js";
import {
  computeFraudProofReleaseFinalityPolicyDigest,
  FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
} from "../src/workflow/release-finality-policy.js";
import { authenticatedHeaderObservation } from "./helpers/canonical-block-evidence-fixture.js";
import { fixtureLedgerEntries } from "./support/fixture-ledger.js";
import { transitionTraceDepositRetainedFixture } from "./support/transition-trace-retained.deposit-events-retained-block.js";

/** Faithful deposit projection remains healthy under the transaction families. */
describe("deposit projection under transaction-only checks", () => {
  it("classifies an underfunded, other-network deposit projection healthy", async () => {
    const { current, predecessor } =
      await transitionTraceDepositRetainedFixture({
        operatorVkey: "11".repeat(28),
        now: 1_800_000_000_000,
        originalAssets: { lovelace: 2_000_000n },
        honest: true,
        event: {
          id: { transactionId: "22".repeat(32), outputIndex: 0n },
          info: {
            l2_address: {
              paymentCredential: { PublicKeyCredential: ["33".repeat(28)] },
              stakeCredential: null,
            },
            l2_network_id: 1n,
            l2_datum: "ab".repeat(1_024),
          },
        },
      });
    const [, outputCbor] = fixtureLedgerEntries(current.header.utxosRoot)[0]!;
    const output = decodeMidgardTxOutput(Buffer.from(outputCbor, "hex"));
    expect(output.address[0]! & 0x0f).toBe(1);
    expect(current.header.expectedNetworkId).toBe(0n);
    expect(
      outputMeetsMinAda(
        MIDGARD_COINS_PER_UTXO_BYTE,
        BigInt(outputCbor.length / 2),
        output.value.lovelace,
      ),
    ).toBe(false);
    const policy = { ...DEPLOYMENT_MANIFEST_L1_FINALITY };
    const fingerprint = "d1".repeat(32);
    const classifier = await createHeaderClassifier({
      deploymentFingerprint: fingerprint,
      replayer: createCompleteCanonicalReplayUnion([
        NETWORK_ID_COMPLETE_CANONICAL_REPLAY,
        MIN_ADA_COMPLETE_CANONICAL_REPLAY,
      ]),
      releaseFinalityAuthority: {
        authorityVersion: FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
        verifyForWorkflow: async () => ({
          schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
          deploymentIdentityDigest: fingerprint,
          blueprintHash: "e1".repeat(32),
          policyDigest: computeFraudProofReleaseFinalityPolicyDigest(policy),
          policy,
        }),
      },
    });
    const observation = authenticatedHeaderObservation(current);
    const authenticatedObservationDigest =
      await authenticatedStateQueueObservationDigest({
        observation,
        minimumConfirmationDepth: policy.confirmationDepth,
      });
    const decision = await classifyHeader({
      classifier,
      observation,
      authenticatedObservationDigest,
      predecessorLedgerAt: async (headerHash) =>
        headerHash === predecessor.headerHash ? [] : undefined,
      sources: [
        {
          sourceId: "deposit-projection",
          fetchPayloadByHeaderHash: async (headerHash) => ({
            ok: true,
            sourceId: "deposit-projection",
            sourcePeerId: "peer",
            attempts: [],
            payloadEnvelopeCbor:
              headerHash === current.headerHash
                ? current.payloadEnvelopeCbor
                : predecessor.payloadEnvelopeCbor,
            provenance: {
              trustClass: "public_or_permissionless_da",
              sourceId: "deposit-projection/peer",
              grade: "security",
            },
          }),
        },
      ],
    });
    expect(decision.decision).toBe("healthy");
  });
});
