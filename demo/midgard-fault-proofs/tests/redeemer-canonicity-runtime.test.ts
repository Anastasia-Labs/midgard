import { encodeMidgardRedeemerWitnessItem } from "@al-ft/midgard-core";
import { expect, it } from "vitest";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../src/evidence/canonical-block-evidence.js";
import {
  admitRedeemerWorkflowArtifact,
  prepareRedeemerCanonicityWorkflowArtifact,
} from "../src/redeemer-canonicity/runtime.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
  outRefCbor,
} from "./helpers/canonical-block-evidence-fixture.js";

it("prepares and admits restart material from an actual accepted noncanonical redeemer", async () => {
  const item = encodeMidgardRedeemerWitnessItem({
    purpose: "Spend",
    index: 0n,
    redeemerCbor: Buffer.from("1800", "hex"),
    executionUnits: { memory: 1n, steps: 2n },
  });
  const fixture = await buildCanonicalBlockFixture({
    transactions: [
      buildFixtureTransaction({
        spendInputs: [outRefCbor(7, 0n)],
        fee: 1n,
        redeemerWitnesses: [item],
      }),
    ],
  });
  const block = await canonicalBlockEvidenceFromVerifiedPayload({
    observation: authenticatedHeaderObservation(fixture),
    payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
    daProvenance: {
      trustClass: "public_or_permissionless_da",
      sourceId: "redeemer-material-fixture",
      grade: "security",
    },
  });
  const artifact = await prepareRedeemerCanonicityWorkflowArtifact(block);
  const admitted = admitRedeemerWorkflowArtifact(artifact);
  expect(admitted.evidence.canonical).toBe(false);
  expect(admitted.accepted?.nativeTxId).toBe(fixture.transactions[0]!.txId);
  expect(admitted.evidence.subject.transaction_id).toBe(
    fixture.transactions[0]!.txId,
  );
  expect(admitted.forced).toBeNull();
  expect(() =>
    admitRedeemerWorkflowArtifact({
      ...artifact,
      fieldCommitmentHex: "00".repeat(32),
    }),
  ).toThrow(/commitment/);
});
