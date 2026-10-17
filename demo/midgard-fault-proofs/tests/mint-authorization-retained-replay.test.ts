import { encodeMidgardVersionedScript } from "@al-ft/midgard-core";
import { EMPTY_MERKLE_TREE_ROOT } from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  admitMintAuthorizationWorkflowArtifact,
  MINT_AUTHORIZATION_ARTIFACT,
} from "../src/mint-authorization/artifact.js";
import {
  detectMintAuthorizationReplay,
  mintAuthorizationDetectionId,
  prepareMintAuthorizationReplay,
} from "../src/mint-authorization/replay.js";
import { completeCanonicalReplayPredecessorLedger } from "../src/workflow/complete-replay.js";
import {
  buildMintAuthorizationSubject,
  directionBNativeScript,
} from "./support/mint-authorization-emulator.js";
import { mintAuthorizationRetainedReferenceFixture } from "./support/mint-authorization-retained-reference.js";
import { buildDecodingBlockFixture } from "./support/native-script-decoding-emulator.js";

const fixture = async (present: boolean) => {
  const policy = directionBNativeScript();
  const witness = encodeMidgardVersionedScript({
    language: "NativeCardano",
    nativeScript: policy.script,
    scriptBytes: Buffer.from(policy.scriptBytesHex, "hex"),
  });
  const subject = buildMintAuthorizationSubject({
    mintItemCbors: [policy.mintItemCbor],
    scriptWitnessItemCbors: present ? [witness.toString("hex")] : [],
  });
  return await buildDecodingBlockFixture({
    priorLedgerRoot: EMPTY_MERKLE_TREE_ROOT,
    operatorVkey: "aa".repeat(28),
    startTime: 1_000_000n,
    subject: { kind: "normal", nativeTx: subject.nativeTx },
  });
};

describe("mint authorization retained evidence", () => {
  it.each([false, true])(
    "reconstructs absent/present policy %s and JSON journal recovery",
    async (present) => {
      const block = await fixture(present);
      const findings = await prepareMintAuthorizationReplay({
        current: block.reconstruction,
        sourceIndex: 0,
      });
      expect(findings).toHaveLength(1);
      expect(findings[0]!.finding.direction).toBe(present ? 1n : 0n);
      const artifact = {
        schemaVersion: MINT_AUTHORIZATION_ARTIFACT,
        headerHash: block.reconstruction.headerHash,
        detectionId: mintAuthorizationDetectionId(findings[0]!.coordinate),
        coordinate: findings[0]!.coordinate,
        payloadEnvelopeCbor:
          block.reconstruction.payloadEnvelopeCbor.toString("hex"),
        predecessorLedger: null,
      };
      const restored = await admitMintAuthorizationWorkflowArtifact(
        JSON.parse(JSON.stringify(artifact)),
      );
      expect(restored.finding).toEqual(findings[0]!.finding);
      await expect(
        admitMintAuthorizationWorkflowArtifact({
          ...artifact,
          coordinate: { sourceIndex: 0, policyIndex: "1" },
        }),
      ).rejects.toThrow();
      await expect(
        admitMintAuthorizationWorkflowArtifact({
          ...artifact,
          headerHash: "00".repeat(28),
        }),
      ).rejects.toThrow();
      await expect(
        admitMintAuthorizationWorkflowArtifact({ ...artifact, direction: 1 }),
      ).rejects.toThrow();
    },
  );
});

/**
 * Phase B resolves a policy among the reference scripts of every resolved
 * output, spent and referenced alike. A policy whose only source is a spent
 * output is present: authorized when its native script holds, unauthorized
 * when it fails, and absent only when no resolved output carries it.
 */
describe("mint authorization policies sourced from spent outputs", () => {
  it.each([
    {
      inputField: "spend",
      present: true,
      satisfied: true,
      expected: [],
    },
    {
      inputField: "spend",
      present: true,
      satisfied: false,
      expected: [1n],
    },
    {
      inputField: "spend",
      present: false,
      satisfied: false,
      expected: [0n],
    },
    {
      inputField: "reference",
      present: true,
      satisfied: true,
      expected: [],
    },
  ] as const)(
    "$inputField source present=$present satisfied=$satisfied proves directions $expected",
    async ({ inputField, present, satisfied, expected }) => {
      const fixture = await mintAuthorizationRetainedReferenceFixture({
        now: 1_000_000,
        operatorVkey: "aa".repeat(28),
        inputField,
        present,
        satisfied,
      });
      const findings = await detectMintAuthorizationReplay({
        block: fixture.evidence,
        predecessor: completeCanonicalReplayPredecessorLedger({
          evidence: fixture.evidence,
          context: fixture.context,
        }),
      });
      expect(
        findings.map(({ prepared }) => prepared.finding.direction),
      ).toEqual(expected);
      for (const { prepared } of findings) {
        expect(prepared.finding.policyIdHex).toBe(fixture.target.policyIdHex);
        expect(prepared.spendInputItemCbors).toHaveLength(
          inputField === "spend" ? 1 : 0,
        );
      }
    },
  );
});
