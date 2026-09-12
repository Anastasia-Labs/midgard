import { describe, expect, it } from "vitest";

import {
  requireTransitionTraceEventAuthority,
  requireTransitionTraceL1Events,
  unsafeCreateTransitionTraceEventAuthorityFromRawForTest,
} from "../src/transition-trace/l1-events.js";
import { FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY } from "../src/workflow/raw-l1-snapshot.js";
import {
  computeFraudProofReleaseFinalityPolicyDigest,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
} from "../src/workflow/release-finality-policy.js";
import {
  buildRetainedPlutusIdentityFixture,
  captureRetainedPlutusIdentityOrigins,
} from "./support/retained-reason-classifier.js";

describe("transition event authority raw test transport", () => {
  it("admits raw observations on every capture and rejects malformed hub output bytes", async () => {
    const fixture = await buildRetainedPlutusIdentityFixture({
      verdict: "accepted",
    });
    const seed = requireTransitionTraceL1Events(
      await captureRetainedPlutusIdentityOrigins(fixture, { omitEvent: true }),
    ).snapshot;
    const policy = {
      confirmationDepth: 30,
      automaticRecoveryMaxDepth: 2160,
      deepRollbackPolicy: "automated_rewind_replay_incident-v1",
    } as const;
    const binding = {
      deploymentFingerprint: "d1".repeat(32),
      network: "Preprod",
      releaseFinality: {
        schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
        deploymentIdentityDigest: "d1".repeat(32),
        blueprintHash: "e1".repeat(32),
        policyDigest: computeFraudProofReleaseFinalityPolicyDigest(policy),
        policy,
      },
      resolvedContracts: { hubOraclePolicyId: "61".repeat(28) },
      definition: { headerHash: fixture.block.headerHash },
    } as Parameters<
      typeof unsafeCreateTransitionTraceEventAuthorityFromRawForTest
    >[0]["binding"];
    let malformed = false;
    let captures = 0;
    const authority = unsafeCreateTransitionTraceEventAuthorityFromRawForTest({
      binding,
      authority: {
        authorityVersion: FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY,
        capture: async (request) => {
          captures++;
          return {
            ...seed,
            headerHash: request.headerHash,
            historyUnits: request.historyUnits,
            history: request.historyUnits.map((unit) => {
              const row = seed.history.find((history) => history.unit === unit);
              if (row === undefined)
                throw new Error("Requested history is unavailable");
              return row;
            }),
            scopes: request.scopes.map((scope) => ({
              ...scope,
              utxos: (
                seed.scopes.find(
                  (existing) => existing.address === scope.address,
                )?.utxos ?? []
              ).map((utxo) =>
                malformed ? { ...utxo, outputCbor: "00" } : utxo,
              ),
            })),
          };
        },
      },
    });
    const capture = requireTransitionTraceEventAuthority(authority);
    const admitted = await capture(fixture.block.headerHash);
    expect(requireTransitionTraceL1Events(admitted).snapshot.headerHash).toBe(
      fixture.block.headerHash,
    );
    const successfulCaptures = captures;
    malformed = true;
    await expect(capture(fixture.block.headerHash)).rejects.toThrow();
    expect(captures).toBeGreaterThan(successfulCaptures);
    expect(() =>
      requireTransitionTraceEventAuthority({
        deploymentFingerprint: binding.deploymentFingerprint,
      }),
    ).toThrow("not admitted");
  });
});
