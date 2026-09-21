import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it, vi } from "vitest";

import { fetchCanonicalBlockEvidence } from "../src/evidence/canonical-block-evidence.js";
import type {
  RetainedDaPayloadSource,
  RetainedDaPayloadSourceResult,
} from "../src/transition-trace/fetch.js";
import {
  assertValidationTraceDisputeChallengeCurrent,
  type ManifestBoundValidationTraceDisputeWorkflow,
} from "../src/validation-dispute/workflow-v1.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
  type CanonicalBlockFixture,
  outRefCbor,
} from "./helpers/canonical-block-evidence-fixture.js";

/**
 * The record-derived dispute runner re-authenticates the canonical block from
 * retained DA before every move and refuses a challenge whose payload
 * coordinate no longer matches it. This is the check the deleted runner
 * surface used to perform; the installed-lifecycle test drives execution
 * below it with a fixed coordinate.
 */
const publicSource = (
  fixture: CanonicalBlockFixture,
): RetainedDaPayloadSource => ({
  sourceId: "live-peer",
  fetchPayloadByHeaderHash: (): Promise<RetainedDaPayloadSourceResult> =>
    Promise.resolve({
      ok: true,
      provenance: SDK.assertSecurityGradeEvidence({
        trustClass: "public_or_permissionless_da",
        sourceId: "live-peer/peer-a",
        grade: "security",
      }),
      sourceId: "live-peer",
      sourcePeerId: "peer-a",
      payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
      attempts: [],
    }),
});

const block = async () => {
  const fixture = await buildCanonicalBlockFixture({
    transactions: [
      buildFixtureTransaction({ spendInputs: [outRefCbor(0x11, 0n)], fee: 1n }),
    ],
  });
  const sources = [publicSource(fixture)];
  const evidence = await fetchCanonicalBlockEvidence({
    observation: authenticatedHeaderObservation(fixture),
    sources,
    retries: 0,
  });
  return { fixture, sources, evidence };
};

const workflowWith = (
  fixture: CanonicalBlockFixture,
  observeHeader: () => Promise<SDK.AuthenticatedStateQueueHeaderObservation>,
  coordinate:
    | { payloadEnvelopeSha256: string; payloadSha256: string }
    | undefined,
): ManifestBoundValidationTraceDisputeWorkflow =>
  ({
    binding: { definition: { headerHash: fixture.headerHash } },
    l1: { observeHeader },
    challenge: coordinate === undefined ? undefined : { coordinate },
  }) as unknown as ManifestBoundValidationTraceDisputeWorkflow;

describe("validation-trace dispute challenge currency", () => {
  it("passes a challenge whose payload coordinate matches the authenticated canonical block", async () => {
    const { fixture, sources, evidence } = await block();
    const observeHeader = vi.fn(async () =>
      authenticatedHeaderObservation(fixture),
    );
    await expect(
      assertValidationTraceDisputeChallengeCurrent({
        workflow: workflowWith(fixture, observeHeader, {
          payloadEnvelopeSha256: evidence.payloadEnvelopeSha256,
          payloadSha256: evidence.payloadSha256,
        }),
        sources,
      }),
    ).resolves.toBeUndefined();
    expect(observeHeader).toHaveBeenCalledWith({
      headerHash: fixture.headerHash,
    });
  });

  it.each(["payloadEnvelopeSha256", "payloadSha256"] as const)(
    "refuses a challenge whose %s diverged from the canonical block",
    async (field) => {
      const { fixture, sources, evidence } = await block();
      const coordinate = {
        payloadEnvelopeSha256: evidence.payloadEnvelopeSha256,
        payloadSha256: evidence.payloadSha256,
        [field]: "ff".repeat(32),
      };
      await expect(
        assertValidationTraceDisputeChallengeCurrent({
          workflow: workflowWith(
            fixture,
            async () => authenticatedHeaderObservation(fixture),
            coordinate,
          ),
          sources,
        }),
      ).rejects.toThrow(
        "validationTraceDispute challenge diverged from the authenticated canonical block",
      );
    },
  );

  it("does not consult L1 or retained DA for a challenge-free workflow", async () => {
    const { fixture } = await block();
    const observeHeader = vi.fn(async () =>
      authenticatedHeaderObservation(fixture),
    );
    await assertValidationTraceDisputeChallengeCurrent({
      workflow: workflowWith(fixture, observeHeader, undefined),
      sources: [],
    });
    expect(observeHeader).not.toHaveBeenCalled();
  });
});
