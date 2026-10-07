import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it, vi } from "vitest";

import { L1SourceIntegrityError } from "../src/l1/source-integrity.js";
import { scanStateQueue } from "../src/l1/state-queue-scanner.js";
import { makeObservedNode, makePayloadFixture } from "./helpers.js";

describe("StateQueueStatusV1 observation disagreements", () => {
  const statuses: readonly SDK.DaAvailabilityStateQueueStatus[] = [
    "Unattested",
    { Attested: { commitment_hash: "33".repeat(32) } },
    {
      Challenged: {
        commitment_hash: "33".repeat(32),
        challenge_asset_name: "44".repeat(32),
      },
    },
    { Published: { terminal_commitment: "55".repeat(32) } },
  ];
  const cases = statuses.flatMap((prior) =>
    statuses.map((observed) => ({
      prior,
      observed,
      priorKind: SDK.daAvailabilityStateQueueStatusKind(prior),
      observedKind: SDK.daAvailabilityStateQueueStatusKind(observed),
    })),
  );

  it.each(cases)(
    "compares stored $priorKind with observed $observedKind at the same output",
    async ({ prior, observed, priorKind, observedKind }) => {
      const { header, headerHash } = await makePayloadFixture();
      const config = {
        deploymentFingerprint: "11".repeat(32),
        deploymentIdentityDigest: "11".repeat(32),
        stateQueuePolicyId: "22".repeat(28),
        daAttestationPolicyId: "33".repeat(28),
        finalityDepth: 3,
        consensusProfile: MIDGARD_CONSENSUS_PROFILE,
      };
      const node = makeObservedNode({ header, headerHash, depth: 10 });
      const previousHeaders = await scanStateQueue(
        {
          fetchStateQueueNodes: async () => [{ ...node, daAttestation: prior }],
        },
        config,
      );
      const stored = structuredClone(previousHeaders);
      const recordL1View = vi.fn();
      const recordReplayAnchor = vi.fn();
      const recordReplayedHeaderSteps = vi.fn();
      const result = scanStateQueue(
        {
          fetchStateQueueNodes: async () => [
            { ...node, daAttestation: observed },
          ],
          fetchStateQueueSnapshot: async () => ({
            nodes: [{ ...node, daAttestation: observed }],
            confirmedHeaderHash: "00".repeat(28),
            confirmedStateOutRef: `${"00".repeat(32)}#0`,
            observedChainPoint: node.chainPoint,
          }),
        },
        {
          ...config,
          previousHeaders,
          recordL1View,
          recordReplayAnchor,
          recordReplayedHeaderSteps,
        },
      );
      if (priorKind === observedKind) {
        await expect(result).resolves.toMatchObject([
          {
            headerHash,
            daAttestation: observed,
            status: observedKind === "Unattested" ? "unattested" : "attested",
            validationErrors: [],
          },
        ]);
        expect(recordL1View).toHaveBeenCalledOnce();
        expect(recordReplayAnchor).toHaveBeenCalledOnce();
      } else {
        await expect(result).rejects.toBeInstanceOf(L1SourceIntegrityError);
        await expect(result).rejects.toThrow(
          `state-queue status disagreement at unchanged output ${node.outRef}: stored=${SDK.daAvailabilityStateQueueStatusIdentity(prior)}, observed=${SDK.daAvailabilityStateQueueStatusIdentity(observed)}`,
        );
        expect(recordL1View).not.toHaveBeenCalled();
        expect(recordReplayAnchor).not.toHaveBeenCalled();
        expect(recordReplayedHeaderSteps).not.toHaveBeenCalled();
      }
      expect(previousHeaders).toEqual(stored);
    },
  );
});
