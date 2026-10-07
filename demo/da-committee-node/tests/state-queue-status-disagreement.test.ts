import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it, vi } from "vitest";

import { L1SourceIntegrityError } from "../src/l1/source-integrity.js";
import {
  scanStateQueue,
  type StateQueueReplayAnchor,
} from "../src/l1/state-queue-scanner.js";
import { makeObservedNode, makePayloadFixture } from "./helpers.js";
import { createStateQueueChain } from "./helpers/state-queue-chain.js";

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
      agrees: prior === observed,
      detail: "constructor pair",
    })),
  );
  for (const { prior, observed, detail } of [
    {
      prior: statuses[1]!,
      observed: { Attested: { commitment_hash: "66".repeat(32) } },
      detail: "different attested commitment",
    },
    {
      prior: statuses[2]!,
      observed: {
        Challenged: {
          commitment_hash: "66".repeat(32),
          challenge_asset_name: "44".repeat(32),
        },
      },
      detail: "different challenged commitment",
    },
    {
      prior: statuses[2]!,
      observed: {
        Challenged: {
          commitment_hash: "33".repeat(32),
          challenge_asset_name: "66".repeat(32),
        },
      },
      detail: "different challenge identity",
    },
    {
      prior: statuses[3]!,
      observed: { Published: { terminal_commitment: "66".repeat(32) } },
      detail: "different terminal commitment",
    },
  ] satisfies readonly {
    prior: SDK.DaAvailabilityStateQueueStatus;
    observed: SDK.DaAvailabilityStateQueueStatus;
    detail: string;
  }[]) {
    cases.push({
      prior,
      observed,
      detail,
      agrees: false,
      priorKind: SDK.daAvailabilityStateQueueStatusKind(prior),
      observedKind: SDK.daAvailabilityStateQueueStatusKind(observed),
    });
  }

  it.each(cases)(
    "compares stored $priorKind with observed $observedKind at the same output ($detail)",
    async ({ prior, observed, agrees, observedKind }) => {
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
      if (agrees) {
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

  it("does not treat a retained terminal record as a datum observation at its last spent output", async () => {
    const { header, headerHash } = await makePayloadFixture();
    const config = {
      deploymentFingerprint: "11".repeat(32),
      deploymentIdentityDigest: "11".repeat(32),
      stateQueuePolicyId: "22".repeat(28),
      daAttestationPolicyId: "33".repeat(28),
      finalityDepth: 3,
      consensusProfile: MIDGARD_CONSENSUS_PROFILE,
    };
    const chain = createStateQueueChain({
      ...config,
      headers: [{ header, headerHash }],
      tip: 5,
    });
    const provider = {
      fetchStateQueueNodes: async () => chain.snapshot().nodes,
      fetchStateQueueSnapshot: async () => chain.snapshot(),
      fetchStateQueueReplayCheckpoints: chain.fetchStateQueueReplayCheckpoints,
    };
    let anchor: StateQueueReplayAnchor | undefined;
    const previousHeaders = await scanStateQueue(provider, {
      ...config,
      recordReplayAnchor: (next) => {
        anchor = next;
      },
    });
    // Replay sees outputs, not their datums: if both moves happened offline,
    // a terminal record retains the older status at the newer spent outRef.
    chain.mine({ attest: headerHash });
    const attestedSnapshot = chain.snapshot();
    chain.mine("merge");
    for (let block = 0; block < 4; block += 1) chain.mine();
    const terminal = await scanStateQueue(provider, {
      ...config,
      previousHeaders,
      terminalReplayAnchor: anchor!,
    });
    expect(terminal).toMatchObject([
      {
        status: "merged",
        daAttestation: "Unattested",
        stateQueueOutRef: attestedSnapshot.nodes[0]!.outRef,
      },
    ]);
    // This scanner bootstrap has no durable anchor to assert history from.
    // The retained outcome alone cannot assert the restored output's datum.
    await expect(
      scanStateQueue(
        {
          fetchStateQueueNodes: async () => attestedSnapshot.nodes,
        },
        { ...config, previousHeaders: terminal },
      ),
    ).resolves.toMatchObject([
      {
        status: "attested",
        daAttestation: { Attested: { commitment_hash: "44".repeat(32) } },
      },
    ]);
  });
});
