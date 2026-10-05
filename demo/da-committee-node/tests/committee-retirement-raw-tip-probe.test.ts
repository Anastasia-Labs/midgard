import { mkdtemp } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { daRetentionPruneDecision } from "@al-ft/midgard-core";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { checkL1RollbackFeed } from "../src/committee-service.check-l1-rollback-feed.js";
import { FileChainSyncConsumerCursorStore } from "../src/l1/provider.file-chain-sync-consumer-cursor-store.js";
import { FileChainSyncCursorStore } from "../src/l1/provider.file-chain-sync-cursor-store.js";
import { LocalNodeChainAuthority } from "../src/l1/provider.local-node-chain-authority.js";
import { LocalNodeStateQueueProvider } from "../src/l1/provider.local-node-state-queue-provider.js";
import { fetchOgmiosTipBlockNo } from "../src/l1/state-queue-replay-provider.create-local-kupmios-state-queue-replay-provider.js";
import { scanStateQueue } from "../src/l1/state-queue-scanner.js";
import { terminalRetentionOutcomes } from "../src/l1/terminal-retention-observation.js";
import { terminalRecoveryFinal } from "../src/store/retention.js";
import {
  after,
  before,
  deployment,
  descendantHeader,
  harness,
  policy,
  targetHeader,
} from "./state-queue-replay-provider.harness.js";
import { record, snapshot } from "./terminal-retention-observation.derive.js";

describe("committee raw selected tip retirement authority probe", () => {
  it.each([2250, 100])(
    "HTTP2251 and raw selected-tip %i cannot authorize retirement",
    async (rawTipHeight) => {
      const dir = await mkdtemp(join(tmpdir(), "codex-rel-committee-tip-"));
      const point = {
        network: "Preview",
        slot: 2261,
        blockHash: "99".repeat(32),
        providerSource: "chain-sync:node-a",
        observedAt: "2026-10-02T00:00:00.000Z",
      };
      const source = {
        next: async () => ({
          event: { direction: "roll_forward" as const, point },
          tip: point,
        }),
      };
      const authority = new LocalNodeChainAuthority(
        "node-a",
        "Preview",
        source,
        new FileChainSyncCursorStore(join(dir, "cursor.json"), "11".repeat(32)),
      );
      const first = {
        ...record(before[1]!.headerHash!, before[1]!.outRef),
        header: targetHeader,
        blockAssetName:
          SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + before[1]!.headerHash,
      };
      const removed = {
        ...record(before[2]!.headerHash!, before[2]!.outRef),
        header: descendantHeader,
        blockAssetName:
          SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + before[2]!.headerHash,
      };
      const current = {
        ...first,
        stateQueueOutRef: after[1]!.outRef,
        finalized: true,
      };
      const node = {
        outRef: current.stateQueueOutRef,
        assetName: current.blockAssetName,
        linkedListKey: current.headerHash,
        header: current.header,
        daAttestation: current.daAttestation,
        chainPoint: { ...point, depth: 30, finalized: true },
      };
      const readSnapshotHeight = () =>
        fetchOgmiosTipBlockNo("ws://ogmios.test", async (_url, init) => {
          expect(JSON.parse(String(init?.body)).method).toBe(
            "queryNetwork/blockHeight",
          );
          return new Response(JSON.stringify({ jsonrpc: "2.0", result: 2251 }));
        });
      const replay = harness({
        tipHeight: await readSnapshotHeight(),
        rawTipHeight,
      });
      const local = new LocalNodeStateQueueProvider(
        authority,
        [
          {
            currentChainPoint: async () => point,
            fetchStateQueueNodes: async () => [node],
            fetchStateQueueSnapshot: async () => ({
              ...snapshot("00".repeat(28), before[0]!.outRef),
              nodes: [node],
              observedChainPoint: point,
              tipBlockNo: await readSnapshotHeight(),
            }),
            fetchStateQueueReplayCheckpoints: (previous, current) =>
              replay(previous, current),
          },
        ],
        ["query:node-a:0"],
        new FileChainSyncConsumerCursorStore(
          join(dir, "consumer.json"),
          "11".repeat(32),
        ),
      );
      const acceptedSnapshot = await local.fetchStateQueueSnapshot();
      expect(acceptedSnapshot.tipBlockNo).toBe(2251);
      expect(local.chainSyncCatchUpProgress()).toBeUndefined();
      const checkpoints = await local.fetchStateQueueReplayCheckpoints(
        before,
        after,
        acceptedSnapshot.tipBlockNo!,
        64,
      );
      expect(checkpoints[0]!.blockNo).toBe("90");
      expect(checkpoints[0]!.finalityDepth).toBe(
        (rawTipHeight - 90 + 1).toString(),
      );
      expect(rawTipHeight - 90).toBeLessThanOrEqual(2160);
      const scanConfig = {
        deploymentFingerprint: deployment,
        deploymentIdentityDigest: deployment,
        stateQueuePolicyId: policy,
        daAttestationPolicyId: "cc".repeat(28),
        finalityDepth: 10,
        automaticRecoveryMaxDepth: 2160,
        consensusProfile: MIDGARD_CONSENSUS_PROFILE,
        previousHeaders: [first, removed],
        terminalReplayAnchor: {
          deploymentIdentityDigest: deployment,
          stateQueuePolicyId: policy,
          queue: before,
          blockNo: "0",
          transactionIndex: "0",
        },
      };
      const actualScanned = await scanStateQueue(local, scanConfig);
      expect(actualScanned.flatMap((r) => r.validationErrors)).toEqual([]);
      expect(
        actualScanned.find((r) => r.headerHash === removed.headerHash)
          ?.observedChainPoint.depth,
      ).toBe(rawTipHeight - 90);
      const observation = terminalRetentionOutcomes(
        [first, removed],
        [current],
        checkpoints,
        { ...acceptedSnapshot, nodes: [] },
        {
          deploymentFingerprint: deployment,
          deploymentIdentityDigest: deployment,
          stateQueuePolicyId: policy,
          finalityDepth: 10,
          automaticRecoveryMaxDepth: 2160,
          replayAnchor: {
            deploymentIdentityDigest: deployment,
            stateQueuePolicyId: policy,
            queue: before,
            blockNo: "0",
            transactionIndex: "0",
          },
        },
      );
      const terminal = observation.records.find(
        (r) => r.headerHash === removed.headerHash,
      )!;
      expect(terminal.status).toBe("removed");
      expect(terminal.observedChainPoint.depth).toBe(rawTipHeight - 90);
      expect(observation.finalAnchor).toBeUndefined();
      const rollback = await checkL1RollbackFeed(
        undefined,
        local,
        acceptedSnapshot.chainSyncCursor,
      );
      expect(rollback.failure).toBeUndefined();
      expect(
        terminalRecoveryFinal(
          terminal,
          {
            automaticRecoveryMaxDepth: 2160,
            deploymentFingerprint: deployment,
          },
          deployment,
        ),
      ).toBe(false);
      expect(
        daRetentionPruneDecision({
          nowMs: 0,
          blockEndTimeMs: 0,
          headerStatus: terminal.status,
          queueReference: "none",
          terminalRecoveryFinal: terminalRecoveryFinal(
            terminal,
            {
              automaticRecoveryMaxDepth: 2160,
              deploymentFingerprint: deployment,
            },
            deployment,
          ),
        }).decision,
      ).toBe("retain");
    },
  );
});
