import { existsSync } from "node:fs";
import { join } from "node:path";

import { canonicalBlockEvidenceFromVerifiedPayload } from "@al-ft/midgard-fault-proofs";
import { authenticatedHeaderObservation } from "@al-ft/midgard-fault-proofs/test-support/canonical-block-evidence-fixture";
import { prepareFabricatedDepositFromCommittedLeaves } from "@al-ft/midgard-fault-proofs/test-support/prepare-fabricated-deposit";
import type { PublishedDepositTraceCheckpoint } from "midgard-watcher/tests/support/published-deposit-trace";
import { expect, it } from "vitest";

import { readJourneyArtifact } from "./artifacts.js";
import { buildJourneyFabricatedDeposit } from "./history-events.js";

const runDirectory = process.env.MIDGARD_WATCHER_JOURNEY_RUN_DIR;
const path =
  runDirectory === undefined
    ? undefined
    : join(runDirectory, "work/journeys/transition-trace/staged.json");

it.skipIf(path === undefined || !existsSync(path))(
  "binds diverted deposit proof to the retained real event and refuses its honest control",
  async () => {
    const staged = await readJourneyArtifact<PublishedDepositTraceCheckpoint>(
      path!,
    );
    for (const honest of [false, true]) {
      const block = await buildJourneyFabricatedDeposit({
        predecessor: staged.predecessor,
        operatorVkey: staged.current.header.operatorVkey,
        endTime: staged.current.header.endTime,
        blockSlot: staged.current.header.blockSlot,
        deposit: {
          event: staged.depositEvent,
          policyId: staged.depositMetadata.depositAuthUnit.slice(0, 56),
          assetName: staged.depositMetadata.depositAssetName,
        },
        honest,
      });
      await canonicalBlockEvidenceFromVerifiedPayload({
        observation: authenticatedHeaderObservation(block),
        payloadEnvelopeCbor: block.payloadEnvelopeCbor,
        daProvenance: {
          trustClass: "public_or_permissionless_da",
          sourceId: "retained-real-event/local-check",
          grade: "security",
        },
      });
      // This checks exact retained real event bytes under local observation
      // metadata. It does not claim new L1 observation or a full selector pass.
      const proof = prepareFabricatedDepositFromCommittedLeaves({
        headerHash: block.headerHash,
        committedDepositsRoot: block.header.depositsRoot,
        depositCount: block.header.depositCount,
        headerStartTime: block.header.startTime,
        headerEndTime: block.header.endTime,
        entries: block.payload.block_body.deposits,
        witness: {
          kind: "present_event",
          observation: authenticatedHeaderObservation(block),
          depositEventPolicyId: staged.depositMetadata.depositAuthUnit.slice(
            0,
            56,
          ),
          observedEventAssetName: staged.depositMetadata.depositAssetName,
          eventDatumCbor: staged.depositEvent.datum!,
        },
        minimumConfirmationDepth: 30,
      });
      if (honest) await expect(proof).rejects.toThrow();
      else expect((await proof).headerHash).toBe(block.headerHash);
    }
  },
);
