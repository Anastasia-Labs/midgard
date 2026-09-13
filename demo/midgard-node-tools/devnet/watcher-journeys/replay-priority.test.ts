import { existsSync } from "node:fs";
import { readFile } from "node:fs/promises";
import { join } from "node:path";

import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import {
  buildCountedRoot,
  canonicalBlockEvidenceFromVerifiedPayload,
  detectTransitionTraceFaults,
  DOUBLE_SPEND_COMPLETE_CANONICAL_REPLAY,
} from "@al-ft/midgard-fault-proofs";
import { authenticatedHeaderObservation } from "@al-ft/midgard-fault-proofs/test-support/canonical-block-evidence-fixture";
import { depositEventsRetainedBlock } from "@al-ft/midgard-fault-proofs/test-support/transition-trace-retained";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import type { PublishedDepositTraceCheckpoint } from "midgard-watcher/tests/support/published-deposit-trace";
import { expect, it } from "vitest";

import { readJourneyArtifact } from "./artifacts.js";
import { classifyFullCatalogueTransactionFixture } from "./full-catalogue-verification.js";
import { buildJourneyHistoryTransaction } from "./history-cases.js";

const runDirectory = process.env.MIDGARD_WATCHER_JOURNEY_RUN_DIR;
const stagedPath =
  runDirectory === undefined
    ? undefined
    : join(runDirectory, "work/journeys/transition-trace/staged.json");

it.skipIf(stagedPath === undefined || !existsSync(stagedPath))(
  "keeps an earlier authenticated transition fault ahead of a later rejected transaction in the installed catalogue",
  async () => {
    const staged = await readJourneyArtifact<PublishedDepositTraceCheckpoint>(
      stagedPath!,
    );
    const accounts = JSON.parse(
      await readFile(
        join(runDirectory!, "secrets/journey-accounts.json"),
        "utf8",
      ),
    );
    const predecessor = await depositEventsRetainedBlock({
      operatorVkey: staged.current.header.operatorVkey,
      startTime: staged.predecessor.header.endTime,
      endTime: staged.current.header.endTime,
      blockSlot: staged.current.header.blockSlot,
      prevHeaderHash: staged.predecessor.headerHash,
      prevUtxosRoot: staged.predecessor.header.utxosRoot,
      priorLedger: staged.predecessor.payload.block_body.utxos,
      events: [
        {
          event: staged.depositEvent,
          depositPolicyId: staged.depositMetadata.depositAuthUnit.slice(0, 56),
          assetName: staged.depositMetadata.depositAssetName,
          honest: true,
        },
      ],
    });
    const retained = await buildJourneyHistoryTransaction({
      category: "doubleSpend",
      predecessor,
      ledgerOwnerSeedPhrase: accounts.operator.seedPhrase,
      operatorVkey: staged.current.header.operatorVkey,
      endTime: predecessor.header.endTime + 40_000n,
      blockSlot: predecessor.header.blockSlot + 40n,
    });
    expect(retained.replays.map((replay) => replay.trace.verdict)).toEqual([
      "accepted",
      "rejected",
    ]);

    // The operator falsely claims both transactions left the ledger unchanged.
    // The first accepted terminal and the later rejected terminal remain the
    // exact retained machine traces. Every trace link and final root agrees,
    // so only semantic replay can expose the earlier transition fault.
    const transitions = retained.payload.block_body.transition_trace.map(
      ([key, value]): SDK.DaPayloadEntry => [
        key,
        Data.to(
          {
            ...Data.from(value, SDK.TransitionStep),
            pre_utxos_root: predecessor.header.utxosRoot,
            post_utxos_root: predecessor.header.utxosRoot,
          },
          SDK.TransitionStep,
        ),
      ],
    );
    const transitionRoot = await buildCountedRoot(
      SDK.ROOT_DOMAINS.transitionTrace,
      transitions.map(([key, value]) => ({
        key: Buffer.from(key, "hex"),
        value: Buffer.from(value, "hex"),
      })),
    );
    const header: SDK.Header = {
      ...retained.header,
      utxosRoot: predecessor.header.utxosRoot,
      transitionTraceRoot: transitionRoot.root,
    };
    const headerHash = await Effect.runPromise(SDK.hashBlockHeader(header));
    const payload: SDK.DaPayload = {
      ...retained.payload,
      block_body: {
        ...retained.payload.block_body,
        header,
        header_hash: headerHash,
        utxos: predecessor.payload.block_body.utxos,
        transition_trace: transitions,
      },
    };
    const block = {
      header,
      headerHash,
      payloadEnvelopeCbor: await wrapDaPayload(SDK.encodeDaPayload(payload), {
        mode: "identity",
      }),
    };
    const evidence = await canonicalBlockEvidenceFromVerifiedPayload({
      observation: authenticatedHeaderObservation(block),
      payloadEnvelopeCbor: block.payloadEnvelopeCbor,
      daProvenance: {
        trustClass: "public_or_permissionless_da",
        sourceId: "replay-priority/local-verification",
        grade: "security",
      },
    });
    expect(await detectTransitionTraceFaults(evidence.reconstruction)).toEqual(
      [],
    );
    const direct =
      await DOUBLE_SPEND_COMPLETE_CANONICAL_REPLAY.replay(evidence);
    expect(direct.detections).toHaveLength(1);
    expect(direct.detections[0]?.position).toBe(1n);
    const full = await classifyFullCatalogueTransactionFixture({
      block,
      predecessor,
      history: [staged.predecessor],
    });
    expect(full.decision.decision).toBe("fault_detected");
    if (full.decision.decision !== "fault_detected")
      throw new Error("Installed catalogue omitted the transition fault");
    expect(full.decision.category).toBe("transitionTrace");
    expect(full.decision.position).toBe("0");
  },
);
