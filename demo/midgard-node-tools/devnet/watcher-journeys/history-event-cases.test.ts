import {
  canonicalBlockEvidenceFromVerifiedPayload,
  prepareWithdrawnInputFromCanonicalEvidence,
  prepareWithdrawnReferenceInput,
} from "@al-ft/midgard-fault-proofs";
import { authenticatedHeaderObservation } from "@al-ft/midgard-fault-proofs/test-support/canonical-block-evidence-fixture";
import { depositEventsRetainedBlock } from "@al-ft/midgard-fault-proofs/test-support/transition-trace-retained";
import * as SDK from "@al-ft/midgard-sdk";
import { afterEach, expect, it } from "vitest";

import type { VerifiableJourneyBlock } from "./fixture-verification.js";
import {
  type LocalHistoryEventStage,
  openLocalHistoryEventStage,
  type StagedLocalHistoryEvent,
} from "./history-event-local-staging.js";
import { classifyLocalHistoryEventFixture } from "./history-event-verification.js";
import {
  buildJourneyWithdrawnTransaction,
  journeyWithdrawalBody,
} from "./history-events.js";

const daProvenance = {
  trustClass: "public_or_permissionless_da",
  sourceId: "history-event-journey/local-verification",
  grade: "security",
} as const;

/**
 * One isolated staging chain per family: the recorded raw L1 authority then
 * serves exactly this family's published events, so no other family's event
 * output can fall due inside the window under classification.
 */
let stage: LocalHistoryEventStage;
let blockSlot = 10n;

afterEach(() => {
  stage?.close();
});

/** Let the wall clock advance so a later event authenticates a later second. */
const separateEventSeconds = async () => {
  await new Promise((resolve) => setTimeout(resolve, 2_000));
};

/** A block interval ends at the last inclusion time it must absorb. */
const latestInclusion = (events: readonly StagedLocalHistoryEvent[]) =>
  events.reduce(
    (latest, event) =>
      event.inclusionTime > latest ? event.inclusionTime : latest,
    0n,
  );

/**
 * Open one family's window pair: an empty genesis ancestor, then a predecessor
 * which absorbs `deposits` real published deposit events so the retained ledger
 * owns actual deposited outputs. Block intervals are placed around the exact
 * inclusion times the published event datums authenticate.
 */
const openFamilyWindow = async (deposits: number) => {
  stage = await openLocalHistoryEventStage();
  const staged: StagedLocalHistoryEvent[] = [];
  for (let index = 0; index < deposits; index += 1)
    staged.push(await stage.publishDeposit());
  const opened =
    staged.reduce(
      (earliest, event) =>
        event.inclusionTime < earliest ? event.inclusionTime : earliest,
      staged[0]?.inclusionTime ?? BigInt(Date.now()),
    ) - 1n;
  const genesis = await depositEventsRetainedBlock({
    operatorVkey: stage.operatorVkey,
    startTime: opened - 60_000n,
    endTime: opened,
    blockSlot: blockSlot++,
    prevHeaderHash: SDK.GENESIS_HEADER_HASH,
    prevUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    priorLedger: [],
    events: [],
  });
  const predecessor = await depositEventsRetainedBlock({
    operatorVkey: stage.operatorVkey,
    startTime: opened,
    endTime: latestInclusion(staged),
    blockSlot: blockSlot++,
    prevHeaderHash: genesis.headerHash,
    prevUtxosRoot: genesis.header.utxosRoot,
    priorLedger: genesis.payload.block_body.utxos,
    events: staged.map((event) => ({
      event: event.event,
      depositPolicyId: event.policyId,
      assetName: event.assetName,
      honest: true,
    })),
  });
  await separateEventSeconds();
  return { predecessor, history: [genesis] };
};

const select = async (input: {
  category: SDK.FraudProofCatalogueCategoryName;
  block: VerifiableJourneyBlock;
  predecessor: VerifiableJourneyBlock;
  history: readonly VerifiableJourneyBlock[];
  honest: boolean;
}) => {
  const result = await classifyLocalHistoryEventFixture({
    stage,
    block: input.block,
    predecessor: input.predecessor,
    history: input.history,
  });
  if (input.honest) expect(result.decision.decision).toBe("healthy");
  else {
    expect(result.decision.decision).toBe("fault_detected");
    if (result.decision.decision === "fault_detected")
      expect(result.decision.category).toBe(input.category);
  }
  return result;
};

it.each(["withdrawnInput", "withdrawnReferenceInput"] as const)(
  "%s is selected against its real published withdrawal and its honest control passes",
  async (category) => {
    const { predecessor, history } = await openFamilyWindow(2);
    const body = journeyWithdrawalBody({
      predecessor,
      owner: stage.ownerKey.to_public().hash().to_hex(),
    });
    const withdrawal = await stage.publishWithdrawal(body, 0);
    const endTime = latestInclusion([withdrawal]);
    const slot = blockSlot++;
    for (const honest of [false, true]) {
      const block = await buildJourneyWithdrawnTransaction({
        predecessor,
        operatorVkey: stage.operatorVkey,
        endTime,
        blockSlot: slot,
        category,
        withdrawal,
        ledgerOwnerSeedPhrase: stage.ownerSeedPhrase,
        honest,
      });
      await select({ category, block, predecessor, history, honest });
      const evidence = await canonicalBlockEvidenceFromVerifiedPayload({
        observation: authenticatedHeaderObservation(block),
        payloadEnvelopeCbor: Buffer.from(block.payloadEnvelopeCbor),
        daProvenance,
      });
      if (category === "withdrawnInput") {
        const prepared = prepareWithdrawnInputFromCanonicalEvidence({
          evidence,
        });
        if (honest) await expect(prepared).rejects.toThrow();
        else expect((await prepared).headerHash).toBe(block.headerHash);
      } else {
        const prepared = prepareWithdrawnReferenceInput({
          header: block.header,
          blockTxs: evidence.transactions,
          withdrawals: evidence.reconstruction.withdrawals.map(
            ({ key, value }) => ({ id: key, info: value }),
          ),
        });
        if (honest) await expect(prepared).rejects.toThrow();
        else
          expect((await prepared).withdrawalMembership.root).toBe(
            block.header.withdrawalsRoot,
          );
      }
    }
  },
  1_800_000,
);
