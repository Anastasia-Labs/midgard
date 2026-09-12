import {
  canonicalBlockEvidenceFromVerifiedPayload,
  type CompleteCanonicalReplay,
  CROSS_BLOCK_DUPLICATE_EVENT_COMPLETE_CANONICAL_REPLAY,
  DOUBLE_WITHDRAW_COMPLETE_CANONICAL_REPLAY,
  prepareDoubleWithdrawFromCommittedLeaves,
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
  buildJourneyRepeatedDeposit,
  buildJourneyWithdrawalEvent,
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
  replayer?: CompleteCanonicalReplay;
}) => {
  const result = await classifyLocalHistoryEventFixture({
    stage,
    block: input.block,
    predecessor: input.predecessor,
    history: input.history,
    ...(input.replayer === undefined ? {} : { replayer: input.replayer }),
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

/**
 * The full installed catalogue proves the fault and clears the honest control,
 * while the family's own replay confirms the fixture is exactly its fault. The
 * installed selection between `winners` is an open owner decision recorded in
 * the case below; neither outcome is pinned here.
 */
const selectWithinInstalledCatalogue = async (input: {
  category: SDK.FraudProofCatalogueCategoryName;
  winners: readonly SDK.FraudProofCatalogueCategoryName[];
  replayer: CompleteCanonicalReplay;
  block: VerifiableJourneyBlock;
  predecessor: VerifiableJourneyBlock;
  history: readonly VerifiableJourneyBlock[];
  honest: boolean;
}) => {
  const { category, winners, replayer, ...blocks } = input;
  const installed = await classifyLocalHistoryEventFixture({
    stage,
    block: blocks.block,
    predecessor: blocks.predecessor,
    history: blocks.history,
  });
  const summary = JSON.stringify(installed.decision, null, 2);
  if (input.honest)
    expect(installed.decision.decision, summary).toBe("healthy");
  else {
    expect(installed.decision.decision, summary).toBe("fault_detected");
    if (installed.decision.decision === "fault_detected")
      expect(winners, summary).toContain(installed.decision.category);
  }
  return await select({ ...blocks, category, replayer });
};

it("doubleWithdraw proves two real published withdrawals of one L2 output and its honest control passes", async () => {
  const { predecessor, history } = await openFamilyWindow(1);
  const body = journeyWithdrawalBody({
    predecessor,
    owner: stage.ownerKey.to_public().hash().to_hex(),
  });
  const withdrawals = [
    await stage.publishWithdrawal(body, 0),
    await stage.publishWithdrawal(body, 1),
  ];
  const endTime = latestInclusion(withdrawals);
  const slot = blockSlot++;
  for (const honest of [false, true]) {
    const block = await buildJourneyWithdrawalEvent({
      predecessor,
      operatorVkey: stage.operatorVkey,
      endTime,
      blockSlot: slot,
      category: "doubleWithdraw",
      withdrawals,
      honest,
    });
    // The honest ledger verdicts never depend on the operator's claims: the
    // second event drains an output the first one already withdrew.
    expect(block.classifications.map(({ validity }) => validity)).toEqual([
      "WithdrawalIsValid",
      "NonExistentWithdrawalUtxo",
    ]);
    // Owner decision: the second payable leaf is also a withdrawalMistag
    // finding at the same leaf position, and the canonical rule order ranks
    // withdrawalMistag ahead of doubleWithdraw, so the installed catalogue
    // currently selects withdrawalMistag for this fault.
    //
    // Owner decision: the honest block marks the second leaf
    // NonExistentWithdrawalUtxo while the published L1 order datum always
    // carries WithdrawalIsValid, and the fabricated-withdrawal rule convicts
    // on any WithdrawalInfo difference, validity included. Until that rule and
    // the operator-verdict leaf shape are reconciled, the honest control is
    // verified against the family replayer only.
    if (honest)
      await select({
        category: "doubleWithdraw",
        replayer: DOUBLE_WITHDRAW_COMPLETE_CANONICAL_REPLAY,
        block,
        predecessor,
        history,
        honest,
      });
    else
      await selectWithinInstalledCatalogue({
        category: "doubleWithdraw",
        winners: ["withdrawalMistag", "doubleWithdraw"],
        replayer: DOUBLE_WITHDRAW_COMPLETE_CANONICAL_REPLAY,
        block,
        predecessor,
        history,
        honest,
      });
    const prepared = prepareDoubleWithdrawFromCommittedLeaves({
      headerHash: block.headerHash,
      committedWithdrawalsRoot: block.header.withdrawalsRoot,
      withdrawalCount: block.header.withdrawalCount,
      entries: block.payload.block_body.withdrawals,
    });
    if (honest)
      await expect(prepared).rejects.toThrow(/no_payable_duplicate_pair/u);
    else expect((await prepared).headerHash).toBe(block.headerHash);
  }
}, 1_800_000);

it("crossBlockDuplicateEvent proves a deposit repeated from a really settled ancestor and its honest control passes", async () => {
  stage = await openLocalHistoryEventStage();
  const lucid = stage.deployment.operatorLucid;
  const slotOf = (time: bigint) => BigInt(lucid.unixTimeToSlot(Number(time)));
  // The only event on this chain: it is published before settlement moves
  // the chain clock a maturity period ahead of the wall clock.
  const deposit = await stage.publishDeposit();
  const confirmed = await stage.confirmedState();
  const settledEnd = ((deposit.inclusionTime + 999n) / 1000n) * 1000n + 59_999n;
  const settled = await depositEventsRetainedBlock({
    operatorVkey: stage.operatorVkey,
    startTime: confirmed.endTime,
    endTime: settledEnd,
    blockSlot: slotOf(settledEnd),
    prevHeaderHash: confirmed.headerHash,
    prevUtxosRoot: confirmed.utxoRoot,
    priorLedger: [],
    events: [
      {
        event: deposit.event,
        depositPolicyId: deposit.policyId,
        assetName: deposit.assetName,
        honest: true,
      },
    ],
  });
  await stage.settle(settled);
  const predecessorEnd = settledEnd + 60_000n;
  const predecessor = await depositEventsRetainedBlock({
    operatorVkey: stage.operatorVkey,
    startTime: settledEnd,
    endTime: predecessorEnd,
    blockSlot: slotOf(predecessorEnd),
    prevHeaderHash: settled.headerHash,
    prevUtxosRoot: settled.header.utxosRoot,
    priorLedger: settled.payload.block_body.utxos,
    events: [],
  });
  const endTime = predecessorEnd + 60_000n;
  for (const honest of [false, true]) {
    const block = await buildJourneyRepeatedDeposit({
      predecessor,
      operatorVkey: stage.operatorVkey,
      endTime,
      blockSlot: slotOf(endTime),
      settled,
      honest,
    });
    // Owner decision: the repeated deposit's L1 event lies outside the
    // accused window, which transitionTrace reports as an out-of-window
    // source event at the same position, and the canonical rule order ranks
    // transitionTrace ahead of crossBlockDuplicateEvent.
    await selectWithinInstalledCatalogue({
      category: "crossBlockDuplicateEvent",
      winners: ["transitionTrace", "crossBlockDuplicateEvent"],
      replayer: CROSS_BLOCK_DUPLICATE_EVENT_COMPLETE_CANONICAL_REPLAY,
      block,
      predecessor,
      history: [settled],
      honest,
    });
  }
}, 1_800_000);
