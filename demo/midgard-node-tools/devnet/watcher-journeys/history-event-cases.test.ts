import { createHash } from "node:crypto";
import { mkdirSync, writeFileSync } from "node:fs";
import { join } from "node:path";

import {
  aikenSerialisedPlutusDataCborPreservingMapOrder as canonical,
  plutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import {
  buildCountedRoot,
  canonicalBlockEvidenceFromVerifiedPayload,
  type CompleteCanonicalReplay,
  CROSS_BLOCK_DUPLICATE_EVENT_COMPLETE_CANONICAL_REPLAY,
  DOUBLE_WITHDRAW_COMPLETE_CANONICAL_REPLAY,
  keyValuePhasProof,
  prepareDoubleWithdrawFromCommittedLeaves,
  prepareWithdrawnInputFromCanonicalEvidence,
  prepareWithdrawnReferenceInput,
  resolveProverSigner,
  WITHDRAWAL_MISTAG_COMPLETE_CANONICAL_REPLAY,
} from "@al-ft/midgard-fault-proofs";
import { authenticatedHeaderObservation } from "@al-ft/midgard-fault-proofs/test-support/canonical-block-evidence-fixture";
import { prepareFabricatedDepositFromCommittedLeaves } from "@al-ft/midgard-fault-proofs/test-support/prepare-fabricated-deposit";
import { prepareFabricatedWithdrawalFromCommittedLeaves } from "@al-ft/midgard-fault-proofs/test-support/prepare-fabricated-withdrawal";
import {
  checkFreshEligibleFamilyRefusal,
  runRetiredFamilyProof,
} from "@al-ft/midgard-fault-proofs/test-support/retired-family-proof";
import { depositEventsRetainedBlock } from "@al-ft/midgard-fault-proofs/test-support/transition-trace-retained";
import { measureCompleteSignedTransaction } from "@al-ft/midgard-fault-proofs/testing/installed-workflow";
import * as SDK from "@al-ft/midgard-sdk";
import { CML, Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { afterEach, expect, it, vi } from "vitest";

import type { VerifiableJourneyBlock } from "./fixture-verification.js";
import {
  type LocalHistoryEventStage,
  openLocalHistoryEventStage,
  type StagedLocalHistoryEvent,
} from "./history-event-local-staging.js";
import { classifyLocalHistoryEventFixture } from "./history-event-verification.js";
import {
  buildJourneyFabricatedDeposit,
  buildJourneyRepeatedDeposit,
  buildJourneyRepeatedWithdrawal,
  buildJourneyWithdrawalEvent,
  buildJourneyWithdrawnTransaction,
  captureStagedHistoryEvent,
  journeyWithdrawalBody,
} from "./history-events.js";

const rawEventCbor = (event: StagedLocalHistoryEvent) =>
  canonical(
    plutusConstrFieldCbor(captureStagedHistoryEvent(event).openingCbor, [0, 0]),
  );

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
  vi.useRealTimers();
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

/** Keep SDK wall-clock validity aligned with actual emulator advances. */
const openSynchronizedStage = async () => {
  const opened = await openLocalHistoryEventStage();
  vi.useFakeTimers({ toFake: ["Date"] });
  const chain = opened.deployment.chain;
  const advance = chain.awaitLedgerTime.bind(chain);
  vi.spyOn(chain, "awaitLedgerTime").mockImplementation(async (time) => {
    await advance(time);
    vi.setSystemTime(new Date(chain.now()));
  });
  return opened;
};

/**
 * Open one family's window pair: an empty genesis ancestor, then a predecessor
 * which absorbs `deposits` real published deposit events so the retained ledger
 * owns actual deposited outputs. Block intervals are placed around the exact
 * inclusion times the published event datums authenticate.
 */
const openFamilyWindow = async (deposits: number) => {
  stage = await openSynchronizedStage();
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
    events: staged.map((event) => {
      const { order } = event;
      if (order.kind !== "Deposit")
        throw new Error("Published deposit resolved to a withdrawal Order");
      return {
        eventCbor: rawEventCbor(event),
        originalAssets: order.originalAssets,
        honest: true,
      };
    }),
  });
  await separateEventSeconds();
  return { predecessor, history: [genesis] };
};

/** Use the deployed bounds and actual hub/history outputs for proof preparation. */
const historyWitness = async (
  block: VerifiableJourneyBlock,
  event: StagedLocalHistoryEvent,
  witness: SDK.EventHistoryWitness = event.order.history,
) => {
  const { contracts, operatorLucid } = stage.deployment;
  const pair = SDK.requireEventHistoryContracts(contracts);
  const history =
    event.order.kind === "Deposit" ? pair.deposit : pair.withdrawal;
  const hub = await Effect.runPromise(
    SDK.fetchHubOracleUTxOProgram(operatorLucid, {
      hubOracleAddress: contracts.hubOracle.spendingScriptAddress,
      hubOraclePolicyId: contracts.hubOracle.policyId,
    }),
  );
  return {
    observation: authenticatedHeaderObservation(block),
    hubOraclePolicyId: contracts.hubOracle.policyId,
    hubOracleUtxo: hub.utxo,
    network: "Custom" as const,
    history: {
      retentionAddress: history.retention.spendingScriptAddress,
      inlineLimitBytes: history.recipe.inlineLimitBytes,
      maxPayloadBytes: history.recipe.maxPayloadBytes,
      maxPayloadNodes: history.recipe.maxPayloadNodes,
    },
    anchor: witness.anchor.utxo,
    ...(witness.kind !== "Present" || witness.retainedDataUtxo === undefined
      ? {}
      : { retainedDataUtxo: witness.retainedDataUtxo }),
  };
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

it("ordinary history admission and withdrawn-input honest control stay healthy", async () => {
  const { predecessor, history } = await openFamilyWindow(2);
  const body = journeyWithdrawalBody({
    predecessor,
    owner: stage.ownerKey.to_public().hash().to_hex(),
  });
  const withdrawal = await stage.publishWithdrawal(body, 0);
  const block = await buildJourneyWithdrawnTransaction({
    predecessor,
    operatorVkey: stage.operatorVkey,
    endTime: latestInclusion([withdrawal]),
    blockSlot: blockSlot++,
    category: "withdrawnInput",
    withdrawal,
    ledgerOwnerSeedPhrase: stage.ownerSeedPhrase,
    honest: true,
  });
  await select({
    category: "withdrawnInput",
    block,
    predecessor,
    history,
    honest: true,
  });
}, 1_800_000);

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
    // The honest control's second leaf carries the NonExistentWithdrawalUtxo
    // verdict it actually earned, which differs from the WithdrawalIsValid
    // placeholder the published L1 order datum always carries. Decision 0007
    // (docs/fault-proofs/decisions/0007-operator-owned-event-validity.md)
    // excludes the committed validity from the fabricated-withdrawal
    // comparison, so that leaf is not a fabrication and the honest control
    // runs through the whole installed catalogue like the fault block.
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
  stage = await openSynchronizedStage();
  const lucid = stage.deployment.operatorLucid;
  const slotOf = (time: bigint) => BigInt(lucid.unixTimeToSlot(Number(time)));
  // The only event on this chain: it is published before settlement moves
  // the chain clock a maturity period ahead of the wall clock.
  const deposit = await stage.publishDeposit();
  if (deposit.order.kind !== "Deposit")
    throw new Error("Published deposit resolved to a withdrawal Order");
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
        eventCbor: rawEventCbor(deposit),
        originalAssets: deposit.order.originalAssets,
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

it("fabricatedDeposit proves a committed deposit leaf that is not its real published L1 event and its honest control passes", async () => {
  const { predecessor, history } = await openFamilyWindow(1);
  const deposit = await stage.publishDeposit();
  const endTime = latestInclusion([deposit]);
  const slot = blockSlot++;
  for (const honest of [false, true]) {
    const block = await buildJourneyFabricatedDeposit({
      predecessor,
      operatorVkey: stage.operatorVkey,
      endTime,
      blockSlot: slot,
      deposit,
      honest,
    });
    // Decision 0007: a committed deposit leaf whose authentic L1 event differs
    // is directly this family's fraud, not a replay abort, so the installed
    // catalogue must name fabricatedDeposit and nothing else.
    await select({
      category: "fabricatedDeposit",
      block,
      predecessor,
      history,
      honest,
    });
    const prepared = prepareFabricatedDepositFromCommittedLeaves({
      headerHash: block.headerHash,
      committedDepositsRoot: block.header.depositsRoot,
      depositCount: block.header.depositCount,
      headerStartTime: block.header.startTime,
      headerEndTime: block.header.endTime,
      entries: block.payload.block_body.deposits,
      witness: await historyWitness(block, deposit),
      minimumConfirmationDepth: 30,
    });
    if (honest) await expect(prepared).rejects.toThrow();
    else expect((await prepared).headerHash).toBe(block.headerHash);
  }
}, 1_800_000);

it("fabricatedWithdrawal proves a committed withdrawal body that is not its real published L1 order and its honest control passes", async () => {
  const { predecessor, history } = await openFamilyWindow(1);
  const body = journeyWithdrawalBody({
    predecessor,
    owner: stage.ownerKey.to_public().hash().to_hex(),
  });
  const withdrawal = await stage.publishWithdrawal(body, 0);
  const endTime = latestInclusion([withdrawal]);
  const slot = blockSlot++;
  for (const honest of [false, true]) {
    const block = await buildJourneyWithdrawalEvent({
      predecessor,
      operatorVkey: stage.operatorVkey,
      endTime,
      blockSlot: slot,
      category: "fabricatedWithdrawal",
      withdrawals: [withdrawal],
      honest,
    });
    // Decision 0007: the committed leaf's body is compared against the
    // authentic L1 order and the committed validity verdict is not, so the
    // diverted payout address is the whole fault and the honest control,
    // which commits the verdict the ledger earned, is clean.
    await select({
      category: "fabricatedWithdrawal",
      block,
      predecessor,
      history,
      honest,
    });
    const prepared = prepareFabricatedWithdrawalFromCommittedLeaves({
      headerHash: block.headerHash,
      committedWithdrawalsRoot: block.header.withdrawalsRoot,
      withdrawalCount: block.header.withdrawalCount,
      headerStartTime: block.header.startTime,
      headerEndTime: block.header.endTime,
      entries: block.payload.block_body.withdrawals,
      witness: await historyWitness(block, withdrawal),
      minimumConfirmationDepth: 30,
    });
    if (honest) await expect(prepared).rejects.toThrow();
    else expect((await prepared).headerHash).toBe(block.headerHash);
  }
}, 1_800_000);

it("withdrawalMistag proves a wrong verdict on a real published withdrawal and its honest control passes", async () => {
  const { predecessor, history } = await openFamilyWindow(1);
  const body = journeyWithdrawalBody({
    predecessor,
    owner: stage.ownerKey.to_public().hash().to_hex(),
  });
  const withdrawal = await stage.publishWithdrawal(body, 0);
  const endTime = latestInclusion([withdrawal]);
  const slot = blockSlot++;
  for (const honest of [false, true]) {
    const block = await buildJourneyWithdrawalEvent({
      predecessor,
      operatorVkey: stage.operatorVkey,
      endTime,
      blockSlot: slot,
      category: "withdrawalMistag",
      withdrawals: [withdrawal],
      honest,
    });
    // The fault leaf keeps the authentic L1 body and only stamps the wrong
    // verdict on it, which decision 0007 assigns to this family alone: the
    // fabricated family excludes validity from its comparison.
    expect(block.classifications.map(({ validity }) => validity)).toEqual([
      "WithdrawalIsValid",
    ]);
    await select({
      category: "withdrawalMistag",
      block,
      predecessor,
      history,
      honest,
    });
    await select({
      category: "withdrawalMistag",
      replayer: WITHDRAWAL_MISTAG_COMPLETE_CANONICAL_REPLAY,
      block,
      predecessor,
      history,
      honest,
    });
  }
}, 1_800_000);

it.each(["Deposit", "Withdrawal"] as const)(
  "%s retired ID remains challengeable in a later header while a fresh eligible event stays healthy",
  async (kind) => {
    stage = await openSynchronizedStage();
    const lucid = stage.deployment.operatorLucid;
    const records: unknown[] = [];
    const record = (entry: unknown) => {
      records.push(entry);
      const directory = process.env.MIDGARD_EVENT_HISTORY_EVIDENCE_DIR;
      if (directory === undefined) return;
      mkdirSync(directory, { recursive: true });
      writeFileSync(
        join(directory, `retired-${kind.toLowerCase()}-proof.json`),
        JSON.stringify(
          {
            scope:
              "Actual local admission/merge/retirement, published catalogue and production proof builders; synthetic local raw finality, honest parent remains unmerged",
            blueprintSha256: createHash("sha256")
              .update(stage.deployment.blueprintJson)
              .digest("hex"),
            manifest: stage.deployment.manifest,
            records,
          },
          (_, value: unknown) =>
            typeof value === "bigint" ? value.toString() : value,
          2,
        ) + "\n",
      );
    };
    const slotOf = (time: bigint) => BigInt(lucid.unixTimeToSlot(Number(time)));
    const closeMinute = (time: bigint) =>
      ((time + 999n) / 1000n) * 1000n + 59_999n;
    const deposits = [await stage.publishDeposit()];
    if (kind === "Withdrawal") deposits.push(await stage.publishDeposit());
    const confirmed = await stage.confirmedState();
    const depositEnd = closeMinute(latestInclusion(deposits));
    const deposited = await depositEventsRetainedBlock({
      operatorVkey: stage.operatorVkey,
      startTime: confirmed.endTime,
      endTime: depositEnd,
      blockSlot: slotOf(depositEnd),
      prevHeaderHash: confirmed.headerHash,
      prevUtxosRoot: confirmed.utxoRoot,
      priorLedger: [],
      events: deposits.map((event) => {
        const { order } = event;
        if (order.kind !== "Deposit") throw new Error("Expected deposit Order");
        return {
          eventCbor: rawEventCbor(event),
          originalAssets: order.originalAssets,
          honest: true,
        };
      }),
    });
    let source: VerifiableJourneyBlock & { payload: SDK.DaPayload } = deposited;
    let settlement = await stage.settle(source);
    let retired = deposits[0]!;
    const history: VerifiableJourneyBlock[] = [];
    if (kind === "Withdrawal") {
      history.push(deposited);
      retired = await stage.publishWithdrawal(
        journeyWithdrawalBody({
          predecessor: deposited,
          owner: stage.ownerKey.to_public().hash().to_hex(),
        }),
        0,
      );
      const withdrawalEnd = closeMinute(retired.inclusionTime);
      const withdrawalSource = await buildJourneyWithdrawalEvent({
        predecessor: deposited,
        operatorVkey: stage.operatorVkey,
        endTime: withdrawalEnd,
        blockSlot: slotOf(withdrawalEnd),
        category: "fabricatedWithdrawal",
        withdrawals: [retired],
        honest: true,
      });
      expect(withdrawalSource.classifications[0]?.validity).toBe(
        "WithdrawalIsValid",
      );
      source = withdrawalSource;
      settlement = await stage.settle(source);
    }
    expect(await stage.confirmedState()).toEqual({
      headerHash: source.headerHash,
      utxoRoot: source.header.utxosRoot,
      endTime: source.header.endTime,
    });
    const originalOrder = retired.order.utxo;
    expect(await lucid.utxosByOutRef([originalOrder])).toHaveLength(1);
    const retirement = await stage.retire(retired, source, settlement);
    record({
      stage: "retirement",
      ...retirement,
      witness: { kind: retirement.witness.kind },
      ...measureCompleteSignedTransaction(retirement.signedCbor),
    });
    const body = CML.Transaction.from_cbor_hex(retirement.signedCbor).body();
    expect(CML.hash_transaction(body).to_hex()).toBe(retirement.txHash);
    expect(
      body
        .mint()
        ?.get(
          CML.ScriptHash.from_hex(retired.policyId),
          CML.AssetName.from_hex(retired.order.assetName),
        ),
    ).toBe(-1n);
    expect(await lucid.utxosByOutRef([originalOrder])).toHaveLength(0);
    expect(
      await lucid.utxosAtWithUnit(
        originalOrder.address,
        retired.policyId + retired.order.assetName,
      ),
    ).toHaveLength(0);
    expect(retirement.witness.kind).toBe("Absent");
    // A fresh nonce and current inclusion distinguish the eligible control from the retired leaf.
    const fresh =
      kind === "Deposit"
        ? await stage.publishDeposit()
        : await stage.publishWithdrawal(
            journeyWithdrawalBody({
              predecessor: source,
              owner: stage.ownerKey.to_public().hash().to_hex(),
            }),
            1,
          );
    expect(fresh.order.assetName).not.toBe(retired.order.assetName);
    expect(fresh.inclusionTime).toBeGreaterThan(source.header.endTime);
    const honestTimed = {
      predecessor: source,
      operatorVkey: stage.operatorVkey,
      endTime: closeMinute(fresh.inclusionTime),
      blockSlot: slotOf(closeMinute(fresh.inclusionTime)),
      honest: true,
    };
    const honest =
      kind === "Deposit"
        ? await buildJourneyFabricatedDeposit({
            ...honestTimed,
            deposit: fresh,
          })
        : await buildJourneyWithdrawalEvent({
            ...honestTimed,
            category: "fabricatedWithdrawal",
            withdrawals: [fresh],
          });
    await select({
      category:
        kind === "Deposit" ? "fabricatedDeposit" : "fabricatedWithdrawal",
      block: honest,
      predecessor: source,
      history,
      honest: true,
    });
    const honestWitness = await historyWitness(honest, fresh);
    const honestPlan =
      kind === "Deposit"
        ? prepareFabricatedDepositFromCommittedLeaves({
            headerHash: honest.headerHash,
            committedDepositsRoot: honest.header.depositsRoot,
            depositCount: honest.header.depositCount,
            headerStartTime: honest.header.startTime,
            headerEndTime: honest.header.endTime,
            entries: honest.payload.block_body.deposits,
            witness: honestWitness,
            minimumConfirmationDepth: 30,
          })
        : prepareFabricatedWithdrawalFromCommittedLeaves({
            headerHash: honest.headerHash,
            committedWithdrawalsRoot: honest.header.withdrawalsRoot,
            withdrawalCount: honest.header.withdrawalCount,
            headerStartTime: honest.header.startTime,
            headerEndTime: honest.header.endTime,
            entries: honest.payload.block_body.withdrawals,
            witness: honestWitness,
            minimumConfirmationDepth: 30,
          });
    await expect(honestPlan).rejects.toThrow(
      /authentic_content_matches_commitment/u,
    );
    const proofInclusion = async (
      block: VerifiableJourneyBlock & { payload: SDK.DaPayload },
      event: StagedLocalHistoryEvent,
    ) => {
      const entries =
        kind === "Deposit"
          ? block.payload.block_body.deposits
          : block.payload.block_body.withdrawals;
      const keyCbor =
        kind === "Deposit"
          ? SDK.committedDepositKeyBytes(event.order.event.id)
          : SDK.committedWithdrawalKeyBytes(event.order.event.id);
      const valueCbor = canonical(
        plutusConstrFieldCbor(rawEventCbor(event), [1]),
      );
      expect(entries).toContainEqual([keyCbor, valueCbor]);
      const tree = await buildCountedRoot(
        kind === "Deposit"
          ? SDK.ROOT_DOMAINS.deposits
          : SDK.ROOT_DOMAINS.withdrawals,
        entries.map(([key, value]) => ({
          key: Buffer.from(key, "hex"),
          value: Buffer.from(value, "hex"),
        })),
      );
      expect(tree.root).toBe(
        kind === "Deposit"
          ? block.header.depositsRoot
          : block.header.withdrawalsRoot,
      );
      return {
        keyCbor,
        valueCbor,
        phasRoot: tree.phasRoot,
        membershipProofCbor: Data.to(
          await keyValuePhasProof(
            { ...tree, root: tree.phasRoot },
            Buffer.from(keyCbor, "hex"),
            Buffer.from(valueCbor, "hex"),
          ),
          SDK.Proof,
        ),
      };
    };
    const signer = resolveProverSigner({
      network: "Custom",
      walletSeedPhrase: stage.ownerSeedPhrase,
    });
    // The published operator is funded at a base address; production proof signers use enterprise addresses.
    const funding = await lucid
      .newTx()
      .pay.ToAddress(signer.address, { lovelace: 1_000_000_000n })
      .pay.ToAddress(signer.address, { lovelace: 1_000_000_000n })
      .complete({ localUPLCEval: true });
    const signedFunding = await funding.sign.withWallet().complete();
    record({
      stage: "fund-prover",
      txHash: signedFunding.toHash(),
      signedCbor: signedFunding.toCBOR(),
      ...measureCompleteSignedTransaction(signedFunding.toCBOR()),
    });
    await lucid.awaitTx(await signedFunding.submit(), 200);
    const proofBase = {
      kind,
      lucid,
      signer,
      deployment: stage.deployment,
      now: () => stage.deployment.chain.now(),
    };
    // The honest parent is committed first: removing the malicious child later slashes its operator.
    const honestQueued = await stage.commit(honest);
    await stage.deployment.chain.awaitLedgerTime(
      Number(honest.header.endTime) + 2_000,
    );
    const honestRefusal = await checkFreshEligibleFamilyRefusal({
      ...proofBase,
      headerHash: honest.headerHash,
      stateQueueBlockOutRef: `${honestQueued.txHash}#${honestQueued.outputIndex}`,
      inclusion: await proofInclusion(honest, fresh),
      onSigned: (transaction) =>
        record({ case: "fresh-eligible", ...transaction }),
    });
    expect(honestRefusal.refusalMessage).toBe(
      "Authentic eligible event content matches the header commitment",
    );
    const preservedThread = await lucid.utxosByOutRef([
      {
        txHash: honestRefusal.preservedThreadOutRef.split("#")[0]!,
        outputIndex: Number(honestRefusal.preservedThreadOutRef.split("#")[1]),
      },
    ]);
    expect(preservedThread).toHaveLength(1);
    const endTime = closeMinute(
      BigInt(stage.deployment.chain.now()) > honest.header.endTime
        ? BigInt(stage.deployment.chain.now())
        : honest.header.endTime + 1n,
    );
    expect(endTime).toBeGreaterThan(source.header.endTime);
    const timed = {
      predecessor: honest,
      operatorVkey: stage.operatorVkey,
      endTime,
      blockSlot: slotOf(endTime),
      settled: source,
    };
    const malicious =
      kind === "Deposit"
        ? await buildJourneyRepeatedDeposit(timed)
        : await buildJourneyRepeatedWithdrawal(timed);
    const entries =
      kind === "Deposit"
        ? malicious.payload.block_body.deposits
        : malicious.payload.block_body.withdrawals;
    expect(entries).toEqual(
      kind === "Deposit"
        ? source.payload.block_body.deposits
        : source.payload.block_body.withdrawals,
    );
    expect(malicious.header.startTime).toBeGreaterThanOrEqual(
      retired.inclusionTime,
    );
    await select({
      category:
        kind === "Deposit" ? "fabricatedDeposit" : "fabricatedWithdrawal",
      block: malicious,
      predecessor: honest,
      history: [...history, source],
      honest: false,
    });
    // The fresh admission and proof capture may have moved the list anchor.
    const currentHistory = SDK.eventHistoryDeploymentFromContracts(
      SDK.requireEventHistoryContracts(stage.deployment.contracts)[
        kind === "Deposit" ? "deposit" : "withdrawal"
      ],
    );
    const currentAbsence = await SDK.fetchEventHistoryWitness(
      { utxosAt: (address) => lucid.utxosAt(address) },
      currentHistory,
      retired.order.event.id,
    );
    expect(currentAbsence.kind).toBe("Absent");
    const witness = await historyWitness(malicious, retired, currentAbsence);
    const plan =
      kind === "Deposit"
        ? await prepareFabricatedDepositFromCommittedLeaves({
            headerHash: malicious.headerHash,
            committedDepositsRoot: malicious.header.depositsRoot,
            depositCount: malicious.header.depositCount,
            headerStartTime: malicious.header.startTime,
            headerEndTime: malicious.header.endTime,
            entries,
            witness,
            minimumConfirmationDepth: 30,
          })
        : await prepareFabricatedWithdrawalFromCommittedLeaves({
            headerHash: malicious.headerHash,
            committedWithdrawalsRoot: malicious.header.withdrawalsRoot,
            withdrawalCount: malicious.header.withdrawalCount,
            headerStartTime: malicious.header.startTime,
            headerEndTime: malicious.header.endTime,
            entries,
            witness,
            minimumConfirmationDepth: 30,
          });
    expect(plan.classification.verdict).toBe(
      kind === "Deposit" ? "DepositIdentityAbsent" : "WithdrawalIdentityAbsent",
    );
    expect(plan.classification.openingCbor).toBeNull();

    expect(malicious.header.prevHeaderHash).toBe(honest.headerHash);
    const maliciousQueued = await stage.commit(malicious);
    await stage.deployment.chain.awaitLedgerTime(
      Number(malicious.header.endTime) + 2_000,
    );
    const confirmedBeforeProof = await stage.confirmedState();
    const honestUnit =
      stage.deployment.contracts.stateQueue.policyId +
      SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX +
      honest.headerHash;
    const honestBeforeProof = await lucid.utxoByUnit(honestUnit);
    const proof = await runRetiredFamilyProof({
      ...proofBase,
      headerHash: malicious.headerHash,
      stateQueueBlockOutRef: `${maliciousQueued.txHash}#${maliciousQueued.outputIndex}`,
      inclusion: await proofInclusion(malicious, retired),
      onSigned: (transaction) =>
        record({ case: "retired-identity", ...transaction }),
    });
    expect(proof.verdict).toBe(
      kind === "Deposit" ? "DepositIdentityAbsent" : "WithdrawalIdentityAbsent",
    );
    expect(await stage.confirmedState()).toEqual(confirmedBeforeProof);
    expect(confirmedBeforeProof).toEqual({
      headerHash: source.headerHash,
      utxoRoot: source.header.utxosRoot,
      endTime: source.header.endTime,
    });
    expect(await lucid.utxosByOutRef([settlement])).toEqual([settlement]);
    const honestAfterProof = await lucid.utxoByUnit(honestUnit);
    const removedUnit =
      stage.deployment.contracts.stateQueue.policyId +
      SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX +
      malicious.headerHash;
    const removal = proof.transactions.find(({ stage }) => stage === "remove")!;
    expect(
      CML.Transaction.from_cbor_hex(removal.signedCbor)
        .body()
        .mint()
        ?.get(
          CML.ScriptHash.from_hex(
            stage.deployment.contracts.stateQueue.policyId,
          ),
          CML.AssetName.from_hex(
            SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + malicious.headerHash,
          ),
        ),
    ).toBe(-1n);
    // Tail removal keeps the child's queue rent in its surviving predecessor.
    // Account for the full input Values and the exact burned child NFT.
    const continuedAssets = { ...honestBeforeProof.assets };
    for (const [unit, quantity] of Object.entries(maliciousQueued.assets)) {
      continuedAssets[unit] = (continuedAssets[unit] ?? 0n) + quantity;
    }
    expect(continuedAssets[removedUnit]).toBe(1n);
    delete continuedAssets[removedUnit];
    expect(honestAfterProof.assets).toEqual(continuedAssets);
    const honestNode = await Effect.runPromise(
      SDK.getLinkedListNodeViewFromUTxO(honestAfterProof),
    );
    expect(
      await Effect.runPromise(SDK.getHeaderFromStateQueueDatum(honestNode)),
    ).toEqual(honest.header);
    expect(honestNode.next).toBe("Empty");
    expect(
      Effect.runSync(SDK.getStateQueueNodeFromStateQueueDatum(honestNode))
        .proven_fraud,
    ).toBeNull();
    expect(await lucid.utxosByOutRef(preservedThread)).toEqual(preservedThread);
    expect(
      (
        await SDK.fetchEventHistoryWitness(
          { utxosAt: (address) => lucid.utxosAt(address) },
          currentHistory,
          retired.order.event.id,
        )
      ).kind,
    ).toBe("Absent");
    expect(
      (
        await SDK.fetchEventHistoryWitness(
          { utxosAt: (address) => lucid.utxosAt(address) },
          currentHistory,
          fresh.order.event.id,
        )
      ).kind,
    ).toBe("Present");
    record({
      stage: "preserved-ledger",
      proof,
      honestRefusal,
      confirmed: confirmedBeforeProof,
      sourceSettlementOutRef: `${settlement.txHash}#${settlement.outputIndex}`,
      honestHeaderHash: honest.headerHash,
      honestQueueOutRef: `${honestAfterProof.txHash}#${honestAfterProof.outputIndex}`,
      queueRentTransfer: {
        predecessorBefore: honestBeforeProof.assets,
        removedChild: maliciousQueued.assets,
        predecessorAfter: honestAfterProof.assets,
        burnedUnit: removedUnit,
        lovelaceDelta:
          honestAfterProof.assets.lovelace! -
          honestBeforeProof.assets.lovelace!,
        removalTxHash: removal.txHash,
      },
      operatorSlashExpected: true,
    });
  },
  1_800_000,
);
