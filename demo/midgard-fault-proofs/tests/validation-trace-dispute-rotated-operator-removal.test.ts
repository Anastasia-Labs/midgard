/** The validation-trace dispute runner removing a convicted block whose
 * operator retired after committing it, with descendants committed both by
 * that operator and by a second operator the scheduler rotated to. The
 * runner's own undefended case is the only proof of the block. It convicts
 * the case and removes the block through the shared removal builder, as
 * production does, with no contract resolution, approach or exemption
 * stubbed. Each descendant is pruned under the validation-trace exemption,
 * with no operator node spent and no reward paid, so the second operator's
 * node and bond stay in place. The convicted block's removal then slashes
 * the retired operator's retired-directory node: the prover receives the
 * reward, the fee is the slashing penalty, and the retired directory's
 * anchor continues with what the node held beyond both, here nothing. */
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  credentialToAddress,
  Data,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { afterAll, beforeAll, describe, expect, it, vi } from "vitest";

import { confirmedValidationTraceStages } from "../src/validation-trace-dispute/actions.js";
import { VALIDATION_TRACE_CASE_OPEN } from "../src/validation-trace-dispute/adapter.js";
import { validationTraceDefenseDeadlineMs } from "../src/validation-trace-dispute/waiting.js";
import { CURSOR_FAMILY_ACTION } from "../src/workflow/cursor-family-state.js";
import type { FraudProofWorkflowJournalEntry } from "../src/workflow/journal.js";
import type { FraudProofWorkflowRunResult } from "../src/workflow/orchestrator.fraud-proof-workflow-run-result.js";
import { parseOwnOutRef } from "../src/workflow/own-outputs.js";
import { recordCrossBlockRawEmulator } from "./support/cross-block-raw-emulator.js";
import {
  type EnrolledOperator,
  enrollSuccessor,
  liveSchedulerOf,
  retireOperator,
} from "./support/rotated-successor-operator.js";
import { expectStateQueueHeaderOrder } from "./support/submit-init-emulator-fixtures.js";
import { network } from "./support/submit-init-emulator-shared.js";
import {
  type Scene,
  setUpScene,
} from "./support/validation-trace-dispute-open-scene.js";
import {
  type RecordedRemoval,
  recordedRemoval,
} from "./support/validation-trace-dispute-removal-l1.js";
import {
  makeRunner,
  type Runner,
  runUntil,
} from "./support/validation-trace-dispute-runner.js";
import {
  commitSceneSuccessor,
  type CommittedBlock,
} from "./support/validation-trace-dispute-scene-successor.js";

const economics = SDK.getProtocolParameters(network);

let recorder: ReturnType<typeof recordCrossBlockRawEmulator>;
let scene: Scene;
let removal: RecordedRemoval;
let runner: Runner;
let second: EnrolledOperator;
let retiredNodeUnit: string;
/** The second operator's blocks on top of the scene's last one. */
let rotated: CommittedBlock[];
/** The queue before any removal, root first. */
let queue: string[];
/** The second operator's active node and the retired directory before any
 * removal. */
let secondNodeBefore: UTxO;
let retiredBefore: UTxO[];
let schedulerBefore: UTxO;
/** Every run's journal, the latest last. */
let entries: readonly FraudProofWorkflowJournalEntry[] = [];
const history: string[] = [];

const outRefOf = (utxo: Pick<UTxO, "txHash" | "outputIndex">) =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

const proverAddress = () =>
  credentialToAddress(network, {
    type: "Key",
    hash: scene.h.proverSigner.paymentKeyHash,
  });

const blockUnit = (headerHash: string) =>
  toUnit(
    scene.h.contracts.stateQueue.policyId,
    SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + headerHash,
  );

/** The queue's blocks still live, in queue order. */
const liveQueue = async () => {
  const { h } = scene;
  const live = await Promise.all(
    queue.map(
      async (headerHash) =>
        (
          await h.funderLucid.utxosAtWithUnit(
            h.contracts.stateQueue.spendingScriptAddress,
            blockUnit(headerHash),
          )
        ).length > 0,
    ),
  );
  return queue.filter((_, index) => live[index]);
};

const secondNode = async () =>
  (
    await scene.h.funderLucid.utxosAtWithUnit(
      scene.h.contracts.activeOperators.spendingScriptAddress,
      second.activeNodeUnit,
    )
  )[0];

const retiredDirectory = async () =>
  await scene.h.funderLucid.utxosAt(
    scene.h.contracts.retiredOperators.spendingScriptAddress,
  );

/** The shared remove stage's journaled transactions, in order. */
const removalTransactions = () =>
  entries.flatMap(({ event }) =>
    event.kind === "submission_intent" &&
    event.actionInput.schemaVersion === CURSOR_FAMILY_ACTION
      ? [
          CML.Transaction.from_cbor_hex(
            recorder.signedCbors.get(event.txHash) ??
              (() => {
                throw new Error(`no recorded removal ${event.txHash}`);
              })(),
          ),
        ]
      : [],
  );

const inputsOf = (tx: CML.Transaction) => {
  const inputs = tx.body().inputs();
  return Array.from({ length: inputs.len() }, (_, index) => {
    const input = inputs.get(index);
    return `${input.transaction_id().to_hex()}#${input.index().toString()}`;
  });
};

const outputsOf = (tx: CML.Transaction) => {
  const outputs = tx.body().outputs();
  return Array.from({ length: outputs.len() }, (_, index) => ({
    address: outputs.get(index).address().to_bech32(),
    lovelace: outputs.get(index).amount().coin(),
    assets: outputs.get(index).amount().multi_asset(),
  }));
};

const mintedPolicies = (tx: CML.Transaction) => {
  const mint = tx.body().mint();
  if (mint === undefined) return [];
  const policies = mint.keys();
  return Array.from({ length: policies.len() }, (_, index) =>
    policies.get(index).to_hex(),
  );
};

/** The outputs paying exactly the fraud-prover reward to the prover. */
const rewardOutputs = (tx: CML.Transaction) =>
  outputsOf(tx).filter(
    (output) =>
      output.address === proverAddress() &&
      output.lovelace === economics.fraud_prover_reward,
  );

/** Runs the runner, on the emulator's clock, until `done` or `limit` runs. */
const runWhile = async (
  done: (result: FraudProofWorkflowRunResult) => Promise<boolean>,
  limit: number,
): Promise<FraudProofWorkflowRunResult> => {
  const clock = vi
    .spyOn(Date, "now")
    .mockImplementation(() => scene.h.emulator.now());
  try {
    for (let run = 0; run < limit; run += 1) {
      const result = await runner.runOnce(scene.failing);
      if ("entries" in result) entries = result.entries;
      history.push(
        "reason" in result ? `${result.kind}: ${result.reason}` : result.kind,
      );
      if (await done(result)) return result;
    }
  } finally {
    clock.mockRestore();
  }
  throw new Error(`the runner did not get there: ${history.join("\n")}`);
};

describe("validation-trace dispute runner: removal of a block whose operator retired, with a rotated operator's descendants", () => {
  beforeAll(async () => {
    // Before the scene, so the raw L1 view has the whole history.
    recorder = recordCrossBlockRawEmulator();
    scene = await setUpScene();
    const { h } = scene;
    const headerHash = scene.failing.evidence.headerHash;
    removal = await recordedRemoval(scene, recorder);
    runner = makeRunner(scene, {
      removal: removal.removal,
      terminalVerifier: removal.terminalVerifier(headerHash),
    });
    const { result } = await runUntil(
      runner,
      scene.failing,
      VALIDATION_TRACE_CASE_OPEN,
    );
    if (!("entries" in result)) throw new Error("no workflow journal");
    const opened = confirmedValidationTraceStages(result.entries).find(
      ({ action }) => action.stage === "verify_source",
    )!.outputs[0]!.outRef;

    // With the case open, the block's operator retires and the scheduler
    // rotates to a second operator, which commits two more blocks.
    const operations = {
      funderLucid: h.funderLucid,
      emulator: h.emulator,
      contracts: h.contracts,
      removalReferenceScriptPublications: { published: removal.published },
    };
    second = await enrollSuccessor(operations, scene.operator.vkey);
    const retirement = await retireOperator(
      operations,
      scene.operator.vkey,
      h.funderLucid,
    );
    retiredNodeUnit = retirement.retiredNodeUnit;
    expect(
      Data.from(
        (await liveSchedulerOf(operations)()).datum!,
        SDK.SchedulerDatum,
      ),
    ).toEqual({
      ActiveOperator: expect.objectContaining({ operator: second.keyHash }),
    });
    const committer = {
      lucid: second.lucid,
      vkey: second.keyHash,
      nodeUnit: second.activeNodeUnit,
    };
    const first = await commitSceneSuccessor(
      scene,
      scene.forced.evidence,
      committer,
    );
    rotated = [first, await commitSceneSuccessor(scene, first, committer)];
    queue = [
      scene.failing.evidence.header.prevHeaderHash,
      headerHash,
      scene.passing.evidence.headerHash,
      scene.forced.evidence.headerHash,
      ...rotated.map((block) => block.headerHash),
    ];
    await expectStateQueueHeaderOrder({
      lucid: h.funderLucid,
      contracts: h.contracts,
      expectedHeaderHashes: queue,
    });
    secondNodeBefore = (await secondNode())!;
    retiredBefore = await retiredDirectory();
    schedulerBefore = await liveSchedulerOf(operations)();

    // Nobody defends: the case's defense deadline passes.
    const [live] = await h.proverLucid.utxosByOutRef([parseOwnOutRef(opened)]);
    const deadline = validationTraceDefenseDeadlineMs(
      Data.from(live!.datum!, SDK.Case),
    );
    h.emulator.awaitSlot(
      h.proverLucid.unixTimeToSlot(deadline) - h.proverLucid.currentSlot() + 1,
    );
  }, 1_800_000);

  afterAll(() => recorder?.restore());

  it("prunes every descendant, the retired operator's and the rotated operator's, under the exemption, spending no operator node and paying no reward", async () => {
    const { h } = scene;
    const [root, convicted] = queue;
    await runWhile(
      async () =>
        JSON.stringify(await liveQueue()) === JSON.stringify([root, convicted]),
      16,
    );
    const prunes = removalTransactions();
    expect(prunes).toHaveLength(4);
    const directories = [
      h.contracts.activeOperators.spendingScriptAddress,
      h.contracts.retiredOperators.spendingScriptAddress,
    ];
    const operatorNodes = [
      outRefOf(secondNodeBefore),
      ...retiredBefore.map(outRefOf),
    ];
    for (const prune of prunes) {
      expect(
        inputsOf(prune).filter((input) => operatorNodes.includes(input)),
      ).toEqual([]);
      expect(
        outputsOf(prune).filter(({ address }) => directories.includes(address)),
      ).toEqual([]);
      expect(mintedPolicies(prune)).not.toContain(
        h.contracts.activeOperators.policyId,
      );
      expect(mintedPolicies(prune)).not.toContain(
        h.contracts.retiredOperators.policyId,
      );
      expect(rewardOutputs(prune)).toEqual([]);
    }
    // The second operator keeps its node, bond and shift.
    expect(await secondNode()).toEqual(secondNodeBefore);
    expect(secondNodeBefore.assets.lovelace).toBe(economics.required_bond);
    expect(await liveSchedulerOf(scene.h)()).toEqual(schedulerBefore);
    expect(await retiredDirectory()).toEqual(retiredBefore);
    await expectStateQueueHeaderOrder({
      lucid: h.funderLucid,
      contracts: h.contracts,
      expectedHeaderHashes: [root!, convicted!],
    });
  }, 900_000);

  it("removes the convicted block by slashing the retired operator's retired-directory node, the bond split exactly into fee, reward and the directory anchor", async () => {
    const { h } = scene;
    const result = await runWhile(
      async (outcome) =>
        outcome.kind === "terminal_included" || outcome.kind === "completed",
      8,
    );
    if (!("terminal" in result)) throw new Error("no terminal");
    const transactions = removalTransactions();
    expect(transactions).toHaveLength(5);
    const target = transactions.at(-1)!;
    const node = retiredBefore.find(
      (utxo) => utxo.assets[retiredNodeUnit] === 1n,
    )!;
    // Retirement moves exactly the bond into the retired directory.
    expect(node.assets.lovelace).toBe(economics.required_bond);
    const retiredAddress = h.contracts.retiredOperators.spendingScriptAddress;
    const spent = inputsOf(target);
    expect(spent).toContain(outRefOf(node));
    // The node is burned; its directory anchor is the other retired input,
    // continued with what the node held beyond the fee and the reward.
    expect(
      target
        .body()
        .mint()
        ?.get_assets(CML.ScriptHash.from_hex(retiredNodeUnit.slice(0, 56)))
        ?.get(CML.AssetName.from_hex(retiredNodeUnit.slice(56))),
    ).toBe(-1n);
    const anchors = retiredBefore.filter(
      (utxo) => utxo !== node && spent.includes(outRefOf(utxo)),
    );
    expect(anchors).toHaveLength(1);
    const continued = outputsOf(target).filter(
      ({ address }) => address === retiredAddress,
    );
    expect(continued).toHaveLength(1);
    const fee = target.body().fee();
    const [reward] = rewardOutputs(target);
    expect(reward).toBeDefined();
    expect(fee).toBe(economics.slashing_penalty);
    // The bond is exactly fee and reward, so the anchor continues as it was.
    expect(continued[0]!.lovelace - anchors[0]!.assets.lovelace!).toBe(0n);
    expect(node.assets.lovelace).toBe(
      fee +
        reward!.lovelace +
        (continued[0]!.lovelace - anchors[0]!.assets.lovelace!),
    );
    // No active-directory node is touched, and the second operator keeps
    // its bond.
    expect(
      outputsOf(target).filter(
        ({ address }) =>
          address === h.contracts.activeOperators.spendingScriptAddress,
      ),
    ).toEqual([]);
    expect(await secondNode()).toEqual(secondNodeBefore);
    expect(
      await h.funderLucid.utxosAtWithUnit(retiredAddress, retiredNodeUnit),
    ).toEqual([]);
    expect(result.terminal.economics).toMatchObject({
      operatorCredential: scene.operator.vkey,
      operatorBondInputOutRef: outRefOf(node),
      operatorBondInputLovelace: node.assets.lovelace!.toString(),
    });
    await expectStateQueueHeaderOrder({
      lucid: h.funderLucid,
      contracts: h.contracts,
      expectedHeaderHashes: [queue[0]!],
    });
  }, 900_000);
});
