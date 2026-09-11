import { existsSync } from "node:fs";
import { readdir } from "node:fs/promises";
import { join } from "node:path";

import {
  DirectoryFraudProofWorkflowJournalStore,
  type FraudProofWorkflowJournalEntry,
  resolveProverSigner,
} from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  coreToUtxo,
  Data,
  paymentCredentialOf,
  toUnit,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import { writeJourneyArtifact } from "./artifacts.js";
import type { loadJourneyContext } from "./live-context.js";
import type { startJourneyNativeRecorder } from "./native-recorder.js";

const transactionOutputs = (transaction: CML.Transaction) => {
  const txHash = CML.hash_transaction(transaction.body());
  const outputs = transaction.body().outputs();
  return Array.from({ length: outputs.len() }, (_, index) =>
    coreToUtxo(
      CML.TransactionUnspentOutput.new(
        CML.TransactionInput.new(txHash, BigInt(index)),
        outputs.get(index),
      ),
    ),
  );
};

/** Read correction's authenticated tail at its immutable transaction output. */
export const verifyJourneyCorrectedTail = async (
  transaction: CML.Transaction,
  expected: { address: string; unit: string; headerHash: string },
) => {
  expect(transaction.is_valid()).toBe(true);
  const matches = transactionOutputs(transaction).filter(
    ({ address, assets }) =>
      address === expected.address && assets[expected.unit] === 1n,
  );
  expect(matches).toHaveLength(1);
  const node = await Effect.runPromise(
    SDK.getLinkedListNodeViewFromUTxO(matches[0]!),
  );
  expect(node.key).toEqual({ Key: { key: expected.headerHash } });
  expect(node.next).toBe("Empty");
  return matches[0]!;
};

/** Shared acceptance assertions for every installed automatic workflow. */
export const verifyJourneyCorrection = async ({
  context,
  native,
  workflowJournalDirectory,
  directory,
  category,
  headerHash,
  predecessorHeaderHash,
  operatorVkey,
  requireLive,
  poll,
  stage,
}: {
  context: Awaited<ReturnType<typeof loadJourneyContext>>;
  native: Awaited<ReturnType<typeof startJourneyNativeRecorder>>;
  workflowJournalDirectory: string;
  directory: string;
  category: SDK.FraudProofCatalogueCategoryName;
  headerHash: string;
  predecessorHeaderHash: string;
  operatorVkey: string;
  requireLive(): void;
  poll<T>(
    name: string,
    action: () => Promise<T | undefined>,
    timeoutMs?: number,
  ): Promise<T>;
  stage<T>(name: string, action: () => Promise<T>): Promise<T>;
}) => {
  const { deployment, provider, accounts } = context;
  const workflowDirectory = join(
    workflowJournalDirectory,
    `fault-proofs/${category}`,
    headerHash,
  );
  const workflow = new DirectoryFraudProofWorkflowJournalStore(
    workflowDirectory,
  );
  const entries = async (): Promise<
    readonly FraudProofWorkflowJournalEntry[]
  > => {
    if (!existsSync(workflowDirectory)) return [];
    const ids = (
      await readdir(workflowDirectory, { withFileTypes: true })
    ).filter(
      (entry) => entry.isDirectory() && /^[0-9a-f]{64}$/u.test(entry.name),
    );
    expect(ids.length).toBeLessThanOrEqual(1);
    return ids.length === 0 ? [] : await workflow.load(ids[0]!.name);
  };
  let reported = -1;
  const completion = await poll(
    "confirmed proof and correction",
    async () => {
      requireLive();
      const records = await entries();
      for (const { sequence, event } of records) {
        if (sequence <= reported) continue;
        if (event.kind === "submitted" || event.kind === "confirmed")
          console.info(`Live proof: ${event.kind} ${event.actionId}`);
        if (event.kind === "stalled")
          throw new Error(`Workflow stalled: ${event.reason}`);
        reported = sequence;
      }
      const terminal = records.find(
        ({ event }) => event.kind === "completed",
      )?.event;
      if (terminal?.kind !== "completed") return undefined;
      const intents = records.flatMap(({ event }) =>
        event.kind === "submission_intent" ? [event.txHash] : [],
      );
      const confirmed = records.flatMap(({ event }) =>
        event.kind === "confirmed" ? [event.txHash] : [],
      );
      expect(confirmed).toEqual(intents);
      for (const txHash of confirmed) await native.transaction(txHash);
      expect(terminal.terminal).toMatchObject({
        category,
        headerHash: headerHash,
        correction: { fraudulentHeaderAbsent: true },
        proofToken: { retainedAtFinalState: true },
      });
      expect(
        terminal.terminal.observedAt.confirmationDepth,
      ).toBeGreaterThanOrEqual(30);
      await writeJourneyArtifact(
        join(directory, "completed-workflow.json"),
        records,
      );
      return terminal.terminal;
    },
    1_800_000,
  );
  const { contracts } = deployment;
  const headerUnit = (hash: string) =>
    toUnit(
      contracts.stateQueue.policyId,
      SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + hash,
    );
  await stage("independent corrected-state observations", async () => {
    const economics = deployment.manifest.economics;
    const readOutput = async (outRef: string | null) => {
      if (outRef === null)
        throw new Error("Expected an actual economic output reference");
      const [txHash, index] = outRef.split("#");
      const observed = await native.transaction(txHash!);
      const tx = CML.Transaction.from_cbor_hex(observed.cbor);
      expect(tx.is_valid()).toBe(true);
      const output = tx.body().outputs().get(Number(index));
      return { tx, output };
    };
    const bond = await readOutput(completion.economics.operatorBondInputOutRef);
    const reward = await readOutput(
      completion.economics.proverRewardOutputOutRef,
    );
    expect(bond.output.amount().coin()).toBe(
      BigInt(economics.requiredBondLovelace),
    );
    expect(reward.output.amount().coin()).toBe(
      BigInt(economics.fraudProverRewardLovelace),
    );
    expect(reward.tx.body().fee()).toBe(
      BigInt(economics.slashingPenaltyLovelace),
    );
    const proverAddress = resolveProverSigner({
      network: "Custom",
      walletSeedPhrase: accounts.publisher.seedPhrase,
    }).address;
    expect(reward.output.address().to_bech32()).toBe(proverAddress);
    const inputs = reward.tx.body().inputs();
    expect(
      Array.from({ length: inputs.len() }, (_, index) => {
        const value = inputs.get(index);
        return `${value.transaction_id().to_hex()}#${value.index()}`;
      }),
    ).toContain(completion.economics.operatorBondInputOutRef);
    expect(completion.economics).toMatchObject({
      operatorBondInputLovelace: economics.requiredBondLovelace.toString(),
      slashedLovelace: economics.slashingPenaltyLovelace.toString(),
      proverRewardLovelace: economics.fraudProverRewardLovelace.toString(),
    });
    const threadResponse = await fetch(
      `${context.kupoUrl}/matches/${contracts.computationThread.policyId}.*?unspent`,
    );
    expect(threadResponse.ok).toBe(true);
    expect(await threadResponse.json()).toEqual([]);
    expect(
      await provider.getUtxosWithUnit(
        contracts.stateQueue.spendingScriptAddress,
        headerUnit(headerHash),
      ),
    ).toHaveLength(0);
    // Correction is a historical chain fact. A resumed runner can already have
    // committed a healthy successor, which legitimately changes both the tail
    // link and scheduler. Authenticate the outputs at removal instead of
    // mistaking that later progress for an incomplete correction.
    const removed = await readOutput(
      completion.correction.removedStateQueueOutRef,
    );
    const removedOutputs = transactionOutputs(removed.tx);
    const removedOutput = removedOutputs.find(
      ({ outputIndex }) =>
        outputIndex ===
        Number(completion.correction.removedStateQueueOutRef.split("#")[1]),
    );
    expect(removedOutput?.address).toBe(
      contracts.stateQueue.spendingScriptAddress,
    );
    expect(removedOutput?.assets[headerUnit(headerHash)]).toBe(1n);
    const removal = CML.Transaction.from_cbor_hex(
      (await native.transaction(completion.correction.removalTxHash)).cbor,
    );
    expect(removal.is_valid()).toBe(true);
    const removalInputs = removal.body().inputs();
    expect(
      Array.from({ length: removalInputs.len() }, (_, index) => {
        const input = removalInputs.get(index);
        return `${input.transaction_id().to_hex()}#${input.index()}`;
      }),
    ).toContain(completion.correction.removedStateQueueOutRef);
    const proofReferences = removal.body().reference_inputs();
    expect(proofReferences).toBeDefined();
    expect(
      Array.from({ length: proofReferences!.len() }, (_, index) => {
        const input = proofReferences!.get(index);
        return `${input.transaction_id().to_hex()}#${input.index()}`;
      }),
    ).toContain(completion.proofToken.outRef);
    const outputsAfterRemoval = transactionOutputs(removal);
    expect(
      outputsAfterRemoval.every(
        ({ assets }) => assets[headerUnit(headerHash)] === undefined,
      ),
    ).toBe(true);
    const schedulerUnit = toUnit(
      contracts.scheduler.policyId,
      SDK.SCHEDULER_ASSET_NAME,
    );
    const schedulers = outputsAfterRemoval.filter(
      ({ address, assets }) =>
        address === contracts.scheduler.spendingScriptAddress &&
        assets[schedulerUnit] === 1n,
    );
    expect(schedulers).toHaveLength(1);
    expect(Data.from(schedulers[0]!.datum!, SDK.SchedulerDatum)).toBe(
      "NoActiveOperators",
    );
    const predecessors = await provider.getUtxosWithUnit(
      contracts.stateQueue.spendingScriptAddress,
      headerUnit(predecessorHeaderHash),
    );
    expect(predecessors).toHaveLength(1);
    await verifyJourneyCorrectedTail(removal, {
      address: contracts.stateQueue.spendingScriptAddress,
      unit: headerUnit(predecessorHeaderHash),
      headerHash: predecessorHeaderHash,
    });
    const expectedProofUnit = toUnit(
      contracts.fraudProof.policyId,
      SDK.FRAUD_PROOF_CATALOGUE_CATEGORY_IDS[category] + headerHash,
    );
    expect(completion.proofToken.unit).toBe(expectedProofUnit);
    const proofs = await provider.getUtxosWithUnit(
      contracts.fraudProof.spendingScriptAddress,
      expectedProofUnit,
    );
    expect(proofs).toHaveLength(1);
    expect(proofs[0]!.assets[expectedProofUnit]).toBe(1n);
    expect(Data.from(proofs[0]!.datum!, SDK.FraudProofTokenDatum)).toEqual({
      fraud_prover: paymentCredentialOf(proverAddress).hash,
    });
    expect(`${proofs[0]!.txHash}#${proofs[0]!.outputIndex}`).toBe(
      completion.proofToken.outRef,
    );
    expect(
      await provider.getUtxosWithUnit(
        contracts.activeOperators.spendingScriptAddress,
        toUnit(
          contracts.activeOperators.policyId,
          SDK.ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX + operatorVkey,
        ),
      ),
    ).toHaveLength(0);
    const locks = await provider.getUtxosWithUnit(
      contracts.correctionLock.spendingScriptAddress,
      toUnit(contracts.hubOracle.policyId, SDK.CORRECTION_LOCK_ASSET_NAME),
    );
    expect(locks).toHaveLength(1);
    expect(Data.from(locks[0]!.datum!, SDK.CorrectionLockDatum)).toBe("Idle");
  });
  return completion;
};
