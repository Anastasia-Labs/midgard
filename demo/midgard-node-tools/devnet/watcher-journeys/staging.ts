import { existsSync } from "node:fs";
import { join } from "node:path";

import { unwrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { DA_TRANSPORT_LIMITS } from "@al-ft/midgard-core/da-transport";
import { depositEventsRetainedBlock } from "@al-ft/midgard-fault-proofs/test-support/transition-trace-retained";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  type LucidEvolution,
  paymentCredentialOf,
  toUnit,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { createPublishedWatcherBlockActor } from "midgard-watcher/tests/support/published-block-actor";
import {
  type PublishedDepositTraceCheckpoint,
  stagePublishedDepositTrace,
} from "midgard-watcher/tests/support/published-deposit-trace";

import { readJourneyArtifact, writeJourneyArtifact } from "./artifacts.js";
import type {
  JourneyBlock,
  JourneyCategory,
  JourneyFixture,
  JourneyFixtureStage,
  JourneySuccessor,
  StagedJourney,
} from "./fixture.js";

export type JourneyRetainedBlock = JourneyBlock & { payload: SDK.DaPayload };

export type JourneyFaultBuildInput = {
  predecessor: JourneyRetainedBlock;
  ledgerOwnerSeedPhrase: string;
  operatorVkey: string;
  endTime: bigint;
  blockSlot: bigint;
};

export type JourneyPreparedFault = {
  /** An actual additional history block committed and retained during preparation. */
  predecessor?: JourneySuccessor;
  buildFault(input: JourneyFaultBuildInput): Promise<JourneyBlock>;
  /** Required when recovery must honestly consume a pending L1 event. */
  buildSuccessor?(input: JourneyFaultBuildInput): Promise<JourneyBlock>;
};

export type JourneyFaultPreparationInput = JourneyFixtureStage & {
  predecessor: JourneyRetainedBlock;
  ledgerOwnerSeedPhrase: string;
  /** Commit genuine prerequisite history through the same operator and journal. */
  commitHistoryBlock(
    name: string,
    build: (input: JourneyFaultBuildInput) => Promise<JourneyBlock>,
  ): Promise<JourneySuccessor>;
};

type StagingCheckpoint = {
  deploymentFingerprint: string;
  predecessor: JourneySuccessor;
  current: JourneyBlock;
  commitTxHash?: string;
  signedCommit?: { txHash: string; signedCbor: string };
};

export const decodeJourneyRetainedBlock = async (
  block: JourneyBlock,
): Promise<JourneyRetainedBlock> => {
  const envelope = await unwrapDaPayload(
    Buffer.from(block.payloadEnvelopeCbor),
    {
      maxPayloadBytes: DA_TRANSPORT_LIMITS.maxPayloadBytes,
    },
  );
  return {
    ...block,
    payload: SDK.decodeDaPayload(Buffer.from(envelope.innerBytes)),
  };
};

/**
 * Deposit-backed transaction fixtures share this real operator staging path.
 * A completed journey hands off its retained healthy tail to the next family.
 * Only the fault constructor varies; it cannot launch or direct the watcher.
 */
export const createTransactionJourneyFixture = (
  category: JourneyCategory,
  buildFault: (input: JourneyFaultBuildInput) => Promise<JourneyBlock>,
): JourneyFixture =>
  createPreparedJourneyFixture(category, async () => ({ buildFault }));

/** Prepare required L1 actions before choosing the next commitment interval. */
export const createPreparedJourneyFixture = (
  category: JourneyCategory,
  prepare: (
    input: JourneyFaultPreparationInput,
  ) => Promise<JourneyPreparedFault>,
): JourneyFixture => ({
  category,
  stage: (input) => stageJourney(input, { mode: "fault", prepare }),
});

/** Stage genuine long-lived prerequisites without constructing a fault. */
export const prepareJourneyHistory = (
  input: JourneyFixtureStage,
  prepare: (input: JourneyFaultPreparationInput) => Promise<void>,
): Promise<JourneySuccessor> =>
  stageJourney(input, { mode: "history", prepare });

type HistoryPreparation = {
  mode: "history";
  prepare(input: JourneyFaultPreparationInput): Promise<void>;
};
type FaultPreparation = {
  mode: "fault";
  prepare(input: JourneyFaultPreparationInput): Promise<JourneyPreparedFault>;
};

function stageJourney(
  input: JourneyFixtureStage,
  task: HistoryPreparation,
): Promise<JourneySuccessor>;
function stageJourney(
  input: JourneyFixtureStage,
  task: FaultPreparation,
): Promise<StagedJourney>;
async function stageJourney(
  {
    context,
    directory,
    historicalNativeScriptProviders,
    retain,
    onStage,
  }: JourneyFixtureStage,
  task: HistoryPreparation | FaultPreparation,
): Promise<JourneySuccessor | StagedJourney> {
  const { deployment, accounts, provider } = context;
  const { contracts, chain } = deployment;
  const fingerprint = deployment.manifest.manifestId;
  const checkpointPath = join(directory, "staged.json");
  const headPath = join(context.runDirectory, "work/journeys/head.json");
  const daSignerConfig = {
    NETWORK: "Custom" as const,
    L1_OPERATOR_SEED_PHRASE: accounts.operator.seedPhrase,
    DA_COSIGNER_SEED_PHRASE: accounts.cosigner.seedPhrase,
  };
  const headerUnit = (hash: string) =>
    toUnit(
      contracts.stateQueue.policyId,
      SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + hash,
    );
  const headerOutput = async (hash: string) => {
    const outputs = await provider.getUtxosWithUnit(
      contracts.stateQueue.spendingScriptAddress,
      headerUnit(hash),
    );
    if (outputs.length > 1) throw new Error(`Ambiguous journey header ${hash}`);
    return outputs[0];
  };
  const requireHeader = async (hash: string) => {
    const output = await headerOutput(hash);
    if (output === undefined) throw new Error(`Missing journey header ${hash}`);
    return output;
  };
  const assertTail = async (block: JourneyBlock) => {
    const output = await requireHeader(block.headerHash);
    const node = await Effect.runPromise(
      SDK.getLinkedListNodeViewFromUTxO(output),
    );
    const header = await Effect.runPromise(
      SDK.getHeaderFromStateQueueDatum(node),
    );
    if (
      node.next !== "Empty" ||
      SDK.encodeHeaderCbor(header).toString("hex") !==
        SDK.encodeHeaderCbor(block.header).toString("hex")
    ) {
      throw new Error(
        "Retained healthy head differs from the actual state queue tail",
      );
    }
    return output;
  };
  const queueHead = async () => {
    const roots = await provider.getUtxosWithUnit(
      contracts.stateQueue.spendingScriptAddress,
      toUnit(contracts.stateQueue.policyId, SDK.STATE_QUEUE_ROOT_ASSET_NAME),
    );
    if (roots.length !== 1) throw new Error("Expected one state queue root");
    const root = await Effect.runPromise(
      SDK.getLinkedListNodeViewFromUTxO(roots[0]!),
    );
    if (root.next === "Empty")
      throw new Error("Journey predecessor is absent from the queue");
    return requireHeader(root.next.Key.key);
  };
  const reconcileCommit = async (
    lucid: LucidEvolution,
    block: JourneyBlock,
    signed: NonNullable<StagingCheckpoint["signedCommit"]>,
  ) => {
    const transaction = CML.Transaction.from_cbor_hex(signed.signedCbor);
    if (CML.hash_transaction(transaction.body()).to_hex() !== signed.txHash)
      throw new Error("Recorded header transaction bytes changed their hash");
    if ((await headerOutput(block.headerHash)) !== undefined)
      return signed.txHash;
    const inputs = transaction.body().inputs();
    const outRefs = Array.from({ length: inputs.len() }, (_, index) => {
      const value = inputs.get(index);
      return {
        txHash: value.transaction_id().to_hex(),
        outputIndex: Number(value.index()),
      };
    });
    if ((await provider.getUtxosByOutRef(outRefs)).length !== outRefs.length)
      throw new Error(
        `Header transaction ${signed.txHash} has spent inputs and no live header; reconcile its canonical outcome before continuing`,
      );
    const expiry = transaction.body().ttl();
    if (expiry !== undefined && BigInt(lucid.currentSlot()) >= expiry)
      throw new Error(
        `Recorded header transaction ${signed.txHash} expired; no replacement was constructed`,
      );
    const restored = await lucid.fromTx(signed.signedCbor).complete();
    if (restored.toCBOR() !== signed.signedCbor)
      throw new Error(
        "Restoring the signed header changed its exact transaction bytes",
      );
    if ((await restored.submit()) !== signed.txHash)
      throw new Error("Resubmitted header differs from its recorded hash");
    await lucid.awaitTx(signed.txHash, 500);
    lucid.overrideUTxOs(await lucid.utxosAt(await lucid.wallet().address()));
    return signed.txHash;
  };
  const resume = existsSync(checkpointPath)
    ? await readJourneyArtifact<StagingCheckpoint>(checkpointPath)
    : undefined;
  if (resume !== undefined && resume.deploymentFingerprint !== fingerprint)
    throw new Error("Journey checkpoint belongs to a different deployment");

  let predecessor: JourneySuccessor;
  if (resume !== undefined) predecessor = resume.predecessor;
  else if (existsSync(headPath)) {
    const head = await readJourneyArtifact<{
      deploymentFingerprint: string;
      block: JourneySuccessor;
    }>(headPath);
    if (head.deploymentFingerprint !== fingerprint)
      throw new Error("Journey head belongs to a different deployment");
    predecessor = head.block;
    // Preparation can already have appended journaled history before a stop.
    // Its checkpoint restores that tail; every new commit verifies the live
    // tail immediately before constructing its inputs below.
  } else {
    const initialPath = join(directory, "honest-deposit.json");
    const initial = await stagePublishedDepositTrace(deployment, {
      daSignerConfig,
      honest: true,
      onStage,
      resume: existsSync(initialPath)
        ? await readJourneyArtifact<PublishedDepositTraceCheckpoint>(
            initialPath,
          )
        : undefined,
      onCheckpoint: (checkpoint) =>
        writeJourneyArtifact(initialPath, checkpoint),
    });
    await retain(initial.predecessor, initial.commits[0]!);
    await retain(initial.current, initial.commits[1]!);
    predecessor = { ...initial.current, commitTxHash: initial.commits[1]! };
  }
  await retain(predecessor, predecessor.commitTxHash);
  let retainedPredecessor = await decodeJourneyRetainedBlock(predecessor);
  const operatorKey = paymentCredentialOf(
    await deployment.operatorLucid.wallet().address(),
  ).hash;
  const publisherKey = paymentCredentialOf(
    await deployment.publisherLucid.wallet().address(),
  ).hash;
  const maliciousKey =
    resume?.current.header.operatorVkey ?? predecessor.header.operatorVkey;
  if (maliciousKey !== operatorKey && maliciousKey !== publisherKey)
    throw new Error(
      "The healthy predecessor operator has no run-owned signing wallet",
    );
  const maliciousLucid =
    maliciousKey === operatorKey
      ? deployment.operatorLucid
      : deployment.publisherLucid;
  const successorLucid =
    maliciousKey === operatorKey
      ? deployment.publisherLucid
      : deployment.operatorLucid;
  const actor = await createPublishedWatcherBlockActor({
    deployment,
    lucid: maliciousLucid,
    daSignerConfig,
    onStage,
  });
  const commitHistoryBlock: JourneyFaultPreparationInput["commitHistoryBlock"] =
    async (name, build) => {
      if (!/^[a-z][a-z0-9-]*$/u.test(name))
        throw new Error("Invalid history checkpoint name");
      const historyPath = join(directory, `history-${name}.json`);
      let history: {
        deploymentFingerprint: string;
        block: JourneyBlock;
        commitTxHash?: string;
        signedCommit?: NonNullable<StagingCheckpoint["signedCommit"]>;
      };
      if (existsSync(historyPath)) {
        history = await readJourneyArtifact(historyPath);
        if (history.deploymentFingerprint !== fingerprint)
          throw new Error(
            "History checkpoint belongs to a different deployment",
          );
      } else {
        if (resume !== undefined)
          throw new Error(
            "Cannot add missing history after the fault was checkpointed",
          );
        await actor.onboardOperator();
        const endTime = BigInt(chain.now() + 59_999);
        history = {
          deploymentFingerprint: fingerprint,
          block: await build({
            predecessor: retainedPredecessor,
            ledgerOwnerSeedPhrase: accounts.operator.seedPhrase,
            operatorVkey: maliciousKey,
            endTime,
            blockSlot: BigInt(maliciousLucid.unixTimeToSlot(Number(endTime))),
          }),
        };
        await writeJourneyArtifact(historyPath, history);
      }
      if (history.commitTxHash === undefined) {
        if (history.signedCommit !== undefined)
          history.commitTxHash = await reconcileCommit(
            maliciousLucid,
            history.block,
            history.signedCommit,
          );
        else {
          const anchor = await assertTail(predecessor);
          history.commitTxHash = await actor.commit(
            history.block,
            anchor,
            await queueHead(),
            async (signed) => {
              history.signedCommit = signed;
              await writeJourneyArtifact(historyPath, history);
            },
          );
        }
        await writeJourneyArtifact(historyPath, history);
      }
      await retain(history.block, history.commitTxHash);
      await actor.attest({
        ...history.block,
        payloadEnvelopeCbor: Buffer.from(history.block.payloadEnvelopeCbor),
      });
      const historyBlock = {
        ...history.block,
        commitTxHash: history.commitTxHash,
      };
      const historyNode = await Effect.runPromise(
        SDK.getLinkedListNodeViewFromUTxO(
          await requireHeader(history.block.headerHash),
        ),
      );
      if (
        history.block.headerHash === predecessor.headerHash ||
        historyNode.next === "Empty"
      ) {
        predecessor = historyBlock;
        retainedPredecessor = await decodeJourneyRetainedBlock(predecessor);
      }
      return historyBlock;
    };
  const preparationInput: JourneyFaultPreparationInput = {
    context,
    directory,
    historicalNativeScriptProviders,
    retain,
    onStage,
    predecessor: retainedPredecessor,
    ledgerOwnerSeedPhrase: accounts.operator.seedPhrase,
    commitHistoryBlock,
  };
  if (task.mode === "history") {
    await task.prepare(preparationInput);
    await assertTail(predecessor);
    await writeJourneyArtifact(headPath, {
      deploymentFingerprint: fingerprint,
      block: predecessor,
    });
    return predecessor;
  }
  const prepared = await task.prepare(preparationInput);
  if (prepared.predecessor !== undefined) {
    predecessor = prepared.predecessor;
    retainedPredecessor = await decodeJourneyRetainedBlock(predecessor);
    await retain(predecessor, predecessor.commitTxHash);
    if (resume === undefined) await assertTail(predecessor);
  }

  // Onboard the faulty publisher now. The next producer registers after
  // confirmed correction, so its eligibility cannot block scheduler removal.
  if (resume === undefined) await actor.onboardOperator();
  const faultEndTime = BigInt(chain.now() + 59_999);
  const current =
    resume?.current ??
    (await prepared.buildFault({
      predecessor: retainedPredecessor,
      ledgerOwnerSeedPhrase: accounts.operator.seedPhrase,
      operatorVkey: maliciousKey,
      endTime: faultEndTime,
      blockSlot: BigInt(maliciousLucid.unixTimeToSlot(Number(faultEndTime))),
    }));
  const checkpoint: StagingCheckpoint = resume ?? {
    deploymentFingerprint: fingerprint,
    predecessor,
    current,
  };
  await writeJourneyArtifact(checkpointPath, checkpoint);
  if (checkpoint.commitTxHash === undefined) {
    // Recover a commit that reached the chain before its checkpoint write.
    const existing = await headerOutput(current.headerHash);
    if (checkpoint.signedCommit !== undefined)
      checkpoint.commitTxHash = await reconcileCommit(
        maliciousLucid,
        current,
        checkpoint.signedCommit,
      );
    else if (existing !== undefined) checkpoint.commitTxHash = existing.txHash;
    else {
      const anchor = await assertTail(predecessor);
      checkpoint.commitTxHash = await actor.commit(
        current,
        anchor,
        await queueHead(),
        async (signed) => {
          checkpoint.signedCommit = signed;
          await writeJourneyArtifact(checkpointPath, checkpoint);
        },
      );
    }
    await writeJourneyArtifact(checkpointPath, checkpoint);
  }
  await retain(current, checkpoint.commitTxHash);
  // A completed correction consumes this header. Resuming must not republish it.
  if ((await headerOutput(current.headerHash)) !== undefined)
    await actor.attest({
      ...current,
      payloadEnvelopeCbor: Buffer.from(current.payloadEnvelopeCbor),
    });

  return {
    predecessor,
    current,
    async commitHonestSuccessor({ beforeCommit }) {
      if ((await headerOutput(current.headerHash)) !== undefined)
        throw new Error("Honest successor requires confirmed correction first");
      const successorPath = join(directory, "prepared-successor.json");
      const successorActor = await createPublishedWatcherBlockActor({
        deployment,
        lucid: successorLucid,
        daSignerConfig,
        onStage,
      });
      await successorActor.onboardOperator();
      const endTime = BigInt(chain.now() + 59_999);
      const successorInput: JourneyFaultBuildInput = {
        predecessor: retainedPredecessor,
        ledgerOwnerSeedPhrase: accounts.operator.seedPhrase,
        operatorVkey: successorActor.operatorVkey,
        endTime,
        blockSlot: BigInt(successorLucid.unixTimeToSlot(Number(endTime))),
      };
      const resumedSuccessor = existsSync(successorPath)
        ? await readJourneyArtifact<{
            block: JourneyBlock;
            commitTxHash?: string;
            signedCommit?: NonNullable<StagingCheckpoint["signedCommit"]>;
          }>(successorPath)
        : undefined;
      const successor =
        resumedSuccessor?.block ??
        (prepared.buildSuccessor !== undefined
          ? await prepared.buildSuccessor(successorInput)
          : await depositEventsRetainedBlock({
              operatorVkey: successorActor.operatorVkey,
              startTime: predecessor.header.endTime,
              endTime,
              blockSlot: BigInt(successorLucid.unixTimeToSlot(Number(endTime))),
              prevHeaderHash: predecessor.headerHash,
              prevUtxosRoot: predecessor.header.utxosRoot,
              priorLedger: retainedPredecessor.payload.block_body.utxos,
              events: [],
            }));
      const successorCheckpoint: {
        block: JourneyBlock;
        commitTxHash?: string;
        signedCommit?: NonNullable<StagingCheckpoint["signedCommit"]>;
      } = resumedSuccessor ?? {
        block: successor,
        commitTxHash: undefined,
      };
      await writeJourneyArtifact(successorPath, successorCheckpoint);
      await beforeCommit(successor);
      const existing = await headerOutput(successor.headerHash);
      let commitTxHash = successorCheckpoint.commitTxHash;
      if (
        commitTxHash === undefined &&
        successorCheckpoint.signedCommit !== undefined
      )
        commitTxHash = await reconcileCommit(
          successorLucid,
          successor,
          successorCheckpoint.signedCommit,
        );
      commitTxHash ??= existing?.txHash;
      if (commitTxHash === undefined) {
        const anchor = await assertTail(predecessor);
        commitTxHash = await successorActor.commit(
          successor,
          anchor,
          await queueHead(),
          async (signed) => {
            successorCheckpoint.signedCommit = signed;
            await writeJourneyArtifact(successorPath, successorCheckpoint);
          },
        );
      }
      successorCheckpoint.commitTxHash = commitTxHash;
      await writeJourneyArtifact(successorPath, successorCheckpoint);
      await successorActor.attest({
        ...successor,
        payloadEnvelopeCbor: Buffer.from(successor.payloadEnvelopeCbor),
      });
      return { ...successor, commitTxHash };
    },
  };
}
