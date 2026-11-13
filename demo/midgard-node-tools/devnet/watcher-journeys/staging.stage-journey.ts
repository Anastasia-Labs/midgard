import { existsSync, mkdirSync, readdirSync, renameSync } from "node:fs";
import { dirname, join } from "node:path";

import * as SDK from "@al-ft/midgard-sdk";
import {
  type LucidEvolution,
  paymentCredentialOf,
  toUnit,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import {
  createPublishedWatcherBlockActor,
  PublishedTransactionExpiredError,
  PublishedTransactionSubmissionError,
} from "midgard-watcher/tests/support/published-block-actor";

import { readJourneyArtifact, writeJourneyArtifact } from "./artifacts.js";
import {
  JOURNEY_HEADER_END_AHEAD_MS,
  type JourneyBlock,
  type JourneyFixtureStage,
  type JourneySuccessor,
  type StagedJourney,
} from "./fixture.js";
import { journeyBlockBuilder } from "./journey-ledger.js";
import { readKupoUnitConsumptions } from "./kupo-consumption.js";
import { reconcileSignedCommit } from "./signed-commit-reconciliation.js";
import {
  classifyStagedCheckpoint,
  supersededCheckpointArchivePath,
} from "./staged-checkpoint.js";
import {
  decodeJourneyRetainedBlock,
  type FaultPreparation,
  type JourneyFaultBuildInput,
  type JourneyFaultPreparationInput,
  type StagingCheckpoint,
} from "./staging.decode-journey-retained-block.js";
import { journeyDaPath } from "./staging.journey-da-path.js";
import { stagingAppendPoint } from "./state-queue-root-anchor.js";
import { buildJourneyTransactionControl } from "./transaction-cases.js";

export async function stageJourney(
  {
    context,
    ledgerAt,
    directory,
    retain,
    readConfirmedTransaction,
    onHealthyPredecessor,
    readSignedCommitRecovery,
    onFaultCommitted,
    beforeFaultCommit,
    createBlockActor = createPublishedWatcherBlockActor,
    onStage,
  }: JourneyFixtureStage,
  task: FaultPreparation,
): Promise<StagedJourney> {
  const decodeRetained = (block: JourneyBlock) =>
    decodeJourneyRetainedBlock(block, ledgerAt);
  const { deployment, provider } = context;
  const { contracts, chain } = deployment;
  const fingerprint = deployment.manifest.manifestId;
  const checkpointPath = join(directory, "staged.json");
  const headPath = join(context.runDirectory, "work/journeys/head.json");
  // An attached stack's own committee attests: the drill publishes payloads
  // as operator-2's node would, and its faults spend the outputs of the
  // drill's own L2 ledger owner. A devnet journey signs as its committee and
  // spends its operator's outputs.
  const daPath = await journeyDaPath(
    { context, directory, retain, readConfirmedTransaction, onStage },
    task,
  );
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
  // A journey that stopped after attesting its fault leaves that header on
  // the queue until its fraud proof removes it. Name the journey so the
  // operator reruns it instead of guessing which family owns the tail.
  const describeQueueTail = async (): Promise<string> => {
    const outputs = await provider.getUtxosWithUnit(
      contracts.stateQueue.spendingScriptAddress,
      toUnit(contracts.stateQueue.policyId, SDK.STATE_QUEUE_ROOT_ASSET_NAME),
    );
    const root = outputs[0];
    if (outputs.length !== 1 || root === undefined) return "";
    let cursor = await Effect.runPromise(
      SDK.getLinkedListNodeViewFromUTxO(root),
    );
    let tail: string | undefined;
    for (let hops = 0; cursor.next !== "Empty" && hops < 1_000; hops += 1) {
      tail = cursor.next.Key.key;
      cursor = await Effect.runPromise(
        SDK.getLinkedListNodeViewFromUTxO(await requireHeader(tail)),
      );
    }
    if (tail === undefined) return " (the state queue is empty)";
    const journeys = join(context.runDirectory, "work/journeys");
    for (const family of readdirSync(journeys, { withFileTypes: true })) {
      const staged = join(journeys, family.name, "staged.json");
      if (!family.isDirectory() || !existsSync(staged)) continue;
      const checkpoint = await readJourneyArtifact<StagingCheckpoint>(staged);
      if (checkpoint.current.headerHash === tail)
        return ` ${tail}: it is the unproven fault header staged by the ${family.name} journey; rerun that journey so its fraud proof removes it`;
    }
    return ` ${tail}`;
  };
  // tailOutput(block): what a block built on `block` is committed on, its
  // node while it is the exact tail or the root once it merged. queueHead:
  // the queue's first block, absent when the queue is empty.
  const { tailOutput, queueHead } = stagingAppendPoint({
    provider,
    stateQueue: contracts.stateQueue,
    readNode: headerOutput,
  });
  const assertTail = async (block: JourneyBlock) => {
    const output = await tailOutput(block);
    if (output === undefined)
      throw new Error(
        `Retained healthy head ${block.headerHash} differs from the actual state queue tail${await describeQueueTail()}`,
      );
    return output;
  };
  /**
   * Resume a header commitment whose signed bytes were persisted before
   * submission. Inclusion continues with its hash. An attempt the chain can
   * provably never include, because it expired unminted or another
   * transaction took one of its inputs, is retired: the caller drops the
   * recorded bytes and rebuilds from the current protocol state instead of
   * failing on the same saved transaction at every restart.
   */
  const reconcileCommit = async (
    lucid: LucidEvolution,
    label: string,
    signed: NonNullable<StagingCheckpoint["signedCommit"]>,
  ): Promise<string | undefined> => {
    const disposition = await reconcileSignedCommit({
      attempt: signed,
      readRecovery: readSignedCommitRecovery,
      pollDelay: async () => {
        await chain.delaySlots(1);
      },
      resubmit: async (signedCbor) => {
        const restored = await lucid.fromTx(signedCbor).complete();
        if (restored.toCBOR() !== signedCbor)
          throw new Error(
            "Restoring the signed header changed its exact transaction bytes",
          );
        return restored.submit();
      },
      onStage,
    });
    if (disposition.kind === "included") return disposition.txHash;
    onStage(
      `${label} ${signed.txHash} retired: ${disposition.reason}; rebuilding from the current protocol state`,
    );
    return undefined;
  };

  // Missing outputs and rejected RPCs cannot decide whether a signed attempt
  // landed. Use the same canonical recovery during execution as after restart.
  // Construction, persistence, and transaction-identity failures remain hard.
  const reconcileFailedCommit = async (
    lucid: LucidEvolution,
    label: string,
    error: unknown,
    signed: StagingCheckpoint["signedCommit"],
  ): Promise<string | undefined> => {
    if (
      !(error instanceof PublishedTransactionExpiredError) &&
      !(error instanceof PublishedTransactionSubmissionError)
    )
      throw error;
    if (signed === undefined) throw error;
    if (error.txHash !== signed.txHash)
      throw new Error("Failed header differs from its recorded transaction");
    onStage(`${label} outcome unresolved; reconciling ${signed.txHash}`);
    return reconcileCommit(lucid, label, signed);
  };
  let resume = existsSync(checkpointPath)
    ? await readJourneyArtifact<StagingCheckpoint>(checkpointPath)
    : undefined;
  if (resume !== undefined && resume.deploymentFingerprint !== fingerprint)
    throw new Error("Journey checkpoint belongs to a different deployment");
  if (
    resume !== undefined &&
    classifyStagedCheckpoint({
      checkpoint: resume,
      faultHeaderOnQueue:
        (await headerOutput(resume.current.headerHash)) !== undefined,
      predecessorIsTail: (await tailOutput(resume.predecessor)) !== undefined,
    }) === "superseded"
  ) {
    // Another journey's correction appended a healthy successor after this
    // fault was built and before it was published. The build binds the old
    // predecessor and can never be committed; keep it aside and stage a
    // fresh fault on the current head.
    const archived = supersededCheckpointArchivePath(directory, new Date());
    mkdirSync(dirname(archived), { recursive: true });
    renameSync(checkpointPath, archived);
    onStage(
      `staged fault ${resume.current.headerHash} superseded: predecessor ${resume.predecessor.headerHash} is no longer the tail; archived to ${archived}`,
    );
    resume = undefined;
  }

  const readHeaderConsumptions = (unit: string) =>
    readKupoUnitConsumptions(context.kupoUrl, unit);
  const blockActor = (lucid: LucidEvolution) =>
    createBlockActor({
      deployment,
      lucid,
      ...daPath.actorOptions,
      readConfirmedTransaction,
      readHeaderConsumptions,
      onStage,
    });
  // On an attached stack the drill signs as operator-2 in operator-2's own
  // shift, where our node cannot commit, so the tail it reads stays the tail.
  const stackActor =
    daPath.kind === "stack" ? await blockActor(deployment.operatorLucid) : null;

  let predecessor: JourneySuccessor =
    resume?.predecessor ??
    (await daPath.resolvePredecessor({ stackActor, decodeRetained }));
  await retain(predecessor, predecessor.commitTxHash);
  await onHealthyPredecessor?.(predecessor.headerHash);
  let retainedPredecessor = await decodeRetained(predecessor);
  const operatorKey = paymentCredentialOf(
    await deployment.operatorLucid.wallet().address(),
  ).hash;
  const publisherKey = paymentCredentialOf(
    await deployment.publisherLucid.wallet().address(),
  ).hash;
  const maliciousKey =
    stackActor?.operatorVkey ??
    resume?.current.header.operatorVkey ??
    predecessor.header.operatorVkey;
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
  const actor = stackActor ?? (await blockActor(maliciousLucid));
  const historyNames = new Set<string>();
  const commitHistoryBlock: JourneyFaultPreparationInput["commitHistoryBlock"] =
    async (name, build) => {
      build = journeyBlockBuilder(build);
      if (!/^[a-z][a-z0-9-]*$/u.test(name))
        throw new Error("Invalid history checkpoint name");
      historyNames.add(name);
      // Operator-2's shift work on a stack counts the declared history.
      if (
        daPath.kind === "stack" &&
        historyNames.size > (task.historyBlocks ?? 0)
      )
        throw new Error(
          `${task.category} commits more history blocks than the ${(task.historyBlocks ?? 0).toString()} it declares`,
        );
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
        const endTime = BigInt(chain.now() + JOURNEY_HEADER_END_AHEAD_MS);
        history = {
          deploymentFingerprint: fingerprint,
          block: await build({
            predecessor: retainedPredecessor,
            ledgerOwnerSeedPhrase: daPath.ledgerOwnerSeedPhrase,
            operatorVkey: maliciousKey,
            endTime,
            blockSlot: BigInt(maliciousLucid.unixTimeToSlot(Number(endTime))),
          }),
        };
        await writeJourneyArtifact(historyPath, history);
      }
      if (history.commitTxHash === undefined) {
        if (history.signedCommit !== undefined) {
          history.commitTxHash = await reconcileCommit(
            maliciousLucid,
            `history commit ${name}`,
            history.signedCommit,
          );
          if (history.commitTxHash === undefined) {
            delete history.signedCommit;
            await writeJourneyArtifact(historyPath, history);
            // The retired attempt's operator state may have moved on; a new
            // commit registers against the current protocol state.
            await actor.onboardOperator();
          }
        }
        if (history.commitTxHash === undefined)
          for (;;) {
            const anchor = await assertTail(predecessor);
            // A block staged by an earlier attempt may have outlived its
            // commit interval unminted; its header end time bounds the
            // commit's validity, so an elapsed one cannot be committed.
            if (history.block.header.endTime <= BigInt(chain.now())) {
              onStage(`staged ${name} interval elapsed unminted; rebuilding`);
              const endTime = BigInt(chain.now() + JOURNEY_HEADER_END_AHEAD_MS);
              history.block = await build({
                predecessor: retainedPredecessor,
                ledgerOwnerSeedPhrase: daPath.ledgerOwnerSeedPhrase,
                operatorVkey: maliciousKey,
                endTime,
                blockSlot: BigInt(
                  maliciousLucid.unixTimeToSlot(Number(endTime)),
                ),
              });
              delete history.signedCommit;
              await writeJourneyArtifact(historyPath, history);
            }
            try {
              history.commitTxHash = await actor.commit(
                history.block,
                anchor,
                await queueHead(),
                async (signed) => {
                  history.signedCommit = signed;
                  await writeJourneyArtifact(historyPath, history);
                },
              );
              break;
            } catch (error) {
              history.commitTxHash = await reconcileFailedCommit(
                maliciousLucid,
                `history commit ${name}`,
                error,
                history.signedCommit,
              );
              if (history.commitTxHash !== undefined) break;
              // Canonical expiry/invalidation authorizes fresh construction;
              // reconcileCommit refreshed the wallet before this retry.
              const endTime = BigInt(chain.now() + JOURNEY_HEADER_END_AHEAD_MS);
              history.block = await build({
                predecessor: retainedPredecessor,
                ledgerOwnerSeedPhrase: daPath.ledgerOwnerSeedPhrase,
                operatorVkey: maliciousKey,
                endTime,
                blockSlot: BigInt(
                  maliciousLucid.unixTimeToSlot(Number(endTime)),
                ),
              });
              delete history.signedCommit;
              await writeJourneyArtifact(historyPath, history);
            }
          }
        await writeJourneyArtifact(historyPath, history);
      }
      await retain(history.block, history.commitTxHash);
      const historyAttested = await daPath.attestHistory(actor, history.block, [
        historyPath,
        checkpointPath,
      ]);
      if (historyAttested.kind !== "attested")
        throw new Error(
          `History block ${history.block.headerHash} was corrected by ${historyAttested.removalTxHash} before its DA attestation applied`,
        );
      const historyBlock = {
        ...history.block,
        commitTxHash: history.commitTxHash,
      };
      if (
        history.block.headerHash === predecessor.headerHash ||
        (await tailOutput(history.block)) !== undefined
      ) {
        predecessor = historyBlock;
        retainedPredecessor = await decodeRetained(predecessor);
      }
      return historyBlock;
    };
  const preparationInput: JourneyFaultPreparationInput = {
    context,
    ledgerAt,
    directory,
    retain,
    readConfirmedTransaction,
    readSignedCommitRecovery,
    onStage,
    predecessor: retainedPredecessor,
    ledgerOwnerSeedPhrase: daPath.ledgerOwnerSeedPhrase,
    commitHistoryBlock,
    // The tail the event follows: the latest history block, if any.
    beforeUserEvent: () =>
      daPath.beforeUserEvent(actor, retainedPredecessor.header.endTime),
  };
  const prepared = await task.prepare(preparationInput);
  if (prepared.predecessor !== undefined) {
    predecessor = prepared.predecessor;
    retainedPredecessor = await decodeRetained(predecessor);
    await retain(predecessor, predecessor.commitTxHash);
    if (resume === undefined) await assertTail(predecessor);
  }

  const buildFault = journeyBlockBuilder(async () => {
    const faultEndTime = BigInt(chain.now() + JOURNEY_HEADER_END_AHEAD_MS);
    return prepared.buildFault({
      predecessor: retainedPredecessor,
      ledgerOwnerSeedPhrase: daPath.ledgerOwnerSeedPhrase,
      operatorVkey: maliciousKey,
      endTime: faultEndTime,
      blockSlot: BigInt(maliciousLucid.unixTimeToSlot(Number(faultEndTime))),
    });
  });
  // An unsigned draft may refer to a forced order that preparation just
  // replaced after authenticated expiry. Signed commitments retain exact bytes.
  let current =
    resume?.signedCommit !== undefined || resume?.commitTxHash !== undefined
      ? resume.current
      : await buildFault();
  const checkpoint: StagingCheckpoint = resume ?? {
    deploymentFingerprint: fingerprint,
    predecessor,
    current,
  };
  checkpoint.current = current;
  await writeJourneyArtifact(checkpointPath, checkpoint);
  if (checkpoint.commitTxHash === undefined) {
    // Recover a commit that reached the chain before its checkpoint write.
    const existing = await headerOutput(current.headerHash);
    if (checkpoint.signedCommit !== undefined) {
      checkpoint.commitTxHash = await reconcileCommit(
        maliciousLucid,
        "fault commit",
        checkpoint.signedCommit,
      );
      if (checkpoint.commitTxHash === undefined) {
        delete checkpoint.signedCommit;
        await writeJourneyArtifact(checkpointPath, checkpoint);
      }
    }
    if (checkpoint.commitTxHash === undefined && existing !== undefined)
      checkpoint.commitTxHash = existing.txHash;
    if (checkpoint.commitTxHash === undefined) {
      // Registration belongs to a new commit, never to recovery of a signed
      // or already published fault whose operator may have since been slashed.
      await actor.onboardOperator();
      // Readied before the late-shift wait, handed over after it.
      const handOver = await beforeFaultCommit?.(retainedPredecessor);
      for (;;) {
        await daPath.beforeFaultCommit(actor);
        const anchor = await assertTail(predecessor);
        // A fault staged by an earlier attempt may have outlived its commit
        // interval before anything referenced it; its header end time bounds
        // the commit's validity, so an elapsed one cannot be committed.
        if (current.header.endTime <= BigInt(chain.now())) {
          onStage(
            "staged fault interval elapsed unminted; rebuilding the fault",
          );
          current = await buildFault();
          checkpoint.current = current;
          delete checkpoint.signedCommit;
          await writeJourneyArtifact(checkpointPath, checkpoint);
        }
        await handOver?.();
        try {
          checkpoint.commitTxHash = await actor.commit(
            current,
            anchor,
            await queueHead(),
            async (signed) => {
              checkpoint.signedCommit = signed;
              await writeJourneyArtifact(checkpointPath, checkpoint);
            },
          );
          break;
        } catch (error) {
          checkpoint.commitTxHash = await reconcileFailedCommit(
            maliciousLucid,
            "fault commit",
            error,
            checkpoint.signedCommit,
          );
          if (checkpoint.commitTxHash !== undefined) break;
          current = await buildFault();
          checkpoint.current = current;
          delete checkpoint.signedCommit;
          await writeJourneyArtifact(checkpointPath, checkpoint);
        }
      }
    }
    await writeJourneyArtifact(checkpointPath, checkpoint);
  }
  await retain(current, checkpoint.commitTxHash);
  await onFaultCommitted?.(current);
  // The running watcher races this attestation: its fraud proof can consume
  // the target between any two lookups, including after DA transactions were
  // submitted. The actor finishes through the authenticated correction of the
  // exact fault instead of failing on the missing header, and every DA
  // transaction it signed is persisted before submission so a resumed run
  // reconciles the same set rather than rebonding or forgetting it.
  // A saved correction is provenance, not authority after a possible rollback.
  const target = await daPath.settleFault({
    actor,
    block: current,
    category: task.category,
    checkpoint,
    checkpointPath,
    reconcileSubmitted: async (record) => ({
      kind:
        (await reconcileCommit(maliciousLucid, `DA ${record.step}`, record)) ===
        undefined
          ? "retired"
          : "included",
    }),
  });
  if (target.kind !== "attested") {
    checkpoint.target = target;
    await writeJourneyArtifact(checkpointPath, checkpoint);
    onStage(
      target.kind === "refused"
        ? `fault ${current.headerHash} refused by the DA committee: ${Object.keys(target.delivery.peers).length.toString()} peer(s) took it, none attested by its deadline`
        : `fault ${current.headerHash} corrected by ${target.removalTxHash} before DA apply; ${target.submittedDaTransactions.length.toString()} submitted DA transaction(s) reconciled`,
    );
  } else if (checkpoint.target !== undefined) {
    delete checkpoint.target;
    await writeJourneyArtifact(checkpointPath, checkpoint);
  }

  return {
    predecessor,
    current,
    target,
    async commitHonestSuccessor({ beforeCommit }) {
      if (daPath.kind === "stack")
        throw new Error(
          "On an attached stack its own node commits the honest successor",
        );
      if ((await headerOutput(current.headerHash)) !== undefined)
        throw new Error("Honest successor requires confirmed correction first");
      const successorPath = join(directory, "prepared-successor.json");
      const successorPredecessorPath = join(
        directory,
        "successor-predecessor.json",
      );
      let successorPredecessor = existsSync(successorPredecessorPath)
        ? await readJourneyArtifact<JourneySuccessor>(successorPredecessorPath)
        : predecessor;
      // A historically completed proof may be resumed after another family
      // advanced the tail. Only a successor with no persisted transaction may
      // choose the new head; signed attempts keep their original predecessor.
      if (
        !existsSync(successorPath) &&
        (await tailOutput(successorPredecessor)) === undefined
      ) {
        const head = await readJourneyArtifact<{
          deploymentFingerprint: string;
          block: JourneySuccessor;
        }>(headPath);
        if (head.deploymentFingerprint !== fingerprint)
          throw new Error("Successor head belongs to a different deployment");
        await assertTail(head.block);
        await retain(head.block, head.block.commitTxHash);
        successorPredecessor = head.block;
      }
      await writeJourneyArtifact(
        successorPredecessorPath,
        successorPredecessor,
      );
      const successorRetainedPredecessor = await decodeJourneyRetainedBlock(
        successorPredecessor,
        ledgerAt,
      );
      const successorActor = await blockActor(successorLucid);
      await successorActor.onboardOperator();
      const buildSuccessor = journeyBlockBuilder(async () => {
        const endTime = BigInt(chain.now() + JOURNEY_HEADER_END_AHEAD_MS);
        const successorInput: JourneyFaultBuildInput = {
          predecessor: successorRetainedPredecessor,
          ledgerOwnerSeedPhrase: daPath.ledgerOwnerSeedPhrase,
          operatorVkey: successorActor.operatorVkey,
          endTime,
          blockSlot: BigInt(successorLucid.unixTimeToSlot(Number(endTime))),
        };
        return prepared.buildSuccessor !== undefined
          ? prepared.buildSuccessor(successorInput)
          : buildJourneyTransactionControl(successorInput);
      });
      const resumedSuccessor = existsSync(successorPath)
        ? await readJourneyArtifact<{
            block: JourneyBlock;
            commitTxHash?: string;
            signedCommit?: NonNullable<StagingCheckpoint["signedCommit"]>;
          }>(successorPath)
        : undefined;
      let successor = resumedSuccessor?.block ?? (await buildSuccessor());
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
      ) {
        commitTxHash = await reconcileCommit(
          successorLucid,
          "successor commit",
          successorCheckpoint.signedCommit,
        );
        if (commitTxHash === undefined) {
          delete successorCheckpoint.signedCommit;
          await writeJourneyArtifact(successorPath, successorCheckpoint);
        }
      }
      commitTxHash ??= existing?.txHash;
      while (commitTxHash === undefined) {
        const anchor = await assertTail(successorPredecessor);
        // A successor staged by an earlier attempt may have outlived its
        // commit interval unminted; rebuild it before committing.
        if (successor.header.endTime <= BigInt(chain.now())) {
          onStage("staged successor interval elapsed unminted; rebuilding");
          successor = await buildSuccessor();
          successorCheckpoint.block = successor;
          delete successorCheckpoint.signedCommit;
          await writeJourneyArtifact(successorPath, successorCheckpoint);
          await beforeCommit(successor);
        }
        try {
          commitTxHash = await successorActor.commit(
            successor,
            anchor,
            await queueHead(),
            async (signed) => {
              successorCheckpoint.signedCommit = signed;
              await writeJourneyArtifact(successorPath, successorCheckpoint);
            },
          );
        } catch (error) {
          commitTxHash = await reconcileFailedCommit(
            successorLucid,
            "successor commit",
            error,
            successorCheckpoint.signedCommit,
          );
          if (commitTxHash !== undefined) break;
          successor = await buildSuccessor();
          successorCheckpoint.block = successor;
          delete successorCheckpoint.signedCommit;
          await writeJourneyArtifact(successorPath, successorCheckpoint);
          await beforeCommit(successor);
        }
      }
      successorCheckpoint.commitTxHash = commitTxHash;
      await writeJourneyArtifact(successorPath, successorCheckpoint);
      const successorAttested = await successorActor.attest({
        ...successor,
        payloadEnvelopeCbor: Buffer.from(successor.payloadEnvelopeCbor),
      });
      if (successorAttested.kind !== "attested")
        throw new Error(
          `Honest successor ${successor.headerHash} was corrected by ${successorAttested.removalTxHash} before its DA attestation applied`,
        );
      return { ...successor, commitTxHash };
    },
  };
}
