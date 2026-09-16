import { existsSync, mkdirSync, readdirSync, renameSync } from "node:fs";
import { dirname, join } from "node:path";

import { unwrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { DA_TRANSPORT_LIMITS } from "@al-ft/midgard-core/da-transport";
import { depositEventsRetainedBlock } from "@al-ft/midgard-fault-proofs/test-support/transition-trace-retained";
import * as SDK from "@al-ft/midgard-sdk";
import {
  type LucidEvolution,
  paymentCredentialOf,
  toUnit,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import {
  createPublishedWatcherBlockActor,
  type PublishedDaAttestationOutcome,
  PublishedTransactionExpiredError,
  PublishedTransactionSubmissionError,
} from "midgard-watcher/tests/support/published-block-actor";
import type {
  PublishedDaTargetCorrection,
  PublishedDaTransactionRecord,
} from "midgard-watcher/tests/support/published-da-target-consumption";
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
import { readKupoUnitConsumptions } from "./kupo-consumption.js";
import { JOURNEY_ACTION_DEPTH } from "./live-context.js";
import { reconcileSignedCommit } from "./signed-commit-reconciliation.js";
import {
  classifyStagedCheckpoint,
  supersededCheckpointArchivePath,
} from "./staged-checkpoint.js";

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
  /** DA transactions signed for the fault, recorded before each submission. */
  daTransactions?: PublishedDaTransactionRecord[];
  /** The fault's authenticated correction when it consumed the target before DA apply. */
  target?: PublishedDaTargetCorrection;
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
    readConfirmedTransaction,
    onHealthyPredecessor,
    readSignedCommitRecovery,
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
  // The queue output holding this block, only while it is the exact tail.
  const tailOutput = async (block: JourneyBlock) => {
    const output = await headerOutput(block.headerHash);
    if (output === undefined) return undefined;
    const node = await Effect.runPromise(
      SDK.getLinkedListNodeViewFromUTxO(output),
    );
    const header = await Effect.runPromise(
      SDK.getHeaderFromStateQueueDatum(node),
    );
    return node.next === "Empty" &&
      SDK.encodeHeaderCbor(header).toString("hex") ===
        SDK.encodeHeaderCbor(block.header).toString("hex")
      ? output
      : undefined;
  };
  const assertTail = async (block: JourneyBlock) => {
    const output = await tailOutput(block);
    if (output === undefined)
      throw new Error(
        `Retained healthy head ${block.headerHash} differs from the actual state queue tail${await describeQueueTail()}`,
      );
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
    lucid.overrideUTxOs(await lucid.utxosAt(await lucid.wallet().address()));
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
      timeoutCorrectionJournalPath: join(directory, "timeout-correction.json"),
      // The watcher observes the queue at its action depth, so the abandoned
      // header removal only has to be that deep before the watcher starts.
      finalityDepth: JOURNEY_ACTION_DEPTH,
    });
    await retain(initial.predecessor, initial.commits[0]!);
    await retain(initial.current, initial.commits[1]!);
    predecessor = { ...initial.current, commitTxHash: initial.commits[1]! };
  }
  await retain(predecessor, predecessor.commitTxHash);
  if (task.mode === "fault")
    await onHealthyPredecessor?.(predecessor.headerHash);
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
  const readHeaderConsumptions = (unit: string) =>
    readKupoUnitConsumptions(context.kupoUrl, unit);
  const actor = await createPublishedWatcherBlockActor({
    deployment,
    lucid: maliciousLucid,
    daSignerConfig,
    readConfirmedTransaction,
    readHeaderConsumptions,
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
              const endTime = BigInt(chain.now() + 59_999);
              history.block = await build({
                predecessor: retainedPredecessor,
                ledgerOwnerSeedPhrase: accounts.operator.seedPhrase,
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
              const endTime = BigInt(chain.now() + 59_999);
              history.block = await build({
                predecessor: retainedPredecessor,
                ledgerOwnerSeedPhrase: accounts.operator.seedPhrase,
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
      const historyAttested = await actor.attest({
        ...history.block,
        payloadEnvelopeCbor: Buffer.from(history.block.payloadEnvelopeCbor),
      });
      if (historyAttested.kind !== "attested")
        throw new Error(
          `History block ${history.block.headerHash} was corrected by ${historyAttested.removalTxHash} before its DA attestation applied`,
        );
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
    readConfirmedTransaction,
    readSignedCommitRecovery,
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

  const buildFault = () => {
    const faultEndTime = BigInt(chain.now() + 59_999);
    return prepared.buildFault({
      predecessor: retainedPredecessor,
      ledgerOwnerSeedPhrase: accounts.operator.seedPhrase,
      operatorVkey: maliciousKey,
      endTime: faultEndTime,
      blockSlot: BigInt(maliciousLucid.unixTimeToSlot(Number(faultEndTime))),
    });
  };
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
      for (;;) {
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
  // The running watcher races this attestation: its fraud proof can consume
  // the target between any two lookups, including after DA transactions were
  // submitted. The actor finishes through the authenticated correction of the
  // exact fault instead of failing on the missing header, and every DA
  // transaction it signed is persisted before submission so a resumed run
  // reconciles the same set rather than rebonding or forgetting it.
  // A saved correction is provenance, not authority after a possible rollback.
  const target: PublishedDaAttestationOutcome = await actor.attest(
    {
      ...current,
      payloadEnvelopeCbor: Buffer.from(current.payloadEnvelopeCbor),
    },
    {
      submitted: checkpoint.daTransactions ?? [],
      reconcileSubmitted: async (record) => ({
        kind:
          (await reconcileCommit(
            maliciousLucid,
            `DA ${record.step}`,
            record,
          )) === undefined
            ? "retired"
            : "included",
      }),
      onSubmitted: async (record) => {
        checkpoint.daTransactions = [
          ...(checkpoint.daTransactions ?? []),
          record,
        ];
        await writeJourneyArtifact(checkpointPath, checkpoint);
      },
    },
  );
  if (target.kind === "corrected") {
    checkpoint.target = target;
    await writeJourneyArtifact(checkpointPath, checkpoint);
    onStage(
      `fault ${current.headerHash} corrected by ${target.removalTxHash} before DA apply; ${target.submittedDaTransactions.length.toString()} submitted DA transaction(s) reconciled`,
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
      const successorRetainedPredecessor =
        await decodeJourneyRetainedBlock(successorPredecessor);
      const successorActor = await createPublishedWatcherBlockActor({
        deployment,
        lucid: successorLucid,
        daSignerConfig,
        readConfirmedTransaction,
        readHeaderConsumptions,
        onStage,
      });
      await successorActor.onboardOperator();
      const buildSuccessor = async (): Promise<JourneyBlock> => {
        const endTime = BigInt(chain.now() + 59_999);
        const successorInput: JourneyFaultBuildInput = {
          predecessor: successorRetainedPredecessor,
          ledgerOwnerSeedPhrase: accounts.operator.seedPhrase,
          operatorVkey: successorActor.operatorVkey,
          endTime,
          blockSlot: BigInt(successorLucid.unixTimeToSlot(Number(endTime))),
        };
        return prepared.buildSuccessor !== undefined
          ? prepared.buildSuccessor(successorInput)
          : depositEventsRetainedBlock({
              operatorVkey: successorActor.operatorVkey,
              startTime: successorPredecessor.header.endTime,
              endTime,
              blockSlot: BigInt(successorLucid.unixTimeToSlot(Number(endTime))),
              prevHeaderHash: successorPredecessor.headerHash,
              prevUtxosRoot: successorPredecessor.header.utxosRoot,
              priorLedger:
                successorRetainedPredecessor.payload.block_body.utxos,
              events: [],
            });
      };
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
