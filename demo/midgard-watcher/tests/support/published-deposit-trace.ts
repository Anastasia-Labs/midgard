import {
  createFileTimeoutCorrectionJournalStore,
  submitUnattestedTimeoutCorrection,
  type TimeoutCorrectionJournal,
} from "@al-ft/midgard-fault-proofs";
import { depositEventsRetainedBlock } from "@al-ft/midgard-fault-proofs/test-support/transition-trace-retained";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  credentialToAddress,
  Data,
  paymentCredentialOf,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import type { DaLocalSignerConfig } from "midgard-node/da/local-signers";
import type { publishWorkflowDeploymentOnChain } from "midgard-node/tests/helpers/published-workflow-deployment";
import { activateRegisteredOperatorProgram } from "midgard-node/transactions/register-active-operator";

import {
  createPublishedWatcherBlockActor,
  type PublishedWatcherBlock,
} from "./published-block-actor.js";

type Published = Awaited<ReturnType<typeof publishWorkflowDeploymentOnChain>>;

export type PublishedDepositTraceCheckpoint = {
  predecessor: Awaited<ReturnType<typeof depositEventsRetainedBlock>>;
  current: Awaited<ReturnType<typeof depositEventsRetainedBlock>>;
  depositEvent: UTxO;
  depositMetadata: Pick<
    Effect.Effect.Success<
      ReturnType<typeof SDK.buildUnsignedDepositTxWithMetadataProgram>
    >["metadata"],
    "depositAssetName" | "depositAuthUnit" | "inclusionTime"
  >;
  commits: string[];
  attestationsComplete: boolean;
  priorDeposits?: readonly {
    event: UTxO;
    metadata: PublishedDepositTraceCheckpoint["depositMetadata"];
  }[];
};

/**
 * The honest successor's durable progress. The block is persisted before its
 * header transaction is signed, the signed bytes before submission, and the
 * hash after inclusion, so a stopped harness resumes from whatever landed.
 */
export type PublishedSuccessorCheckpoint = {
  block: PublishedWatcherBlock;
  signedCommit?: { txHash: string; signedCbor: string };
  commitTxHash?: string;
};

/** The successor header's validity window; block production is Poisson. */
export const SUCCESSOR_HEADER_INTERVAL_MS = 179_999;

/** Publish an ordinary deposit and retain the existing deposit-trace fixture. */
export const stagePublishedDepositTrace = async (
  deployment: Published,
  {
    daSignerConfig,
    honest = false,
    onStage = () => {},
    resume,
    onCheckpoint = async () => {},
    timeoutCorrectionJournalPath,
  }: {
    daSignerConfig: DaLocalSignerConfig;
    honest?: boolean;
    onStage?: (name: string) => void;
    resume?: PublishedDepositTraceCheckpoint;
    onCheckpoint?: (
      checkpoint: PublishedDepositTraceCheckpoint,
    ) => Promise<void>;
    /** Durable journal of abandoned-header removals; in memory when absent. */
    timeoutCorrectionJournalPath?: string;
  },
) => {
  const { contracts, chain, references } = deployment;
  let lucid = deployment.operatorLucid;
  let address = await lucid.wallet().address();
  const awaitConfirmed = async (txHash: string) => {
    await lucid.awaitTx(txHash, 500);
    // Operator onboarding maintains an explicit wallet snapshot. Refresh it
    // after direct SDK transactions so their successors use the live ledger.
    lucid.overrideUTxOs(await lucid.utxosAt(address));
  };
  let operatorVkey = paymentCredentialOf(address).hash;
  const one = async (scriptAddress: string, unit: string): Promise<UTxO> => {
    const values = await lucid.utxosAtWithUnit(scriptAddress, unit);
    if (values.length !== 1)
      throw new Error(`Expected one actual published state: ${unit}`);
    return values[0]!;
  };
  const reference = (name: string) => {
    const value = references.get(name);
    if (value === undefined)
      throw new Error(`Missing actual publication ${name}`);
    return value;
  };
  const rootUnit = toUnit(
    contracts.stateQueue.policyId,
    SDK.STATE_QUEUE_ROOT_ASSET_NAME,
  );
  const root = await one(contracts.stateQueue.spendingScriptAddress, rootUnit);
  const rootDatum = await Effect.runPromise(
    SDK.getLinkedListNodeViewFromUTxO(root),
  );
  const genesis = await Effect.runPromise(
    SDK.getConfirmedStateFromStateQueueDatum(rootDatum),
  );
  const publisher = deployment.publisherLucid;
  let actor = await createPublishedWatcherBlockActor({
    deployment,
    lucid,
    daSignerConfig,
    onStage,
  });
  const onboardOperator = () => actor.onboardOperator();
  // This fixture appends to the confirmed state, so the queue must be empty.
  // A stopped run can leave headers whose DA attestation never followed. The
  // protocol lets anyone remove such a head once its attestation timeout
  // passes; apply that correction, waiting for the timeout, before publishing.
  const removeAbandonedHeaders = async () => {
    let memory: TimeoutCorrectionJournal | undefined;
    const journalStore =
      timeoutCorrectionJournalPath === undefined
        ? {
            load: async () => memory,
            save: async (journal: TimeoutCorrectionJournal) => {
              memory = journal;
            },
          }
        : createFileTimeoutCorrectionJournalStore(timeoutCorrectionJournalPath);
    for (;;) {
      const liveRoot = await Effect.runPromise(
        SDK.getLinkedListNodeViewFromUTxO(
          await one(contracts.stateQueue.spendingScriptAddress, rootUnit),
        ),
      );
      if (liveRoot.next === "Empty") return;
      const result = await submitUnattestedTimeoutCorrection({
        lucid,
        deploymentInfo: deployment.deploymentInfo,
        network: daSignerConfig.NETWORK,
        signer: {
          source: "journey-operator-wallet",
          address,
          paymentKeyHash: operatorVkey,
          selectWallet: () => undefined,
        },
        journalStore,
        awaitConfirmation: true,
        nowMs: () => chain.now(),
      });
      if (result.status === "not-ready") {
        onStage(`abandoned header timeout ${result.targetHeaderHash}`);
        // Chain time is slot-quantised: land one slot past the deadline.
        await chain.awaitSlot(
          Math.max(
            1,
            Math.ceil((Number(result.deadlineMs) - chain.now()) / 1000) + 1,
          ),
        );
      } else if (result.status === "complete") {
        onStage(`abandoned header removal ${result.targetHeaderHash}`);
        lucid.overrideUTxOs(await lucid.utxosAt(address));
      } else {
        throw new Error(
          `State queue holds headers this fixture cannot remove: ${liveRoot.next.Key.key}`,
        );
      }
    }
  };
  const prepare = async () => {
    await removeAbandonedHeaders();
    // A stopped preparation may have published an event before its block was
    // checkpointed. Recover every real pending deposit from the isolated chain.
    const priorDeposits: NonNullable<
      PublishedDepositTraceCheckpoint["priorDeposits"]
    > = (await lucid.utxosAt(contracts.deposit.spendingScriptAddress)).map(
      (event) => {
        if (event.datum == null)
          throw new Error("Published deposit has no datum");
        const datum = Data.from(event.datum, SDK.DepositDatum);
        const units = Object.entries(event.assets).filter(
          ([unit, amount]) =>
            unit.startsWith(contracts.deposit.policyId) && amount === 1n,
        );
        if (units.length !== 1)
          throw new Error("Published deposit authentication is ambiguous");
        const depositAuthUnit = units[0]![0];
        return {
          event,
          metadata: {
            depositAuthUnit,
            depositAssetName: depositAuthUnit.slice(56),
            inclusionTime: Number(datum.inclusion_time),
          },
        };
      },
    );
    // Validate the ledger before publishing another transaction.
    await depositEventsRetainedBlock({
      operatorVkey,
      startTime: genesis.data.endTime,
      endTime: genesis.data.endTime + 1n,
      blockSlot: BigInt(lucid.currentSlot()),
      prevHeaderHash: genesis.data.headerHash,
      prevUtxosRoot: genesis.data.utxoRoot,
      priorLedger: [],
      events: [],
    });
    await onboardOperator();
    // The successor registers only after correction. A registration that
    // matures while proof construction runs prevents the last-operator rewind.
    // Genesis itself closes a real protocol interval. Faster onboarding must
    // not let the first header end before that confirmed-state cutoff.
    await chain.awaitSlot(
      Math.max(
        0,
        Math.ceil((Number(genesis.data.endTime) + 1 - chain.now()) / 1000),
      ),
    );
    // Retained deposits of a stopped run become eligible at their own
    // inclusion times; the empty predecessor must close after every one.
    const latestPriorInclusion = Math.max(
      0,
      ...priorDeposits.map((prior) => prior.metadata.inclusionTime),
    );
    await chain.awaitSlot(
      Math.max(
        0,
        Math.ceil((latestPriorInclusion - 39_999 + 1_000 - chain.now()) / 1000),
      ),
    );
    const l2Address = credentialToAddress("Preprod", {
      type: "Key",
      hash: operatorVkey,
    });
    const deposit = await Effect.runPromise(
      SDK.buildUnsignedDepositTxWithMetadataProgram(lucid, contracts, {
        l2Address,
        l2Datum: null,
        lovelace: 10_000_000n,
        additionalAssets: {},
        referenceScripts: { depositMinting: reference("depositMint") },
      }),
    );
    onStage("deposit publication");
    await awaitConfirmed(
      await (await deposit.tx.sign.withWallet().complete()).submit(),
    );
    const event = await one(
      contracts.deposit.spendingScriptAddress,
      deposit.metadata.depositAuthUnit,
    );
    const emptyEnd = BigInt(chain.now() + 39_999);
    if (emptyEnd >= BigInt(deposit.metadata.inclusionTime))
      throw new Error(
        "Ordinary deposit must become eligible after the empty predecessor",
      );
    for (const prior of priorDeposits) {
      if (
        !(
          genesis.data.endTime < BigInt(prior.metadata.inclusionTime) &&
          BigInt(prior.metadata.inclusionTime) <= emptyEnd
        )
      )
        throw new Error(
          "A retained deposit is outside the recovered predecessor interval",
        );
    }
    const empty = await depositEventsRetainedBlock({
      operatorVkey,
      startTime: genesis.data.endTime,
      endTime: emptyEnd,
      blockSlot: BigInt(lucid.unixTimeToSlot(Number(emptyEnd))),
      prevHeaderHash: genesis.data.headerHash,
      prevUtxosRoot: genesis.data.utxoRoot,
      priorLedger: [],
      events: priorDeposits.map(({ event, metadata }) => ({
        event,
        depositPolicyId: contracts.deposit.policyId,
        assetName: metadata.depositAssetName,
        honest: true,
      })),
    });
    const depositEnd = BigInt(
      Math.ceil(deposit.metadata.inclusionTime / 1000) * 1000 + 59_999,
    );
    const deposited = await depositEventsRetainedBlock({
      operatorVkey,
      prevHeaderHash: empty.headerHash,
      prevUtxosRoot: empty.header.utxosRoot,
      priorLedger: empty.payload.block_body.utxos,
      startTime: emptyEnd,
      endTime: depositEnd,
      blockSlot: BigInt(lucid.unixTimeToSlot(Number(depositEnd))),
      events: [
        {
          event,
          depositPolicyId: contracts.deposit.policyId,
          assetName: deposit.metadata.depositAssetName,
          honest,
        },
      ],
    });
    return {
      predecessor: empty,
      current: deposited,
      depositEvent: event,
      depositMetadata: deposit.metadata,
      commits: [],
      attestationsComplete: false,
      priorDeposits,
    };
  };
  const prepared =
    resume === undefined
      ? await prepare()
      : resume.commits.length === 0 &&
          (resume.predecessor.header.endTime <= BigInt(chain.now()) ||
            resume.predecessor.header.startTime >=
              resume.predecessor.header.endTime)
        ? await prepare()
        : resume;
  const {
    predecessor: empty,
    current: deposited,
    depositEvent: event,
    depositMetadata,
  } = prepared;
  // A stopped run may have registered the successor before its correction
  // completed. Once that registration matures it blocks the last-operator
  // rewind, so activate it now (permissionless; the faulty operator's wallet
  // pays the fee) and let removal appoint the activated successor instead.
  // The prover wallet is never touched: its outputs are leased by the
  // watcher's funding reservation for the whole correction.
  const activateDanglingSuccessorRegistration = async () => {
    const successorVkey = paymentCredentialOf(
      await publisher.wallet().address(),
    ).hash;
    const activeUnit = toUnit(
      contracts.activeOperators.policyId,
      SDK.ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX + successorVkey,
    );
    if (
      (
        await lucid.utxosAtWithUnit(
          contracts.activeOperators.spendingScriptAddress,
          activeUnit,
        )
      ).length !== 0
    )
      return;
    const registrations = await Promise.all(
      (
        await lucid.utxosAt(contracts.registeredOperators.spendingScriptAddress)
      ).map((utxo) =>
        Effect.runPromise(SDK.getLinkedListNodeViewFromUTxO(utxo)),
      ),
    );
    const matching = registrations.flatMap((node) =>
      node.key !== "Empty" &&
      Data.castFrom(node.data, SDK.RegisteredOperatorDatum).operator ===
        successorVkey
        ? [node.key.Key.key]
        : [],
    );
    if (matching.length === 0) return;
    if (matching.length !== 1)
      throw new Error("The journey successor has ambiguous registrations");
    onStage("dangling successor activation");
    const activationTime = Number(BigInt(`0x${matching[0]!}`));
    await chain.awaitSlot(
      Math.max(0, Math.ceil((activationTime + 1 - chain.now()) / 1000)),
    );
    lucid.overrideUTxOs(await lucid.utxosAt(address));
    await Effect.runPromise(
      activateRegisteredOperatorProgram(
        lucid,
        contracts,
        SDK.getProtocolParameters("Preprod").required_bond,
        successorVkey,
        publisher,
        await publisher.wallet().address(),
      ),
    );
    if (
      (
        await lucid.utxosAtWithUnit(
          contracts.activeOperators.spendingScriptAddress,
          activeUnit,
        )
      ).length !== 1
    )
      throw new Error("Successor activation did not produce its active node");
    lucid.overrideUTxOs(await lucid.utxosAt(address));
  };
  await activateDanglingSuccessorRegistration();
  // Abandoned-header removal continues the root in a new output.
  let anchor = await one(contracts.stateQueue.spendingScriptAddress, rootUnit);
  let head: UTxO | undefined;
  let headUnit: string | undefined;
  const commits: string[] = [...prepared.commits];
  const checkpoint: PublishedDepositTraceCheckpoint = {
    predecessor: empty,
    current: deposited,
    depositEvent: event,
    depositMetadata,
    commits,
    attestationsComplete: prepared.attestationsComplete,
    priorDeposits: prepared.priorDeposits,
  };
  await onCheckpoint(checkpoint);
  const commit = async (block: Pick<typeof empty, "header" | "headerHash">) => {
    const txHash = await actor.commit(block, anchor, head);
    commits.push(txHash);
    anchor = await one(
      contracts.stateQueue.spendingScriptAddress,
      toUnit(
        contracts.stateQueue.policyId,
        SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + block.headerHash,
      ),
    );
    headUnit ??= toUnit(
      contracts.stateQueue.policyId,
      SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + block.headerHash,
    );
    head = await one(contracts.stateQueue.spendingScriptAddress, headUnit);
  };
  const attest = (block: PublishedWatcherBlock) => actor.attest(block);
  if (!checkpoint.attestationsComplete) {
    if (commits.length === 0) {
      await commit(empty);
      await onCheckpoint(checkpoint);
    } else if (commits.length === 1) {
      headUnit = toUnit(
        contracts.stateQueue.policyId,
        SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + empty.headerHash,
      );
      anchor = await one(contracts.stateQueue.spendingScriptAddress, headUnit);
      head = anchor;
    }
    if (commits.length === 1) {
      await chain.awaitSlot(
        Math.max(
          0,
          Math.ceil((depositMetadata.inclusionTime - chain.now()) / 1000),
        ),
      );
      await commit(deposited);
      await onCheckpoint(checkpoint);
    }
    if (commits.length !== 2)
      throw new Error("Journey requires exactly two staged commitments");
    await attest(empty);
    await attest(deposited);
    checkpoint.attestationsComplete = true;
    await onCheckpoint(checkpoint);
  }
  let honestSuccessor:
    | Promise<PublishedWatcherBlock & { commitTxHash: string }>
    | undefined;
  /** Publish new work only after the watcher's actual correction has completed. */
  const commitHonestSuccessor = ({
    beforeCommit = () => {},
    resume: successorResume,
    onCheckpoint: onSuccessorCheckpoint = async () => {},
  }: Readonly<{
    beforeCommit?: (block: PublishedWatcherBlock) => void | Promise<void>;
    resume?: PublishedSuccessorCheckpoint;
    onCheckpoint?: (checkpoint: PublishedSuccessorCheckpoint) => Promise<void>;
  }> = {}) =>
    (honestSuccessor ??= (async () => {
      const fraudulentUnit = toUnit(
        contracts.stateQueue.policyId,
        SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + deposited.headerHash,
      );
      if (
        (
          await lucid.utxosAtWithUnit(
            contracts.stateQueue.spendingScriptAddress,
            fraudulentUnit,
          )
        ).length !== 0
      )
        throw new Error(
          "Honest successor requires the fraudulent commitment to be removed first",
        );
      const headerOutput = async (hash: string) => {
        const outputs = await lucid.utxosAtWithUnit(
          contracts.stateQueue.spendingScriptAddress,
          toUnit(
            contracts.stateQueue.policyId,
            SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + hash,
          ),
        );
        if (outputs.length > 1)
          throw new Error(`Ambiguous successor header ${hash}`);
        return outputs[0];
      };
      const resumedBlock =
        successorResume === undefined
          ? undefined
          : {
              ...successorResume.block,
              payloadEnvelopeCbor: Buffer.from(
                successorResume.block.payloadEnvelopeCbor,
              ),
            };
      let landed =
        resumedBlock === undefined
          ? undefined
          : await headerOutput(resumedBlock.headerHash);
      headUnit = toUnit(
        contracts.stateQueue.policyId,
        SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + empty.headerHash,
      );
      anchor = await one(contracts.stateQueue.spendingScriptAddress, headUnit);
      const predecessorDatum = await Effect.runPromise(
        SDK.getLinkedListNodeViewFromUTxO(anchor),
      );
      const predecessor = await Effect.runPromise(
        SDK.getHeaderFromStateQueueDatum(predecessorDatum),
      );
      if (
        SDK.encodeHeaderCbor(predecessor).toString("hex") !==
        SDK.encodeHeaderCbor(empty.header).toString("hex")
      )
        throw new Error(
          "Corrected state queue does not contain the retained honest predecessor",
        );
      if (landed === undefined && predecessorDatum.next !== "Empty")
        throw new Error(
          "Corrected state queue does not end at the retained honest predecessor",
        );
      const previousOperator = operatorVkey;
      lucid = publisher;
      address = await lucid.wallet().address();
      operatorVkey = paymentCredentialOf(address).hash;
      if (operatorVkey === previousOperator)
        throw new Error("Continued production requires a second operator");
      actor = await createPublishedWatcherBlockActor({
        deployment,
        lucid,
        daSignerConfig,
        onStage,
      });
      await onboardOperator();
      // A signed header that has not landed is resubmitted while its window
      // is open; an expired one is replaced by a fresh block since nothing
      // it referenced was spent.
      if (
        landed === undefined &&
        resumedBlock !== undefined &&
        successorResume?.signedCommit !== undefined &&
        successorResume.commitTxHash === undefined
      ) {
        const { signedCbor, txHash } = successorResume.signedCommit;
        if (
          CML.hash_transaction(
            CML.Transaction.from_cbor_hex(signedCbor).body(),
          ).to_hex() !== txHash
        )
          throw new Error("Recorded successor header bytes changed their hash");
        const expiry = Number(resumedBlock.header.endTime) + 1;
        if (chain.now() + 15_000 < expiry) {
          onStage(`header commit resubmission ${resumedBlock.headerHash}`);
          await lucid
            .config()
            .provider!.submitTx(signedCbor)
            .catch(() => undefined);
          while (chain.now() < expiry + 30_000) {
            landed = await headerOutput(resumedBlock.headerHash);
            if (landed !== undefined) break;
            await chain.awaitSlot(1);
          }
        }
      }
      if (landed !== undefined && resumedBlock !== undefined) {
        const commitTxHash = successorResume?.commitTxHash ?? landed.txHash;
        commits.push(commitTxHash);
        anchor = landed;
        head = landed;
        await onSuccessorCheckpoint({ block: resumedBlock, commitTxHash });
        await attest(resumedBlock);
        return { ...resumedBlock, commitTxHash };
      }
      const liveEvent = await one(
        contracts.deposit.spendingScriptAddress,
        depositMetadata.depositAuthUnit,
      );
      const actualDeposit = Data.from(liveEvent.datum!, SDK.DepositDatum);
      const endTime = BigInt(chain.now() + SUCCESSOR_HEADER_INTERVAL_MS);
      if (
        !(
          predecessor.endTime < actualDeposit.inclusion_time &&
          actualDeposit.inclusion_time <= endTime
        )
      )
        throw new Error(
          "The unspent ordinary deposit is not due in the corrected successor interval",
        );
      const built = await depositEventsRetainedBlock({
        operatorVkey,
        prevHeaderHash: empty.headerHash,
        prevUtxosRoot: predecessor.utxosRoot,
        priorLedger: empty.payload.block_body.utxos,
        startTime: predecessor.endTime,
        endTime,
        blockSlot: BigInt(lucid.unixTimeToSlot(Number(endTime))),
        events: [
          {
            event: liveEvent,
            depositPolicyId: contracts.deposit.policyId,
            assetName: depositMetadata.depositAssetName,
            honest: true,
          },
        ],
      });
      const block: PublishedWatcherBlock = {
        header: built.header,
        headerHash: built.headerHash,
        payloadEnvelopeCbor: Buffer.from(built.payloadEnvelopeCbor),
      };
      const successorCheckpoint: PublishedSuccessorCheckpoint = { block };
      await onSuccessorCheckpoint(successorCheckpoint);
      // Removal spent the predecessor to update its link and replaced the lock.
      // Refetch both through the ordinary commit path before constructing inputs.
      anchor = await one(contracts.stateQueue.spendingScriptAddress, headUnit);
      head = anchor;
      await beforeCommit(block);
      const txHash = await actor.commit(block, anchor, head, async (signed) => {
        successorCheckpoint.signedCommit = signed;
        await onSuccessorCheckpoint(successorCheckpoint);
      });
      commits.push(txHash);
      anchor = await one(
        contracts.stateQueue.spendingScriptAddress,
        toUnit(
          contracts.stateQueue.policyId,
          SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + block.headerHash,
        ),
      );
      head = anchor;
      successorCheckpoint.commitTxHash = txHash;
      await onSuccessorCheckpoint(successorCheckpoint);
      await attest(block);
      return { ...block, commitTxHash: txHash };
    })());
  return {
    predecessor: empty,
    current: deposited,
    commits,
    depositEvent: event,
    checkpoint,
    commitHonestSuccessor,
  };
};
