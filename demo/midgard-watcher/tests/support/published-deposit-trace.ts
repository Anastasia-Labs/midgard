import { depositEventsRetainedBlock } from "@al-ft/midgard-fault-proofs/test-support/transition-trace-retained";
import * as SDK from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  Data,
  paymentCredentialOf,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import type { DaLocalSignerConfig } from "midgard-node/da/local-signers";
import type { publishWorkflowDeploymentOnChain } from "midgard-node/tests/helpers/published-workflow-deployment";

import { createPublishedWatcherBlockActor } from "./published-block-actor.js";

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

/** Publish an ordinary deposit and retain the existing deposit-trace fixture. */
export const stagePublishedDepositTrace = async (
  deployment: Published,
  {
    daSignerConfig,
    honest = false,
    onStage = () => {},
    resume,
    onCheckpoint = async () => {},
  }: {
    daSignerConfig: DaLocalSignerConfig;
    honest?: boolean;
    onStage?: (name: string) => void;
    resume?: PublishedDepositTraceCheckpoint;
    onCheckpoint?: (
      checkpoint: PublishedDepositTraceCheckpoint,
    ) => Promise<void>;
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
  const prepare = async () => {
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
    // Genesis itself closes a real protocol interval. Faster onboarding must
    // not let the first header end before that confirmed-state cutoff.
    await chain.awaitSlot(
      Math.max(
        0,
        Math.ceil((Number(genesis.data.endTime) + 1 - chain.now()) / 1000),
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
  let anchor = root;
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
  const attest = (
    block: Awaited<ReturnType<typeof depositEventsRetainedBlock>>,
  ) => actor.attest(block);
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
    | Promise<
        Awaited<ReturnType<typeof depositEventsRetainedBlock>> & {
          commitTxHash: string;
        }
      >
    | undefined;
  /** Publish new work only after the watcher's actual correction has completed. */
  const commitHonestSuccessor = ({
    beforeCommit = () => {},
  }: Readonly<{
    beforeCommit?: (
      block: Awaited<ReturnType<typeof depositEventsRetainedBlock>>,
    ) => void | Promise<void>;
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
        predecessorDatum.next !== "Empty" ||
        SDK.encodeHeaderCbor(predecessor).toString("hex") !==
          SDK.encodeHeaderCbor(empty.header).toString("hex")
      )
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
      // Register and activate only after correction. A pending eligible
      // successor prevents the last active operator's scheduler rewind.
      await onboardOperator();
      const liveEvent = await one(
        contracts.deposit.spendingScriptAddress,
        depositMetadata.depositAuthUnit,
      );
      const actualDeposit = Data.from(liveEvent.datum!, SDK.DepositDatum);
      const endTime = BigInt(chain.now() + 39_999);
      if (
        !(
          predecessor.endTime < actualDeposit.inclusion_time &&
          actualDeposit.inclusion_time <= endTime
        )
      )
        throw new Error(
          "The unspent ordinary deposit is not due in the corrected successor interval",
        );
      const block = await depositEventsRetainedBlock({
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
      // Removal spent the predecessor to update its link and replaced the lock.
      // Refetch both through the ordinary commit path before constructing inputs.
      anchor = await one(contracts.stateQueue.spendingScriptAddress, headUnit);
      head = anchor;
      await beforeCommit(block);
      await commit(block);
      await attest(block);
      return { ...block, commitTxHash: commits.at(-1)! };
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
