import {
  aikenSerialisedPlutusDataCborPreservingMapOrder as canonical,
  plutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import {
  buildCountedRoot,
  keyValuePhasProof,
} from "@al-ft/midgard-fault-proofs";
import { recordCrossBlockRawEmulator } from "@al-ft/midgard-fault-proofs/test-support/cross-block-raw-emulator";
import * as SDK from "@al-ft/midgard-sdk";
import {
  type CML,
  Data,
  paymentCredentialOf,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import {
  createPublishedWorkflowDeploymentAccounts,
  publishWorkflowDeployment,
} from "midgard-node/tests/helpers/published-workflow-deployment";
import { createPublishedWatcherBlockActor } from "midgard-watcher/tests/support/published-block-actor";

import type { VerifiableJourneyBlock } from "./fixture-verification.js";
import {
  journeyDepositEventRequest,
  type JourneyEventPublicationRequest,
  journeyLedgerOwnerKey,
  journeyWithdrawalEventRequest,
  readJourneyHistoryEvent,
} from "./history-event-staging.js";
import {
  captureStagedHistoryEvent,
  type StagedHistoryEvent,
} from "./history-events.js";

/** A published event together with the inclusion time its datum authenticates. */
export type StagedLocalHistoryEvent = StagedHistoryEvent & {
  inclusionTime: bigint;
};

type LocalDeployment = Awaited<ReturnType<typeof publishWorkflowDeployment>>;

/** The confirmed state a committed local history must chain from. */
export type LocalConfirmedState = {
  headerHash: string;
  utxoRoot: string;
  endTime: bigint;
};

/** The staging chain a local event fixture publishes its L1 events on. */
export type LocalHistoryEventStage = {
  deployment: LocalDeployment;
  operatorVkey: string;
  ownerSeedPhrase: string;
  ownerKey: CML.PrivateKey;
  rawAuthority: ReturnType<typeof recordCrossBlockRawEmulator>["authority"];
  publishDeposit(): Promise<StagedLocalHistoryEvent>;
  publishWithdrawal(
    body: SDK.WithdrawalBody,
    ordinal: number,
  ): Promise<StagedLocalHistoryEvent>;
  /** The live confirmed state of the staging chain's state queue. */
  confirmedState(): Promise<LocalConfirmedState>;
  /**
   * Commit, attest and merge one block through the protocol's own state queue
   * so that its settlement output exists on the staging chain. Maturity is
   * reached by advancing the emulator clock. Callers publishing later events
   * must synchronize Date with that clock because the SDK dates admissions.
   */
  settle(block: VerifiableJourneyBlock): Promise<UTxO>;
  /** Commit against the current queue tail without attesting or merging. */
  commit(block: VerifiableJourneyBlock): Promise<UTxO>;
  /** Retire an actual source leaf against its merged settlement and current frontier. */
  retire(
    event: StagedLocalHistoryEvent,
    settled: VerifiableJourneyBlock & { payload: SDK.DaPayload },
    settlement: UTxO,
  ): Promise<{
    txHash: string;
    signedCbor: string;
    witness: SDK.EventHistoryWitness;
  }>;
  close(): void;
};

/** Only ordinary funding is spent by an event publication. */
const isOrdinaryFunding = (utxo: UTxO) =>
  utxo.datum == null &&
  utxo.datumHash == null &&
  utxo.scriptRef == null &&
  Object.keys(utxo.assets).every((unit) => unit === "lovelace");

/**
 * An isolated emulator deployment of the frozen blueprint that publishes the
 * same SDK-built deposit and withdrawal events the live journeys publish. The
 * fixtures under verification therefore consume real L1 event outputs, and the
 * recorded raw authority serves the actual submitted transactions back to the
 * installed classifier. Nothing here stubs an event, a datum or a role token.
 */
export const openLocalHistoryEventStage =
  async (): Promise<LocalHistoryEventStage> => {
    const recorder = recordCrossBlockRawEmulator();
    let deployment: LocalDeployment;
    try {
      const accounts = createPublishedWorkflowDeploymentAccounts();
      deployment = await publishWorkflowDeployment({ accounts });
      // The SDK derives each event's transaction deadline and authenticated
      // inclusion time from the wall clock, exactly as it does on a devnet. The
      // emulator's own clock therefore has to stay behind that deadline; the
      // committing block windows are placed around the real inclusion times.
      if (deployment.emulator.now() >= Date.now())
        throw new Error(
          "Local event staging chain is ahead of the wall clock event deadline",
        );
      const lucid = deployment.operatorLucid;
      const address = await lucid.wallet().address();
      const ownerSeedPhrase = accounts.operator.seedPhrase;
      const ownerKey = journeyLedgerOwnerKey(ownerSeedPhrase);
      const operatorVkey = paymentCredentialOf(address).hash;
      const publish = async (
        request: JourneyEventPublicationRequest,
        kind: "deposit" | "withdrawal",
      ): Promise<StagedLocalHistoryEvent> => {
        const funding = (await lucid.utxosAt(address)).filter(
          isOrdinaryFunding,
        );
        if (funding.length === 0)
          throw new Error("No ordinary funding inputs for event publication");
        lucid.overrideUTxOs(funding);
        // Respect actual predecessor protection before using the SDK's
        // 60-second validity backoff. Tests synchronize Date with this advance.
        const history = SDK.eventHistoryDeploymentFromContracts(
          SDK.requireEventHistoryContracts(deployment.contracts)[kind],
        );
        const nodes = SDK.authenticateHistoryNodes(
          await lucid.utxosAt(history.address),
          history,
        );
        if (!nodes.some(({ key }) => key === null))
          throw new Error("History admission requires its initialized root");
        const protectedUntil = nodes.reduce(
          (latest, { node }) =>
            node.protected_until > latest ? node.protected_until : latest,
          0n,
        );
        const readyAt = Number(protectedUntil) + 60_000;
        if (!Number.isSafeInteger(readyAt))
          throw new Error(
            "History protection exceeds the emulator clock range",
          );
        const buildTime = Math.max(Date.now(), readyAt);
        await deployment.chain.awaitLedgerTime(buildTime);
        if (deployment.chain.now() < buildTime)
          throw new Error("Event staging clock did not reach the build window");
        const built = await request.build();
        const signed = await built.tx.sign.withWallet().complete();
        const submitted = await signed.submitSafe();
        if (submitted._tag === "Left")
          throw new Error(
            `Published ${request.name} submission failed: ${submitted.left.message}`,
            { cause: submitted.left },
          );
        const txHash = submitted.right;
        await lucid.awaitTx(txHash, 200);
        lucid.overrideUTxOs(await lucid.utxosAt(address));
        const outputs = (
          await lucid.utxosAtWithUnit(
            built.metadata.address,
            built.metadata.unit,
          )
        ).filter((utxo) => utxo.txHash === txHash);
        const event = outputs[0];
        if (outputs.length !== 1 || event === undefined)
          throw new Error(
            `Published ${request.name} has no unique event output`,
          );
        return {
          ...(await readJourneyHistoryEvent(
            lucid,
            deployment.contracts,
            built.metadata.unit,
          )),
          inclusionTime: BigInt(built.metadata.inclusionTime),
        };
      };
      const { contracts, chain } = deployment;
      const rootUnit = toUnit(
        contracts.stateQueue.policyId,
        SDK.STATE_QUEUE_ROOT_ASSET_NAME,
      );
      const one = async (scriptAddress: string, unit: string) => {
        const found = await lucid.utxosAtWithUnit(scriptAddress, unit);
        if (found.length !== 1 || found[0] === undefined)
          throw new Error(`Expected one actual published state: ${unit}`);
        return found[0];
      };
      const reference = (name: string) => {
        const found = deployment.references.get(name);
        if (found === undefined)
          throw new Error(`Missing published ${name} reference`);
        return found;
      };
      const confirmedState = async (): Promise<LocalConfirmedState> => {
        const root = await one(
          contracts.stateQueue.spendingScriptAddress,
          rootUnit,
        );
        const confirmed = await Effect.runPromise(
          SDK.getConfirmedStateFromStateQueueDatum(
            await Effect.runPromise(SDK.getLinkedListNodeViewFromUTxO(root)),
          ),
        );
        return {
          headerHash: confirmed.data.headerHash,
          utxoRoot: confirmed.data.utxoRoot,
          endTime: confirmed.data.endTime,
        };
      };
      const awaitTime = async (time: number) => {
        await chain.awaitLedgerTime(time);
      };
      let blockActor:
        | Awaited<ReturnType<typeof createPublishedWatcherBlockActor>>
        | undefined;
      const actorForCommit = async () => {
        if (blockActor === undefined) {
          blockActor = await createPublishedWatcherBlockActor({
            deployment,
            lucid,
            daSignerConfig: {
              NETWORK: "Custom",
              L1_OPERATOR_SEED_PHRASE: accounts.operator.seedPhrase,
              DA_COSIGNER_SEED_PHRASE: accounts.cosigner.seedPhrase,
            },
          });
          await blockActor.onboardOperator();
        }
        return blockActor;
      };
      const commit = async (block: VerifiableJourneyBlock) => {
        // Proof builders select an enterprise prover wallet on this Lucid instance.
        lucid.selectWallet.fromSeed(ownerSeedPhrase);
        const actor = await actorForCommit();
        // The commit's short validity range fixes the header end time (Q60),
        // so the chain clock must first reach the block's closing minute.
        await awaitTime(Number(block.header.endTime) - 59_999);
        lucid.overrideUTxOs(await lucid.utxosAt(address));
        const queue = await Effect.runPromise(
          SDK.fetchSortedStateQueueUTxOsProgram(lucid, {
            stateQueueAddress: contracts.stateQueue.spendingScriptAddress,
            stateQueuePolicyId: contracts.stateQueue.policyId,
          }),
        );
        const tail = queue[queue.length - 1];
        if (tail === undefined)
          throw new Error("History commit has no queue root");
        await actor.commit(block, tail.utxo, queue[1]?.utxo);
        lucid.overrideUTxOs(await lucid.utxosAt(address));
        return one(
          contracts.stateQueue.spendingScriptAddress,
          contracts.stateQueue.policyId +
            SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX +
            block.headerHash,
        );
      };
      const settle = async (block: VerifiableJourneyBlock) => {
        await commit(block);
        const actor = await actorForCommit();
        const attested = await actor.attest({
          ...block,
          payloadEnvelopeCbor: Buffer.from(block.payloadEnvelopeCbor),
        });
        if (attested.kind !== "attested")
          throw new Error(
            `History block ${block.headerHash} was corrected before its DA attestation applied`,
          );
        const maturity = Number(
          block.header.endTime + SDK.MATURITY_DURATION_MS,
        );
        const validFrom = lucid.slotToUnixTime(
          lucid.unixTimeToSlot(maturity) + 1,
        );
        await awaitTime(validFrom + 1_000);
        const fetchConfig = {
          stateQueueAddress: contracts.stateQueue.spendingScriptAddress,
          stateQueuePolicyId: contracts.stateQueue.policyId,
        };
        const queue = await Effect.runPromise(
          SDK.fetchConfirmedStateAndItsLinkProgram(lucid, fetchConfig),
        );
        if (
          queue.link.datum.key === "Empty" ||
          queue.link.datum.key.Key.key !== block.headerHash
        )
          throw new Error(
            "The committed block is not the oldest queued header",
          );
        const [hub, correctionLock] = await Promise.all([
          Effect.runPromise(
            SDK.fetchHubOracleUTxOProgram(lucid, {
              hubOracleAddress: contracts.hubOracle.spendingScriptAddress,
              hubOraclePolicyId: contracts.hubOracle.policyId,
            }),
          ),
          Effect.runPromise(
            SDK.fetchCorrectionLockUTxOProgram(lucid, {
              correctionLockAddress:
                contracts.correctionLock.spendingScriptAddress,
              hubOraclePolicyId: contracts.hubOracle.policyId,
            }),
          ),
        ]);
        const funding = (await lucid.utxosAt(address)).filter(
          isOrdinaryFunding,
        );
        if (funding.length === 0)
          throw new Error("Settlement has no ordinary funding input");
        lucid.overrideUTxOs(funding);
        const merged = await Effect.runPromise(
          SDK.buildMergeToConfirmedStateTxProgram({
            lucid,
            contracts,
            fetchConfig,
            confirmedUTxO: queue.confirmed,
            firstBlockUTxO: queue.link,
            validFrom,
            presetWalletInputs: funding,
            hubOracleRefInput: hub.utxo,
            correctionLockRefInput: correctionLock,
            stateQueueMergeYieldRefInput: reference("stateQueueMergeWithdraw"),
            referenceScripts: {
              stateQueueSpending: reference("stateQueueSpend"),
              stateQueueMinting: reference("stateQueueMint"),
              settlementMinting: reference("settlementMint"),
            },
          }),
        );
        const signed = await merged.tx.sign.withWallet().complete();
        await lucid.awaitTx(await signed.submit(), 200);
        lucid.overrideUTxOs(await lucid.utxosAt(address));
        const settlement = await one(
          contracts.settlement.spendingScriptAddress,
          contracts.settlement.policyId + block.headerHash,
        );
        const datum = Data.from(settlement.datum!, SDK.SettlementDatum);
        if (
          datum.deposits_root !== block.header.depositsRoot ||
          datum.withdrawals_root !== block.header.withdrawalsRoot
        )
          throw new Error(
            "Settlement does not preserve the retained event roots",
          );
        return settlement;
      };
      const retire: LocalHistoryEventStage["retire"] = async (
        event,
        settled,
        settlement,
      ) => {
        const order = event.order;
        const deposit = order.kind === "Deposit";
        const entries = deposit
          ? settled.payload.block_body.deposits
          : settled.payload.block_body.withdrawals;
        const key = deposit
          ? SDK.committedDepositKeyBytes(order.event.id)
          : SDK.committedWithdrawalKeyBytes(order.event.id);
        const captured = captureStagedHistoryEvent(event);
        const value = canonical(
          plutusConstrFieldCbor(captured.openingCbor, [0, 0, 1]),
        );
        const entry = entries.find(([candidate]) => candidate === key);
        if (entry?.[1] !== value)
          throw new Error(
            "Retirement source does not contain the exact admitted event",
          );
        const tree = await buildCountedRoot(
          deposit ? SDK.ROOT_DOMAINS.deposits : SDK.ROOT_DOMAINS.withdrawals,
          entries.map(([key, value]) => ({
            key: Buffer.from(key, "hex"),
            value: Buffer.from(value, "hex"),
          })),
        );
        const root = deposit
          ? settled.header.depositsRoot
          : settled.header.withdrawalsRoot;
        if (
          tree.root !== root ||
          settlement.assets[
            contracts.settlement.policyId + settled.headerHash
          ] !== 1n
        )
          throw new Error(
            "Retirement source differs from its actual settlement",
          );
        const proof = await keyValuePhasProof(
          { ...tree, root: tree.phasRoot },
          Buffer.from(key, "hex"),
          Buffer.from(value, "hex"),
        );
        const funding = (await lucid.utxosAt(address)).filter(
          isOrdinaryFunding,
        );
        if (funding[0] === undefined)
          throw new Error("Retirement has no ordinary funding input");
        lucid.overrideUTxOs(funding);
        const name = deposit ? "deposit" : "withdrawal";
        const config = {
          feeInput: funding[0],
          settlementRefInput: settlement,
          membershipProof: {
            key,
            value,
            domain: tree.domain,
            root: tree.root,
            phas_root: tree.phasRoot,
            count: tree.count,
            proof,
          },
          nowMs: chain.now(),
          referenceScripts: {
            historyList: reference(`${name}Spend`),
            historyRetirement: reference(`${name}HistoryRetirementWithdraw`),
            ...(deposit ? {} : { payoutMinting: reference("payoutMint") }),
          },
        };
        const built =
          order.kind === "Deposit"
            ? await Effect.runPromise(
                SDK.buildAbsorbConfirmedDepositToReserveTxProgram(
                  lucid,
                  contracts,
                  { ...config, deposit: order },
                ),
              )
            : await Effect.runPromise(
                SDK.buildInitializePayoutTxProgram(lucid, contracts, {
                  ...config,
                  withdrawal: order,
                }),
              );
        const signed = await built.tx.sign.withWallet().complete();
        const txHash = await signed.submit();
        await lucid.awaitTx(txHash, 200);
        lucid.overrideUTxOs(await lucid.utxosAt(address));
        // Re-read the complete authenticated list after the retirement lands.
        const witness = await SDK.fetchEventHistoryWitness(
          { utxosAt: (address) => lucid.utxosAt(address) },
          SDK.eventHistoryDeploymentFromContracts(
            SDK.requireEventHistoryContracts(contracts)[name],
          ),
          order.event.id,
        );
        if (witness.kind !== "Absent")
          throw new Error("Retired event remains in the authenticated history");
        return { txHash, signedCbor: signed.toCBOR(), witness };
      };
      return {
        deployment,
        operatorVkey,
        ownerSeedPhrase,
        ownerKey,
        rawAuthority: recorder.authority,
        confirmedState,
        commit,
        settle,
        retire,
        publishDeposit: () =>
          publish(
            journeyDepositEventRequest({ deployment, ownerKey }),
            "deposit",
          ),
        publishWithdrawal: (body: SDK.WithdrawalBody, ordinal: number) =>
          publish(
            journeyWithdrawalEventRequest({
              deployment,
              ownerKey,
              body,
              ordinal,
            }),
            "withdrawal",
          ),
        close: () => recorder.restore(),
      };
    } catch (cause) {
      recorder.restore();
      throw cause;
    }
  };
