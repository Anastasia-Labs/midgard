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
} from "./history-event-staging.js";
import type { StagedHistoryEvent } from "./history-events.js";

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
   * reached by advancing the emulator clock, so no event may be published on
   * this chain afterwards: the SDK dates events by the wall clock.
   */
  settle(block: VerifiableJourneyBlock): Promise<UTxO>;
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
      ): Promise<StagedLocalHistoryEvent> => {
        const funding = (await lucid.utxosAt(address)).filter(
          isOrdinaryFunding,
        );
        if (funding.length === 0)
          throw new Error("No ordinary funding inputs for event publication");
        lucid.overrideUTxOs(funding);
        const built = await request.build();
        const signed = await built.tx.sign.withWallet().complete();
        const txHash = await signed.submit();
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
          event,
          policyId: built.metadata.unit.slice(0, 56),
          assetName: built.metadata.unit.slice(56),
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
      const settle = async (block: VerifiableJourneyBlock) => {
        const actor = await createPublishedWatcherBlockActor({
          deployment,
          lucid,
          daSignerConfig: {
            NETWORK: "Custom",
            L1_OPERATOR_SEED_PHRASE: accounts.operator.seedPhrase,
            DA_COSIGNER_SEED_PHRASE: accounts.cosigner.seedPhrase,
          },
        });
        await actor.onboardOperator();
        // The commit's short validity range fixes the header end time (Q60),
        // so the chain clock must first reach the block's closing minute.
        await awaitTime(Number(block.header.endTime) - 59_999);
        lucid.overrideUTxOs(await lucid.utxosAt(address));
        const anchor = await one(
          contracts.stateQueue.spendingScriptAddress,
          rootUnit,
        );
        await actor.commit(block, anchor);
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
        if (datum.deposits_root !== block.header.depositsRoot)
          throw new Error(
            "Settlement does not preserve the retained deposit root",
          );
        return settlement;
      };
      return {
        deployment,
        operatorVkey,
        ownerSeedPhrase,
        ownerKey,
        rawAuthority: recorder.authority,
        confirmedState,
        settle,
        publishDeposit: () =>
          publish(journeyDepositEventRequest({ deployment, ownerKey })),
        publishWithdrawal: (body: SDK.WithdrawalBody, ordinal: number) =>
          publish(
            journeyWithdrawalEventRequest({
              deployment,
              ownerKey,
              body,
              ordinal,
            }),
          ),
        close: () => recorder.restore(),
      };
    } catch (cause) {
      recorder.restore();
      throw cause;
    }
  };
