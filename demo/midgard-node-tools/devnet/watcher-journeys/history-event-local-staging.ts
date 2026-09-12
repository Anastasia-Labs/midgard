import { recordCrossBlockRawEmulator } from "@al-ft/midgard-fault-proofs/test-support/cross-block-raw-emulator";
import type * as SDK from "@al-ft/midgard-sdk";
import {
  type CML,
  paymentCredentialOf,
  type UTxO,
} from "@lucid-evolution/lucid";
import {
  createPublishedWorkflowDeploymentAccounts,
  publishWorkflowDeployment,
} from "midgard-node/tests/helpers/published-workflow-deployment";

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
      return {
        deployment,
        operatorVkey,
        ownerSeedPhrase,
        ownerKey,
        rawAuthority: recorder.authority,
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
