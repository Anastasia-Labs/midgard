import { openAvailabilityOperationJournal } from "@al-ft/midgard-core/availability-operation-journal";
import * as SDK from "@al-ft/midgard-sdk";
import { paymentCredentialOf } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  type CommitteeL1ClientConfig,
  l1SourceAuthorityDigest,
} from "../config.js";
import {
  correctionLockValidatorFromDeploymentInfo,
  daAttestationValidatorsFromDeployment,
} from "../l1/deployment.js";
import { lucidFromProviderUrl } from "../l1/lucid.js";
import type { StateQueueProvider } from "../l1/state-queue-scanner.js";
import { selectL1SubmitterWallet } from "../l1/submitter.js";
import type { CommitteeStore } from "../store.js";
import {
  availabilityParametersFromConfig,
  availabilityResponderCollateral,
  availabilityResponderL1ReadersFromConfig,
  availabilityResponderOperations,
} from "./factory.availability-responder-operations.js";
import {
  buildAvailabilityResponderTransaction,
  discoverAvailabilityResponderChallenges,
} from "./factory.discover-availability-responder-challenges.js";
import { availabilityResponderReferenceScripts } from "./reference-scripts.js";
import { AvailabilityResponder } from "./responder.js";

export const availabilityResponderFromConfig = async (
  config: CommitteeL1ClientConfig,
  store: CommitteeStore,
  chainProvider: StateQueueProvider,
  deps: { readonly lucidFromProviderUrl: typeof lucidFromProviderUrl } = {
    lucidFromProviderUrl,
  },
): Promise<{
  readonly responder: AvailabilityResponder;
  readonly close: () => void;
}> => {
  if (
    !config.availabilityJournalPath ||
    !config.availabilitySubmitterKeySource
  ) {
    throw new Error(
      "Live availability responder requires DA_AVAILABILITY_JOURNAL_PATH and a dedicated DA_AVAILABILITY_SUBMITTER_KEY_SOURCE",
    );
  }
  if (
    config.l1Source.sourceMode !== "local_node" ||
    chainProvider.currentChainSyncCursor === undefined
  ) {
    throw new Error(
      "Live availability responder requires the configured local_node canonical chain-sync authority",
    );
  }
  const providerUrl = config.cardanoProviderUrls[0];
  if (
    !providerUrl?.startsWith("kupmios:") ||
    !config.l1Source.queryProviderUrls.includes(providerUrl)
  ) {
    throw new Error(
      "Live availability responder requires a canonical kupmios query provider from L1_SOURCE_QUERY_PROVIDER_URLS",
    );
  }
  const [kupoUrl, ogmiosUrl] = providerUrl.slice("kupmios:".length).split("|");
  if (!kupoUrl || !ogmiosUrl)
    throw new Error(
      "Availability responder has an invalid kupmios provider URL",
    );
  const { lucid } = await deps.lucidFromProviderUrl(
    providerUrl,
    config.network,
    config.nativeLedger,
    config.cardanoL1Source.networkMagic,
  );
  await selectL1SubmitterWallet(lucid, config.availabilitySubmitterKeySource);
  const actor = paymentCredentialOf(await lucid.wallet().address());
  if (actor.type !== "Key")
    throw new Error("Availability responder requires a payment-key wallet");
  if (config.l1SubmitterKeySource === undefined)
    throw new Error(
      "Availability responder requires the attestation wallet identity for isolation checks",
    );
  await selectL1SubmitterWallet(lucid, config.l1SubmitterKeySource);
  if (paymentCredentialOf(await lucid.wallet().address()).hash === actor.hash) {
    throw new Error(
      "Availability responder and attestation submitter must use different payment credentials",
    );
  }
  await selectL1SubmitterWallet(lucid, config.availabilitySubmitterKeySource);
  const contractManifestId = config.contractDeploymentInfo.manifestId;
  if (
    typeof contractManifestId !== "string" ||
    !/^[0-9a-f]{64}$/u.test(contractManifestId)
  ) {
    throw new Error(
      "Availability responder requires the verified contract manifest identity",
    );
  }
  const contracts = daAttestationValidatorsFromDeployment(
    config.midgardNodeDeployment,
  );
  const parameters = availabilityParametersFromConfig(config);
  const referenceScripts = await availabilityResponderReferenceScripts(
    lucid,
    config.midgardNodeDeployment,
  );
  const hubOracle = await Effect.runPromise(
    SDK.fetchHubOracleUTxOProgram(lucid, {
      hubOracleAddress: contracts.hubOracle.spendingScriptAddress,
      hubOraclePolicyId: contracts.hubOracle.policyId,
    }),
  );
  const deployment: SDK.DaAvailabilityDeployment = {
    contracts: {
      ...contracts,
      correctionLock: correctionLockValidatorFromDeploymentInfo(
        config.contractDeploymentInfo,
        config.network,
      ),
    },
    hubOraclePolicyId: config.midgardNodeDeployment.hubOraclePolicyId,
    referenceScriptAuthPolicyId:
      config.midgardNodeDeployment.referenceScriptAuthPolicyId,
    parameters,
    referenceScripts,
    hubOracleRefInput: hubOracle.utxo,
  };
  const reportedSkips = new Set<string>();
  const assertSourceHealthy = async (): Promise<void> => {
    const source = await store.getL1SourceState();
    if (
      source?.status !== "healthy" ||
      source.sourceMode !== "local_node" ||
      source.network !== config.network ||
      source.authoritySha256 !==
        l1SourceAuthorityDigest(config.network, config.l1Source)
    ) {
      throw new Error(
        "Availability responder requires a healthy authenticated committee node L1 source; rollback recovery must finish first",
      );
    }
  };
  const provider = lucid.config().provider;
  if (provider === undefined)
    throw new Error("Availability responder has no transaction provider");
  // Only collateral comes from this wallet: publication and settlement fees are
  // protected shares of the challenger's on-chain bond.
  await availabilityResponderCollateral(
    lucid,
    [
      parameters.max_close_fee_lovelace,
      parameters.max_publication_fee_lovelace,
      parameters.max_settlement_fee_lovelace,
    ].reduce((maximum, fee) => (fee > maximum ? fee : maximum)),
  );
  const journal = openAvailabilityOperationJournal(
    config.availabilityJournalPath,
  );
  const { readBoundary, assertActuationCurrent, context, reconcile } =
    availabilityResponderOperations({
      lucid,
      readers: availabilityResponderL1ReadersFromConfig({
        config,
        lucid,
        kupoUrl,
        ogmiosUrl,
        currentCursor: chainProvider.currentChainSyncCursor.bind(chainProvider),
      }),
      assertSourceHealthy,
      context: {
        deploymentIdentity: contractManifestId,
        actor: actor.hash,
        journal,
        stateQueuePolicyId: deployment.contracts.stateQueue.policyId,
        minimumConfirmationDepth: config.finalityDepth,
        transactionLimits: SDK.daAvailabilityOperationLimits(
          lucid,
          deployment.parameters,
        ),
        submit: (signedCbor) => provider.submitTx(signedCbor),
      },
    });
  return {
    close: () => journal.close(),
    responder: new AvailabilityResponder({
      deploymentFingerprint: config.deploymentFingerprint,
      deploymentIdentity: deployment.hubOraclePolicyId,
      store,
      reconcile,
      discover: async () => {
        await assertActuationCurrent();
        const before = await readBoundary();
        const snapshots = await discoverAvailabilityResponderChallenges(
          lucid,
          deployment,
          (skipped) => {
            // A stranded record stays on L1 for good; report it once.
            const key = `${skipped.outRef}:${skipped.reason}`;
            if (reportedSkips.has(key)) return;
            reportedSkips.add(key);
            process.stderr.write(
              `${JSON.stringify({ event: "availability_responder_skipped_record", ...skipped })}\n`,
            );
          },
        );
        const after = await readBoundary();
        if (before.pointId !== after.pointId)
          throw new Error(
            "Canonical source changed during availability challenge discovery",
          );
        return snapshots;
      },
      execute: async (action) => {
        const result = await SDK.runDaAvailabilityOperation(context, {
          action: action.kind,
          headerHash: action.challenge.record.datum.commitment.header_hash,
          build: async () =>
            (
              await buildAvailabilityResponderTransaction(
                lucid,
                deployment,
                action,
              )
            ).tx,
        });
        if (result.status === "conflict")
          throw new Error(
            "Availability responder transaction conflicts with canonical L1 history",
          );
        return result.status === "confirmed" || result.status === "included"
          ? result.status
          : "pending";
      },
    }),
  };
};
