import { openAvailabilityOperationJournal } from "@al-ft/midgard-core/availability-operation-journal";
import * as SDK from "@al-ft/midgard-sdk";
import { paymentCredentialOf } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type { AvailabilityResponseLoopEnforcement } from "../availability-response-loop.js";
import { type CommitteeL1ClientConfig } from "../config.js";
import {
  correctionLockValidatorFromDeploymentInfo,
  daAttestationValidatorsFromDeployment,
} from "../l1/deployment.js";
import { committeeAvailabilityReads } from "../l1/follower/availability-reads.js";
import type { CommitteeL1Follower } from "../l1/follower/l1-follower.js";
import { selectL1SubmitterWallet } from "../l1/submitter.js";
import type { CommitteeStore } from "../store.js";
import { createCommitteePromiseAdmissionSource } from "./create-promise-admission-source.js";
import {
  availabilityParametersFromConfig,
  availabilityResponderCollateral,
  availabilityResponderOperations,
} from "./factory.availability-responder-operations.js";
import { configuredCommitteePromiseRuntime } from "./factory.configured-promise-runtime.js";
import {
  availabilityResponderTransactionOperation,
  discoverAvailabilityResponderChallenges,
} from "./factory.discover-availability-responder-challenges.js";
import type { CommitteePromiseAdmissionSource } from "./promise-admission.js";
import { assertCommitteePromiseEnrollment } from "./promise-profile-selection.js";
import { availabilityResponderReferenceScripts } from "./reference-scripts.js";
import { AvailabilityResponder } from "./responder.js";
import { assertAvailabilityResponderSourceHealthy } from "./source-authority.js";

/**
 * The availability responder and promise admission source on the committee's
 * L1 follower: every L1 read is the follower's facts or the node's ledger
 * state, and every submit goes to the node's LocalTxSubmission through the
 * follower's provider. Build it once the follower is ready: its first reads
 * (reference scripts, the hub oracle, collateral) need the follower's facts.
 */
export const availabilityResponderFromConfig = async (
  config: CommitteeL1ClientConfig,
  store: CommitteeStore,
  follower: Pick<
    CommitteeL1Follower,
    "source" | "store" | "provider" | "lucid"
  >,
): Promise<{
  readonly responder: AvailabilityResponder;
  readonly promiseAdmissionSource: CommitteePromiseAdmissionSource;
  readonly close: () => void;
  readonly promiseLoopEnforcement?: AvailabilityResponseLoopEnforcement;
  readonly bindRetirementOperationalPins?: (
    read: () => readonly string[],
  ) => void;
  readonly compactRetainedPromises?: () => Promise<readonly string[]>;
}> => {
  await assertCommitteePromiseEnrollment(config, store);
  if (
    !config.availabilityJournalPath ||
    !config.availabilitySubmitterKeySource
  ) {
    throw new Error(
      "Live availability responder requires DA_AVAILABILITY_JOURNAL_PATH and a dedicated DA_AVAILABILITY_SUBMITTER_KEY_SOURCE",
    );
  }
  const { provider } = follower;
  if (follower.store === null || provider === null)
    throw new Error(
      "Live availability responder requires the committee's L1 follower",
    );
  const reads = committeeAvailabilityReads({
    store: follower.store,
    readiness: () => follower.source.readiness(),
  });
  const lucid = await follower.lucid();
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
  const assertSourceHealthy = (): Promise<void> =>
    assertAvailabilityResponderSourceHealthy(store, config);
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
  if (config.availabilityPromiseAdoption) {
    try {
      return await configuredCommitteePromiseRuntime({
        config,
        store,
        lucid,
        reads,
        ledger: {
          protocolParameters: () => provider.getProtocolParameters(),
          slotConfig: () => provider.slotConfig(),
        },
        deployment,
        actorId: actor.hash,
        journal,
      });
    } catch (error) {
      journal.close();
      throw error;
    }
  }
  const { readBoundary, assertActuationCurrent, context, reconcile } =
    availabilityResponderOperations({
      lucid,
      reads,
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
        // The node's LocalTxSubmission, through the follower's provider.
        submit: (signedCbor) => provider.submitTx(signedCbor),
      },
    });
  return {
    close: () => journal.close(),
    // Finite preparation/signing/persistence/submission bounds are not yet
    // established. No policy is inferred from a lease or a read timeout.
    promiseAdmissionSource: createCommitteePromiseAdmissionSource({
      config,
      deployment,
      actorId: actor.hash,
      store,
      journal,
      lucid,
      reads,
      readBoundary,
      assertActuationCurrent,
    }),
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
        if (
          before.pointId !== after.pointId ||
          before.generation !== after.generation
        )
          throw new Error(
            "Canonical source changed during availability challenge discovery",
          );
        return snapshots;
      },
      execute: async (action) => {
        const result = await SDK.runDaAvailabilityOperation(
          context,
          availabilityResponderTransactionOperation(lucid, deployment, action),
        );
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
