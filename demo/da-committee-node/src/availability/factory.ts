import { openAvailabilityOperationJournal } from "@al-ft/midgard-core/availability-operation-journal";
import * as SDK from "@al-ft/midgard-sdk";
import {
  Data,
  type LucidEvolution,
  paymentCredentialOf,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { type CommitteeConfig, l1SourceAuthorityDigest } from "../config.js";
import {
  correctionLockValidatorFromDeploymentInfo,
  daAttestationValidatorsFromDeployment,
} from "../l1/deployment.js";
import { lucidFromProviderUrl } from "../l1/lucid.js";
import {
  kupmiosChainPointResolver,
  kupmiosCurrentChainPointResolver,
} from "../l1/provider.js";
import type { StateQueueProvider } from "../l1/state-queue-scanner.js";
import { selectL1SubmitterWallet } from "../l1/submitter.js";
import type { CommitteeStore } from "../store.js";
import { availabilityResponderReferenceScripts } from "./reference-scripts.js";
import {
  AvailabilityResponder,
  type AvailabilityResponderAction,
  type AvailabilityResponderChallenge,
} from "./responder.js";

export const availabilityResponderFromConfig = async (
  config: CommitteeConfig,
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
  const currentCursor =
    chainProvider.currentChainSyncCursor.bind(chainProvider);
  let expectedRollbackGeneration: number | undefined;
  const currentPoint = kupmiosCurrentChainPointResolver(
    config.network,
    kupoUrl,
    ogmiosUrl,
  );
  const readBoundary =
    async (): Promise<SDK.DaAvailabilityCanonicalBoundary> => {
      const point = await currentPoint();
      const cursor = await currentCursor();
      if (
        expectedRollbackGeneration !== undefined &&
        cursor.rollbackGeneration !== expectedRollbackGeneration
      ) {
        throw new Error(
          "Availability responder canonical generation changed; durable operations must reconcile on the next scan",
        );
      }
      if (
        cursor.point.slot !== point.slot ||
        cursor.point.blockHash !== point.blockHash ||
        cursor.point.network !== point.network
      ) {
        throw new Error(
          "Availability responder awaits the next canonical committee node L1 scan before acting",
        );
      }
      return { pointId: `${point.slot}:${point.blockHash}`, slot: point.slot };
    };
  const assertActuationCurrent = async (): Promise<void> => {
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
    await readBoundary();
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
  const context: SDK.DaAvailabilityOperationContext = {
    deploymentIdentity: contractManifestId,
    actor: actor.hash,
    journal,
    stateQueuePolicyId: deployment.contracts.stateQueue.policyId,
    minimumConfirmationDepth: config.finalityDepth,
    transactionLimits: SDK.daAvailabilityOperationLimits(
      lucid,
      deployment.parameters,
    ),
    assertActuationCurrent,
    observe: SDK.createDaAvailabilityOperationObserver({
      lucid,
      readBoundary,
      resolveInclusion: kupmiosChainPointResolver(
        lucid,
        kupoUrl,
        fetch,
        ogmiosUrl,
        config.network,
        config.finalityDepth,
      ),
    }),
    submit: (signedCbor) => provider.submitTx(signedCbor),
  };
  return {
    close: () => journal.close(),
    responder: new AvailabilityResponder({
      deploymentFingerprint: config.deploymentFingerprint,
      deploymentIdentity: deployment.hubOraclePolicyId,
      store,
      reconcile: async () => {
        // A new tick may adopt a recovered generation only for reconciliation;
        // no new action is selected until every durable intent is checked.
        expectedRollbackGeneration = (await currentCursor()).rollbackGeneration;
        const results = await SDK.reconcileDaAvailabilityOperations(context);
        if (results.some((result) => result.status === "conflict"))
          throw new Error(
            "Availability responder journal contains a conflicting transaction; authenticated recovery is required",
          );
        return results.some(
          (result) =>
            result.status !== "confirmed" &&
            result.status !== "included" &&
            result.status !== "expired",
        )
          ? "pending"
          : "ready";
      },
      discover: async () => {
        await assertActuationCurrent();
        const before = await readBoundary();
        const snapshots = await discoverAvailabilityResponderChallenges(
          lucid,
          deployment,
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
          headerHash:
            action.challenge.bond.datum.ChallengedBond.commitment.header_hash,
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

const availabilityParametersFromConfig = (
  config: CommitteeConfig,
): SDK.DaAvailabilityParameters => {
  const p = config.availabilityChallenge;
  return SDK.daAvailabilityParameters({
    responseGeometry: SDK.availabilityResponseGeometry(p.responseGeometry),
    daBondLovelace: BigInt(p.daBondLovelace),
    challengerBondLovelace: BigInt(p.challengerBondLovelace),
    maxOpenFeeLovelace: BigInt(p.maxOpenFeeLovelace),
    maxPublicationFeeLovelace: BigInt(p.maxPublicationFeeLovelace),
    maxSettlementFeeLovelace: BigInt(p.maxSettlementFeeLovelace),
    maxCloseFeeLovelace: BigInt(p.maxCloseFeeLovelace),
    maxTimeoutFeeLovelace: BigInt(p.maxTimeoutFeeLovelace),
  });
};

export const availabilityResponderCollateral = async (
  lucid: Pick<LucidEvolution, "config" | "utxosAt"> & {
    readonly wallet: () => { readonly address: () => Promise<string> };
  },
  fee: bigint,
): Promise<readonly UTxO[]> => {
  const protocol = lucid.config().protocolParameters;
  if (protocol === undefined)
    throw new Error("Availability responder requires live protocol parameters");
  const required = (fee * BigInt(protocol.collateralPercentage) + 99n) / 100n;
  const address = await lucid.wallet().address();
  const candidates = (await lucid.utxosAt(address))
    .filter(
      (utxo) =>
        !utxo.datum &&
        !utxo.datumHash &&
        !utxo.scriptRef &&
        Object.keys(utxo.assets).length === 1 &&
        (utxo.assets.lovelace ?? 0n) >= required,
    )
    .sort((a, b) =>
      `${a.txHash}#${a.outputIndex}`.localeCompare(
        `${b.txHash}#${b.outputIndex}`,
      ),
    );
  if (candidates[0] === undefined)
    throw new Error(
      `Availability responder wallet lacks separate plain-ADA collateral of at least ${required} lovelace`,
    );
  return [candidates[0]];
};

export const discoverAvailabilityResponderChallenges = async (
  lucid: LucidEvolution,
  deployment: SDK.DaAvailabilityDeployment,
): Promise<readonly AvailabilityResponderChallenge[]> => {
  const [availabilityUtxos, stateQueueUtxos, correctionLockUtxos] =
    await Promise.all([
      lucid.utxosAt(
        deployment.contracts.availabilityChallenge.spendingScriptAddress,
      ),
      lucid.utxosAt(deployment.contracts.stateQueue.spendingScriptAddress),
      lucid.utxosAt(deployment.contracts.correctionLock.spendingScriptAddress),
    ]);
  const prefix =
    deployment.contracts.availabilityChallenge.policyId +
    SDK.DA_AVAILABILITY_BOND_ASSET_NAME_PREFIX;
  const result: AvailabilityResponderChallenge[] = [];
  for (const utxo of availabilityUtxos) {
    if (!Object.keys(utxo.assets).some((unit) => unit.startsWith(prefix)))
      continue;
    if (typeof utxo.datum !== "string")
      throw new Error("Authenticated availability bond has no inline datum");
    const datum = Data.from(utxo.datum, SDK.DaAvailabilityBondDatum);
    SDK.assertCanonicalDaAvailabilityBondDatum(datum, deployment.parameters);
    if (!("ChallengedBond" in datum)) continue;
    const snapshot = await SDK.daAvailabilityChallengeSnapshotFromUtxos(
      deployment,
      datum.ChallengedBond.commitment.header_hash,
      {
        availabilityUtxos,
        stateQueueUtxos,
        correctionLockUtxos,
      },
    );
    if (
      !snapshot.bond ||
      !snapshot.bondDatum ||
      !("ChallengedBond" in snapshot.bondDatum) ||
      !snapshot.terminal ||
      !snapshot.terminalDatum ||
      !snapshot.queue
    ) {
      throw new Error(
        "Authenticated availability challenge has incomplete live state",
      );
    }
    result.push({
      bond: { utxo: snapshot.bond, datum: snapshot.bondDatum },
      terminal: { utxo: snapshot.terminal, datum: snapshot.terminalDatum },
      queue: snapshot.queue.utxo,
      tranches: snapshot.tranches,
    });
  }
  return result.sort((a, b) => {
    const left = a.bond.datum.ChallengedBond,
      right = b.bond.datum.ChallengedBond;
    return left.response_deadline === right.response_deadline
      ? left.commitment.header_hash.localeCompare(right.commitment.header_hash)
      : left.response_deadline < right.response_deadline
        ? -1
        : 1;
  });
};

export const buildAvailabilityResponderTransaction = async (
  lucid: LucidEvolution,
  deployment: SDK.DaAvailabilityDeployment,
  action: AvailabilityResponderAction,
  nowMs = Date.now(),
): Promise<SDK.BuiltDaAvailabilityTransaction> => {
  const p = deployment.parameters;
  const feeLovelace =
    action.kind === "publish"
      ? p.max_publication_fee_lovelace
      : action.kind === "settle"
        ? p.max_settlement_fee_lovelace
        : p.max_close_fee_lovelace;
  const validFrom = BigInt(Math.max(0, nowMs - 60_000));
  const unconstrainedUpper = validFrom + 120_000n;
  const deadlineUpper =
    action.challenge.bond.datum.ChallengedBond.response_deadline + 1n;
  const validTo =
    action.kind === "publish" && deadlineUpper < unconstrainedUpper
      ? deadlineUpper
      : unconstrainedUpper;
  const resources = {
    feeLovelace,
    validFrom,
    validTo,
    collateralInputs: await availabilityResponderCollateral(lucid, feeLovelace),
  };
  switch (action.kind) {
    case "publish":
      return Effect.runPromise(
        SDK.buildPublishDaAvailabilityChunkTxProgram(lucid, deployment, {
          ...resources,
          thread: action.tranche.utxo,
          previousCarrier: action.tranche.carrier,
          publication: action.publication,
        }),
      );
    case "settle":
      return Effect.runPromise(
        SDK.buildSettleDaAvailabilityTrancheTxProgram(lucid, deployment, {
          ...resources,
          bond: action.challenge.bond.utxo,
          terminal: action.challenge.terminal.utxo,
          thread: action.tranche.utxo,
          carrier: action.tranche.carrier,
        }),
      );
    case "close":
      return Effect.runPromise(
        SDK.buildCloseDaAvailabilityChallengeTxProgram(lucid, deployment, {
          ...resources,
          bond: action.challenge.bond.utxo,
          terminal: action.challenge.terminal.utxo,
          queue: action.challenge.queue,
        }),
      );
  }
};
