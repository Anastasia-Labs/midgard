import { openAvailabilityOperationJournal } from "@al-ft/midgard-core/availability-operation-journal";
import * as SDK from "@al-ft/midgard-sdk";
import {
  type LucidEvolution,
  paymentCredentialOf,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  type CommitteeConfig,
  type CommitteeL1ClientConfig,
  l1SourceAuthorityDigest,
} from "../config.js";
import {
  correctionLockValidatorFromDeploymentInfo,
  daAttestationValidatorsFromDeployment,
} from "../l1/deployment.js";
import { lucidFromProviderUrl } from "../l1/lucid.js";
import {
  type CanonicalChainPoint,
  type ChainSyncCursor,
  kupmiosChainPointResolver,
  kupmiosCurrentChainPointResolver,
} from "../l1/provider.js";
import {
  L1SourceIntegrityError,
  StateQueueHistoryNotExtendingAnchorError,
} from "../l1/source-integrity.js";
import {
  fetchAncestor,
  fetchOgmiosTipBlockNo,
  fetchSpend,
  readTransaction,
  type StateQueueReplayFetch,
  type StateQueueReplayWebSocket,
  type StateQueueReplayWebSocketFactory,
} from "../l1/state-queue-replay-provider.js";
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
      readers: {
        currentPoint: kupmiosCurrentChainPointResolver(
          config.network,
          kupoUrl,
          ogmiosUrl,
        ),
        currentCursor: chainProvider.currentChainSyncCursor.bind(chainProvider),
        tipBlockNo: () => fetchOgmiosTipBlockNo(ogmiosUrl, fetch),
        resolveInclusion: kupmiosChainPointResolver(
          lucid,
          kupoUrl,
          fetch,
          ogmiosUrl,
          config.network,
          config.finalityDepth,
        ),
        foreignSpend: availabilityForeignSpendReaders({ kupoUrl, ogmiosUrl }),
      },
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

export type AvailabilityResponderL1Readers = Readonly<{
  /** The aligned Kupmios tip: Kupo and Ogmios at one chain point. */
  currentPoint: () => Promise<CanonicalChainPoint>;
  /** The committee node's chain-sync cursor and rollback generation. */
  currentCursor: () => Promise<ChainSyncCursor>;
  /** Ogmios's tip block height (`queryNetwork/blockHeight`). */
  tipBlockNo: () => Promise<number>;
  resolveInclusion: (
    output: UTxO,
  ) => Promise<Readonly<{ slot?: number; blockHash?: string; depth?: number }>>;
  foreignSpend: Omit<SDK.DaAvailabilityForeignSpendReaders, "readBoundary">;
}>;

/**
 * The responder's canonical boundary, operation context and reconcile step,
 * built on injected L1 readers.
 *
 * The boundary is the point where the committee's chain-sync cursor and the
 * aligned Kupmios tip agree, with that point's block height read between two
 * tip reads that must both still be the cursor's point. The same boundary
 * brackets inclusion reads and the verified foreign-spend check, so a
 * responder whose Publish, Settle or Close lost its race to another
 * transaction (every one of them spends only protocol UTxOs) expires that
 * intent once the rival spend is final, instead of waiting on it forever.
 */
export const availabilityResponderOperations = (input: {
  readonly lucid: LucidEvolution;
  readonly readers: AvailabilityResponderL1Readers;
  readonly assertSourceHealthy: () => Promise<void>;
  readonly context: Omit<
    SDK.DaAvailabilityOperationContext,
    "observe" | "assertActuationCurrent"
  >;
}) => {
  const { readers } = input;
  let expectedRollbackGeneration: number | undefined;
  const awaitingScan = () =>
    new Error(
      "Availability responder awaits the next canonical committee node L1 scan before acting",
    );
  const readBoundary = async (): Promise<
    SDK.DaAvailabilityCanonicalBoundary &
      Readonly<{ blockHash: string; blockNo: number }>
  > => {
    const point = await readers.currentPoint();
    const cursor = await readers.currentCursor();
    if (
      expectedRollbackGeneration !== undefined &&
      cursor.rollbackGeneration !== expectedRollbackGeneration
    ) {
      throw new Error(
        "Availability responder canonical generation changed; durable operations must reconcile on the next scan",
      );
    }
    const atCursor = (tip: CanonicalChainPoint) =>
      cursor.point.slot === tip.slot &&
      cursor.point.blockHash === tip.blockHash &&
      cursor.point.network === tip.network;
    if (!atCursor(point)) throw awaitingScan();
    // The tip's height, bound to the cursor's point by a tip read on each
    // side; the tip's own optional height is never read.
    const tipBefore = await readers.currentPoint();
    const blockNo = await readers.tipBlockNo();
    const tipAfter = await readers.currentPoint();
    if (!atCursor(tipBefore) || !atCursor(tipAfter)) throw awaitingScan();
    return {
      pointId: `${point.slot}:${point.blockHash}`,
      slot: point.slot,
      blockHash: point.blockHash,
      blockNo,
    };
  };
  const assertActuationCurrent = async (): Promise<void> => {
    await input.assertSourceHealthy();
    await readBoundary();
  };
  const context: SDK.DaAvailabilityOperationContext = {
    ...input.context,
    assertActuationCurrent,
    observe: SDK.createDaAvailabilityOperationObserver({
      lucid: input.lucid,
      readBoundary,
      resolveInclusion: readers.resolveInclusion,
      resolveForeignSpend: (outRef) =>
        SDK.resolveDaAvailabilityForeignSpend({
          ...readers.foreignSpend,
          outRef,
          readBoundary,
        }),
    }),
  };
  const reconcile = async (): Promise<"ready" | "pending"> => {
    // A new tick may adopt a recovered generation only for reconciliation;
    // no new action is selected until every durable intent is checked.
    expectedRollbackGeneration = (await readers.currentCursor())
      .rollbackGeneration;
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
  };
  return { readBoundary, assertActuationCurrent, context, reconcile };
};

/**
 * The committee's readers for the SDK's verified foreign-spend check: the
 * same Kupo and Ogmios reads its state-queue replay trusts. A Kupo match set
 * with no entry for the ref is no evidence, and a transaction missing from
 * the block Kupo named fails verification; chain-moved and rollback errors
 * propagate so the next tick retries.
 */
export const availabilityForeignSpendReaders = (input: {
  readonly kupoUrl: string;
  readonly ogmiosUrl: string;
  readonly fetchImpl?: StateQueueReplayFetch;
  readonly webSocketFactory?: StateQueueReplayWebSocketFactory;
}): Omit<SDK.DaAvailabilityForeignSpendReaders, "readBoundary"> => {
  const fetchImpl = input.fetchImpl ?? fetch;
  const webSocketFactory =
    input.webSocketFactory ??
    ((url: string) =>
      new WebSocket(url) as unknown as StateQueueReplayWebSocket);
  return {
    fetchSpend: async (outRef) => {
      try {
        const spend = await fetchSpend(
          input.kupoUrl,
          `${outRef.txHash}#${outRef.outputIndex.toString()}`,
          fetchImpl,
        );
        return spend === null
          ? undefined
          : { transactionId: spend.transactionHash, point: spend.point };
      } catch (error) {
        if (error instanceof StateQueueHistoryNotExtendingAnchorError)
          return undefined;
        throw error;
      }
    },
    fetchAncestor: (slot) => fetchAncestor(input.kupoUrl, slot, fetchImpl),
    readTransaction: async ({ ancestor, point, txHash }) => {
      try {
        const transaction = await readTransaction(
          input.ogmiosUrl,
          ancestor,
          { transactionHash: txHash, point },
          webSocketFactory,
        );
        return {
          txHash: transaction.transactionHash,
          point: {
            slot: transaction.slot,
            blockHash: transaction.blockHash,
            blockNo: transaction.blockNo,
          },
          ...(transaction.cbor === undefined ? {} : { cbor: transaction.cbor }),
        };
      } catch (error) {
        if (error instanceof L1SourceIntegrityError) return undefined;
        throw error;
      }
    },
  };
};

/**
 * The deployment's `ParametersV1` from the manifest-pinned configuration: the
 * value compiled into the availability and DA attestation validators, shared
 * by the responder and by the attestation apply's pool check.
 */
export const availabilityParametersFromConfig = (
  config: Pick<CommitteeConfig, "availabilityChallenge">,
): SDK.DaAvailabilityParameters => {
  const p = config.availabilityChallenge;
  return SDK.daAvailabilityParameters({
    responseGeometry: SDK.availabilityResponseGeometry(p.responseGeometry),
    daBondLovelace: BigInt(p.daBondLovelace),
    daSlashPenaltyLovelace: BigInt(p.daSlashPenaltyLovelace),
    daBondMinTopUpLovelace: BigInt(p.daBondMinTopUpLovelace),
    daBondPoolFloorLovelace: BigInt(p.daBondPoolFloorLovelace),
    challengeRecordLovelace: BigInt(p.challengeRecordLovelace),
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

/**
 * Every live availability challenge, read from its challenge record
 * (`ChallengeRecordV1`). A record is the UTxO at the availability script that
 * holds a 32-byte DACH token under the availability policy; its datum is parsed
 * canonically against the deployment's parameters, and the SDK snapshot of its
 * header then authenticates the record against the `Challenged` state-queue
 * node, the terminal accumulator and every unsettled tranche. Ordered by
 * response deadline, then header hash.
 *
 * A record is judged on its own: one that cannot be answered is reported to
 * `onSkipped` and left out, and never stops discovery of the others. A record
 * whose state-queue node is gone, or is not `Challenged` by it, is stranded
 * (a timeout or fraud removal pruned its node while it was challenged; nothing
 * can spend it again), so there is nothing to answer.
 */
export const discoverAvailabilityResponderChallenges = async (
  lucid: LucidEvolution,
  deployment: SDK.DaAvailabilityDeployment,
  onSkipped: (skipped: AvailabilityResponderSkippedRecord) => void = () => {},
): Promise<readonly AvailabilityResponderChallenge[]> => {
  const [availabilityUtxos, stateQueueUtxos, correctionLockUtxos] =
    await Promise.all([
      lucid.utxosAt(
        deployment.contracts.availabilityChallenge.spendingScriptAddress,
      ),
      lucid.utxosAt(deployment.contracts.stateQueue.spendingScriptAddress),
      lucid.utxosAt(deployment.contracts.correctionLock.spendingScriptAddress),
    ]);
  const recordUnitPrefix =
    deployment.contracts.availabilityChallenge.policyId +
    SDK.DA_AVAILABILITY_CHALLENGE_ASSET_NAME_PREFIX;
  const result: AvailabilityResponderChallenge[] = [];
  for (const utxo of availabilityUtxos) {
    const recordUnit = Object.keys(utxo.assets).find(
      (unit) =>
        unit.length === recordUnitPrefix.length + DACH_SUFFIX_HEX_LENGTH &&
        unit.startsWith(recordUnitPrefix),
    );
    if (recordUnit === undefined) continue;
    const skip = (stranded: boolean, reason: string) =>
      onSkipped({
        outRef: `${utxo.txHash}#${utxo.outputIndex}`,
        stranded,
        reason,
      });
    try {
      if (typeof utxo.datum !== "string")
        throw new Error(
          "Authenticated availability challenge record has no inline datum",
        );
      const record = SDK.parseDaAvailabilityChallengeRecordCbor(
        utxo.datum,
        deployment.parameters,
      );
      const challengeAssetName = recordUnit.slice(
        deployment.contracts.availabilityChallenge.policyId.length,
      );
      if (record.challenge_asset_name !== challengeAssetName)
        throw new Error(
          "Availability challenge record datum names a different challenge than its token",
        );
      const snapshot = await SDK.daAvailabilityChallengeSnapshotFromUtxos(
        deployment,
        record.commitment.header_hash,
        {
          availabilityUtxos,
          stateQueueUtxos,
          correctionLockUtxos,
        },
      );
      if (
        !snapshot.queue ||
        !snapshot.record ||
        !snapshot.recordDatum ||
        snapshot.record.txHash !== utxo.txHash ||
        snapshot.record.outputIndex !== utxo.outputIndex ||
        snapshot.recordDatum.challenge_asset_name !== challengeAssetName
      ) {
        skip(
          true,
          "Availability challenge record is not the one its state-queue node is challenged by",
        );
        continue;
      }
      if (!snapshot.terminal || !snapshot.terminalDatum) {
        throw new Error(
          "Authenticated availability challenge has incomplete live state",
        );
      }
      result.push({
        record: { utxo: snapshot.record, datum: snapshot.recordDatum },
        terminal: { utxo: snapshot.terminal, datum: snapshot.terminalDatum },
        queue: snapshot.queue.utxo,
        tranches: snapshot.tranches,
      });
    } catch (error) {
      skip(false, error instanceof Error ? error.message : String(error));
    }
  }
  return result.sort((a, b) => {
    const left = a.record.datum,
      right = b.record.datum;
    return left.response_deadline === right.response_deadline
      ? left.commitment.header_hash.localeCompare(right.commitment.header_hash)
      : left.response_deadline < right.response_deadline
        ? -1
        : 1;
  });
};

/** A challenge record discovery left out, and why. */
export type AvailabilityResponderSkippedRecord = Readonly<{
  outRef: string;
  /** True when its state-queue node is gone or challenged by another record. */
  stranded: boolean;
  reason: string;
}>;

/** A DACH asset name is 32 bytes: the 4-byte prefix and a 28-byte identity. */
const DACH_SUFFIX_HEX_LENGTH = 56;

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
  const deadlineUpper = action.challenge.record.datum.response_deadline + 1n;
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
          record: action.challenge.record.utxo,
          terminal: action.challenge.terminal.utxo,
          thread: action.tranche.utxo,
          carrier: action.tranche.carrier,
        }),
      );
    case "close":
      return Effect.runPromise(
        SDK.buildCloseDaAvailabilityChallengeTxProgram(lucid, deployment, {
          ...resources,
          record: action.challenge.record.utxo,
          terminal: action.challenge.terminal.utxo,
          queue: action.challenge.queue,
        }),
      );
  }
};
