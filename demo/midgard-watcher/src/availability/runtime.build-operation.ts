import type { openAvailabilityOperationJournal } from "@al-ft/midgard-core/availability-operation-journal";
import * as SDK from "@al-ft/midgard-sdk";
import {
  Data,
  type LucidEvolution,
  type TxSignBuilder,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type { WatcherAuthenticatedStateQueueObservation } from "../indexers/authenticated-state-queue-observation.js";
import type { WatcherProcessConfig } from "../runtime/process-config.js";
import type { WatcherAvailabilityAction } from "./action.js";
import { minimumWatcherAvailabilityChange } from "./runtime.protocol-parameter-refresh.js";
import {
  DA_CHALLENGE_WINDOW_MS,
  required,
  WatcherAvailabilityCapitalShortfall,
  WatcherAvailabilityTimeoutPoolUnavailable,
} from "./runtime.release-watcher-availability-workflows.js";
import {
  selectWatcherAvailabilityFunding,
  watcherAvailabilityTimeoutCollateralLovelace,
  watcherAvailabilityValidity,
} from "./runtime.select-watcher-availability-funding.js";

export const buildWatcherAvailabilityOperation = async (
  snapshot: SDK.DaAvailabilityChallengeSnapshot,
  action: WatcherAvailabilityAction,
  observation: WatcherAuthenticatedStateQueueObservation,
  commitment: SDK.DaAvailabilityCommitment | undefined,
  buildLucid: LucidEvolution,
  context: {
    deployment: SDK.DaAvailabilityDeployment;
    walletAddress: string;
    actor: string;
    config: WatcherProcessConfig;
    journal: ReturnType<typeof openAvailabilityOperationJournal>;
    proverWalletAddress: string;
  },
  scope?: SDK.DaAvailabilityReadScope,
): Promise<{
  action: string;
  completesWorkflow?: boolean;
  build(): Promise<TxSignBuilder | SDK.DaAvailabilityOperationBuild>;
}> => {
  const {
    deployment,
    walletAddress,
    actor,
    config,
    journal,
    proverWalletAddress,
  } = context;
  const parameters = deployment.parameters;
  const openingLovelace =
    parameters.challenger_bond_lovelace +
    parameters.challenge_record_lovelace +
    parameters.max_open_fee_lovelace;
  const feeLovelace =
    action.action === "open"
      ? parameters.max_open_fee_lovelace
      : action.action === "settle"
        ? parameters.max_settlement_fee_lovelace
        : action.action === "close"
          ? parameters.max_close_fee_lovelace
          : parameters.max_timeout_fee_lovelace;
  const protocol = required(
    buildLucid.config().protocolParameters,
    "live protocol parameters",
  );
  const liveQueue =
    action.action === "timeout"
      ? await Effect.runPromise(
          SDK.fetchSortedStateQueueUTxOsProgram(buildLucid, {
            stateQueueAddress:
              deployment.contracts.stateQueue.spendingScriptAddress,
            stateQueuePolicyId: deployment.contracts.stateQueue.policyId,
          }),
        )
      : undefined;
  // Anyone may TopUp the one shared pool and every Timeout spends it, so a
  // Timeout reads it at the tip rather than from the finalized snapshot,
  // which lags the tip. Without an authentic pool there the Timeout is
  // skipped this reconciliation, never built against the snapshot's pool.
  let livePool: Awaited<ReturnType<typeof SDK.fetchDaBondPool>> | undefined;
  if (action.action === "timeout") {
    try {
      livePool = await SDK.fetchDaBondPool(buildLucid, {
        policyId: deployment.contracts.daBondPool.policyId,
        address: deployment.contracts.daBondPool.spendingScriptAddress,
      });
    } catch (cause) {
      throw new WatcherAvailabilityTimeoutPoolUnavailable(
        cause instanceof Error ? cause.message : String(cause),
      );
    }
  }
  const minChange = minimumWatcherAvailabilityChange(protocol, walletAddress);
  const removalReserve =
    BigInt(liveQueue?.length ?? observation.finalizedHeaders.length + 1) *
      parameters.max_timeout_fee_lovelace +
    minChange;
  const configured = BigInt(config.availability.minimumFundingLovelace);
  const requiredWorking =
    action.action === "open"
      ? openingLovelace + removalReserve + parameters.max_open_fee_lovelace
      : action.action === "timeout" ||
          action.action === "prune" ||
          action.action === "remove"
        ? removalReserve
        : 0n;
  // G9: an Open is only worth taking when its Timeout stays reachable, and
  // the Timeout's fee includes the slashed penalty.
  const collateralRequired =
    action.action === "open" || action.action === "timeout"
      ? watcherAvailabilityTimeoutCollateralLovelace({
          parameters,
          collateralPercentage: protocol.collateralPercentage,
          minimumReturnLovelace: minChange,
        })
      : (feeLovelace * BigInt(protocol.collateralPercentage) + 99n) / 100n +
        minChange;
  // An Open (and its preparation) is taken only while the wallet also holds
  // the queue-bounded removal reserve and one Timeout collateral set, once
  // per wallet: removals are serialized by the correction lock and every
  // Timeout removes the head and prunes its descendants, so all live
  // challenges share one removal path. Bonds already opened sit on chain.
  const funds = selectWatcherAvailabilityFunding({
    utxos: await buildLucid.wallet().getUtxos(),
    reservedOutRefs: new Set(journal.reservedOutRefs(actor)),
    collateralLovelace: collateralRequired,
    openingLovelace,
    requiredWorkingLovelace:
      action.action === "open" && configured > requiredWorking
        ? configured
        : requiredWorking,
  });
  if (action.action === "open" && funds.exactOpening === undefined) {
    const preparing =
      openingLovelace + parameters.max_open_fee_lovelace + minChange;
    if (funds.funding.assets.lovelace < preparing) {
      throw new WatcherAvailabilityCapitalShortfall(
        "Availability funding needs one input large enough to prepare the exact challenger bond",
        preparing,
        funds.funding.assets.lovelace,
      );
    }
    return {
      action: "prepare",
      build: () =>
        SDK.buildDaAvailabilityFundingPreparationTx(
          buildLucid,
          {
            fundingInput: funds.funding,
            outputLovelace: openingLovelace,
            feeLovelace: parameters.max_open_fee_lovelace,
            ...watcherAvailabilityValidity(),
          },
          scope,
        ),
    };
  }
  return {
    action: action.action,
    ...(action.action === "timeout"
      ? { completesWorkflow: snapshot.descendant === undefined }
      : {}),
    build: async () => {
      const resources = {
        collateralInputs: funds.collateral,
        feeLovelace,
        ...watcherAvailabilityValidity(),
      };
      if (action.action === "open") {
        // The Open's inclusive upper bound must stay before the header's
        // end_time + da_challenge_window_ms.
        const node = Data.castFrom(
          required(snapshot.queue, "queue").datum.data,
          SDK.StateQueueNode,
        );
        const deadline = node.header.endTime + DA_CHALLENGE_WINDOW_MS;
        return (
          await Effect.runPromise(
            SDK.buildOpenDaAvailabilityChallengeTxProgram(
              buildLucid,
              deployment,
              {
                ...resources,
                validTo:
                  resources.validTo < deadline
                    ? resources.validTo
                    : deadline - 1n,
                commitment: required(commitment, "attested commitment"),
                queue: required(snapshot.queue, "queue").utxo,
                challengerFunding: required(
                  funds.exactOpening,
                  "exact challenger funding",
                ),
                challenger: actor,
                daChallengeWindowMs: DA_CHALLENGE_WINDOW_MS,
              },
            ),
          )
        ).tx;
      }
      if (action.action === "settle") {
        const tranche = required(action.tranche, "next tranche");
        return (
          await Effect.runPromise(
            SDK.buildSettleDaAvailabilityTrancheTxProgram(
              buildLucid,
              deployment,
              {
                ...resources,
                record: required(snapshot.record, "challenge record"),
                terminal: required(snapshot.terminal, "terminal accumulator"),
                thread: tranche.utxo,
                ...(tranche.carrier === undefined
                  ? {}
                  : { carrier: tranche.carrier }),
              },
            ),
          )
        ).tx;
      }
      if (action.action === "close")
        return (
          await Effect.runPromise(
            SDK.buildCloseDaAvailabilityChallengeTxProgram(
              buildLucid,
              deployment,
              {
                ...resources,
                record: required(snapshot.record, "challenge record"),
                terminal: required(snapshot.terminal, "terminal accumulator"),
                queue: required(snapshot.queue, "queue").utxo,
              },
            ),
          )
        ).tx;
      const removalTarget = {
        collateralInputs: resources.collateralInputs,
        validFrom: resources.validFrom,
        validTo: resources.validTo,
        queue: required(snapshot.queue, "queue").utxo,
        confirmedState: snapshot.confirmedState.utxo,
        correctionLock: snapshot.correctionLock,
        ...(snapshot.descendant === undefined
          ? {}
          : { descendant: snapshot.descendant.utxo }),
        challengeAssetName: required(
          action.challengeAssetName,
          "challenge identity",
        ),
        headerHash: snapshot.headerHash,
        rentRefundAddress: proverWalletAddress,
      };
      if (action.action === "timeout") {
        // The record, terminal and pool pay the exact fee
        // min(penalty, taken) + c, so no wallet coin funds it (E2). The
        // pool is the one read at the tip for this step.
        const built = await Effect.runPromise(
          SDK.buildTimeoutDaAvailabilityChallengeTxProgram(
            buildLucid,
            deployment,
            {
              ...removalTarget,
              fundingQueueTailRefInput: required(
                liveQueue?.at(-1),
                "live funding queue tail",
              ).utxo,
              record: required(snapshot.record, "challenge record"),
              terminal: required(snapshot.terminal, "terminal accumulator"),
              pool: required(livePool, "tip DA bond pool").utxo,
            },
          ),
        );
        return {
          tx: built.tx,
          timeoutFeePartLovelace: required(
            built.timeoutFeePartLovelace,
            "timeout fee part",
          ),
        };
      }
      const removal = {
        ...removalTarget,
        feeLovelace,
        feeFunding: funds.funding,
      };
      return (
        await Effect.runPromise(
          action.action === "prune"
            ? SDK.buildPruneDaUnavailableBlockDescendantTxProgram(
                buildLucid,
                deployment,
                removal,
              )
            : SDK.buildRemoveDaUnavailableHeadTxProgram(
                buildLucid,
                deployment,
                removal,
              ),
        )
      ).tx;
    },
  };
};
