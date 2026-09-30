import { readFile, stat } from "node:fs/promises";

import * as SDK from "@al-ft/midgard-sdk";
import {
  calculateMinLovelaceFromUTxO,
  Data,
  type LucidEvolution,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  assertAvailabilityCommandRemovalCapital,
  assertAvailabilityTimeoutCollateral,
  type AvailabilityCommandBuildContext,
  type AvailabilityCommandOptions,
  availabilityTimeoutRentRefundAddress,
  liveOutRef,
  recoverAvailabilityOpenCommitment,
  required,
} from "./availability-challenge.plan-availability-command-action.js";

/**
 * Builds one availability action from the canonical snapshot: Open from the
 * queue node and the indexed commitment, Timeout from the challenge record,
 * the terminal accumulator and the DA bond pool.
 */
export const buildAvailabilityCommandTransaction = async (
  lucid: LucidEvolution,
  deployment: SDK.DaAvailabilityDeployment,
  context: AvailabilityCommandBuildContext,
  snapshot: SDK.DaAvailabilityChallengeSnapshot,
  action: SDK.DaAvailabilityTransactionAction,
  options: Pick<
    AvailabilityCommandOptions,
    | "headerHash"
    | "collateralOutRef"
    | "fundingOutRef"
    | "payloadFile"
    | "trancheIndex"
  >,
  actor: string,
  reservedOutRefs: ReadonlySet<string>,
): Promise<SDK.BuiltDaAvailabilityTransaction> => {
  const p = deployment.parameters;
  const feeLovelace =
    action === "open"
      ? p.max_open_fee_lovelace
      : action === "publish"
        ? p.max_publication_fee_lovelace
        : action === "settle"
          ? p.max_settlement_fee_lovelace
          : action === "close"
            ? p.max_close_fee_lovelace
            : p.max_timeout_fee_lovelace;
  const collateral = await liveOutRef(
    lucid,
    options.collateralOutRef,
    "--collateral-out-ref",
  );
  const protocol = required(
    lucid.config().protocolParameters,
    "live protocol parameters",
  );
  if (action === "timeout")
    assertAvailabilityTimeoutCollateral({
      parameters: p,
      collateralPercentage: protocol.collateralPercentage,
      collateral,
    });
  const liveQueue =
    action === "open" ||
    action === "timeout" ||
    action === "prune" ||
    action === "remove"
      ? await SDK.fetchSortedStateQueueUTxOs(lucid, {
          stateQueueAddress:
            deployment.contracts.stateQueue.spendingScriptAddress,
          stateQueuePolicyId: deployment.contracts.stateQueue.policyId,
        })
      : undefined;
  if (
    liveQueue !== undefined &&
    (action === "open" ||
      action === "timeout" ||
      action === "prune" ||
      action === "remove")
  ) {
    const queue = required(snapshot.queue, "the authenticated queue header");
    const targetIndex = liveQueue.findIndex(
      ({ utxo }) =>
        utxo.txHash === queue.utxo.txHash &&
        utxo.outputIndex === queue.utxo.outputIndex,
    );
    if (targetIndex < 1)
      throw new Error(
        "Availability capital check cannot find the current challenged queue header",
      );
    const walletAddress = await lucid.wallet().address();
    assertAvailabilityCommandRemovalCapital({
      action,
      parameters: p,
      remainingRemovalSteps: liveQueue.length - targetIndex,
      minimumChangeLovelace: calculateMinLovelaceFromUTxO(
        protocol.coinsPerUtxoByte,
        {
          address: walletAddress,
          assets: { lovelace: 2_000_000n },
          txHash: "00".repeat(32),
          outputIndex: 0,
        },
      ),
      walletAddress,
      walletUtxos: await lucid.wallet().getUtxos(),
      collateral,
      reservedOutRefs,
    });
  }
  const record = snapshot.recordDatum;
  const now = Date.now();
  const expiredSettlement =
    action === "settle" &&
    snapshot.tranches.some(
      ({ datum }) =>
        "Active" in datum &&
        datum.Active.descriptor.tranche_index ===
          snapshot.terminalDatum?.next_tranche_index,
    );
  if (expiredSettlement && record && BigInt(now) <= record.response_deadline)
    throw new Error(
      "Active availability tranches cannot settle before the strict response deadline",
    );
  const protocolLower =
    (action === "timeout" || expiredSettlement) && record
      ? record.response_deadline + 1n
      : 0n;
  const backedOff = BigInt(Math.max(0, now - 60_000));
  const validFrom = protocolLower > backedOff ? protocolLower : backedOff;
  let validTo = validFrom + 120_000n;
  if (action === "publish" && record && validTo > record.response_deadline + 1n)
    validTo = record.response_deadline + 1n;
  const queue = () =>
    required(snapshot.queue, "the authenticated queue header").utxo;
  const challengeRecord = () =>
    required(snapshot.record, "the authenticated challenge record");
  const terminal = () =>
    required(snapshot.terminal, "the terminal accumulator");
  if (action === "open") {
    const node = Data.castFrom(
      required(snapshot.queue, "the authenticated queue header").datum.data,
      SDK.StateQueueNode,
    );
    const status = node.da_attestation;
    if (typeof status !== "object" || !("Attested" in status))
      throw new Error("Availability open requires an Attested queue node");
    const { daChallengeWindowMs, kupoUrl } = context;
    // The validator requires the inclusive upper bound, validTo - 1, to fall
    // before the window's end.
    const windowEnd = node.header.endTime + daChallengeWindowMs;
    if (validTo > windowEnd) validTo = windowEnd;
    const commitment = await recoverAvailabilityOpenCommitment({
      kupoUrl,
      daAttestationPolicyId: required(
        context.daAttestationPolicyId,
        "the deployment's DA attestation policy",
      ),
      headerHash: options.headerHash,
      commitmentHash: status.Attested.commitment_hash,
    });
    return Effect.runPromise(
      SDK.buildOpenDaAvailabilityChallengeTxProgram(lucid, deployment, {
        collateralInputs: [collateral],
        feeLovelace,
        validFrom,
        validTo,
        commitment,
        queue: queue(),
        challengerFunding: await liveOutRef(
          lucid,
          options.fundingOutRef,
          "--funding-out-ref",
        ),
        challenger: actor,
        daChallengeWindowMs,
      }),
    );
  }
  const resources = {
    collateralInputs: [collateral],
    feeLovelace,
    validFrom,
    validTo,
  };
  if (action === "publish") {
    const active = required(record, "an opened challenge record");
    const path = required(
      options.payloadFile,
      "--payload-file with the exact retained envelope bytes",
    );
    if (
      BigInt((await stat(path)).size) !== active.commitment.payload_byte_length
    )
      throw new Error(
        "Availability payload file length differs from the frozen commitment",
      );
    const payload = await readFile(path);
    const plans = SDK.planDaAvailabilityPublications({
      commitment: active.commitment,
      payload,
      challengeAssetName: active.challenge_asset_name,
    });
    const tranche = required(
      snapshot.tranches.find(
        ({ datum }) =>
          "Active" in datum &&
          (options.trancheIndex === undefined ||
            datum.Active.descriptor.tranche_index ===
              BigInt(options.trancheIndex)),
      ),
      "an active requested tranche",
    );
    if (!("Active" in tranche.datum))
      throw new Error("Availability response tranche is already terminal");
    const thread = tranche.datum.Active;
    const publication = required(
      plans
        .find(
          (plan) =>
            plan.descriptor.tranche_index === thread.descriptor.tranche_index,
        )
        ?.publications.find((item) => item.chunk_offset === thread.next_offset),
      "the next committed chunk",
    );
    return Effect.runPromise(
      SDK.buildPublishDaAvailabilityChunkTxProgram(lucid, deployment, {
        ...resources,
        thread: tranche.utxo,
        previousCarrier: tranche.carrier,
        publication,
      }),
    );
  }
  if (action === "settle") {
    const tranche = required(
      snapshot.tranches.find(
        ({ datum }) =>
          ("Active" in datum ? datum.Active : datum.Receipt).descriptor
            .tranche_index === snapshot.terminalDatum?.next_tranche_index,
      ),
      "the next unsettled tranche",
    );
    return Effect.runPromise(
      SDK.buildSettleDaAvailabilityTrancheTxProgram(lucid, deployment, {
        ...resources,
        record: challengeRecord(),
        terminal: terminal(),
        thread: tranche.utxo,
        carrier: tranche.carrier,
      }),
    );
  }
  if (action === "close")
    return Effect.runPromise(
      SDK.buildCloseDaAvailabilityChallengeTxProgram(lucid, deployment, {
        ...resources,
        record: challengeRecord(),
        terminal: terminal(),
        queue: queue(),
      }),
    );
  const lock = snapshot.correctionLock.datum
    ? Data.from(snapshot.correctionLock.datum, SDK.CorrectionLockDatum)
    : undefined;
  const lockChallenge =
    typeof lock === "object" &&
    lock &&
    "Locked" in lock &&
    typeof lock.Locked.correction_identity === "object" &&
    "AvailabilityChallenge" in lock.Locked.correction_identity
      ? lock.Locked.correction_identity.AvailabilityChallenge
          .challenge_asset_name
      : undefined;
  const removal = {
    collateralInputs: resources.collateralInputs,
    validFrom,
    validTo,
    queue: queue(),
    confirmedState: snapshot.confirmedState.utxo,
    descendant: snapshot.descendant?.utxo,
    correctionLock: snapshot.correctionLock,
    challengeAssetName: required(
      record?.challenge_asset_name ?? lockChallenge,
      "the authenticated removal challenge identity",
    ),
    headerHash: options.headerHash,
  };
  if (action === "timeout")
    // Exact fee, paid by the pool's slash and the challenger reserve: no
    // wallet input and no change output. The wallet backs collateral only.
    return Effect.runPromise(
      SDK.buildTimeoutDaAvailabilityChallengeTxProgram(lucid, deployment, {
        ...removal,
        rentRefundAddress: availabilityTimeoutRentRefundAddress(
          required(lucid.config().network, "the Lucid network"),
          actor,
        ),
        fundingQueueTailRefInput: liveQueue?.at(-1)?.utxo,
        record: challengeRecord(),
        terminal: terminal(),
        pool: required(snapshot.pool, "the authenticated DA bond pool"),
      }),
    );
  const followUp = {
    ...removal,
    feeLovelace,
    rentRefundAddress: await lucid.wallet().address(),
    feeFunding: options.fundingOutRef
      ? await liveOutRef(lucid, options.fundingOutRef, "--funding-out-ref")
      : undefined,
  };
  if (action === "prune")
    return Effect.runPromise(
      SDK.buildPruneDaUnavailableBlockDescendantTxProgram(
        lucid,
        deployment,
        followUp,
      ),
    );
  return Effect.runPromise(
    SDK.buildRemoveDaUnavailableHeadTxProgram(lucid, deployment, followUp),
  );
};
