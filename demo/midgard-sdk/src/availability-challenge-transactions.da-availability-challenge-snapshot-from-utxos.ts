import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import * as Availability from "./availability-challenge.js";
import {
  auth,
  type DaAvailabilityChallengeSnapshot,
  type DaAvailabilityDeployment,
  datum,
  effect,
  fail,
  state,
  terminalDatum,
  trancheDatum,
} from "./availability-challenge-transactions.at.js";
import { authenticPool } from "./availability-challenge-transactions.build-close-da-availability-challenge-tx-program.js";
import { authenticateCarrier } from "./availability-challenge-transactions.build-open-da-availability-challenge-tx-program.js";
import {
  assertChallengedNode,
  challenged,
} from "./availability-challenge-transactions.build-publish-da-availability-chunk-tx-program.js";
import { type DaAvailabilitySnapshotUtxos } from "./availability-challenge-transactions.recover-da-availability-commitment-from-apply-tx.js";
import { CorrectionLockDatum, correctionLockUnit } from "./correction-lock.js";
import { daBondPoolUnit } from "./da-bond-pool.js";
import { StateQueueNode } from "./ledger-state.js";
import { STATE_QUEUE_NODE_ASSET_NAME_PREFIX } from "./linked-list.js";
import {
  STATE_QUEUE_ROOT_ASSET_NAME,
  type StateQueueUTxO,
} from "./state-queue.js";

export const daAvailabilityChallengeSnapshotFromUtxos = async (
  d: DaAvailabilityDeployment,
  headerHash: string,
  utxos: DaAvailabilitySnapshotUtxos,
): Promise<DaAvailabilityChallengeSnapshot> => {
  const policy = d.contracts.availabilityChallenge.policyId;
  const byUnit = (
    list: readonly UTxO[],
    unit: string,
    required = false,
  ): UTxO | undefined => {
    const matches = list.filter((u) => (u.assets[unit] ?? 0n) !== 0n);
    if (
      matches.length > 1 ||
      (required && matches.length !== 1) ||
      matches.some((u) => u.assets[unit] !== 1n)
    )
      fail(`Nonunique authenticated unit ${unit}`);
    return matches[0];
  };
  const queueU = byUnit(
    utxos.stateQueueUtxos,
    d.contracts.stateQueue.policyId +
      STATE_QUEUE_NODE_ASSET_NAME_PREFIX +
      headerHash,
  );
  const root = await state(
    byUnit(
      utxos.stateQueueUtxos,
      d.contracts.stateQueue.policyId + STATE_QUEUE_ROOT_ASSET_NAME,
      true,
    )!,
    d,
  );
  const queue = queueU ? await state(queueU, d) : undefined;
  const correctionLock = byUnit(
    utxos.correctionLockUtxos,
    correctionLockUnit(d.hubOraclePolicyId),
    true,
  )!;
  auth(correctionLock, d.contracts.correctionLock.spendingScriptAddress, [
    correctionLockUnit(d.hubOraclePolicyId),
  ]);
  Data.from(datum(correctionLock), CorrectionLockDatum);
  const pool =
    utxos.poolUtxos === undefined
      ? undefined
      : byUnit(
          utxos.poolUtxos,
          daBondPoolUnit(d.contracts.daBondPool.policyId),
        );
  const poolDatum = pool ? authenticPool(d, pool) : undefined;
  let descendant: StateQueueUTxO | undefined;
  if (queue && queue.datum.next !== "Empty") {
    const u = byUnit(
      utxos.stateQueueUtxos,
      d.contracts.stateQueue.policyId +
        STATE_QUEUE_NODE_ASSET_NAME_PREFIX +
        queue.datum.next.Key.key,
      true,
    )!;
    descendant = await state(u, d);
  }
  let record: UTxO | undefined,
    r: Availability.DaAvailabilityChallengeRecord | undefined,
    terminal: UTxO | undefined,
    td: Availability.DaAvailabilityTerminalAccumulatorDatum | undefined;
  const tranches: DaAvailabilityChallengeSnapshot["tranches"][number][] = [];
  if (queue) {
    const node = Data.castFrom(queue.datum.data, StateQueueNode),
      status = node.da_attestation;
    // Only a Challenged node has a record; an Attested one has nothing to read.
    if (typeof status === "object" && "Challenged" in status) {
      const name = status.Challenged.challenge_asset_name;
      record = byUnit(utxos.availabilityUtxos, policy + name);
      // A locked removal legitimately outlives the burned record.
      if (!record) {
        const lock = Data.from(datum(correctionLock), CorrectionLockDatum);
        if (
          typeof lock !== "object" ||
          lock.Locked.target_header_hash !== headerHash ||
          typeof lock.Locked.correction_identity !== "object" ||
          !("AvailabilityChallenge" in lock.Locked.correction_identity) ||
          lock.Locked.correction_identity.AvailabilityChallenge
            .challenge_asset_name !== name
        )
          fail("Authenticated challenge record is missing");
      } else {
        terminal = byUnit(
          utxos.availabilityUtxos,
          policy +
            Availability.daAvailabilityTerminalAccumulatorAssetName(name),
          true,
        )!;
        r = challenged(d, record, terminal);
        if (r.commitment.header_hash !== headerHash)
          fail("Challenge record deployment/header mismatch");
        assertChallengedNode(queue, r);
        td = terminalDatum(terminal);
        if (
          td.next_tranche_index >
          BigInt(r.commitment.tranche_descriptors.length)
        )
          fail("Terminal tranche cursor exceeds commitment");
        for (
          let i = Number(td.next_tranche_index);
          i < r.commitment.tranche_descriptors.length;
          i++
        ) {
          const u = byUnit(
            utxos.availabilityUtxos,
            policy +
              Availability.daAvailabilityTrancheAssetName({
                challengeAssetName: name,
                trancheIndex: i,
              }),
            true,
          )!;
          const t = trancheDatum(u),
            tv = "Active" in t ? t.Active : t.Receipt;
          if (
            tv.header_hash !== headerHash ||
            tv.deployment_identity !== d.hubOraclePolicyId ||
            tv.descriptor.tranche_index !== BigInt(i) ||
            tv.challenger !== r.challenger ||
            ("Active" in t &&
              t.Active.response_deadline !== r.response_deadline) ||
            Data.to(
              tv.descriptor,
              Availability.DaAvailabilityTrancheDescriptor,
            ) !==
              Data.to(
                r.commitment.tranche_descriptors[i]!,
                Availability.DaAvailabilityTrancheDescriptor,
              )
          )
            fail("Tranche commitment mismatch");
          const index =
            "Active" in t
              ? t.Active.latest_carrier_output_index
              : t.Receipt.terminal_carrier_output_index;
          const carrier =
            index === null
              ? undefined
              : [
                  ...utxos.availabilityUtxos,
                  ...(utxos.carrierUtxos ?? []),
                ].find(
                  (c) =>
                    c.txHash === u.txHash && BigInt(c.outputIndex) === index,
                );
          authenticateCarrier(d, u, t, carrier);
          tranches.push({
            utxo: u,
            datum: t,
            ...(carrier ? { carrier } : {}),
          });
        }
      }
    }
  }
  return {
    headerHash,
    confirmedState: root,
    correctionLock,
    tranches,
    ...(queue ? { queue } : {}),
    ...(descendant ? { descendant } : {}),
    ...(record ? { record } : {}),
    ...(r ? { recordDatum: r } : {}),
    ...(pool ? { pool } : {}),
    ...(poolDatum ? { poolDatum } : {}),
    ...(terminal ? { terminal } : {}),
    ...(td ? { terminalDatum: td } : {}),
  };
};

export const fetchDaAvailabilityChallengeSnapshot = async (
  lucid: Pick<LucidEvolution, "utxosAt">,
  d: DaAvailabilityDeployment,
  headerHash: string,
): Promise<DaAvailabilityChallengeSnapshot> => {
  const [availabilityUtxos, stateQueueUtxos, correctionLockUtxos, poolUtxos] =
    await Promise.all([
      lucid.utxosAt(d.contracts.availabilityChallenge.spendingScriptAddress),
      lucid.utxosAt(d.contracts.stateQueue.spendingScriptAddress),
      lucid.utxosAt(d.contracts.correctionLock.spendingScriptAddress),
      lucid.utxosAt(d.contracts.daBondPool.spendingScriptAddress),
    ]);
  return daAvailabilityChallengeSnapshotFromUtxos(d, headerHash, {
    availabilityUtxos,
    stateQueueUtxos,
    correctionLockUtxos,
    poolUtxos,
  });
};

export const fetchDaAvailabilityChallengeSnapshotProgram = (
  lucid: Pick<LucidEvolution, "utxosAt">,
  d: DaAvailabilityDeployment,
  headerHash: string,
) => effect(() => fetchDaAvailabilityChallengeSnapshot(lucid, d, headerHash));
