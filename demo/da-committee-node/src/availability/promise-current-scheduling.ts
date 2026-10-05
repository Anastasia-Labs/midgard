import { canonicalJson } from "@al-ft/midgard-core/canonical-json";
import { computeDaSha256Hash } from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type { StateQueueHeaderRecord } from "../domain.js";
import { hashBlockHeader } from "../l1/state-queue-scanner.js";
import type { PromiseCapacityPoint } from "./promise-capacity-evidence.js";
import type { PromiseCanonicalPointReader } from "./promise-cutoff-source.js";

export type PromiseSchedulingLiability = Readonly<{
  header: StateQueueHeaderRecord;
  commitment: SDK.DaAvailabilityCommitment;
  commitmentDigest: string;
}>;

export const promiseCurrentSchedulingDigest = (
  input: Readonly<{
    boundary: PromiseCapacityPoint;
    rollbackGeneration: number;
    current: ReadonlySet<string>;
    liabilities: readonly PromiseSchedulingLiability[];
    rawSnapshot: SDK.DaAvailabilitySnapshotUtxos;
  }>,
): string => {
  const rows = (utxos: readonly UTxO[]) =>
    utxos
      .map((row) => ({
        txHash: row.txHash,
        outputIndex: row.outputIndex,
        address: row.address,
        assets: Object.fromEntries(
          Object.entries(row.assets).map(([unit, amount]) => [
            unit,
            amount.toString(),
          ]),
        ),
        datum: row.datum ?? null,
        datumHash: row.datumHash ?? null,
        scriptRef: row.scriptRef ?? null,
      }))
      .sort((a, b) =>
        `${a.txHash}#${a.outputIndex}`.localeCompare(
          `${b.txHash}#${b.outputIndex}`,
          "en",
        ),
      );
  const value = {
    boundary: input.boundary,
    rollbackGeneration: input.rollbackGeneration,
    current: [...input.current].sort(),
    liabilities: input.liabilities
      .map((row) => ({
        headerHash: row.header.headerHash,
        commitmentDigest: row.commitmentDigest,
        checkpoint: {
          slot: row.header.observedChainPoint.slot ?? null,
          blockNo: row.header.observedChainPoint.blockHeight ?? null,
          blockHash: row.header.observedChainPoint.blockHash ?? null,
        },
        endTime: row.header.header.endTime.toString(),
      }))
      .sort((a, b) =>
        a.commitmentDigest.localeCompare(b.commitmentDigest, "en"),
      ),
    availability: rows(input.rawSnapshot.availabilityUtxos),
    stateQueue: rows(input.rawSnapshot.stateQueueUtxos),
    correctionLock: rows(input.rawSnapshot.correctionLockUtxos),
  };
  return computeDaSha256Hash(
    Buffer.from(canonicalJson(value, "current promise scheduling certificate")),
  ).toString("hex");
};

/** A fresh scheduling certificate never retires bytes, capital or the full
 * protected restoration allocation. Each later admission re-proves this view. */
export const currentPromiseScheduling = async (
  args: Readonly<{
    deployment: SDK.DaAvailabilityDeployment;
    boundary: PromiseCapacityPoint;
    canonicalTimeMs: number;
    openWindowMs: number;
    rawSnapshot: SDK.DaAvailabilitySnapshotUtxos;
    complete: boolean;
    liabilities: readonly PromiseSchedulingLiability[];
    readCanonicalPoint: PromiseCanonicalPointReader;
    assertCurrent: () => Promise<void>;
  }>,
): Promise<ReadonlySet<string>> => {
  if (
    !args.complete ||
    !Number.isSafeInteger(args.canonicalTimeMs) ||
    args.canonicalTimeMs < 0 ||
    !Number.isSafeInteger(args.openWindowMs) ||
    args.openWindowMs <= 0
  )
    throw new Error("Complete current scheduling source is unavailable");
  // Authentication is still required when there are no existing signatures.
  const rootSnapshot = await SDK.daAvailabilityChallengeSnapshotFromUtxos(
    args.deployment,
    SDK.makeGenesisConfirmedState(0n).headerHash,
    args.rawSnapshot,
  );
  await Effect.runPromise(
    SDK.getConfirmedStateFromStateQueueDatum(rootSnapshot.confirmedState.datum),
  );
  if (
    rootSnapshot.correctionLock.datum === undefined ||
    rootSnapshot.correctionLock.datum === null ||
    Data.from(rootSnapshot.correctionLock.datum, SDK.CorrectionLockDatum) !==
      "Idle"
  )
    throw new Error(
      "Locked correction prevents a complete scheduling certificate",
    );
  const current = new Set<string>();
  for (const liability of args.liabilities) {
    const { header, commitment, commitmentDigest } = liability;
    const digest = computeDaSha256Hash(
      Buffer.from(SDK.encodeDaAvailabilityCommitment(commitment), "hex"),
    ).toString("hex");
    if (
      digest !== commitmentDigest ||
      commitment.header_hash !== header.headerHash ||
      hashBlockHeader(header.header) !== header.headerHash ||
      commitment.deployment_identity !== args.deployment.hubOraclePolicyId
    )
      throw new Error(
        "Scheduling certificate changed its signed header or commitment",
      );
    const snapshot = await SDK.daAvailabilityChallengeSnapshotFromUtxos(
      args.deployment,
      header.headerHash,
      args.rawSnapshot,
    );
    // The SDK authenticates the root NFT/address; also decode its root domain.
    await Effect.runPromise(
      SDK.getConfirmedStateFromStateQueueDatum(snapshot.confirmedState.datum),
    );
    if (
      snapshot.correctionLock.datum === undefined ||
      snapshot.correctionLock.datum === null ||
      Data.from(snapshot.correctionLock.datum, SDK.CorrectionLockDatum) !==
        "Idle"
    )
      throw new Error(
        "Locked correction prevents a complete scheduling certificate",
      );
    const node =
      snapshot.queue === undefined
        ? undefined
        : Data.castFrom(snapshot.queue.datum.data, SDK.StateQueueNode);
    if (
      snapshot.queue !== undefined &&
      (snapshot.queue.datum.key === "Empty" ||
        snapshot.queue.datum.key.Key.key !== header.headerHash ||
        node === undefined ||
        hashBlockHeader(node.header) !== header.headerHash)
    )
      throw new Error(
        "Scheduling state-queue identity differs from signed header",
      );
    if (
      node &&
      typeof node.da_attestation === "object" &&
      "Attested" in node.da_attestation &&
      node.da_attestation.Attested.commitment_hash !==
        SDK.daAvailabilityCommitmentHash(commitment)
    )
      throw new Error("Scheduling node attests another commitment");
    const cutoff = header.header.endTime + BigInt(args.openWindowMs);
    if (cutoff < 0n || cutoff > BigInt(Number.MAX_SAFE_INTEGER))
      throw new Error("Scheduling cutoff is out of range");
    if (
      BigInt(args.canonicalTimeMs) < cutoff ||
      snapshot.record !== undefined ||
      (node &&
        typeof node.da_attestation === "object" &&
        "Challenged" in node.da_attestation)
    ) {
      current.add(commitmentDigest);
      continue;
    }
    const observed = header.observedChainPoint;
    if (
      !Number.isSafeInteger(observed.slot) ||
      observed.slot! < 0 ||
      !Number.isSafeInteger(observed.blockHeight) ||
      observed.blockHeight! < 0 ||
      typeof observed.blockHash !== "string" ||
      !/^[0-9a-f]{64}$/u.test(observed.blockHash)
    )
      throw new Error("Scheduling original checkpoint is unavailable");
    const point = {
      slot: observed.slot!,
      blockHash: observed.blockHash,
      blockNo: observed.blockHeight!,
    };
    const proof = await args.readCanonicalPoint(point);
    if (
      proof === null ||
      proof.point.slot !== point.slot ||
      proof.point.blockHash !== point.blockHash ||
      proof.point.blockNo !== point.blockNo ||
      proof.tip.slot !== args.boundary.slot ||
      proof.tip.blockHash !== args.boundary.blockHash ||
      proof.tip.blockNo !== args.boundary.blockNo
    )
      throw new Error(
        "Scheduling checkpoint is not canonical at this exact boundary",
      );
    await args.assertCurrent();
  }
  await args.assertCurrent();
  return current;
};
