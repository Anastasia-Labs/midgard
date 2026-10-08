import { createHash } from "node:crypto";
import type { DatabaseSync } from "node:sqlite";

import {
  requireRetentionDays,
  retentionDeadlineForBlock,
} from "@al-ft/midgard-core";
import { Header } from "@al-ft/midgard-sdk";
import {
  Data,
  SLOT_CONFIG_NETWORK,
  type SlotConfig,
} from "@lucid-evolution/lucid";

import {
  assertWatcherStateQueueHeaderObservation,
  assertWatcherStateQueueObservation,
  type WatcherAuthenticatedStateQueueObservation,
  type WatcherStateQueueHeaderObservation,
} from "../indexers/authenticated-state-queue-observation.js";
import { WATCHER_CARDANO_SECURITY_PARAMETER_K } from "../runtime/config.js";
import {
  assertVerifiedWatcherDeploymentIdentity,
  type VerifiedWatcherDeploymentIdentity,
} from "../runtime/deployment-identity.js";
import { createWatcherReplayTranscriptAbsence } from "./replay-transcript-absence.js";
import { createWatcherReplayTranscriptOperationDoors } from "./replay-transcript-operation-doors.js";
import type { WatcherReplayTranscriptIdentity } from "./replay-transcript-store.js";
import {
  assertWatcherCanonicalRetentionWindow,
  type WatcherCanonicalRetentionWindow,
} from "./retention-window.js";

export type WatcherReplayTranscriptLifecycle = Readonly<{
  header: WatcherStateQueueHeaderObservation;
  operationDigest: string;
  retentionWindow: WatcherCanonicalRetentionWindow;
  deploymentIdentity: VerifiedWatcherDeploymentIdentity;
}>;
export type WatcherReplayTranscriptRetirementInput = Readonly<{
  observation: WatcherAuthenticatedStateQueueObservation;
  network: "Mainnet" | "Preprod" | "Preview" | "Custom";
  customSlotConfig?: Readonly<SlotConfig>;
}>;

const hash = (fields: readonly unknown[]) =>
  createHash("sha256").update(JSON.stringify(fields)).digest("hex");
const natural = /^(?:0|[1-9][0-9]{0,19})$/u;
export type WatcherReplayTranscriptLifecyclePin = Readonly<{
  operation_digest: string;
  end_time: string;
  retention_days: number;
  network: string;
  completed_slot: string | null;
  checksum: string;
  classification_complete: number;
  proof_started: number;
}>;

/** Lifecycle rows live in the same transaction as their entire version chain. */
export const createWatcherReplayTranscriptLifecycle = (input: {
  readonly database: DatabaseSync;
  readonly identityKey: (identity: WatcherReplayTranscriptIdentity) => string;
  readonly audit: (key: string) => unknown;
  readonly transaction: <T>(
    mode: "BEGIN" | "BEGIN IMMEDIATE",
    action: () => T,
  ) => T;
  readonly maximumPins: number;
}) => {
  const { database, identityKey, audit, transaction } = input;
  const absence = createWatcherReplayTranscriptAbsence(database);
  database.exec(`CREATE TABLE IF NOT EXISTS watcher_replay_transcript_operation (
    identity TEXT NOT NULL,
    operation_digest TEXT NOT NULL CHECK(length(operation_digest) = 64),
    end_time TEXT NOT NULL,
    retention_days INTEGER NOT NULL,
    network TEXT NOT NULL,
    completed_slot TEXT,
    checksum TEXT NOT NULL CHECK(length(checksum) = 64),
    classification_complete INTEGER NOT NULL CHECK(classification_complete IN (0, 1)),
    proof_started INTEGER NOT NULL CHECK(proof_started IN (0, 1)),
    PRIMARY KEY(identity, operation_digest)
  ) STRICT;
  CREATE TABLE IF NOT EXISTS watcher_replay_transcript_lifecycle (
    identity TEXT PRIMARY KEY,
    pin_count INTEGER NOT NULL CHECK(pin_count BETWEEN 1 AND 4096),
    unresolved_legacy INTEGER NOT NULL CHECK(unresolved_legacy IN (0, 1)),
    pins_digest TEXT NOT NULL CHECK(length(pins_digest) = 64)
  ) STRICT;`);
  const rows = database.prepare(
    "SELECT operation_digest, end_time, retention_days, network, completed_slot, checksum, classification_complete, proof_started FROM watcher_replay_transcript_operation WHERE identity = ? ORDER BY operation_digest",
  );
  const summary = database.prepare(
    "SELECT pin_count, unresolved_legacy, pins_digest FROM watcher_replay_transcript_lifecycle WHERE identity = ?",
  );
  const digestPins = (pins: readonly WatcherReplayTranscriptLifecyclePin[]) =>
    hash(pins.map((pin) => [pin.operation_digest, pin.checksum]));
  const writeSummary = (key: string, unresolvedLegacy = false) => {
    const pins = rows.all(key) as WatcherReplayTranscriptLifecyclePin[];
    const prior = summary.get(key) as { unresolved_legacy: number } | undefined;
    const legacy = unresolvedLegacy || prior?.unresolved_legacy === 1 ? 1 : 0;
    database
      .prepare(
        "INSERT INTO watcher_replay_transcript_lifecycle VALUES (?, ?, ?, ?) ON CONFLICT(identity) DO UPDATE SET pin_count = excluded.pin_count, unresolved_legacy = excluded.unresolved_legacy, pins_digest = excluded.pins_digest",
      )
      .run(key, pins.length, legacy, hash([key, legacy, digestPins(pins)]));
  };
  const check = (key: string) => {
    const pins = rows.all(key) as WatcherReplayTranscriptLifecyclePin[];
    const state = summary.get(key) as
      | { pin_count: number; unresolved_legacy: number; pins_digest: string }
      | undefined;
    if (
      (state === undefined && pins.length !== 0) ||
      (state !== undefined &&
        (![0, 1].includes(state.unresolved_legacy) ||
          state.pin_count !== pins.length ||
          state.pins_digest !==
            hash([key, state.unresolved_legacy, digestPins(pins)])))
    ) {
      throw new Error("replay transcript lifecycle is incomplete or corrupt");
    }
    if (pins.length > input.maximumPins)
      throw new Error("replay transcript lifecycle exceeds pin limit");
    for (const pin of pins) {
      if (
        !/^[0-9a-f]{64}$/u.test(pin.operation_digest) ||
        !natural.test(pin.end_time) ||
        (pin.completed_slot !== null && !natural.test(pin.completed_slot)) ||
        pin.retention_days !==
          requireRetentionDays(
            pin.retention_days,
            "replay transcript retention days",
          ) ||
        !["Mainnet", "Preprod", "Preview", "Custom"].includes(pin.network) ||
        ![0, 1].includes(pin.classification_complete) ||
        ![0, 1].includes(pin.proof_started) ||
        pin.checksum !==
          hash([
            key,
            pin.operation_digest,
            pin.end_time,
            pin.retention_days,
            pin.network,
            pin.completed_slot,
            pin.classification_complete,
            pin.proof_started,
          ])
      ) {
        throw new Error("replay transcript lifecycle is corrupt");
      }
    }
    return pins;
  };
  const operationDoors = createWatcherReplayTranscriptOperationDoors({
    mutate: (operationDigest, change) =>
      transaction("BEGIN IMMEDIATE", () => {
        const candidates = database
          .prepare(
            "SELECT DISTINCT identity FROM watcher_replay_transcript_operation WHERE operation_digest = ?",
          )
          .all(operationDigest) as { identity: string }[];
        for (const { identity: key } of candidates) {
          const current = audit(key) as { headTranscriptDigest: string } | null;
          if (current === null)
            throw new Error("replay transcript operation has no archive");
          const pin = check(key).find(
            (entry) => entry.operation_digest === operationDigest,
          )!;
          const next = change(
            JSON.parse(key) as string[],
            pin,
            current.headTranscriptDigest,
          );
          database
            .prepare(
              "UPDATE watcher_replay_transcript_operation SET completed_slot = ?, classification_complete = ?, proof_started = ?, checksum = ? WHERE identity = ? AND operation_digest = ?",
            )
            .run(
              next.completed_slot,
              next.classification_complete,
              next.proof_started,
              hash([
                key,
                next.operation_digest,
                next.end_time,
                next.retention_days,
                next.network,
                next.completed_slot,
                next.classification_complete,
                next.proof_started,
              ]),
              key,
              operationDigest,
            );
          writeSummary(key);
        }
      }),
  });
  return Object.freeze({
    register: (
      identity: WatcherReplayTranscriptIdentity,
      lifecycle: WatcherReplayTranscriptLifecycle | undefined,
      existingChain: boolean,
    ) => {
      if (lifecycle === undefined) {
        if (check(identityKey(identity)).length !== 0)
          throw new Error("replay transcript append requires a lifecycle pin");
        return;
      }
      assertWatcherStateQueueHeaderObservation(lifecycle.header);
      assertVerifiedWatcherDeploymentIdentity(lifecycle.deploymentIdentity);
      const header = lifecycle.header;
      const window = assertWatcherCanonicalRetentionWindow(
        lifecycle.retentionWindow,
      );
      if (
        window.manifestId !== identity.deploymentFingerprint ||
        lifecycle.deploymentIdentity.manifestId !==
          identity.deploymentFingerprint
      )
        throw new Error("replay transcript retention deployment differs");
      const key = identityKey(identity);
      const pins = check(key);
      const unresolvedLegacy = existingChain && pins.length === 0;
      if (
        header.headerHash !== identity.headerHash ||
        header.observedSlot !== identity.inclusionPoint.slot ||
        header.observedBlockNo !== identity.inclusionPoint.blockNo ||
        header.observedBlockHash !== identity.inclusionPoint.blockHash ||
        header.observedTransactionHash !==
          identity.inclusionPoint.transactionHash ||
        header.observedChainPointId !== identity.inclusionPoint.chainPointId ||
        !/^[0-9a-f]{64}$/u.test(lifecycle.operationDigest)
      )
        throw new Error("replay transcript lifecycle identity differs");
      const decoded = Data.from(header.headerCborHex, Header);
      const endTime = decoded.endTime.toString();
      if (
        Data.to(decoded, Header) !== header.headerCborHex ||
        !natural.test(endTime)
      )
        throw new Error("replay transcript lifecycle header is invalid");
      const prior = pins.find(
        (pin) => pin.operation_digest === lifecycle.operationDigest,
      );
      if (prior !== undefined) {
        if (
          prior.completed_slot !== null ||
          prior.end_time !== endTime ||
          prior.retention_days !== window.retentionDays ||
          prior.network !== lifecycle.deploymentIdentity.network
        )
          throw new Error(
            "replay transcript completed operation cannot reopen",
          );
        database
          .prepare(
            "UPDATE watcher_replay_transcript_operation SET classification_complete = 0, checksum = ? WHERE identity = ? AND operation_digest = ?",
          )
          .run(
            hash([
              key,
              prior.operation_digest,
              prior.end_time,
              prior.retention_days,
              prior.network,
              prior.completed_slot,
              0,
              prior.proof_started,
            ]),
            key,
            prior.operation_digest,
          );
        writeSummary(key);
        return;
      }
      if (pins.length >= input.maximumPins)
        throw new Error("replay transcript lifecycle exceeds pin limit");
      database
        .prepare(
          "INSERT INTO watcher_replay_transcript_operation VALUES (?, ?, ?, ?, ?, NULL, ?, 0, 0)",
        )
        .run(
          key,
          lifecycle.operationDigest,
          endTime,
          window.retentionDays,
          lifecycle.deploymentIdentity.network,
          hash([
            key,
            lifecycle.operationDigest,
            endTime,
            window.retentionDays,
            lifecycle.deploymentIdentity.network,
            null,
            0,
            0,
          ]),
        );
      writeSummary(key, unresolvedLegacy);
    },
    ...operationDoors,
    resetRetirementWitnesses: async (): Promise<void> =>
      transaction("BEGIN IMMEDIATE", () => absence.clear()),
    retireExpired: async (
      request: WatcherReplayTranscriptRetirementInput,
    ): Promise<number> => {
      assertWatcherStateQueueObservation(request.observation);
      const observation = request.observation;
      const clock =
        request.network === "Custom"
          ? request.customSlotConfig
          : SLOT_CONFIG_NETWORK[request.network];
      if (
        clock === undefined ||
        !Number.isSafeInteger(clock.slotLength) ||
        clock.slotLength <= 0 ||
        !Number.isSafeInteger(clock.zeroTime) ||
        !Number.isSafeInteger(clock.zeroSlot)
      )
        throw new Error("replay transcript retirement clock is invalid");
      const slot = BigInt(observation.nativePoint.slot);
      const now =
        BigInt(clock.zeroTime) +
        (slot - BigInt(clock.zeroSlot)) * BigInt(clock.slotLength);
      return transaction("BEGIN IMMEDIATE", () => {
        let retired = 0;
        const identities = database
          .prepare("SELECT identity FROM watcher_replay_transcript_head")
          .all() as { identity: string }[];
        for (const { identity: key } of identities) {
          const fields = JSON.parse(key) as string[];
          if (fields[0] !== observation.deploymentIdentityDigest) continue;
          const pins = check(key);
          // No lifecycle record means legacy or unresolved work, never permission.
          if (
            pins.length === 0 ||
            (summary.get(key) as { unresolved_legacy: number })
              .unresolved_legacy === 1 ||
            pins.some((pin) => pin.network !== request.network) ||
            pins.some(
              (pin) =>
                pin.completed_slot === null &&
                (pin.classification_complete === 0 || pin.proof_started === 1),
            ) ||
            observation.finalizedCorrectionLock === null ||
            observation.finalizedCorrectionLock.datum !== "Idle" ||
            observation.finalizedHeaders.some(
              (header) => header.headerHash === fields[1],
            ) ||
            observation.finalizedQueue.some(
              (node) => node.headerHash === fields[1],
            ) ||
            BigInt(observation.nativePoint.blockNo) - BigInt(fields[4]!) <=
              BigInt(WATCHER_CARDANO_SECURITY_PARAMETER_K)
          ) {
            absence.clear(key);
            continue;
          }
          const absenceExpired = absence.expired(
            key,
            digestPins(pins),
            observation,
          );
          const inclusionTime =
            BigInt(clock.zeroTime) +
            (BigInt(fields[5]!) - BigInt(clock.zeroSlot)) *
              BigInt(clock.slotLength);
          let eligible = true;
          for (const pin of pins) {
            const endTime = Number(pin.end_time);
            const deadline = retentionDeadlineForBlock({
              blockEndTimeMs: endTime,
              retentionDays: pin.retention_days,
            });
            const duration = BigInt(deadline.deployedRetentionMs);
            const completionTime =
              BigInt(clock.zeroTime) +
              (BigInt(pin.completed_slot ?? fields[5]!) -
                BigInt(clock.zeroSlot)) *
                BigInt(clock.slotLength);
            if (
              now <= BigInt(deadline.retainUntilMs) ||
              now <= completionTime + duration ||
              now <= inclusionTime + duration
            )
              eligible = false;
          }
          if (!eligible || !absenceExpired) continue;
          // Integrity failure stops reclamation; it must never erase its evidence.
          audit(key);
          absence.clear(key);
          database
            .prepare("DELETE FROM watcher_replay_transcript WHERE identity = ?")
            .run(key);
          database
            .prepare(
              "DELETE FROM watcher_replay_transcript_lifecycle WHERE identity = ?",
            )
            .run(key);
          database
            .prepare(
              "DELETE FROM watcher_replay_transcript_head WHERE identity = ?",
            )
            .run(key);
          database
            .prepare(
              "DELETE FROM watcher_replay_transcript_operation WHERE identity = ?",
            )
            .run(key);
          retired += 1;
        }
        return retired;
      });
    },
  });
};
