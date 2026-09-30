import { MIDGARD_RETENTION_WINDOW } from "@al-ft/midgard-core";

import { type VerifiedWatcherDeploymentIdentity } from "../runtime/deployment-identity.js";
import {
  exactSlot,
  exactString,
  fail,
  HEX_32,
  WATCHER_CANONICAL_BLOCK_STORE_MAX_CAS_ATTEMPTS,
  WATCHER_CANONICAL_BLOCK_STORE_SCHEMA_VERSION,
  type WatcherCanonicalBlockRecord,
} from "./canonical-block-store.parse-provenance.js";
import {
  msToSlots,
  type WatcherCanonicalRetentionWindow,
} from "./canonical-block-store.parse-watcher-canonical-block-record.js";
import {
  assertWatcherCanonicalRetentionWindow,
  parseWatcherCanonicalBlockStoreSnapshot,
} from "./canonical-block-store.parse-watcher-canonical-block-store-snapshot.js";
import {
  commitSnapshot,
  decodeWatcherCanonicalBlockStoreSnapshot,
  encodeWatcherCanonicalBlockStoreSnapshot,
  nextRevision,
  readSnapshotBytes,
  verifySnapshot,
  type WatcherCanonicalPruneDecision,
  type WatcherCanonicalPruneResult,
} from "./canonical-block-store.persist-watcher-canonical-public-bytes.js";
import { type WatcherDurableAtomicBackend } from "./durable-store.js";

/**
 * The only deletion path. A record leaves the store only when its retention
 * deadline has strictly passed AND it is not in the still-challengeable set;
 * every other outcome is a refusal carrying a deterministic reason code. A
 * record whose remaining headroom has been consumed raises `deadline_at_risk`
 * before it can expire.
 */
export const pruneWatcherCanonicalBlockStore = async (input: {
  readonly backend: WatcherDurableAtomicBackend;
  readonly deploymentIdentity: VerifiedWatcherDeploymentIdentity;
  readonly atSlot: number;
  readonly stillChallengeableInputIds: readonly string[];
  readonly inputIds?: readonly string[];
  readonly retentionWindow?: WatcherCanonicalRetentionWindow;
}): Promise<WatcherCanonicalPruneResult> => {
  const marker = input.deploymentIdentity.durableMarker;
  const atSlot = exactSlot(input.atSlot, "$.atSlot");
  const challengeable = new Set(
    input.stillChallengeableInputIds.map((inputId, index) =>
      exactString(
        inputId,
        `$.stillChallengeableInputIds[${String(index)}]`,
        HEX_32,
      ),
    ),
  );
  const alertHeadroomSlots =
    input.retentionWindow === undefined
      ? msToSlots(MIDGARD_RETENTION_WINDOW.marginMs)
      : assertWatcherCanonicalRetentionWindow(input.retentionWindow)
          .alertHeadroomSlots;
  const requested =
    input.inputIds === undefined
      ? null
      : new Set(
          input.inputIds.map((inputId, index) =>
            exactString(inputId, `$.inputIds[${String(index)}]`, HEX_32),
          ),
        );

  for (
    let attempt = 0;
    attempt < WATCHER_CANONICAL_BLOCK_STORE_MAX_CAS_ATTEMPTS;
    attempt += 1
  ) {
    const stored = await readSnapshotBytes(input.backend);
    if (stored === null) {
      const decisions = Object.freeze(
        [...(requested ?? [])].map((inputId) =>
          Object.freeze({
            inputId,
            decision: "retained" as const,
            reasonCode: "unknown_input_id" as const,
            retainUntilSlot: null,
            remainingSlots: null,
            alertCode: null,
          }),
        ),
      );
      return Object.freeze({
        committed: false,
        revision: "0",
        snapshotSha256: null,
        prunedInputIds: Object.freeze([]),
        decisions,
        alerts: Object.freeze([]),
      });
    }
    const current = await verifySnapshot(
      decodeWatcherCanonicalBlockStoreSnapshot(stored.bytes),
      marker,
    );
    const known = new Set(
      current.records.map((record) => record.input.inputId),
    );
    const decisions: WatcherCanonicalPruneDecision[] = [];
    const kept: WatcherCanonicalBlockRecord[] = [];
    const pruned: string[] = [];
    for (const record of current.records) {
      const { inputId, retainUntilSlot } = record.metadata;
      if (requested !== null && !requested.has(inputId)) {
        kept.push(record);
        continue;
      }
      const remainingSlots = retainUntilSlot - atSlot;
      if (challengeable.has(inputId)) {
        kept.push(record);
        decisions.push(
          Object.freeze({
            inputId,
            decision: "retained" as const,
            reasonCode: "still_challengeable" as const,
            retainUntilSlot,
            remainingSlots,
            alertCode:
              remainingSlots >= 0 && remainingSlots <= alertHeadroomSlots
                ? ("deadline_at_risk" as const)
                : null,
          }),
        );
        continue;
      }
      if (!(retainUntilSlot < atSlot)) {
        kept.push(record);
        decisions.push(
          Object.freeze({
            inputId,
            decision: "retained" as const,
            reasonCode: "retention_not_expired" as const,
            retainUntilSlot,
            remainingSlots,
            alertCode:
              remainingSlots <= alertHeadroomSlots
                ? ("deadline_at_risk" as const)
                : null,
          }),
        );
        continue;
      }
      pruned.push(inputId);
      decisions.push(
        Object.freeze({
          inputId,
          decision: "pruned" as const,
          reasonCode: "expired_and_not_challengeable" as const,
          retainUntilSlot,
          remainingSlots,
          alertCode: null,
        }),
      );
    }
    for (const inputId of requested ?? []) {
      if (!known.has(inputId)) {
        decisions.push(
          Object.freeze({
            inputId,
            decision: "retained" as const,
            reasonCode: "unknown_input_id" as const,
            retainUntilSlot: null,
            remainingSlots: null,
            alertCode: null,
          }),
        );
      }
    }
    const alerts = Object.freeze(
      decisions.filter((decision) => decision.alertCode !== null),
    );
    if (pruned.length === 0) {
      return Object.freeze({
        committed: false,
        revision: current.revision,
        snapshotSha256: stored.sha256,
        prunedInputIds: Object.freeze([]),
        decisions: Object.freeze(decisions),
        alerts,
      });
    }
    const next = parseWatcherCanonicalBlockStoreSnapshot({
      schemaVersion: WATCHER_CANONICAL_BLOCK_STORE_SCHEMA_VERSION,
      revision: nextRevision(current.revision),
      deploymentMarker: {
        schemaVersion: current.deploymentMarker.schemaVersion,
        manifestId: current.deploymentMarker.manifestId,
      },
      records: kept,
    });
    const sha256 = await commitSnapshot({
      backend: input.backend,
      expectedSha256: stored.sha256,
      next: encodeWatcherCanonicalBlockStoreSnapshot(next),
    });
    if (sha256 !== null) {
      return Object.freeze({
        committed: true,
        revision: next.revision,
        snapshotSha256: sha256,
        prunedInputIds: Object.freeze(pruned),
        decisions: Object.freeze(decisions),
        alerts,
      });
    }
  }
  return fail("cas_contention", "$.backend.compareAndSwap");
};
