import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import { eventHistoryCanonicalJson } from "../l1-event-history-source.js";
import type { Checkpoint } from "./eventHistoryJournal.js";
import {
  digest,
  DISPLACED_BLOCK_REVIVAL_RECOVERY_DOMAIN,
  fail,
  type HistoryRecoveryIntent,
  isHash,
  isHeaderHash,
  lockCheckpoint,
  persistRecoveryPlan,
  table,
} from "./eventHistoryRecoveryPlans.prepare-history-recovery-plan.js";
import { DatabaseError, sqlErrorToDatabaseError } from "./utils/common.js";

export const DISPLACEMENT_COMPENSATION_RECOVERY_DOMAIN =
  "midgard-history-displacement-compensation-intent-v1";

/** Retains the complete original operation and its closure while a new branch
 * returns only part of its finalized chain. Prefix remains locally finalized;
 * only the proven absent suffix is reopened. Each native attempt is durable. */
export type DisplacementCompensationIntent = Readonly<{
  bindingDigest: string;
  manifestId: string;
  headerHash: string;
  originalRecoveryId: string;
  originalIntent: HistoryRecoveryIntent;
  prefixHeaderHashes: readonly string[];
  suffixHeaderHashes: readonly string[];
  suffixMembers: readonly Readonly<{
    headerHash: string;
    transitionDigest: string;
    kind: "displaced" | "removed";
  }>[];
  expectedRoot: string;
  targetRoot: string;
  journalDigest: string;
}>;
export type DisplacementCompensationPlan = Readonly<{
  kind: "displacement_compensation";
  recoveryId: string;
  intent: DisplacementCompensationIntent;
  evidenceDigest: string;
  checkpointRevision: string;
  state: "prepared" | "applied";
  native: Readonly<{
    recoveryId: string;
    expectedRoot: string;
    targetRoot: string;
  }>;
}>;

export const parseDisplacementCompensationIntent = (
  value: unknown,
): DisplacementCompensationIntent | undefined => {
  if (value === null || typeof value !== "object" || Array.isArray(value))
    return undefined;
  const input = value as Record<string, unknown>;
  const original = input.originalIntent;
  if (
    original === null ||
    typeof original !== "object" ||
    Array.isArray(original)
  )
    return undefined;
  const record = original as Record<string, unknown>;
  const headers = record.displacedHeaderHashes;
  const prefix = input.prefixHeaderHashes;
  const suffix = input.suffixHeaderHashes;
  const originalKeys = [
    "bindingDigest",
    "manifestId",
    "headerHash",
    "signedTransactionHash",
    "signedTransactionCborSha256",
    "expectedRoot",
    "targetRoot",
    "journalDigest",
    "displacedHeaderHashes",
    "operationNonce",
  ];
  const keys = [
    "bindingDigest",
    "manifestId",
    "headerHash",
    "originalRecoveryId",
    "originalIntent",
    "prefixHeaderHashes",
    "suffixHeaderHashes",
    "suffixMembers",
    "expectedRoot",
    "targetRoot",
    "journalDigest",
  ];
  if (
    Object.keys(record).length !== originalKeys.length ||
    !originalKeys.every((key) => Object.hasOwn(record, key)) ||
    Object.keys(input).length !== keys.length ||
    !keys.every((key) => Object.hasOwn(input, key)) ||
    !originalKeys
      .filter((key) => key !== "headerHash" && key !== "displacedHeaderHashes")
      .every((key) => typeof record[key] === "string" && isHash(record[key])) ||
    !keys
      .filter(
        (key) =>
          ![
            "headerHash",
            "originalIntent",
            "prefixHeaderHashes",
            "suffixHeaderHashes",
            "suffixMembers",
          ].includes(key),
      )
      .every((key) => typeof input[key] === "string" && isHash(input[key])) ||
    !isHeaderHash(record.headerHash as string) ||
    !isHeaderHash(input.headerHash as string) ||
    !Array.isArray(headers) ||
    headers.length === 0 ||
    !headers.every(
      (header) => typeof header === "string" && isHeaderHash(header),
    ) ||
    new Set(headers).size !== headers.length ||
    !Array.isArray(prefix) ||
    !Array.isArray(suffix) ||
    ![...prefix, ...suffix].every(
      (header) => typeof header === "string" && isHeaderHash(header),
    ) ||
    [...prefix, ...suffix].join(",") !== headers.join(",") ||
    !Array.isArray(input.suffixMembers) ||
    input.suffixMembers.length !== suffix.length ||
    !input.suffixMembers.every(
      (member, index) =>
        member !== null &&
        typeof member === "object" &&
        !Array.isArray(member) &&
        Object.keys(member).length === 3 &&
        member.headerHash === suffix[index] &&
        typeof member.transitionDigest === "string" &&
        isHash(member.transitionDigest) &&
        (member.kind === "displaced" || member.kind === "removed"),
    ) ||
    input.headerHash !== (prefix.at(-1) ?? record.headerHash) ||
    input.bindingDigest !== record.bindingDigest ||
    input.manifestId !== record.manifestId ||
    input.journalDigest !== record.journalDigest ||
    digest(
      eventHistoryCanonicalJson({
        domain: DISPLACED_BLOCK_REVIVAL_RECOVERY_DOMAIN,
        ...record,
      }),
    ) !== input.originalRecoveryId
  )
    return undefined;
  const intent = input as unknown as DisplacementCompensationIntent;
  return Object.freeze({
    ...intent,
    originalIntent: Object.freeze({
      ...intent.originalIntent,
      displacedHeaderHashes: Object.freeze([...headers]),
    }),
    prefixHeaderHashes: Object.freeze([...prefix]),
    suffixHeaderHashes: Object.freeze([...suffix]),
    suffixMembers: Object.freeze(
      intent.suffixMembers.map((member) => Object.freeze({ ...member })),
    ),
  });
};

/** Fresh evidence authorizes replacing one prepared obligation with another.
 * DELETE + INSERT occurs inside the caller's recovery transaction: there is
 * never a committed interval without a prepared gate. Full original identity
 * remains nested in the replacement; an applied row is never reused here. */
export const prepareDisplacementCompensation = (
  checkpoint: Checkpoint,
  priorRecoveryId: string,
  intent: DisplacementCompensationIntent,
  evidenceDigest: string,
) =>
  Effect.gen(function* () {
    const captured = parseDisplacementCompensationIntent(intent);
    yield* lockCheckpoint(checkpoint);
    if (
      captured === undefined ||
      captured.bindingDigest !== checkpoint.bindingDigest ||
      captured.manifestId !== checkpoint.manifestId ||
      !isHash(priorRecoveryId) ||
      !isHash(evidenceDigest)
    )
      return yield* fail("Invalid displacement compensation identity");
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{
      recovery_id: Buffer;
      intent: string;
    }>`SELECT recovery_id, intent FROM event_history_recovery_plans WHERE binding_digest = ${Buffer.from(checkpoint.bindingDigest, "hex")} AND state = 'prepared' FOR UPDATE`;
    if (
      rows.length !== 1 ||
      rows[0]!.recovery_id.toString("hex") !== priorRecoveryId ||
      digest(rows[0]!.intent) !== priorRecoveryId
    )
      return yield* fail(
        "The prepared displacement changed before compensation",
      );
    const identity = eventHistoryCanonicalJson({
      domain: DISPLACEMENT_COMPENSATION_RECOVERY_DOMAIN,
      ...captured,
    });
    const recoveryId = digest(identity);
    if (recoveryId !== priorRecoveryId) {
      // Replacing only a decoded matching original/compensation plan prevents
      // stealing an unrelated signed-intent or correction operation's gate.
      const previous: unknown = yield* Effect.try({
        try: () => JSON.parse(rows[0]!.intent) as unknown,
        catch: (cause) =>
          new DatabaseError({
            table,
            message: "Malformed prepared compensation provenance",
            cause,
          }),
      });
      if (
        previous === null ||
        typeof previous !== "object" ||
        Array.isArray(previous)
      )
        return yield* fail("Malformed prepared compensation provenance");
      const envelope = previous as Record<string, unknown>;
      const { domain, ...prior } = envelope;
      const originalMatches =
        domain === DISPLACED_BLOCK_REVIVAL_RECOVERY_DOMAIN &&
        eventHistoryCanonicalJson(prior) ===
          eventHistoryCanonicalJson(captured.originalIntent) &&
        priorRecoveryId === captured.originalRecoveryId;
      const compensation =
        domain === DISPLACEMENT_COMPENSATION_RECOVERY_DOMAIN
          ? parseDisplacementCompensationIntent(prior)
          : undefined;
      if (
        !originalMatches &&
        (compensation === undefined ||
          compensation.originalRecoveryId !== captured.originalRecoveryId ||
          eventHistoryCanonicalJson(compensation.originalIntent) !==
            eventHistoryCanonicalJson(captured.originalIntent))
      )
        return yield* fail(
          "Compensation does not retain the prepared original operation",
        );
      yield* sql`DELETE FROM event_history_recovery_plans WHERE recovery_id = ${rows[0]!.recovery_id} AND state = 'prepared'`;
    }
    const persisted = yield* persistRecoveryPlan(
      checkpoint,
      DISPLACEMENT_COMPENSATION_RECOVERY_DOMAIN,
      captured,
      evidenceDigest,
      captured.headerHash,
    );
    if (persisted.state !== "prepared")
      return yield* fail(
        "A distinct compensation cannot reuse an applied receipt",
      );
    return Object.freeze({
      kind: "displacement_compensation" as const,
      ...persisted,
      intent: captured,
      evidenceDigest,
      checkpointRevision: checkpoint.revision,
      native: Object.freeze({
        recoveryId,
        expectedRoot: captured.expectedRoot,
        targetRoot: captured.targetRoot,
      }),
    }) satisfies DisplacementCompensationPlan;
  }).pipe(
    sqlErrorToDatabaseError(
      table,
      "Failed to prepare displacement compensation",
    ),
  );
