import type { PoolClient } from "pg";

import type { InFlightDecisionAttempts } from "../store.js";
import { parseL1SourceState } from "../store.parse-l1-source-state.js";
import {
  decodeRow,
  encodeRecord,
  type JsonRecordRow,
} from "./postgres.assert-postgres-decision-retry.js";
import { readPostgresRetirementFloor } from "./retirement-postgres.js";

/**
 * The rows a signed decision's header hash keys besides the decision itself
 * (plan §11, class B): every availability signature (the member's own and
 * its peers'), the decision outbox, the peer broadcasts and the L1
 * submissions. Conflict evidence and peer nonces are kept by their own rules.
 */
const DECISION_TABLES = [
  "committee_da_signatures",
  "committee_decision_outbox",
  "committee_peer_broadcasts",
  "committee_l1_submissions",
] as const;

/**
 * The header row and its attestation candidates: deleted only once no
 * payload is retained for the header. A retained payload keeps them, as its
 * release clock and its served proofs read the header row; they then go with
 * the payload (`releasePostgresHeaderRows`).
 */
const HEADER_TABLES = [
  "committee_da_attestation_candidates",
  "committee_state_queue_headers",
] as const;

/** Drops the L1 observations of `headerHashes` from the source state. */
const dropObservations = async (
  client: PoolClient,
  headerHashes: ReadonlySet<string>,
): Promise<void> => {
  const result = await client.query<JsonRecordRow>(
    "SELECT record FROM committee_l1_source_state WHERE id = 1 FOR UPDATE",
  );
  const decoded = decodeRow<unknown>(result.rows[0]);
  if (decoded === undefined) return;
  const state = parseL1SourceState(decoded);
  const observations = state.observations.filter(
    ({ headerHash }) => !headerHashes.has(headerHash),
  );
  if (observations.length === state.observations.length) return;
  await client.query(
    "UPDATE committee_l1_source_state SET record = $1::jsonb, updated_at = NOW() WHERE id = 1",
    [encodeRecord({ ...state, observations })],
  );
};

/**
 * Deletes the signed decisions of `headerHashes` and the rows they key,
 * inside the caller's write transaction. A header with a decision effect in
 * flight in this store instance is skipped. Returns the hashes deleted.
 */
export const prunePostgresSignedDecisions = async (
  client: PoolClient,
  headerHashes: readonly string[],
  inFlight: Pick<InFlightDecisionAttempts, "has">,
): Promise<readonly string[]> => {
  if (headerHashes.length === 0) return [];
  const effects = await client.query<{
    readonly effect_id: string;
    readonly header_hash: string;
  }>(
    "SELECT effect_id, header_hash FROM committee_decision_outbox WHERE header_hash = ANY($1::text[])",
    [[...headerHashes]],
  );
  const busy = new Set(
    effects.rows
      .filter(({ effect_id }) => inFlight.has(effect_id))
      .map(({ header_hash }) => header_hash),
  );
  const pruned = [...new Set(headerHashes)].filter((hash) => !busy.has(hash));
  if (pruned.length === 0) return [];
  for (const table of DECISION_TABLES)
    await client.query(
      `DELETE FROM ${table} WHERE header_hash = ANY($1::text[])`,
      [pruned],
    );
  for (const table of HEADER_TABLES)
    await client.query(
      `DELETE FROM ${table} AS row WHERE row.header_hash = ANY($1::text[])
         AND NOT EXISTS (SELECT 1 FROM committee_da_payloads payload
                          WHERE payload.header_hash = row.header_hash)`,
      [pruned],
    );
  await dropObservations(client, new Set(pruned));
  return pruned;
};

/**
 * `prunePostgresSignedDecisions`, refused under a retirement floor: with
 * promise adoption, retirement is the only deleter of these rows.
 */
export const prunePostgresSignedDecisionsUnlessRetiring = async (
  client: PoolClient,
  headerHashes: readonly string[],
  inFlight: Pick<InFlightDecisionAttempts, "has">,
): Promise<readonly string[]> =>
  headerHashes.length === 0 ||
  (await readPostgresRetirementFloor(client)) !== undefined
    ? []
    : prunePostgresSignedDecisions(client, headerHashes, inFlight);

/**
 * With its payload released, deletes the header row of `headerHash` and
 * the rows still keyed by it (peer signatures, outbox, broadcasts,
 * submissions, attestation candidates), inside the caller's write
 * transaction. Kept while the member's own signed decision for it is
 * stored: that decision goes first (`prunePostgresSignedDecisions`), which
 * then takes the header row too.
 */
export const releasePostgresHeaderRows = async (
  client: PoolClient,
  headerHash: string,
  inFlight: Pick<InFlightDecisionAttempts, "has">,
): Promise<boolean> => {
  const signed = await client.query(
    "SELECT 1 FROM committee_da_signatures WHERE header_hash = $1 AND end_time_ms IS NOT NULL LIMIT 1",
    [headerHash],
  );
  if (signed.rows.length > 0) return false;
  return (
    (await prunePostgresSignedDecisions(client, [headerHash], inFlight))
      .length > 0
  );
};
