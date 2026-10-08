import postgres from "postgres";

import type { AcceptanceNativePayoutScope } from "./acceptance-native-boundary.js";
import {
  AcceptanceReadSockets,
  acceptanceRemainingMs,
} from "./acceptance-payout-sources.js";
import { requireAcceptance } from "./acceptance-payout-types.js";
import type { RunEnv } from "./layout.js";

export type AcceptanceSettlementRow = {
  event_id: string;
  tx_hash: string;
  phase: "initialize" | "fund" | "conclude";
  signed_cbor: string;
  required_outputs: number[];
};
export type AcceptanceSettlementSnapshot = {
  generation: string;
  attempts: readonly AcceptanceSettlementRow[];
};

/** Dedicated read-only connection. No normal service/migration/owner layer is instantiated. */
export const readAcceptanceSettlements = async (
  run: RunEnv,
  scope: AcceptanceNativePayoutScope,
  deploymentId: string,
  eventIds: readonly string[],
  maxRows: number,
  maxTransactionBytes: number,
): Promise<AcceptanceSettlementSnapshot> => {
  requireAcceptance(
    /^[0-9a-f]{64}$/u.test(deploymentId) &&
      eventIds.length === 4 &&
      new Set(eventIds).size === 4 &&
      eventIds.every((id) => /^[0-9a-f]+$/u.test(id)),
    "invalid deployment/events for settlement read",
  );
  requireAcceptance(
    Number.isSafeInteger(maxRows) && maxRows > 0 && maxRows < 2_147_483_647,
    "invalid settlement row bound",
  );
  requireAcceptance(
    Number.isSafeInteger(maxTransactionBytes) && maxTransactionBytes > 0,
    "invalid settlement CBOR bound",
  );
  acceptanceRemainingMs(scope);
  const sockets = new AcceptanceReadSockets(scope);
  const options = {
    host: "127.0.0.1",
    port: run.postgresPort,
    username: run.postgresUser,
    password: run.postgresPassword,
    database: run.postgresDatabase,
    max: 1,
    fetch_types: false,
    prepare: false,
    connect_timeout: 5,
    connection: {
      default_transaction_read_only: true,
      statement_timeout: 5000,
      application_name: "midgard-exact-payout-reader",
    },
    onnotice: () => {},
    // Postgres.js's documented custom socket option is absent from its type declaration.
    socket: () =>
      new Promise<ReturnType<AcceptanceReadSockets["open"]>>(
        (resolve, reject) => {
          let socket: ReturnType<AcceptanceReadSockets["open"]>;
          try {
            socket = sockets.open();
          } catch {
            reject(new Error("settlement read scope revoked before connect"));
            return;
          }
          const failed = () =>
            reject(new Error("settlement read connection closed"));
          socket.once("error", failed);
          socket.once("close", failed);
          socket.connect(run.postgresPort, "127.0.0.1", () => {
            socket.off("error", failed);
            socket.off("close", failed);
            resolve(socket);
          });
        },
      ),
  };
  const sql = postgres(options);
  try {
    const result = await sql.begin("read only", async (read) => {
      scope.assertCurrent();
      const authority = await read<
        { generation: string }[]
      >`SELECT generation::text
        FROM event_history_authority WHERE singleton
        AND deployment_identity = decode(${deploymentId}, 'hex')
        AND state = 'ready' AND lease_until > clock_timestamp()`;
      scope.assertCurrent();
      requireAcceptance(
        authority.length === 1 && /^[0-9]+$/u.test(authority[0]!.generation),
        "settlement authenticated history is not currently ready",
      );
      const attempts = await read<
        AcceptanceSettlementRow[]
      >`SELECT a.event_id, a.tx_hash, a.phase,
        CASE WHEN octet_length(a.signed_cbor) <= ${maxTransactionBytes * 2}
          THEN a.signed_cbor ELSE NULL END AS signed_cbor,
        CASE WHEN cardinality(a.required_outputs) <= ${maxTransactionBytes}
          THEN array_to_json(a.required_outputs) ELSE NULL END AS required_outputs
        FROM settlement_attempts a JOIN settlement_jobs j USING (deployment_id, kind, event_id)
        WHERE a.deployment_id = ${deploymentId} AND a.kind = 'withdrawal'
        AND a.status <> 'expired' AND j.phase = 'complete' AND a.event_id IN ${read(eventIds)}
        ORDER BY a.event_id, a.tx_hash LIMIT ${maxRows + 1}`;
      scope.assertCurrent();
      requireAcceptance(
        attempts.length <= maxRows,
        "settlement read exceeds explicit row bound",
      );
      for (const row of attempts)
        requireAcceptance(
          eventIds.includes(row.event_id) &&
            /^[0-9a-f]{64}$/u.test(row.tx_hash) &&
            ["initialize", "fund", "conclude"].includes(row.phase) &&
            typeof row.signed_cbor === "string" &&
            /^(?:[0-9a-f]{2})+$/u.test(row.signed_cbor) &&
            row.signed_cbor.length <= maxTransactionBytes * 2 &&
            Array.isArray(row.required_outputs) &&
            row.required_outputs.length > 0 &&
            row.required_outputs.every(
              (index) => Number.isSafeInteger(index) && index >= 0,
            ),
          "invalid or oversized settlement receipt locator",
        );
      return { generation: authority[0]!.generation, attempts };
    });
    scope.assertCurrent();
    return result;
  } catch {
    throw new Error("exact payout: read-only settlement source refused");
  } finally {
    await Promise.all([sql.end({ timeout: 0 }), sockets.close()]);
  }
};
