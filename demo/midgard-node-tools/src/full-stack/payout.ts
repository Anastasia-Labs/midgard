import { L1NodeTransport } from "@al-ft/l1-node-transport";

import { stackPaths } from "./deployment.js";
import { readJsonIfPresent } from "./journal.js";
import { hostL1Node } from "./native-ledger.js";
import {
  includedPayout,
  payoutConclusion,
  payoutOutRef,
  type SettlementObservation,
} from "./payout-body.js";
import { poll, type StackProcesses } from "./process.js";

export async function settlementEvidence(
  processes: StackProcesses,
  kind: "deposit" | "withdrawal",
  eventId: string,
) {
  if (!/^[0-9a-f]+$/.test(eventId)) throw new Error("Noncanonical event ID");
  const manifest = (await readJsonIfPresent(
    stackPaths(processes).manifest,
  )) as { manifestId: string };
  if (!/^[0-9a-f]{64}$/.test(manifest.manifestId))
    throw new Error("Noncanonical deployment ID");
  const query = `SELECT json_build_object('jobs', (SELECT json_agg(json_build_object('phase', phase)) FROM settlement_jobs WHERE deployment_id='${manifest.manifestId}' AND kind='${kind}' AND event_id='${eventId}'), 'attempts', (SELECT json_agg(json_build_object('phase',phase,'status',status,'txHash',tx_hash,'signedCbor',signed_cbor)) FROM settlement_attempts WHERE deployment_id='${manifest.manifestId}' AND kind='${kind}' AND event_id='${eventId}'))`;
  return (await processes.compose(`settlement-${kind}-observation`, [
    "exec",
    "-T",
    "postgres",
    "psql",
    "-U",
    processes.env.POSTGRES_USER!,
    "-d",
    processes.env.POSTGRES_DB!,
    "-A",
    "-t",
    "-c",
    query,
  ])) as SettlementObservation;
}

/** One bounded read of the payout output reference at the local node's tip. */
async function queryPayoutOutput(
  processes: StackProcesses,
  outRef: { txHash: string; outputIndex: number },
) {
  const transport = new L1NodeTransport({
    ...hostL1Node(processes),
    requestTimeoutMs: 30_000,
    readyTimeoutMs: 30_000,
  });
  try {
    return await transport.query({
      query: "utxo_by_txin",
      txIns: [{ txId: outRef.txHash, index: outRef.outputIndex }],
    });
  } finally {
    await transport.close();
  }
}

/**
 * Waits until the withdrawal's single confirmed conclusion is on L1 with the
 * exact payout output. A provider that cannot answer yet is waited out, never
 * a failure; a conclusion that is not the exact payout is refused.
 */
export async function awaitExactPayout(
  processes: StackProcesses,
  eventId: string,
  address: string,
  assets: Record<string, string>,
) {
  return poll(
    "exact canonical withdrawal payout",
    processes.config.timeoutMs,
    async () => {
      const status = await settlementEvidence(processes, "withdrawal", eventId);
      const attempt = payoutConclusion(status);
      if (attempt === undefined) return undefined;
      const outRef = payoutOutRef(attempt, address, assets);
      const answer = await queryPayoutOutput(processes, outRef).catch(
        () => undefined,
      );
      if (answer === undefined) return undefined;
      const output = includedPayout(outRef, answer);
      if (output === undefined) return undefined;
      return {
        eventId,
        txHash: attempt.txHash,
        outputIndex: output.outputIndex,
        address,
        assets,
      };
    },
  );
}
