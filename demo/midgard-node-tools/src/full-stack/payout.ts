import {
  createLocalKupmiosHttpOgmiosRawSource,
  readAdmittedLocalKupmiosSignedTransactionRecovery,
} from "@al-ft/midgard-fault-proofs";
import { watcherDeploymentReleaseFinalityAuthority } from "midgard-watcher";

import { stackPaths } from "./deployment.js";
import { readJsonIfPresent } from "./journal.js";
import {
  includedPayout,
  payoutConclusion,
  type SettlementObservation,
} from "./payout-body.js";
import { poll, type StackProcesses } from "./process.js";
import { verifyStackRelease } from "./release.js";

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

export async function awaitExactPayout(
  processes: StackProcesses,
  eventId: string,
  address: string,
  assets: Record<string, string>,
) {
  const authority = await verifyStackRelease(processes);
  const releaseFinality = await watcherDeploymentReleaseFinalityAuthority(
    authority.deploymentIdentity,
  ).verifyForWorkflow({
    deploymentFingerprint: authority.deploymentIdentity.manifestId,
  });
  const source = createLocalKupmiosHttpOgmiosRawSource({
    sourceId: "full-stack-payout-verification",
    kupoHttpUrl: processes.env.L1_KUPO_KEY!,
    ogmiosUrl: processes.env.L1_OGMIOS_KEY!,
    releaseFinality,
    timeoutMs: 30_000,
  });
  return poll(
    "exact canonical withdrawal payout",
    processes.config.timeoutMs,
    async () => {
      const status = await settlementEvidence(processes, "withdrawal", eventId);
      const attempt = payoutConclusion(status);
      if (attempt === undefined) return undefined;
      const observation =
        await readAdmittedLocalKupmiosSignedTransactionRecovery({
          source,
          transactionHash: attempt.txHash,
          signedTransactionCborHex: attempt.signedCbor,
        });
      const output = includedPayout(
        attempt,
        observation.status,
        address,
        assets,
      );
      if (output === undefined) return undefined;
      return {
        eventId,
        txHash: attempt.txHash,
        outputIndex: output.outputIndex,
        address,
        assets,
        observation,
      };
    },
  );
}
