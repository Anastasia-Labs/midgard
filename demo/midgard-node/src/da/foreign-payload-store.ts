import { MIDGARD_CONSENSUS_PROFILE_ID } from "@al-ft/midgard-core/consensus-profile";
import { computeDaSha256Hash } from "@al-ft/midgard-core/da-transport";
import {
  assertDeploymentMarkerMatches,
  type DeploymentMarker,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import type { WatcherPublicDaPayload } from "midgard-watcher/public-da-client";

import {
  DaPayloadsDB,
  ForeignTipReconciliationsDB,
} from "../database/index.js";
import { decodeStoredPayload } from "../workers/t2-foreign-event-reconciliation.resolve-t2-foreign-event-evidence.js";
import { verifyDownloadedForeignPayload } from "./foreign-payload-retriever.js";

/** Called only after the peer-selection validator reconstructs every header commitment. */
export const downloadedForeignPayloadRow = async (
  entry: ForeignTipReconciliationsDB.Entry,
  header: SDK.Header,
  fetched: WatcherPublicDaPayload,
  deploymentMarker: DeploymentMarker,
): Promise<DaPayloadsDB.InsertInput> => {
  const reconciliation =
    ForeignTipReconciliationsDB.decodeForeignTipReconciliation(entry);
  assertDeploymentMarkerMatches(
    reconciliation.deploymentMarker,
    deploymentMarker,
    "downloaded foreign DA",
  );
  if (
    fetched.schemaVersion !== "midgard-watcher-public-da-client-v1" ||
    fetched.deploymentFingerprint !== deploymentMarker.manifestId ||
    fetched.headerHash !== reconciliation.foreignHeaderHash.toString("hex") ||
    fetched.payloadHash !==
      computeDaSha256Hash(fetched.payloadEnvelopeCbor).toString("hex")
  )
    throw new Error("Downloaded foreign DA identity mismatch");
  ForeignTipReconciliationsDB.authenticateForeignTipDaEvidence({
    reconciliation,
    deploymentMarker,
    evidence: {
      headerHash: reconciliation.foreignHeaderHash,
      consensusProfileId: MIDGARD_CONSENSUS_PROFILE_ID,
      schemaVersion: 1,
      payloadCbor: fetched.payloadEnvelopeCbor,
      payloadSha256: Buffer.from(fetched.payloadHash, "hex"),
    },
  });
  const payload = await decodeStoredPayload({
    payloadCbor: fetched.payloadEnvelopeCbor,
    schemaVersion: 1,
  });
  if (
    !(await Effect.runPromise(
      verifyDownloadedForeignPayload(fetched.headerHash, header, payload),
    ))
  )
    throw new Error(
      "Downloaded foreign DA failed header, root, or count verification",
    );
  return {
    header_hash: reconciliation.foreignHeaderHash,
    consensus_profile_id: MIDGARD_CONSENSUS_PROFILE_ID,
    version: 1,
    payload_cbor: fetched.payloadEnvelopeCbor,
    payload_sha256: Buffer.from(fetched.payloadHash, "hex"),
    utxos_root: header.utxosRoot,
    forced_transactions_root: header.forcedTransactionsRoot,
    transactions_root: header.transactionsRoot,
    deposits_root: header.depositsRoot,
    withdrawals_root: header.withdrawalsRoot,
    transition_trace_root: header.transitionTraceRoot,
    event_to_step_root: header.eventToStepRoot,
    validation_traces_root: header.validationTracesRoot,
    withdrawal_count: header.withdrawalCount,
    forced_transaction_count: header.forcedTransactionCount,
    l2_transaction_count: header.l2TransactionCount,
    deposit_count: header.depositCount,
    total_event_count: header.totalEventCount,
    transition_step_count: header.transitionStepCount,
    validation_trace_count: header.validationTraceCount,
    block_start_time: reconciliation.blockStartTime,
    block_end_time: reconciliation.blockEndTime,
  };
};
