import { computeHash28, computeHash32 } from "@al-ft/midgard-core";
import {
  countedRootPreimageHex,
  encodeHeaderCbor,
  forcedVerdictSubject,
  ROOT_DOMAINS,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import type { PreparedNetworkIdAcceptedForced } from "./accepted-forced.js";
import { bindForcedLeafMembership } from "./workflow-adapter.bind-forced-leaf.js";
import {
  ARTIFACT_VERSION,
  type ForcedSourcePayload,
  NetworkIdForcedSourceSchema,
  requireString,
} from "./workflow-adapter.prepared-from-artifact.js";
import { openAuthenticatedForcedNetworks } from "./wrongful-rejection.js";

export type NetworkIdAcceptedForcedWorkflowArtifact = Readonly<{
  schemaVersion: typeof ARTIFACT_VERSION;
  direction: "accepted_forced";
  headerHash: string;
  expectedNetworkId: "0" | "1";
  badTxId: string;
  fullTransactionCbor: string;
  forcedSourceCbor: string;
  faultKind: "transaction-network" | "output-network";
  outputIndex: string | null;
}>;

export const networkIdAcceptedForcedArtifactFromPrepared = (
  prepared: PreparedNetworkIdAcceptedForced,
): NetworkIdAcceptedForcedWorkflowArtifact => ({
  schemaVersion: ARTIFACT_VERSION,
  direction: "accepted_forced",
  headerHash: prepared.headerHash,
  expectedNetworkId: prepared.expectedNetworkId.toString() as "0" | "1",
  badTxId: prepared.badTxId,
  fullTransactionCbor: prepared.fullTransactionCbor,
  forcedSourceCbor: Data.to(
    {
      header: prepared.forcedSource.header,
      membership: prepared.forcedSource.membership,
    } as never,
    NetworkIdForcedSourceSchema as never,
  ),
  faultKind: prepared.faultClaim.kind,
  outputIndex:
    prepared.faultClaim.kind === "output-network"
      ? prepared.faultClaim.outputIndex.toString()
      : null,
});

/** Re-open the counted-root leaf and every retained byte before restarting a proof. */
export const admitNetworkIdAcceptedForcedArtifact = (
  artifact: NetworkIdAcceptedForcedWorkflowArtifact,
): PreparedNetworkIdAcceptedForced => {
  if (
    artifact.schemaVersion !== ARTIFACT_VERSION ||
    artifact.direction !== "accepted_forced"
  )
    throw new Error(
      "network-id accepted-forced artifact has an unsupported shape",
    );
  const expectedNetworkId =
    artifact.expectedNetworkId === "0"
      ? 0n
      : artifact.expectedNetworkId === "1"
        ? 1n
        : undefined;
  if (expectedNetworkId === undefined)
    throw new Error("network-id workflow artifact has an invalid network id");
  const headerHash = requireString(artifact.headerHash, "header hash");
  const sourceCbor = requireString(artifact.forcedSourceCbor, "forced source");
  const source = Data.from(
    sourceCbor,
    NetworkIdForcedSourceSchema as never,
  ) as ForcedSourcePayload;
  const { header, membership } = source;
  if (
    Data.to(source as never, NetworkIdForcedSourceSchema as never) !==
      sourceCbor ||
    header.protocolVersion !== 1n ||
    membership.domain !== ROOT_DOMAINS.forcedTransactionsV1 ||
    membership.count <= 0n ||
    membership.count !== header.forcedTransactionCount ||
    membership.root !== header.forcedTransactionsRoot ||
    computeHash32(
      Buffer.from(
        countedRootPreimageHex({
          domain: membership.domain,
          phasRoot: membership.phas_root,
          count: membership.count,
        }),
        "hex",
      ),
    ).toString("hex") !== membership.root ||
    computeHash28(encodeHeaderCbor(header)).toString("hex") !== headerHash
  )
    throw new Error(
      "network-id accepted-forced artifact does not bind its authenticated header",
    );
  bindForcedLeafMembership(source);
  const leaf = membership.value;
  if (leaf.verdict !== "ForcedTxValid" || leaf.tx_id !== artifact.badTxId)
    throw new Error(
      "network-id accepted-forced artifact does not bind an accepted forced leaf",
    );
  const fullTransactionCbor = requireString(
    artifact.fullTransactionCbor,
    "full forced transaction",
  );
  if (!/^(?:[0-9a-f]{2})+$/u.test(fullTransactionCbor))
    throw new Error(
      "network-id accepted-forced artifact has noncanonical transaction bytes",
    );
  const opened = openAuthenticatedForcedNetworks({
    value: leaf,
    fullTransactionCbor: Buffer.from(fullTransactionCbor, "hex"),
  });
  let faultClaim: PreparedNetworkIdAcceptedForced["faultClaim"];
  if (
    artifact.faultKind === "transaction-network" &&
    artifact.outputIndex === null
  ) {
    if (
      opened.committedNetworkId === 255n ||
      opened.committedNetworkId === expectedNetworkId
    )
      throw new Error(
        "network-id accepted-forced artifact names an honest body network",
      );
    faultClaim = { kind: "transaction-network" };
  } else if (
    artifact.faultKind === "output-network" &&
    typeof artifact.outputIndex === "string" &&
    /^(0|[1-9][0-9]*)$/u.test(artifact.outputIndex)
  ) {
    const outputIndex = BigInt(artifact.outputIndex);
    if (
      outputIndex >= BigInt(opened.outputNetworkIds.length) ||
      opened.outputNetworkIds[Number(outputIndex)] === null ||
      opened.outputNetworkIds[Number(outputIndex)] === expectedNetworkId
    )
      throw new Error(
        "network-id accepted-forced artifact names an absent or honest output network",
      );
    faultClaim = { kind: "output-network", outputIndex };
  } else
    throw new Error(
      "network-id accepted-forced artifact has an inconsistent claim",
    );
  return Object.freeze({
    headerHash,
    expectedNetworkId,
    badTxId: leaf.tx_id,
    nativeTxCompactCbor: leaf.submitted_source.compact_cbor,
    fullTransactionCbor,
    outputsItemCbors: opened.outputsItemCbors,
    committedNetworkId: opened.committedNetworkId,
    outputNetworkIds: opened.outputNetworkIds,
    faultClaim,
    fault:
      faultClaim.kind === "transaction-network"
        ? "TransactionNetwork"
        : { OutputNetwork: { output_index: faultClaim.outputIndex } },
    subject: forcedVerdictSubject({
      transactionId: leaf.tx_id,
      sourceKey: membership.key,
      rejectionReason: null,
    }),
    forcedSource: Object.freeze({ header, membership, direction: 0n as const }),
  });
};
