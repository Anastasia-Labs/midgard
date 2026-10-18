import {
  forcedVerdictSubject,
  type NetworkIdFault,
  type VerdictSubject,
} from "@al-ft/midgard-sdk";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { buildForcedTransactionLeafMembershipProof } from "../transition-trace/witnesses.js";
import { forcedTransactionSubject } from "../workflow/detection-subject.js";
import type { NetworkIdFaultClaim } from "./evidence.js";
import {
  openAuthenticatedForcedNetworks,
  type PreparedNetworkIdWrongfulRejection,
} from "./wrongful-rejection.js";

export const NETWORK_ID_ACCEPTED_FORCED_VIOLATION_ID =
  "network-id-accepted-forced" as const;

/**
 * An accepted forced transaction (`ForcedTxValid` leaf) whose body or named
 * output carries a foreign network. Step 01 dispatches the claim to the forced
 * door, which binds the leaf and hands step 02 the same `TransactionNetwork` or
 * `OutputNetwork` adjudication an accepted normal transaction gets, with the
 * forced root key fixing the source its outputs open under.
 */
export type PreparedNetworkIdAcceptedForced = Readonly<{
  headerHash: string;
  expectedNetworkId: 0n | 1n;
  badTxId: string;
  /** The leaf's forced compact source; field 2 opens under source kind 1. */
  nativeTxCompactCbor: string;
  fullTransactionCbor: string;
  outputsItemCbors: readonly string[];
  committedNetworkId: bigint;
  /** Network id of every output, in field-2 order. */
  outputNetworkIds: readonly (bigint | null)[];
  faultClaim: NetworkIdFaultClaim;
  fault: NetworkIdFault;
  subject: VerdictSubject;
  forcedSource: Readonly<{
    header: PreparedNetworkIdWrongfulRejection["forcedSource"]["header"];
    membership: PreparedNetworkIdWrongfulRejection["forcedSource"]["membership"];
  }>;
}>;

export const isAcceptedForcedPrepared = (prepared: {
  readonly faultClaim: { readonly kind: string };
}): prepared is PreparedNetworkIdAcceptedForced =>
  (prepared.faultClaim.kind === "transaction-network" ||
    prepared.faultClaim.kind === "output-network") &&
  "forcedSource" in prepared;

const faultOf = (claim: NetworkIdFaultClaim): NetworkIdFault =>
  claim.kind === "transaction-network"
    ? "TransactionNetwork"
    : { OutputNetwork: { output_index: claim.outputIndex } };

/**
 * Every foreign-network claim on a `ForcedTxValid` leaf, in the same order an
 * accepted normal transaction's claims are found: the body network first,
 * then each output whose network differs from the deployed expectation.
 */
export const detectNetworkIdAcceptedForced = ({
  block,
  expectedNetworkId,
}: {
  readonly block: CanonicalBlockEvidence;
  readonly expectedNetworkId: 0n | 1n;
}) =>
  Object.freeze(
    block.reconstruction.forcedTransactions.flatMap((forced, forcedIndex) => {
      if (forced.value.verdict !== "ForcedTxValid") return [];
      const opened = openAuthenticatedForcedNetworks(forced);
      const claims: NetworkIdFaultClaim[] = [
        ...(opened.committedNetworkId !== 255n &&
        opened.committedNetworkId !== expectedNetworkId
          ? [{ kind: "transaction-network" as const }]
          : []),
        ...opened.outputNetworkIds.flatMap((network, index) =>
          network === null || network === expectedNetworkId
            ? []
            : [{ kind: "output-network" as const, outputIndex: BigInt(index) }],
        ),
      ];
      return claims.map((faultClaim) =>
        Object.freeze({
          ...forcedTransactionSubject(forced.key),
          detectionId: `${NETWORK_ID_ACCEPTED_FORCED_VIOLATION_ID}:${forcedIndex.toString()}:${forced.value.tx_id}:${
            faultClaim.kind === "transaction-network"
              ? "tx"
              : faultClaim.outputIndex.toString()
          }`,
          headerHash: block.headerHash,
          violationId: NETWORK_ID_ACCEPTED_FORCED_VIOLATION_ID,
          position: BigInt(forcedIndex),
          forcedIndex,
          transactionId: forced.value.tx_id,
          faultClaim,
          opened,
        }),
      );
    }),
  );

/**
 * Plans the accepted-forced proof for one detection (the first when none is
 * named), deriving the claim, the leaf's membership and the forced source
 * from the block alone.
 */
export const prepareNetworkIdAcceptedForced = async ({
  block,
  expectedNetworkId,
  detectionId,
}: Readonly<{
  block: CanonicalBlockEvidence;
  expectedNetworkId: 0n | 1n;
  detectionId?: string;
}>): Promise<PreparedNetworkIdAcceptedForced> => {
  const detections = detectNetworkIdAcceptedForced({
    block,
    expectedNetworkId,
  });
  const detection =
    detectionId === undefined
      ? detections[0]
      : detections.find((value) => value.detectionId === detectionId);
  if (detection === undefined)
    throw new Error("networkId: no foreign network on an accepted forced leaf");
  const forced =
    block.reconstruction.forcedTransactions[detection.forcedIndex]!;
  const membership = await buildForcedTransactionLeafMembershipProof({
    reconstruction: block.reconstruction,
    eventKey: { ForcedTransactionEventKey: { tx_order_id: forced.key } },
  });
  return Object.freeze({
    headerHash: block.headerHash,
    expectedNetworkId,
    badTxId: forced.value.tx_id,
    nativeTxCompactCbor: forced.value.submitted_source.compact_cbor,
    fullTransactionCbor: forced.fullTransactionCbor.toString("hex"),
    outputsItemCbors: detection.opened.outputsItemCbors,
    committedNetworkId: detection.opened.committedNetworkId,
    outputNetworkIds: detection.opened.outputNetworkIds,
    faultClaim: detection.faultClaim,
    fault: faultOf(detection.faultClaim),
    subject: forcedVerdictSubject({
      transactionId: forced.value.tx_id,
      sourceKey: forced.key,
      rejectionReason: null,
    }),
    forcedSource: Object.freeze({
      header: block.header,
      membership,
    }),
  });
};
