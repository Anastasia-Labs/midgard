import { DaGossipTopic } from "@al-ft/midgard-core/da-transport";

import type { DaCommitteeValidation } from "../../signer.js";
import type { CommitteeStore } from "../../store.js";
import { daSignatureRecordFromAttestation } from "./attestations.da-signature-record-from-attestation.js";
import {
  decodeDaAttestationGossip,
  encodeDaAttestationGossip,
  StoreBackedDaAttestationProtocol,
} from "./attestations.store-backed-da-attestation-protocol.js";
import type { DaGossipMessageHandler } from "./DaGossip.js";
import type { DaPeerRegistry } from "./DaPeerRegistry.js";

/**
 * Ingests committee signatures gossiped on the attestations topic. The
 * gossip author is authenticated by StrictSign; it must be a manifest
 * committee peer publishing its own signer index, because members only
 * gossip their own signatures. Accepted records are stored as peer
 * signatures for the local coordinator. A rejection throws, which the gossip
 * pipeline reports through its message error hook. Gossip is best effort:
 * `attestationsByHeader` pulls remain the recovery path for signatures that
 * arrive before the local payload is verified or while a peer is offline.
 */
export const createDaLibp2pAttestationGossipHandlers = ({
  deploymentFingerprint,
  registry,
  protocol,
  committeeValidation,
  store,
}: {
  readonly deploymentFingerprint: string;
  readonly registry: DaPeerRegistry;
  readonly protocol: Pick<
    StoreBackedDaAttestationProtocol,
    "acceptAttestation"
  >;
  readonly committeeValidation: DaCommitteeValidation;
  readonly store: Pick<CommitteeStore, "getDaPayload" | "getStateQueueHeader">;
}): ReadonlyMap<DaGossipTopic, DaGossipMessageHandler> =>
  new Map([
    [
      DaGossipTopic.attestations,
      async (context) => {
        if (context.topicName !== DaGossipTopic.attestations) {
          throw new Error("DA attestation gossip arrived on the wrong topic");
        }
        const sender = registry.requireKnownPeer(context.remotePeerId);
        const attestation = decodeDaAttestationGossip(context.data);
        if (!encodeDaAttestationGossip(attestation).equals(context.data)) {
          throw new Error("DA attestation gossip must use canonical CBOR");
        }
        if (
          sender.signerIndex === undefined ||
          sender.signerIndex !== attestation.signerIndex
        ) {
          throw new Error(
            `DA attestation gossip signer index ${attestation.signerIndex.toString()} does not belong to authenticated peer ${sender.peerId}`,
          );
        }
        const converted = await daSignatureRecordFromAttestation({
          deploymentFingerprint,
          committeeValidation,
          store,
          announcerPeerId: sender.peerId,
          attestation,
        });
        if (converted.status === "rejected") {
          throw new Error(
            `rejected DA attestation gossip from ${sender.peerId}: ${converted.reason}`,
          );
        }
        const accepted = await protocol.acceptAttestation({
          record: converted.record,
          sourcePeerId: sender.peerId,
        });
        if (accepted.status !== "accepted") {
          throw new Error(
            `rejected DA attestation gossip from ${sender.peerId}: ${accepted.reason}`,
          );
        }
      },
    ],
  ]);
