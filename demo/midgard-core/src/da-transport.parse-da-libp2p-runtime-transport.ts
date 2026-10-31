import { MidgardTxCodecErrorCodes } from "./codec/errors.js";
import {
  DA_TRANSPORT_LIMITS,
  type DaLibp2pRuntimeManifest,
} from "./da-transport.da-libp2p-runtime-manifest.js";
import { fail } from "./da-transport.enum-label.js";
import {
  exactRecordKeys,
  nonEmptyStringValue,
  recordValue,
  safeIntegerValue,
  stringArrayValue,
} from "./da-transport.parse-da-libp2p-runtime-manifest-deployment.js";

export const parseDaLibp2pRuntimeTransport = (
  value: unknown,
  fieldName: string,
): DaLibp2pRuntimeManifest["da_transport"] => {
  const transport = recordValue(value, fieldName);
  exactRecordKeys(
    transport,
    [
      "kind",
      "no_http_da_transport",
      "listen_multiaddrs",
      "announce_multiaddrs",
      "bootstrap_multiaddrs",
      "gossip",
      "limits",
      "retention_days",
    ],
    fieldName,
  );
  if (transport.kind !== "libp2p") {
    fail(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      `${fieldName}.kind must be libp2p`,
      String(transport.kind),
    );
  }
  if (transport.no_http_da_transport !== true) {
    fail(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      `${fieldName}.no_http_da_transport must be true`,
      String(transport.no_http_da_transport),
    );
  }
  const gossipFieldName = `${fieldName}.gossip`;
  const gossip = recordValue(transport.gossip, gossipFieldName);
  exactRecordKeys(
    gossip,
    [
      "strict_sign",
      "emit_self",
      "allowed_topics_only",
      "max_gossip_message_bytes",
    ],
    gossipFieldName,
  );
  if (
    gossip.strict_sign !== true ||
    gossip.emit_self !== false ||
    gossip.allowed_topics_only !== true
  ) {
    fail(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      `${gossipFieldName} must use strict_sign=true, emit_self=false, and allowed_topics_only=true`,
    );
  }
  if (
    gossip.max_gossip_message_bytes !==
    DA_TRANSPORT_LIMITS.maxGossipMessageBytes
  ) {
    fail(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      `${gossipFieldName}.max_gossip_message_bytes must be ${DA_TRANSPORT_LIMITS.maxGossipMessageBytes.toString()}`,
      String(gossip.max_gossip_message_bytes),
    );
  }
  const limitsFieldName = `${fieldName}.limits`;
  const limits = recordValue(transport.limits, limitsFieldName);
  exactRecordKeys(
    limits,
    [
      "max_payload_bytes",
      "max_inline_response_bytes",
      "max_chunk_bytes",
      "max_streams_per_peer",
      "request_timeout_ms",
    ],
    limitsFieldName,
  );
  const expectedLimits = {
    max_payload_bytes: DA_TRANSPORT_LIMITS.maxPayloadBytes,
    max_inline_response_bytes: DA_TRANSPORT_LIMITS.maxInlineResponseBytes,
    max_chunk_bytes: DA_TRANSPORT_LIMITS.maxChunkBytes,
    max_streams_per_peer: DA_TRANSPORT_LIMITS.maxStreamsPerPeer,
    request_timeout_ms: DA_TRANSPORT_LIMITS.requestTimeoutMs,
  } as const;
  for (const [key, expected] of Object.entries(expectedLimits)) {
    if (limits[key] !== expected) {
      fail(
        MidgardTxCodecErrorCodes.InvalidFieldType,
        `${limitsFieldName}.${key} must be ${expected.toString()}`,
        String(limits[key]),
      );
    }
  }
  const retentionDays = safeIntegerValue(
    transport.retention_days,
    `${fieldName}.retention_days`,
    DA_TRANSPORT_LIMITS.minimumRetentionDays,
  );
  return {
    kind: "libp2p",
    no_http_da_transport: true,
    listen_multiaddrs: stringArrayValue(
      transport.listen_multiaddrs,
      `${fieldName}.listen_multiaddrs`,
    ),
    announce_multiaddrs: stringArrayValue(
      transport.announce_multiaddrs,
      `${fieldName}.announce_multiaddrs`,
    ),
    bootstrap_multiaddrs: stringArrayValue(
      transport.bootstrap_multiaddrs,
      `${fieldName}.bootstrap_multiaddrs`,
      true,
    ),
    gossip: {
      strict_sign: true,
      emit_self: false,
      allowed_topics_only: true,
      max_gossip_message_bytes: DA_TRANSPORT_LIMITS.maxGossipMessageBytes,
    },
    limits: expectedLimits,
    retention_days: retentionDays,
  };
};

export const parseDaLibp2pRuntimeCommittee = (
  value: unknown,
  fieldName: string,
): DaLibp2pRuntimeManifest["da_committee"] => {
  const committee = recordValue(value, fieldName);
  exactRecordKeys(committee, ["threshold", "members"], fieldName);
  const threshold = safeIntegerValue(
    committee.threshold,
    `${fieldName}.threshold`,
    1,
  );
  const rawMembers = committee.members;
  if (!Array.isArray(rawMembers)) {
    return fail(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      `${fieldName}.members must be an array`,
    );
  }
  if (rawMembers.length === 0) {
    return fail(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      `${fieldName}.members must be a non-empty array`,
    );
  }
  const members = rawMembers.map((value, index) => {
    const memberFieldName = `${fieldName}.members[${index.toString()}]`;
    const member = recordValue(value, memberFieldName);
    exactRecordKeys(
      member,
      ["signer_index", "da_vkey", "peer_id", "multiaddrs", "roles"],
      memberFieldName,
    );
    return {
      signer_index: safeIntegerValue(
        member.signer_index,
        `${memberFieldName}.signer_index`,
        0,
        255,
      ),
      da_vkey: nonEmptyStringValue(
        member.da_vkey,
        `${memberFieldName}.da_vkey`,
      ),
      peer_id: nonEmptyStringValue(
        member.peer_id,
        `${memberFieldName}.peer_id`,
      ),
      multiaddrs: stringArrayValue(
        member.multiaddrs,
        `${memberFieldName}.multiaddrs`,
      ),
      roles: stringArrayValue(member.roles, `${memberFieldName}.roles`),
    };
  });
  if (threshold > members.length) {
    fail(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      `${fieldName}.threshold must be no greater than member count`,
    );
  }
  return { threshold, members };
};
