import "./finalized.validate-finalized-contracts.js";

import { getAddressDetails } from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";
import { bytesToHex, hexToBytes } from "@noble/hashes/utils.js";

import { decodeMidgardNativeScript } from ".././codec/native-script.js";
import {
  DA_RUNTIME_MANIFEST_SCHEMA_VERSION,
  DA_TRANSPORT_LIMITS,
  DA_TRANSPORT_PROTOCOL_VERSION,
} from ".././da-transport.js";
import {
  MIDGARD_RETENTION_WINDOW,
  retentionDaysCoverWindow,
} from ".././retention-window.js";
import {
  normalizeDeploymentManifestJsonValueInternal,
  stableJson,
} from "./identity.js";
import {
  requireExactKeys,
  requireHex,
  requireInteger,
  requireRecord,
} from "./primitives.js";

export const validateFinalizedDa = (value: unknown): void => {
  const da = requireRecord(value, "Deployment manifest da");
  requireExactKeys(
    da,
    ["committeeVkeys", "committeeSignersHash", "threshold", "transportProfile"],
    [],
    "da",
  );
  if (!Array.isArray(da.committeeVkeys) || da.committeeVkeys.length === 0) {
    throw new Error(
      "Deployment manifest da.committeeVkeys must be a non-empty array",
    );
  }
  const committeeVkeys = da.committeeVkeys.map((entry, index) =>
    requireHex(entry, 32, `da.committeeVkeys[${index.toString()}]`),
  );
  if (new Set(committeeVkeys).size !== committeeVkeys.length) {
    throw new Error("Deployment manifest da.committeeVkeys must be unique");
  }
  const committeeSignersHash = requireHex(
    da.committeeSignersHash,
    32,
    "da.committeeSignersHash",
  );
  const expectedCommitteeSignersHash = bytesToHex(
    blake2b(hexToBytes(committeeVkeys.join("")), { dkLen: 32 }),
  );
  if (committeeSignersHash !== expectedCommitteeSignersHash) {
    throw new Error(
      `Deployment manifest da.committeeSignersHash mismatch: expected ${expectedCommitteeSignersHash}`,
    );
  }
  const threshold = requireInteger(da.threshold, "da.threshold", 1);
  if (threshold > committeeVkeys.length) {
    throw new Error("Deployment manifest da.threshold exceeds committee size");
  }
  const transport = requireRecord(
    da.transportProfile,
    "Deployment manifest da.transportProfile",
  );
  requireExactKeys(
    transport,
    [
      "protocolVersion",
      "runtimeManifestSchemaVersion",
      "envelopeEncoding",
      "zstdLevel",
      "limits",
      "retentionDays",
    ],
    [],
    "da.transportProfile",
  );
  if (transport.protocolVersion !== DA_TRANSPORT_PROTOCOL_VERSION) {
    throw new Error(
      "Deployment manifest da.transportProfile.protocolVersion is unsupported",
    );
  }
  if (
    transport.runtimeManifestSchemaVersion !==
    DA_RUNTIME_MANIFEST_SCHEMA_VERSION
  ) {
    throw new Error(
      "Deployment manifest da.transportProfile.runtimeManifestSchemaVersion is unsupported",
    );
  }
  if (
    transport.envelopeEncoding !== "identity" &&
    transport.envelopeEncoding !== "zstd"
  ) {
    throw new Error(
      "Deployment manifest da.transportProfile.envelopeEncoding is unsupported",
    );
  }
  requireInteger(transport.zstdLevel, "da.transportProfile.zstdLevel", 1);
  if (
    stableJson(
      normalizeDeploymentManifestJsonValueInternal(
        transport.limits,
        "Deployment manifest da.transportProfile.limits",
        false,
      ),
    ) !== stableJson(DA_TRANSPORT_LIMITS)
  ) {
    throw new Error(
      "Deployment manifest da.transportProfile.limits must exactly match canonical V1",
    );
  }
  const retentionDays = requireInteger(
    transport.retentionDays,
    "da.transportProfile.retentionDays",
    1,
  );
  // Existing >= 15-day transport-profile floor: never weakened.
  if (retentionDays < DA_TRANSPORT_LIMITS.minimumRetentionDays) {
    throw new Error(
      "Deployment manifest da.transportProfile.retentionDays is too short",
    );
  }
  // Q54: additionally bind the window to the derived challengeability horizon
  // (block maturity + worst-case proof-time bound), so deployment identity -
  // not a literal - is what the DA and proof stores enforce against.
  if (
    !retentionDaysCoverWindow(
      retentionDays,
      "Deployment manifest da.transportProfile.retentionDays",
    )
  ) {
    throw new Error(
      `Deployment manifest da.transportProfile.retentionDays must cover the canonical V1 retention window (requiredRetentionMs=${String(
        MIDGARD_RETENTION_WINDOW.requiredRetentionMs,
      )})`,
    );
  }
};

// manifestIds whose deep finalized verification already succeeded in this
// process. Reusing one is sound only because verifyDeploymentManifestIdentity
// runs uncached on every call: it re-hashes the manifest's full normalized
// content and requires manifestId to equal that hash, so a mutated manifest
// either fails identity verification outright or arrives under a new
// manifestId and misses this cache. Everything the deep pass checks is a pure
// function of that same content plus module constants.
export const VERIFIED_FINALIZED_MANIFEST_ID_CACHE_LIMIT = 64;

export const verifiedFinalizedManifestIds = new Set<string>();

export type ReferenceScriptPublicationAuthority =
  | {
      readonly kind: "publisher-signature";
      readonly expiresAtSlot: number;
      readonly publisherKeyHash: string;
    }
  | {
      readonly kind: "time-only";
      readonly expiresAtSlot: number;
    };

/**
 * Immediate publication audits trust the named publisher while its minting
 * window remains open. Historical time-only policies have no signer and still
 * require expiry before their role tokens can be treated as unique. The policy
 * identifies the publisher independently of the reference-output recipient;
 * callers publishing with a wallet can additionally check that wallet here.
 */
export const verifyReferenceScriptPublicationAuthority = (input: {
  readonly cborHex: string;
  readonly expiresAtSlot: number;
  readonly publisherAddress?: string;
  readonly postTimelockAuditRequired: boolean;
}): ReferenceScriptPublicationAuthority => {
  const cborHex = requireHex(
    input.cborHex,
    undefined,
    "referenceScriptAuthPolicy.nativeScript.cborHex",
  );
  const expiresAtSlot = requireInteger(
    input.expiresAtSlot,
    "referenceScriptAuthPolicy.nativeScript.expiresAtSlot",
  );
  const { script } = decodeMidgardNativeScript(hexToBytes(cborHex));
  const signature = script.type === "all" ? script.scripts[0] : undefined;
  const signed =
    script.type === "all" &&
    script.scripts.length === 2 &&
    signature?.type === "sig" &&
    script.scripts[1]?.type === "before";
  const deadline = signed ? script.scripts[1] : script;
  if (deadline.type !== "before") {
    throw new Error(
      "Deployment manifest reference-script authority must be an exact publisher signature AND expiry policy, or a historical time-only policy",
    );
  }
  if (deadline.slot !== BigInt(expiresAtSlot)) {
    throw new Error(
      "Deployment manifest referenceScriptAuthPolicy.nativeScript.expiresAtSlot must match the native policy CBOR",
    );
  }
  if (!signed) {
    if (input.postTimelockAuditRequired !== true) {
      throw new Error(
        "Deployment manifest time-only reference-script authority requires postTimelockAudit.required to be true",
      );
    }
    return { kind: "time-only", expiresAtSlot };
  }
  const publisherKeyHash = signature.keyHash.toString("hex");
  if (input.publisherAddress !== undefined) {
    const paymentCredential = getAddressDetails(
      input.publisherAddress,
    ).paymentCredential;
    if (
      paymentCredential?.type !== "Key" ||
      paymentCredential.hash !== publisherKeyHash
    ) {
      throw new Error(
        "Deployment manifest reference-script authority signer must match the publisherAddress payment key",
      );
    }
  }
  if (input.postTimelockAuditRequired !== false) {
    throw new Error(
      "Deployment manifest publisher-signed reference-script authority requires postTimelockAudit.required to be false; audit immediately after publication",
    );
  }
  return { kind: "publisher-signature", expiresAtSlot, publisherKeyHash };
};
