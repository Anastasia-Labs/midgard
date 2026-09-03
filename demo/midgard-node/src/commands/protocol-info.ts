import {
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
} from "@al-ft/midgard-core/codec";
import {
  assertMidgardConsensusReleaseReady,
  isMidgardConsensusProfile,
  MIDGARD_CONSENSUS_LIMITS,
  MIDGARD_CONSENSUS_PROFILE,
  MIDGARD_PROTOCOL_INFO_API_VERSION,
  type MidgardConsensusProfile,
} from "@al-ft/midgard-core/consensus-profile-v1";
import {
  type DeploymentMarker,
  parseDeploymentMarker,
} from "@al-ft/midgard-core/deployment-manifest-identity-v1";
import type { Network } from "@lucid-evolution/lucid";

import { positiveSafeInteger } from "../artifact-schema.js";

export const PROTOCOL_INFO_API_VERSION = MIDGARD_PROTOCOL_INFO_API_VERSION;

type ProtocolInfoConfig = {
  readonly NETWORK: Network;
  readonly MIN_FEE_A: bigint;
  readonly MIN_FEE_B: bigint;
  readonly MAX_SUBMIT_TX_CBOR_BYTES: number;
  readonly VALIDATION_STRICTNESS_PROFILE: string;
};

type ProtocolInfoCommon = {
  readonly deploymentMarker: DeploymentMarker;
  readonly network: Network;
  readonly currentSlot: string;
  readonly codecSupportedScriptLanguages: typeof MIDGARD_SUPPORTED_SCRIPT_LANGUAGES;
  readonly protocolFeeParameters: {
    readonly minFeeA: string;
    readonly minFeeB: string;
  };
  readonly submissionLimits: {
    readonly maxSubmitTxCborBytes: number;
  };
  readonly validation: {
    readonly strictnessProfile: string;
    readonly localValidationIsAuthoritative: false;
  };
};

export type ProtocolInfo = ProtocolInfoCommon & {
  readonly apiVersion: typeof PROTOCOL_INFO_API_VERSION;
  readonly midgardNativeTxVersion: 1;
  readonly consensusProfile: MidgardConsensusProfile;
  readonly supportedScriptLanguages: typeof MIDGARD_SUPPORTED_SCRIPT_LANGUAGES;
};

const stringifyNonNegativeBigInt = (
  value: bigint,
  fieldName: string,
): string => {
  if (value < 0n) {
    throw new Error(`${fieldName} must be non-negative`);
  }
  return value.toString(10);
};

const stringifyCurrentSlot = (slot: number | bigint): string => {
  if (typeof slot === "bigint") {
    return stringifyNonNegativeBigInt(slot, "currentSlot");
  }
  if (!Number.isSafeInteger(slot) || slot < 0) {
    throw new Error(`currentSlot must be a non-negative safe integer`);
  }
  return slot.toString(10);
};

export const encodeProtocolInfo = ({
  nodeConfig,
  currentSlot,
  deploymentMarker,
  consensusProfile = MIDGARD_CONSENSUS_PROFILE,
}: {
  readonly nodeConfig: ProtocolInfoConfig;
  readonly currentSlot: number | bigint;
  readonly deploymentMarker: unknown;
  readonly consensusProfile?: MidgardConsensusProfile;
}): ProtocolInfo => {
  if (!isMidgardConsensusProfile(consensusProfile)) {
    throw new Error("Unsupported consensus profile");
  }
  const configuredMax = positiveSafeInteger(
    nodeConfig.MAX_SUBMIT_TX_CBOR_BYTES,
    "MAX_SUBMIT_TX_CBOR_BYTES",
  );
  if (configuredMax > MIDGARD_CONSENSUS_LIMITS.maxTxCanonicalCborBytes) {
    throw new Error(
      "MAX_SUBMIT_TX_CBOR_BYTES must not exceed the canonical V1 transaction bound",
    );
  }
  const common: ProtocolInfoCommon = {
    deploymentMarker: parseDeploymentMarker(deploymentMarker),
    network: nodeConfig.NETWORK,
    currentSlot: stringifyCurrentSlot(currentSlot),
    codecSupportedScriptLanguages: MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
    protocolFeeParameters: {
      minFeeA: stringifyNonNegativeBigInt(nodeConfig.MIN_FEE_A, "MIN_FEE_A"),
      minFeeB: stringifyNonNegativeBigInt(nodeConfig.MIN_FEE_B, "MIN_FEE_B"),
    },
    submissionLimits: {
      maxSubmitTxCborBytes: configuredMax,
    },
    validation: {
      strictnessProfile: nodeConfig.VALIDATION_STRICTNESS_PROFILE,
      localValidationIsAuthoritative: false,
    },
  };
  assertMidgardConsensusReleaseReady();
  return {
    ...common,
    apiVersion: PROTOCOL_INFO_API_VERSION,
    midgardNativeTxVersion: Number(MIDGARD_NATIVE_TX_VERSION) as 1,
    consensusProfile: MIDGARD_CONSENSUS_PROFILE,
    supportedScriptLanguages: MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
  };
};
