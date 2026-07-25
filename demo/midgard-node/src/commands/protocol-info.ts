import {
  MIDGARD_NATIVE_TX_V1_VERSION,
  MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
} from "@al-ft/midgard-core/codec";
import {
  assertMidgardConsensusV1ReleaseReady,
  isMidgardConsensusProfileV1,
  MIDGARD_CONSENSUS_LIMITS_V1,
  MIDGARD_CONSENSUS_PROFILE_V1,
  MIDGARD_PROTOCOL_INFO_V1_API_VERSION,
  type MidgardConsensusProfileV1,
} from "@al-ft/midgard-core/consensus-profile-v1";
import type { Network } from "@lucid-evolution/lucid";

export const PROTOCOL_INFO_API_VERSION =
  MIDGARD_PROTOCOL_INFO_V1_API_VERSION;

type ProtocolInfoConfig = {
  readonly NETWORK: Network;
  readonly MIN_FEE_A: bigint;
  readonly MIN_FEE_B: bigint;
  readonly MAX_SUBMIT_TX_CBOR_BYTES: number;
  readonly VALIDATION_STRICTNESS_PROFILE: string;
};

type ProtocolInfoCommon = {
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
  readonly consensusProfile: MidgardConsensusProfileV1;
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

const asPositiveSafeInteger = (value: number, fieldName: string): number => {
  if (!Number.isSafeInteger(value) || value <= 0) {
    throw new Error(`${fieldName} must be a positive safe integer`);
  }
  return value;
};

export const encodeProtocolInfo = ({
  nodeConfig,
  currentSlot,
  consensusProfile = MIDGARD_CONSENSUS_PROFILE_V1,
}: {
  readonly nodeConfig: ProtocolInfoConfig;
  readonly currentSlot: number | bigint;
  readonly consensusProfile?: MidgardConsensusProfileV1;
}): ProtocolInfo => {
  if (!isMidgardConsensusProfileV1(consensusProfile)) {
    throw new Error("Unsupported consensus profile");
  }
  const configuredMax = asPositiveSafeInteger(
    nodeConfig.MAX_SUBMIT_TX_CBOR_BYTES,
    "MAX_SUBMIT_TX_CBOR_BYTES",
  );
  if (configuredMax !== MIDGARD_CONSENSUS_LIMITS_V1.maxTxCanonicalCborBytes) {
    throw new Error(
      "MAX_SUBMIT_TX_CBOR_BYTES must equal the canonical V1 transaction bound",
    );
  }
  const common: ProtocolInfoCommon = {
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
  assertMidgardConsensusV1ReleaseReady();
  return {
    ...common,
    apiVersion: PROTOCOL_INFO_API_VERSION,
    midgardNativeTxVersion: Number(
      MIDGARD_NATIVE_TX_V1_VERSION,
    ) as 1,
    consensusProfile: MIDGARD_CONSENSUS_PROFILE_V1,
    supportedScriptLanguages: MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
  };
};
