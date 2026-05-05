import type { Network } from "@lucid-evolution/lucid";
import {
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
} from "@/midgard-tx-codec/index.js";

export const PROTOCOL_INFO_API_VERSION = 1 as const;

type ProtocolInfoConfig = {
  readonly NETWORK: Network;
  readonly MIN_FEE_A: bigint;
  readonly MIN_FEE_B: bigint;
  readonly MAX_SUBMIT_TX_CBOR_BYTES: number;
  readonly VALIDATION_STRICTNESS_PROFILE: string;
};

export type ProtocolInfo = {
  readonly apiVersion: typeof PROTOCOL_INFO_API_VERSION;
  readonly network: Network;
  readonly midgardNativeTxVersion: number;
  readonly currentSlot: string;
  readonly supportedScriptLanguages: typeof MIDGARD_SUPPORTED_SCRIPT_LANGUAGES;
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
}: {
  readonly nodeConfig: ProtocolInfoConfig;
  readonly currentSlot: number | bigint;
}): ProtocolInfo => ({
  apiVersion: PROTOCOL_INFO_API_VERSION,
  network: nodeConfig.NETWORK,
  midgardNativeTxVersion: asPositiveSafeInteger(
    Number(MIDGARD_NATIVE_TX_VERSION),
    "MIDGARD_NATIVE_TX_VERSION",
  ),
  currentSlot: stringifyCurrentSlot(currentSlot),
  supportedScriptLanguages: MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
  protocolFeeParameters: {
    minFeeA: stringifyNonNegativeBigInt(nodeConfig.MIN_FEE_A, "MIN_FEE_A"),
    minFeeB: stringifyNonNegativeBigInt(nodeConfig.MIN_FEE_B, "MIN_FEE_B"),
  },
  submissionLimits: {
    maxSubmitTxCborBytes: asPositiveSafeInteger(
      nodeConfig.MAX_SUBMIT_TX_CBOR_BYTES,
      "MAX_SUBMIT_TX_CBOR_BYTES",
    ),
  },
  validation: {
    strictnessProfile: nodeConfig.VALIDATION_STRICTNESS_PROFILE,
    localValidationIsAuthoritative: false,
  },
});
