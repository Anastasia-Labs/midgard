import {
  MIDGARD_DEPLOYMENT_MANIFEST_SCHEMA_VERSION,
  type MidgardConsensusProfile,
} from ".././consensus-profile.js";
import {
  DA_RUNTIME_MANIFEST_SCHEMA_VERSION,
  DA_TRANSPORT_LIMITS,
  DA_TRANSPORT_PROTOCOL_VERSION,
} from ".././da-transport.js";
import {
  DEPLOYMENT_MANIFEST_ECONOMICS_BY_PROFILE,
  type DeploymentProfile,
  SELECTED_DEPLOYMENT_PROFILE,
} from ".././deployment-profile.js";
import { type DeploymentManifestFraudProofCatalogueIdentity } from "./catalogue-roles.js";
import {
  type DeploymentManifestEventHistoryRecipe,
  type DeploymentManifestEventHistoryRetentionAddresses,
} from "./event-history.js";
import { DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_TOKEN_NAMES } from "./reference-script-tokens.js";

export const DEPLOYMENT_MANIFEST_STEP_NAMES = Object.freeze([
  "prepareHubOracleNonce",
  "deployNodeRuntimeReferenceScripts",
  "initProtocol",
  "phasRegistration",
  "availabilityRegistration",
  "operatorRegistration",
  "operatorActivation",
] as const);

/**
 * The confirmation depth and the commit-event depth are per deployment
 * profile, so they are typed as numbers. An event is committed only below the
 * commit anchor, `commitEventDepth` blocks under the view a commit is planned
 * at.
 */
export type DeploymentManifestL1Finality = Readonly<{
  confirmationDepth: number;
  commitEventDepth: number;
  automaticRecoveryMaxDepth: 2160;
  deepRollbackPolicy: "automated_rewind_replay_incident-v1";
}>;

export const DEPLOYMENT_MANIFEST_L1_FINALITY: DeploymentManifestL1Finality =
  Object.freeze({
    confirmationDepth:
      SELECTED_DEPLOYMENT_PROFILE.l1_finality.confirmation_depth,
    commitEventDepth:
      SELECTED_DEPLOYMENT_PROFILE.l1_finality.commit_event_depth,
    automaticRecoveryMaxDepth: 2160,
    deepRollbackPolicy: "automated_rewind_replay_incident-v1",
  });

export type DeploymentManifestCanonicalRational = Readonly<{
  numerator: string;
  denominator: string;
}>;

/**
 * Exact release-bound subset used to size prover funding, collateral and
 * reference-script fees. All ledger naturals use canonical decimal strings;
 * rationals retain numerator/denominator identity and never pass through a
 * JavaScript float.
 */
export type DeploymentManifestCardanoProtocolParameters = Readonly<{
  minFeeA: string;
  minFeeB: string;
  priceMemory: DeploymentManifestCanonicalRational;
  priceSteps: DeploymentManifestCanonicalRational;
  coinsPerUtxoByte: string;
  collateralPercentage: string;
  maxCollateralInputs: string;
  maxTxSize: string;
  maxValueSize: string;
  maxTxExUnits: Readonly<{ memory: string; steps: string }>;
  referenceScriptFee: Readonly<{
    base: DeploymentManifestCanonicalRational;
    range: string;
    multiplier: DeploymentManifestCanonicalRational;
    maximumSizeBytes: string;
  }>;
}>;

export type DeploymentManifestEconomicsProfile =
  keyof typeof DEPLOYMENT_MANIFEST_ECONOMICS_BY_PROFILE;

export type DeploymentManifestEconomics =
  (typeof DEPLOYMENT_MANIFEST_ECONOMICS_BY_PROFILE)[DeploymentManifestEconomicsProfile];

export type DeploymentManifestAvailabilityChallenge = Readonly<{
  responseClasses: Readonly<{
    smallPayloadMaxBytes: 65_536;
    smallResponseWindowMs: number;
    fullPayloadMaxBytes: 67_108_864;
    fullResponseWindowMs: number;
  }>;
  responseGeometry: Readonly<{
    chunkByteLength: number;
    trancheByteLength: number;
    maxTrancheCount: number;
  }>;
  /** The pooled DA bond amounts equal the selected profile's `da_bond`. */
  daBondLovelace: number;
  /** Deploy-time; independent of the DA bond, and the only bond the fee reserve binds. */
  challengerBondLovelace: number;
  maxOpenFeeLovelace: number;
  maxPublicationFeeLovelace: number;
  maxSettlementFeeLovelace: number;
  maxCloseFeeLovelace: number;
  maxTimeoutFeeLovelace: number;
  daSlashPenaltyLovelace: number;
  daBondMinTopUpLovelace: number;
  daBondPoolFloorLovelace: number;
  challengeRecordLovelace: number;
}>;

/** Applied payload bounds for a fabricated-event family. Strings preserve the
 * exact integer parameters in JSON; no runtime default is an identity source. */
export type DeploymentManifestEventHistoryBounds = Readonly<{
  inlineLimitBytes: string;
  maxPayloadBytes: string;
  maxPayloadNodes: string;
}>;

export type DeploymentManifestContractEntry = {
  readonly refScriptUTxO: {
    readonly txHash: string;
    readonly outputIndex: number;
  } | null;
  readonly contract: {
    readonly type: "Native" | "PlutusV1" | "PlutusV2" | "PlutusV3";
    readonly cborHex: string;
  };
  readonly scriptHash: string;
  readonly fraudProofCatalogue?: DeploymentManifestFraudProofCatalogueIdentity;
  readonly eventHistoryRecipe?: DeploymentManifestEventHistoryRecipe;
  readonly eventHistoryBounds?: DeploymentManifestEventHistoryBounds;
  readonly eventHistoryRetentionAddress?: string;
  readonly eventHistoryRetentionAddresses?: DeploymentManifestEventHistoryRetentionAddresses;
};

export type DeploymentManifestStepStatus =
  | "pending"
  | "in_progress"
  | "submitted"
  | "complete"
  | "attached"
  | "failed"
  | "blocked_requires_fresh_redeploy";

/**
 * Complete structural view returned by finalized document verification.
 * Verification preserves the caller's object; this type neither freezes it
 * nor certifies current chain state or application-specific authority.
 */
export type DeploymentManifest = {
  readonly schemaVersion: typeof MIDGARD_DEPLOYMENT_MANIFEST_SCHEMA_VERSION;
  readonly manifestId: string;
  readonly consensusProfile: MidgardConsensusProfile;
  readonly consensusProfileDigest: string;
  readonly network: "Mainnet" | "Preprod" | "Preview" | "Custom";
  readonly cardanoProtocolParameters: {
    readonly snapshot: DeploymentManifestCardanoProtocolParameters;
    readonly digest: string;
  };
  readonly genesis: {
    readonly headerHash: string;
    readonly utxoSetDigest: string;
  };
  readonly createdAt: string;
  readonly updatedAt: string;
  readonly referenceScriptDeployAddress: string;
  readonly hubOracleOneShot: {
    readonly txHash: string;
    readonly outputIndex: number;
    readonly outRef: string;
    readonly status: "consumed_by_init";
  };
  readonly referenceScriptAuthPolicy: {
    readonly policyId: string;
    readonly nativeScript: {
      readonly type: "Native";
      readonly cborHex: string;
      readonly expiresAtSlot: number;
      readonly expiresAtUnixTime: number;
      readonly timelockDurationMs: number;
    };
    readonly tokenNames: Readonly<
      Record<
        keyof typeof DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_TOKEN_NAMES,
        string
      >
    >;
    readonly postTimelockAudit: {
      readonly required: boolean;
      readonly rule: string;
    };
  };
  readonly contracts: Readonly<Record<string, DeploymentManifestContractEntry>>;
  readonly referenceScripts: Readonly<
    Record<
      string,
      {
        readonly status: "confirmed";
        readonly roleUnit: string;
        readonly scriptHash: string;
        readonly outRef: string;
      }
    >
  >;
  readonly da: {
    readonly committeeVkeys: readonly string[];
    readonly committeeSignersHash: string;
    readonly threshold: number;
    readonly transportProfile: {
      readonly protocolVersion: typeof DA_TRANSPORT_PROTOCOL_VERSION;
      readonly runtimeManifestSchemaVersion: typeof DA_RUNTIME_MANIFEST_SCHEMA_VERSION;
      readonly envelopeEncoding: "identity" | "zstd";
      readonly zstdLevel: number;
      readonly limits: typeof DA_TRANSPORT_LIMITS;
      readonly retentionDays: number;
    };
  };
  readonly artifacts: {
    readonly blueprintHash: string;
  };
  readonly steps: Readonly<
    Record<
      (typeof DEPLOYMENT_MANIFEST_STEP_NAMES)[number],
      {
        // The existing verifier checks String(status); preserve its accepted
        // JSON values rather than promising a string it does not establish.
        readonly status: DeploymentManifestJsonValue;
        readonly txHash?: string;
      }
    >
  >;
  readonly validationDispute: {
    readonly version: number;
    readonly responseWindowMs: number;
    readonly maxBisectionRounds: number;
    readonly maturityMs: number;
  };
  readonly l1Finality: DeploymentManifestL1Finality;
  readonly economics: DeploymentManifestEconomics;
  readonly deploymentProfile: DeploymentProfile;
  readonly deploymentProfileDigest: string;
  readonly availabilityChallenge: DeploymentManifestAvailabilityChallenge;
};

export const DEPLOYMENT_MANIFEST_ROOT_KEYS = Object.freeze([
  "schemaVersion",
  "manifestId",
  "consensusProfile",
  "consensusProfileDigest",
  "network",
  "cardanoProtocolParameters",
  "genesis",
  "createdAt",
  "updatedAt",
  "referenceScriptDeployAddress",
  "hubOracleOneShot",
  "referenceScriptAuthPolicy",
  "contracts",
  "referenceScripts",
  "da",
  "artifacts",
  "steps",
  "validationDispute",
  "l1Finality",
  "economics",
  "deploymentProfile",
  "deploymentProfileDigest",
  "availabilityChallenge",
] as const);

export type DeploymentManifestJsonValue =
  | null
  | boolean
  | number
  | string
  | readonly DeploymentManifestJsonValue[]
  | { readonly [key: string]: DeploymentManifestJsonValue };

export const MIDGARD_DEPLOYMENT_MARKER_SCHEMA_VERSION =
  "midgard-deployment-marker-v1" as const;

export type DeploymentMarker = {
  readonly schemaVersion: typeof MIDGARD_DEPLOYMENT_MARKER_SCHEMA_VERSION;
  readonly manifestId: string;
};
