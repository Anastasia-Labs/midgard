import { createHash } from "node:crypto";
import { existsSync, readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

import { type MidgardConsensusProfile } from "@al-ft/midgard-core/consensus-profile";
import type { DeploymentManifest } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  type DeploymentManifestL1Finality,
  type DeploymentMarker,
  parseDeploymentManifestAvailabilityChallenge,
  parseDeploymentManifestEventHistoryBounds,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  SELECTED_DEPLOYMENT_PROFILE,
  verifyDeploymentProfileBinding,
} from "@al-ft/midgard-core/deployment-profile";
import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import * as SDK from "@al-ft/midgard-sdk";
import { MintingPolicy, mintingPolicyToId } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { parseDeploymentManifestValue } from "../deployment-manifest.js";
import {
  defaultDeploymentRunStatePath,
  loadDeploymentRunState,
} from "../e2e/run-state.js";
import {
  contractDeploymentInfoPathOverride,
  daAvailabilityChallengeEnvironmentInput,
  realBlueprintPathOverride,
} from "../environment.js";

/**
 * Contract-loading service for Midgard validators.
 *
 * This module can either expose the always-succeeds bundle for test flows or
 * derive the real script set from a blueprint, applying protocol parameters
 * where required.
 */
type Blueprint = SDK.FaultProofBlueprint;

export type ContractDeploymentIdentityValue = {
  readonly kind: "manifest" | "derived";
  readonly manifestId?: string;
  readonly deploymentMarker?: DeploymentMarker;
  readonly path?: string;
  readonly consensusProfile: MidgardConsensusProfile;
  readonly l1Finality?: DeploymentManifestL1Finality;
  /** Exact parser-admitted manifest; absent for derived/dev contract bundles. */
  readonly manifest?: DeploymentManifest;
};

export type MidgardContractRuntimeValue = {
  readonly contracts: SDK.MidgardValidators;
  readonly identity: ContractDeploymentIdentityValue;
};

export const availabilityParametersFromManifest = (
  value: unknown,
): SDK.DaAvailabilityParameters => {
  const parsed = parseDeploymentManifestAvailabilityChallenge(value);
  return SDK.daAvailabilityParameters({
    responseGeometry: SDK.availabilityResponseGeometry(parsed.responseGeometry),
    daBondLovelace: BigInt(parsed.daBondLovelace),
    daSlashPenaltyLovelace: BigInt(parsed.daSlashPenaltyLovelace),
    daBondMinTopUpLovelace: BigInt(parsed.daBondMinTopUpLovelace),
    daBondPoolFloorLovelace: BigInt(parsed.daBondPoolFloorLovelace),
    challengeRecordLovelace: BigInt(parsed.challengeRecordLovelace),
    challengerBondLovelace: BigInt(parsed.challengerBondLovelace),
    maxOpenFeeLovelace: BigInt(parsed.maxOpenFeeLovelace),
    maxPublicationFeeLovelace: BigInt(parsed.maxPublicationFeeLovelace),
    maxSettlementFeeLovelace: BigInt(parsed.maxSettlementFeeLovelace),
    maxCloseFeeLovelace: BigInt(parsed.maxCloseFeeLovelace),
    maxTimeoutFeeLovelace: BigInt(parsed.maxTimeoutFeeLovelace),
  });
};

export const availabilityParametersFromExplicitEnvironment =
  (): SDK.DaAvailabilityParameters =>
    availabilityParametersFromManifest(
      daAvailabilityChallengeEnvironmentInput(
        (name) =>
          `${name} must be set to an explicit positive integer before deriving Q58 scripts without a finalized manifest`,
      ),
    );

/** No implicit payload-bound profile when deriving a fresh deployment. */
export const eventHistoryBoundsFromExplicitEnvironment =
  (): SDK.EventHistoryPayloadBounds => {
    const bounds = parseDeploymentManifestEventHistoryBounds({
      inlineLimitBytes: process.env.MIDGARD_EVENT_HISTORY_INLINE_LIMIT_BYTES,
      maxPayloadBytes: process.env.MIDGARD_EVENT_HISTORY_MAX_PAYLOAD_BYTES,
      maxPayloadNodes: process.env.MIDGARD_EVENT_HISTORY_MAX_PAYLOAD_NODES,
    });
    return {
      inlineLimitBytes: BigInt(bounds.inlineLimitBytes),
      maxPayloadBytes: BigInt(bounds.maxPayloadBytes),
      maxPayloadNodes: BigInt(bounds.maxPayloadNodes),
    };
  };

export const eventHistoryProtectionDurationFromExplicitEnvironment =
  (): bigint => {
    const value = process.env.MIDGARD_EVENT_HISTORY_PROTECTION_DURATION_MS;
    if (
      value === undefined ||
      !/^[1-9][0-9]{0,15}$/u.test(value) ||
      BigInt(value) > BigInt(Number.MAX_SAFE_INTEGER)
    )
      throw new Error(
        "MIDGARD_EVENT_HISTORY_PROTECTION_DURATION_MS must be an explicit positive safe integer",
      );
    return BigInt(value);
  };

const moduleDir = path.dirname(fileURLToPath(import.meta.url));

const DEFAULT_REAL_BLUEPRINT_CANDIDATES = [
  path.resolve(moduleDir, "../../../../onchain/aiken/plutus.json"),
  path.resolve(moduleDir, "../../../onchain/aiken/plutus.json"),
  path.resolve(process.cwd(), "../../onchain/aiken/plutus.json"),
  path.resolve(process.cwd(), "onchain/aiken/plutus.json"),
] as const;

/**
 * Cached real blueprint loaded from either `MIDGARD_REAL_BLUEPRINT_PATH` or
 * the canonical onchain Aiken build output.
 */
export let cachedRealBlueprint:
  | {
      readonly path: string;
      readonly blueprint: Blueprint;
    }
  | undefined;

const parseBlueprint = (raw: string, sourcePath: string): Blueprint => {
  try {
    return SDK.parseFaultProofBlueprint(JSON.parse(raw) as unknown);
  } catch (cause) {
    throw new Error(`Invalid blueprint at "${sourcePath}"`, { cause });
  }
};

const resolveDefaultRealBlueprintPath = (): string => {
  for (const candidate of new Set(DEFAULT_REAL_BLUEPRINT_CANDIDATES)) {
    if (existsSync(candidate)) {
      return candidate;
    }
  }

  throw new Error(
    `Failed to locate canonical real blueprint. Looked in: ${DEFAULT_REAL_BLUEPRINT_CANDIDATES.join(", ")}`,
  );
};

const resolveConfiguredRealBlueprintPath = (): string =>
  realBlueprintPathOverride() ?? resolveDefaultRealBlueprintPath();

const verifyBlueprintDeploymentProfile = (
  blueprintPath: string,
  raw: Buffer,
): void => {
  const binding = JSON.parse(
    readFileSync(`${blueprintPath}.deployment.json`, "utf8"),
  );
  verifyDeploymentProfileBinding(
    binding.profile,
    binding.profileDigest,
    SELECTED_DEPLOYMENT_PROFILE.network,
  );
  if (
    binding.blueprintHash !== createHash("sha256").update(raw).digest("hex")
  ) {
    throw new Error(
      "Blueprint does not match its deployment profile build record",
    );
  }
};

export const loadRealBlueprintSha256 = (): Effect.Effect<string, Error> =>
  Effect.try({
    try: () => {
      const blueprintPath = resolveConfiguredRealBlueprintPath();
      const raw = readFileSync(blueprintPath);
      verifyBlueprintDeploymentProfile(blueprintPath, raw);
      parseBlueprint(raw.toString("utf8"), blueprintPath);
      return createHash("sha256").update(raw).digest("hex");
    },
    catch: (cause) =>
      new Error(
        `Failed to hash canonical real blueprint: ${formatUnknownError(cause)}`,
      ),
  });

/**
 * Loads the real-contract blueprint, optionally honoring an override path from
 * the environment.
 */
export const loadRealBlueprint = (): Effect.Effect<Blueprint, Error> =>
  Effect.try({
    try: () => {
      const blueprintPath = resolveConfiguredRealBlueprintPath();

      if (cachedRealBlueprint?.path === blueprintPath) {
        return cachedRealBlueprint.blueprint;
      }

      const raw = readFileSync(blueprintPath);
      verifyBlueprintDeploymentProfile(blueprintPath, raw);
      const blueprint = parseBlueprint(raw.toString("utf8"), blueprintPath);

      cachedRealBlueprint = {
        path: blueprintPath,
        blueprint,
      };
      return blueprint;
    },
    catch: (cause) =>
      new Error(`Failed to load real blueprint: ${formatUnknownError(cause)}`),
  });

export const loadReferenceScriptAuthValidator = (): Effect.Effect<
  SDK.MintingValidator,
  Error
> =>
  Effect.tryPromise({
    try: async () => {
      const runStatePath = defaultDeploymentRunStatePath();
      const runState = await loadDeploymentRunState(runStatePath);
      if (runState === null) {
        throw new Error(`Deployment run state does not exist: ${runStatePath}`);
      }
      const referenceScriptAuthPolicy =
        runState.identity.referenceScriptAuthPolicy;
      const policyId =
        typeof referenceScriptAuthPolicy?.policyId === "string"
          ? referenceScriptAuthPolicy.policyId
          : "";
      const cborHex =
        referenceScriptAuthPolicy?.nativeScript?.type === "Native" &&
        typeof referenceScriptAuthPolicy.nativeScript.cborHex === "string"
          ? referenceScriptAuthPolicy.nativeScript.cborHex
          : "";
      if (!/^[0-9a-fA-F]{56}$/.test(policyId)) {
        throw new Error(
          `Deployment run state at "${runStatePath}" does not contain a valid identity.referenceScriptAuthPolicy.policyId`,
        );
      }
      if (!/^[0-9a-fA-F]+$/.test(cborHex)) {
        throw new Error(
          `Deployment run state at "${runStatePath}" does not contain a valid identity.referenceScriptAuthPolicy.nativeScript.cborHex`,
        );
      }
      const mintingScript: MintingPolicy = {
        type: "Native",
        script: cborHex,
      };
      const derivedPolicyId = mintingPolicyToId(mintingScript);
      if (derivedPolicyId !== policyId.toLowerCase()) {
        throw new Error(
          `referenceScriptAuthPolicy policy id mismatch: configured=${policyId}, derived=${derivedPolicyId}`,
        );
      }
      return {
        mintingScriptCBOR: cborHex,
        mintingScript,
        policyId: derivedPolicyId,
      };
    },
    catch: (cause) =>
      new Error(
        `Failed to load reference-script auth policy id from deployment run state: ${formatUnknownError(
          cause,
        )}`,
      ),
  });

export const parseRuntimeDeploymentManifest = (
  raw: unknown,
): DeploymentManifest => parseDeploymentManifestValue(raw);

export const readRuntimeDeploymentManifestFile = (
  deploymentInfoPath: string,
  required: boolean,
):
  | {
      readonly path: string;
      readonly manifest: DeploymentManifest;
    }
  | undefined => {
  if (!existsSync(deploymentInfoPath)) {
    if (required) {
      throw new Error(
        `Configured deployment manifest does not exist: ${deploymentInfoPath}`,
      );
    }
    return undefined;
  }
  const parsed = JSON.parse(
    readFileSync(deploymentInfoPath, "utf8"),
  ) as unknown;
  return {
    path: deploymentInfoPath,
    manifest: parseRuntimeDeploymentManifest(parsed),
  };
};

export const readConfiguredDeploymentManifest = () => {
  const configuredPath = contractDeploymentInfoPathOverride();
  if (configuredPath === undefined) {
    return undefined;
  }
  return readRuntimeDeploymentManifestFile(path.resolve(configuredPath), true);
};

export const requireManifestString = (
  value: unknown,
  field: string,
  sourcePath: string,
): string => {
  if (typeof value === "string" && value.length > 0) {
    return value;
  }
  throw new Error(`Deployment manifest at "${sourcePath}" is missing ${field}`);
};
