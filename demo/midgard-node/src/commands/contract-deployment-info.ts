/**
 * Builds and writes a deployment manifest for the currently configured Midgard
 * validator bundle.
 *
 * The manifest is keyed by explicit script names such as `depositMint` and
 * `depositSpend`, because many logical contracts compile to distinct scripts for
 * different purposes. Each entry records the compiled script bytes, its
 * corresponding script hash/policy id, and any matching reference-script UTxO
 * currently published in the dedicated reference-script wallet.
 */
import { createHash } from "node:crypto";
import { existsSync, readFileSync } from "node:fs";
import { dirname, resolve as resolvePath } from "node:path";
import { fileURLToPath } from "node:url";

import {
  MIDGARD_CONSENSUS_PROFILE,
  MIDGARD_CONSENSUS_PROFILE_DIGEST,
} from "@al-ft/midgard-core/consensus-profile";
import {
  DA_RUNTIME_MANIFEST_SCHEMA_VERSION,
  DA_TRANSPORT_LIMITS,
  DA_TRANSPORT_PROTOCOL_VERSION,
} from "@al-ft/midgard-core/da-transport";
import {
  DEPLOYMENT_MANIFEST_ECONOMICS_BY_PROFILE,
  DEPLOYMENT_MANIFEST_L1_FINALITY,
  type DeploymentManifest,
  type DeploymentManifestAvailabilityChallenge,
  type DeploymentManifestCanonicalRational,
  type DeploymentManifestCardanoProtocolParameters,
  type DeploymentManifestContractEntry,
  type DeploymentManifestEconomics,
  type DeploymentManifestEconomicsProfile,
  deriveDeploymentManifestCardanoProtocolParametersFromOgmios,
  makeDeploymentMarker,
  parseDeploymentManifestAvailabilityChallenge,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import { MIDGARD_RETENTION_WINDOW } from "@al-ft/midgard-core/retention-window";
import * as SDK from "@al-ft/midgard-sdk";
import {
  GENESIS_HEADER_HASH,
  type ReferenceScriptAuthPolicyDeploymentInfo,
  type ReferenceScriptAuthPolicyRef,
  type ReferenceScriptAuthTokenTarget,
  referenceScriptAuthUnit,
} from "@al-ft/midgard-sdk";
import {
  type Script,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  type DeployableScript,
  manifestDeployableScripts,
  referenceScriptRoleForContract,
} from "../deployable-scripts.js";
import {
  computeDeploymentManifestDaCommitteeSignersHash,
  computeDeploymentManifestId,
  computeDeploymentManifestJsonDigest,
  DEPLOYMENT_MANIFEST_SCHEMA_VERSION,
  normalizeDeploymentManifestJsonValue,
  parseDeploymentManifestValue,
} from "../deployment-manifest.js";
import {
  bindDeploymentRunStateToMarker,
  defaultDeploymentRunStatePath,
  loadDeploymentRunState,
  mutateDeploymentRunState,
  sha256File,
} from "../e2e/run-state.js";
import {
  contractDeploymentInfoPathOverride,
  daAvailabilityChallengeEnvironmentInput,
  deploymentEconomicsProfileFromEnvironment,
} from "../environment.js";
import { writeJsonFileAtomic } from "../files/atomic-write.js";
import { normalizeOgmiosHttpUrl } from "../local-ledger-slot.js";
import {
  loadRealBlueprintSha256,
  Lucid,
  MidgardContracts,
  NodeConfig,
  type NodeConfigDep,
} from "../services/index.js";
import {
  buildFraudProofCatalogueDeploymentInfo,
  deriveOperatorDaParams,
  fetchProtocolDeploymentStatus,
  fraudProofsToIndexedValidators,
} from "../transactions/initialization.js";
import { fetchReferenceScriptUtxosAt } from "../transactions/reference-scripts.js";
import { queryScriptRewardRegistrationProgram } from "../transactions/script-reward-registration.js";
import { compareOutRefs } from "../tx-context.js";

export type ContractDeploymentInfoRefScriptUTxO = {
  readonly txHash: string;
  readonly outputIndex: number;
};

export type ContractDeploymentInfoEntry = DeploymentManifestContractEntry;

export type ContractDeploymentInfo = {
  readonly referenceScriptAuthPolicy: ReferenceScriptAuthPolicyDeploymentInfo;
  readonly contracts: Readonly<Record<string, ContractDeploymentInfoEntry>>;
};

export { computeDeploymentManifestId, DEPLOYMENT_MANIFEST_SCHEMA_VERSION };

export type {
  DeploymentManifest,
  DeploymentManifestStepStatus,
} from "@al-ft/midgard-core/deployment-manifest-identity";

export type DeploymentManifestVerificationReport = {
  readonly ok: boolean;
  readonly manifestId?: string;
  readonly path?: string;
  readonly mismatches: readonly string[];
  readonly recommendation:
    | "attach"
    | "correct_attach_config"
    | "fresh_redeploy_required";
};

export type FinalizedDeploymentIdentity = {
  readonly path: string;
  readonly manifestId: string;
  readonly contractDeploymentInfoSha256: string;
  readonly manifest: DeploymentManifest;
};

const DEFAULT_CONTRACT_DEPLOYMENT_INFO_FILENAME =
  "contract-deployment-info.json";
const DEFAULT_CONTRACT_DEPLOYMENT_INFO_DIRECTORY_NAME = "deploymentInfo";

const resolvePackageRootFromModuleUrl = (moduleUrl: string): string => {
  let currentDir = dirname(fileURLToPath(moduleUrl));
  while (true) {
    if (existsSync(resolvePath(currentDir, "package.json"))) {
      return currentDir;
    }
    const parentDir = resolvePath(currentDir, "..");
    if (parentDir === currentDir) {
      return resolvePath(process.cwd());
    }
    currentDir = parentDir;
  }
};

type ScriptDescriptor = {
  readonly name: string;
  readonly script: Script;
  readonly scriptHash: string;
  readonly contract: ContractDeploymentInfoEntry["contract"];
  readonly referenceScriptTargetName?: ReferenceScriptAuthTokenTarget;
};

const fetchLiveReferenceScriptUtxos = (): Effect.Effect<
  readonly UTxO[],
  Error,
  Lucid
> =>
  Effect.gen(function* () {
    const lucidService = yield* Lucid;
    const referenceScriptsLucid = lucidService.referenceScriptsApi;
    const referenceScriptsAddress = lucidService.referenceScriptsAddress;
    return yield* fetchReferenceScriptUtxosAt(
      referenceScriptsLucid,
      referenceScriptsAddress,
      "contract deployment info reference-script UTxO fetch",
      `Failed to fetch reference-script UTxOs at ${referenceScriptsAddress}`,
    ).pipe(
      Effect.mapError(
        (cause) =>
          new Error("Failed to resolve contract deployment reference scripts", {
            cause,
          }),
      ),
    );
  });

export const buildReferenceScriptOutRefMap = (
  utxos: readonly UTxO[],
  descriptors: readonly ScriptDescriptor[],
  authPolicy: ReferenceScriptAuthPolicyRef,
): ReadonlyMap<string, ContractDeploymentInfoRefScriptUTxO> => {
  const byDescriptorName = new Map<
    string,
    ContractDeploymentInfoRefScriptUTxO
  >();
  for (const descriptor of descriptors) {
    if (descriptor.referenceScriptTargetName === undefined) {
      continue;
    }
    const roleUnit = referenceScriptAuthUnit(
      authPolicy.policyId,
      descriptor.referenceScriptTargetName,
    );
    const candidates = utxos.filter(
      (utxo) => (utxo.assets[roleUnit] ?? 0n) !== 0n,
    );
    if (candidates.length === 0) {
      continue;
    }
    if (candidates.length !== 1) {
      throw new Error(
        `Reference-script role ${descriptor.referenceScriptTargetName} is ambiguous: expected exactly one live ${roleUnit} UTxO, found ${candidates.length.toString()}`,
      );
    }
    const candidate = candidates[0]!;
    if (candidate.assets[roleUnit] !== 1n) {
      throw new Error(
        `Reference-script role ${descriptor.referenceScriptTargetName} must carry exactly one ${roleUnit} token`,
      );
    }
    const authPolicyUnits = Object.entries(candidate.assets).filter(
      ([unit, quantity]) =>
        unit.startsWith(authPolicy.policyId) && quantity !== 0n,
    );
    if (authPolicyUnits.length !== 1 || authPolicyUnits[0]?.[0] !== roleUnit) {
      throw new Error(
        `Reference-script role ${descriptor.referenceScriptTargetName} UTxO must carry no other ${authPolicy.policyId} role token`,
      );
    }
    if (candidate.scriptRef == null) {
      throw new Error(
        `Reference-script role ${descriptor.referenceScriptTargetName} token is not attached to a reference script`,
      );
    }
    const observedScriptHash = validatorToScriptHash(candidate.scriptRef);
    if (observedScriptHash !== descriptor.scriptHash) {
      throw new Error(
        `Reference-script role ${descriptor.referenceScriptTargetName} script hash mismatch: expected ${descriptor.scriptHash}, found ${observedScriptHash}`,
      );
    }
    byDescriptorName.set(descriptor.name, {
      txHash: candidate.txHash,
      outputIndex: candidate.outputIndex,
    });
  }
  return byDescriptorName;
};

const scriptDescriptor = ({
  contract,
  script,
  scriptHash,
  role,
}: Pick<
  DeployableScript,
  "contract" | "script" | "scriptHash" | "role"
>): ScriptDescriptor => ({
  name: contract,
  script,
  scriptHash,
  contract: {
    type: script.type,
    cborHex: script.script,
  },
  ...(role === undefined ? {} : { referenceScriptTargetName: role }),
});

/**
 * Manifest descriptors for every deployable script, in manifest order. When
 * the reference-script auth policy is known, its native script replaces the
 * bundle's placeholder as `referenceScriptAuthMint`.
 */
export const collectScriptDescriptors = (
  contracts: SDK.MidgardValidators,
  referenceScriptAuthPolicy?: ReferenceScriptAuthPolicyDeploymentInfo,
): readonly ScriptDescriptor[] =>
  manifestDeployableScripts(contracts).map((deployable) =>
    deployable.contract === "referenceScriptAuthMint" &&
    referenceScriptAuthPolicy !== undefined
      ? scriptDescriptor({
          ...deployable,
          script: {
            type: "Native",
            script: referenceScriptAuthPolicy.nativeScript.cborHex,
          },
          scriptHash: referenceScriptAuthPolicy.policyId,
        })
      : scriptDescriptor(deployable),
  );

const defaultSteps = (): DeploymentManifest["steps"] => ({
  prepareHubOracleNonce: { status: "pending" },
  deployNodeRuntimeReferenceScripts: { status: "pending" },
  initProtocol: { status: "pending" },
  phasRegistration: { status: "pending" },
  availabilityRegistration: { status: "pending" },
  operatorRegistration: { status: "pending" },
  operatorActivation: { status: "pending" },
});

const buildReferenceScriptRecords = (
  deploymentInfo: ContractDeploymentInfo,
): DeploymentManifest["referenceScripts"] => {
  const entries: [string, DeploymentManifest["referenceScripts"][string]][] =
    [];
  for (const [contractName, entry] of Object.entries(
    deploymentInfo.contracts,
  )) {
    const targetName = referenceScriptRoleForContract(contractName);
    if (targetName === undefined) {
      continue;
    }
    const refScript = entry.refScriptUTxO;
    if (refScript === null) {
      throw new Error(
        `Cannot finalize DeploymentManifestV1 without reference script ${targetName}`,
      );
    }
    entries.push([
      targetName,
      {
        status: "confirmed",
        roleUnit: referenceScriptAuthUnit(
          deploymentInfo.referenceScriptAuthPolicy.policyId,
          targetName,
        ),
        scriptHash: entry.scriptHash,
        outRef: `${refScript.txHash}#${refScript.outputIndex.toString()}`,
      },
    ]);
  }
  return Object.fromEntries(
    entries.sort(([left], [right]) => left.localeCompare(right)),
  );
};

export type DeploymentManifestBuildContext = {
  readonly network: DeploymentManifest["network"];
  readonly cardanoProtocolParameters: DeploymentManifest["cardanoProtocolParameters"];
  readonly genesis: DeploymentManifest["genesis"];
  readonly da: DeploymentManifest["da"];
  readonly artifacts: DeploymentManifest["artifacts"];
  readonly economics: DeploymentManifestEconomics;
  readonly availabilityChallenge: DeploymentManifestAvailabilityChallenge;
  readonly referenceScriptDeployAddress: string;
  readonly hubOracleOneShotTxHash: string;
  readonly hubOracleOneShotOutputIndex: number;
  readonly hubOracleOneShotStatus?: DeploymentManifest["hubOracleOneShot"]["status"];
  readonly now?: Date;
  readonly existingManifest?: DeploymentManifest;
  readonly steps?: Partial<DeploymentManifest["steps"]>;
};

export type DeploymentManifestIdentityContext = Pick<
  DeploymentManifestBuildContext,
  | "cardanoProtocolParameters"
  | "genesis"
  | "da"
  | "artifacts"
  | "economics"
  | "availabilityChallenge"
>;

const configuredDeploymentEconomics = (): DeploymentManifestEconomics =>
  DEPLOYMENT_MANIFEST_ECONOMICS_BY_PROFILE[
    deploymentEconomicsProfileFromEnvironment()
  ];

const configuredAvailabilityChallenge =
  (): DeploymentManifestAvailabilityChallenge =>
    parseDeploymentManifestAvailabilityChallenge(
      daAvailabilityChallengeEnvironmentInput(
        (name) => `${name} must be an explicit positive decimal integer`,
      ),
    );

const protocolRecord = (
  value: unknown,
  field: string,
): Record<string, unknown> => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    throw new Error(`${field} must be an object`);
  }
  return value as Record<string, unknown>;
};

const protocolNatural = (value: unknown, field: string): string => {
  if (typeof value === "bigint" && value >= 0n) return value.toString(10);
  if (typeof value === "number" && Number.isSafeInteger(value) && value >= 0) {
    return value.toString(10);
  }
  if (typeof value === "string" && /^(?:0|[1-9][0-9]*)$/u.test(value)) {
    return value;
  }
  throw new Error(`${field} must be a canonical natural`);
};

const protocolGcd = (left: bigint, right: bigint): bigint => {
  let a = left < 0n ? -left : left;
  let b = right < 0n ? -right : right;
  while (b !== 0n) {
    const remainder = a % b;
    a = b;
    b = remainder;
  }
  return a;
};

const protocolRational = (
  value: unknown,
  field: string,
): DeploymentManifestCanonicalRational => {
  let numerator: bigint;
  let denominator: bigint;
  if (typeof value === "string" && /^[0-9]+\/[1-9][0-9]*$/u.test(value)) {
    const [rawNumerator, rawDenominator] = value.split("/") as [string, string];
    numerator = BigInt(rawNumerator);
    denominator = BigInt(rawDenominator);
  } else if (
    (typeof value === "number" && Number.isFinite(value) && value >= 0) ||
    (typeof value === "string" &&
      /^(?:0|[1-9][0-9]*)(?:\.[0-9]+)?$/u.test(value))
  ) {
    const decimal = typeof value === "number" ? value.toString() : value;
    if (/e/i.test(decimal)) {
      throw new Error(`${field} must not use exponent notation`);
    }
    const [whole, fractional = ""] = decimal.split(".") as [string, string?];
    denominator = 10n ** BigInt(fractional.length);
    numerator = BigInt(`${whole}${fractional}`);
  } else {
    throw new Error(`${field} must be a nonnegative exact rational`);
  }
  if (denominator <= 0n)
    throw new Error(`${field} denominator must be positive`);
  const divisor = protocolGcd(numerator, denominator);
  return Object.freeze({
    numerator: (numerator / divisor).toString(10),
    denominator: (denominator / divisor).toString(10),
  });
};

const sameRational = (
  left: DeploymentManifestCanonicalRational,
  right: DeploymentManifestCanonicalRational,
): boolean =>
  left.numerator === right.numerator && left.denominator === right.denominator;

const exactProtocolParameterSnapshot = (
  providerValue: unknown,
  rawOgmiosValue: unknown,
): DeploymentManifestCardanoProtocolParameters => {
  const provider = protocolRecord(providerValue, "Lucid protocol parameters");
  const snapshot =
    deriveDeploymentManifestCardanoProtocolParametersFromOgmios(rawOgmiosValue);
  const providerChecks: readonly [string, string][] = Object.freeze([
    [protocolNatural(provider.minFeeA, "provider.minFeeA"), snapshot.minFeeA],
    [protocolNatural(provider.minFeeB, "provider.minFeeB"), snapshot.minFeeB],
    [
      protocolNatural(provider.maxTxSize, "provider.maxTxSize"),
      snapshot.maxTxSize,
    ],
    [
      protocolNatural(provider.maxValSize, "provider.maxValSize"),
      snapshot.maxValueSize,
    ],
    [
      protocolNatural(provider.maxTxExMem, "provider.maxTxExMem"),
      snapshot.maxTxExUnits.memory,
    ],
    [
      protocolNatural(provider.maxTxExSteps, "provider.maxTxExSteps"),
      snapshot.maxTxExUnits.steps,
    ],
    [
      protocolNatural(provider.coinsPerUtxoByte, "provider.coinsPerUtxoByte"),
      snapshot.coinsPerUtxoByte,
    ],
    [
      protocolNatural(
        provider.collateralPercentage,
        "provider.collateralPercentage",
      ),
      snapshot.collateralPercentage,
    ],
    [
      protocolNatural(
        provider.maxCollateralInputs,
        "provider.maxCollateralInputs",
      ),
      snapshot.maxCollateralInputs,
    ],
  ]);
  if (providerChecks.some(([observed, expected]) => observed !== expected)) {
    throw new Error("Lucid and raw Ogmios protocol parameters disagree");
  }
  if (
    !sameRational(
      protocolRational(provider.priceMem, "provider.priceMem"),
      snapshot.priceMemory,
    ) ||
    !sameRational(
      protocolRational(provider.priceStep, "provider.priceStep"),
      snapshot.priceSteps,
    ) ||
    !sameRational(
      protocolRational(
        provider.minFeeRefScriptCostPerByte,
        "provider.minFeeRefScriptCostPerByte",
      ),
      snapshot.referenceScriptFee.base,
    )
  ) {
    throw new Error(
      "Lucid and raw Ogmios rational protocol parameters disagree",
    );
  }
  return snapshot;
};

export const cardanoProtocolParametersIdentityFromProvider = async (
  provider: {
    readonly getProtocolParameters: () => Promise<unknown>;
  },
  rawOgmiosProtocolParameters: unknown,
): Promise<DeploymentManifest["cardanoProtocolParameters"]> => {
  const snapshot = exactProtocolParameterSnapshot(
    await provider.getProtocolParameters(),
    rawOgmiosProtocolParameters,
  );
  return {
    snapshot,
    digest: computeDeploymentManifestJsonDigest(snapshot),
  };
};

export const queryLocalOgmiosProtocolParameters = async (
  ogmiosUrl: string,
  fetchImpl: typeof fetch = fetch,
): Promise<unknown> => {
  const response = await fetchImpl(normalizeOgmiosHttpUrl(ogmiosUrl), {
    method: "POST",
    headers: { "content-type": "application/json" },
    body: JSON.stringify({
      jsonrpc: "2.0",
      method: "queryLedgerState/protocolParameters",
      id: "midgard-deployment-protocol-parameters-v1",
    }),
    signal: AbortSignal.timeout(30_000),
  });
  const body = await response.text();
  if (!response.ok) {
    throw new Error(
      `Ogmios protocol-parameter query failed with HTTP ${response.status.toString()}`,
    );
  }
  let payload: unknown;
  try {
    payload = JSON.parse(body) as unknown;
  } catch (cause) {
    throw new Error("Ogmios protocol-parameter response is not JSON", {
      cause,
    });
  }
  const envelope = protocolRecord(
    payload,
    "Ogmios protocol parameters response",
  );
  if (
    envelope.jsonrpc !== "2.0" ||
    envelope.id !== "midgard-deployment-protocol-parameters-v1" ||
    Object.prototype.hasOwnProperty.call(envelope, "error") ||
    !Object.prototype.hasOwnProperty.call(envelope, "result")
  ) {
    throw new Error("Ogmios protocol-parameter response identity is invalid");
  }
  return payload;
};

const genesisUtxoIdentitySnapshot = (
  utxos: readonly UTxO[],
): ReturnType<typeof normalizeDeploymentManifestJsonValue> =>
  normalizeDeploymentManifestJsonValue(
    [...utxos].sort(compareOutRefs).map((utxo) => ({
      txHash: utxo.txHash,
      outputIndex: utxo.outputIndex,
      address: utxo.address,
      assets: Object.fromEntries(
        Object.entries(utxo.assets)
          .sort(([left], [right]) => left.localeCompare(right))
          .map(([unit, amount]) => [unit, amount.toString(10)]),
      ),
      datumHash: utxo.datumHash ?? null,
      datum: utxo.datum ?? null,
      scriptRef:
        utxo.scriptRef == null
          ? null
          : {
              type: utxo.scriptRef.type,
              script: utxo.scriptRef.script,
            },
    })),
    "genesisUtxos",
  );

/**
 * The DA transport profile the deployment commits to. Its retention window is
 * the canonical deployment horizon, not the node's `RETENTION_DAYS`: that
 * setting only switches local wall-clock pruning (0 disables it) and must
 * itself cover this window.
 */
export const deploymentDaTransportProfile = (
  nodeConfig: Pick<
    NodeConfigDep,
    "MIDGARD_DA_PAYLOAD_ENVELOPE" | "MIDGARD_DA_ZSTD_LEVEL"
  >,
): DeploymentManifestIdentityContext["da"]["transportProfile"] => ({
  protocolVersion: DA_TRANSPORT_PROTOCOL_VERSION,
  runtimeManifestSchemaVersion: DA_RUNTIME_MANIFEST_SCHEMA_VERSION,
  envelopeEncoding: nodeConfig.MIDGARD_DA_PAYLOAD_ENVELOPE,
  zstdLevel: nodeConfig.MIDGARD_DA_ZSTD_LEVEL,
  limits: DA_TRANSPORT_LIMITS,
  retentionDays: MIDGARD_RETENTION_WINDOW.retentionDays,
});

export const buildDeploymentManifestIdentityContextProgram: Effect.Effect<
  DeploymentManifestIdentityContext,
  Error,
  Lucid | NodeConfig
> = Effect.gen(function* () {
  const nodeConfig = yield* NodeConfig;
  const lucidService = yield* Lucid;
  const cardanoProtocolParameters = yield* Effect.tryPromise({
    try: async () => {
      const provider = lucidService.api.config().provider;
      if (provider === undefined) {
        throw new Error("Lucid has no configured Cardano provider");
      }
      const rawOgmiosProtocolParameters =
        await queryLocalOgmiosProtocolParameters(nodeConfig.L1_OGMIOS_KEY);
      return cardanoProtocolParametersIdentityFromProvider(
        provider,
        rawOgmiosProtocolParameters,
      );
    },
    catch: (cause) =>
      new Error(
        `Failed to obtain the trusted Cardano protocol-parameter snapshot: ${String(cause)}`,
      ),
  });
  const daParams = yield* deriveOperatorDaParams(nodeConfig).pipe(
    Effect.mapError(
      (cause) =>
        new Error(
          `Failed to derive deployment-manifest DA identity: ${String(cause)}`,
        ),
    ),
  );
  const committeeVkeys = daParams.committee.match(/[0-9a-f]{64}/gu) ?? [];
  if (committeeVkeys.join("") !== daParams.committee) {
    return yield* Effect.fail(
      new Error(
        "Failed to split the packed DA committee into exact 32-byte verification keys",
      ),
    );
  }
  const threshold = Number(daParams.da_threshold);
  if (!Number.isSafeInteger(threshold) || threshold <= 0) {
    return yield* Effect.fail(
      new Error("DA threshold does not fit the V1 manifest integer envelope"),
    );
  }
  const blueprintHash = yield* loadRealBlueprintSha256();
  const genesisSnapshot = genesisUtxoIdentitySnapshot(nodeConfig.GENESIS_UTXOS);
  return {
    economics: configuredDeploymentEconomics(),
    availabilityChallenge: configuredAvailabilityChallenge(),
    cardanoProtocolParameters,
    genesis: {
      headerHash: GENESIS_HEADER_HASH,
      utxoSetDigest: computeDeploymentManifestJsonDigest(genesisSnapshot),
    },
    da: {
      committeeVkeys,
      committeeSignersHash:
        computeDeploymentManifestDaCommitteeSignersHash(committeeVkeys),
      threshold,
      transportProfile: deploymentDaTransportProfile(nodeConfig),
    },
    artifacts: {
      blueprintHash,
    },
  };
});

const assertOutRefFields = (txHash: string, outputIndex: number): void => {
  if (!/^[0-9a-fA-F]{64}$/.test(txHash)) {
    throw new Error("hubOracleOneShot.txHash must be 32 bytes of hex");
  }
  if (!Number.isSafeInteger(outputIndex) || outputIndex < 0) {
    throw new Error(
      "hubOracleOneShot.outputIndex must be a safe non-negative integer",
    );
  }
};

const withManifestId = (
  manifest: Omit<DeploymentManifest, "manifestId">,
): DeploymentManifest => ({
  ...manifest,
  manifestId: computeDeploymentManifestId(manifest),
});

/**
 * Builds the sole canonical V1 manifest and re-parses it before return so
 * missing contracts, tuple drift, and dispute-schedule drift fail closed.
 */
export const buildDeploymentManifest = (
  deploymentInfo: ContractDeploymentInfo,
  context: DeploymentManifestBuildContext,
): DeploymentManifest => {
  assertOutRefFields(
    context.hubOracleOneShotTxHash,
    context.hubOracleOneShotOutputIndex,
  );
  const nowIso = (context.now ?? new Date()).toISOString();
  const referenceScripts = buildReferenceScriptRecords(deploymentInfo);
  const hubOracleOneShotStatus =
    context.hubOracleOneShotStatus ??
    context.existingManifest?.hubOracleOneShot.status;
  if (hubOracleOneShotStatus !== "consumed_by_init") {
    throw new Error(
      "Cannot finalize DeploymentManifestV1 before the hub-oracle one-shot is consumed by initialization",
    );
  }
  const baseSteps = {
    ...defaultSteps(),
    prepareHubOracleNonce: { status: "complete" as const },
    deployNodeRuntimeReferenceScripts: {
      status: "complete" as const,
    },
    ...(context.existingManifest?.steps ?? {}),
    ...(context.steps ?? {}),
  };
  const manifest = withManifestId({
    schemaVersion: DEPLOYMENT_MANIFEST_SCHEMA_VERSION,
    consensusProfile: MIDGARD_CONSENSUS_PROFILE,
    consensusProfileDigest: MIDGARD_CONSENSUS_PROFILE_DIGEST,
    network: context.network,
    cardanoProtocolParameters: context.cardanoProtocolParameters,
    genesis: context.genesis,
    createdAt: context.existingManifest?.createdAt ?? nowIso,
    updatedAt: context.existingManifest?.updatedAt ?? nowIso,
    referenceScriptDeployAddress: context.referenceScriptDeployAddress,
    hubOracleOneShot: {
      txHash: context.hubOracleOneShotTxHash.toLowerCase(),
      outputIndex: context.hubOracleOneShotOutputIndex,
      outRef: `${context.hubOracleOneShotTxHash.toLowerCase()}#${context.hubOracleOneShotOutputIndex.toString()}`,
      status: hubOracleOneShotStatus,
    },
    referenceScriptAuthPolicy: deploymentInfo.referenceScriptAuthPolicy,
    contracts: deploymentInfo.contracts,
    referenceScripts,
    da: context.da,
    artifacts: context.artifacts,
    steps: baseSteps,
    validationDispute: {
      version: MIDGARD_CONSENSUS_PROFILE.validationDisputeVersion,
      responseWindowMs:
        MIDGARD_CONSENSUS_PROFILE.limits.validationDisputeResponseWindowMs,
      maxBisectionRounds:
        MIDGARD_CONSENSUS_PROFILE.limits.maxValidationBisectionRounds,
      maturityMs: MIDGARD_CONSENSUS_PROFILE.limits.blockMaturityMs,
    },
    l1Finality: DEPLOYMENT_MANIFEST_L1_FINALITY,
    economics: context.economics,
    availabilityChallenge: context.availabilityChallenge,
  });
  return parseDeploymentManifestValue(manifest);
};

export const parseDeploymentManifest = (value: unknown): DeploymentManifest =>
  parseDeploymentManifestValue(value);

export const readDeploymentManifestFile = (
  outputPath: string,
): DeploymentManifest => {
  const resolvedOutputPath = normalizeOutputPath(outputPath);
  const parsed = JSON.parse(readFileSync(resolvedOutputPath, "utf8"));
  return parseDeploymentManifest(parsed);
};

export const readFinalizedDeploymentIdentity = (
  outputPath: string,
): FinalizedDeploymentIdentity => {
  const resolvedOutputPath = normalizeOutputPath(outputPath);
  const raw = readFileSync(resolvedOutputPath);
  const parsed = JSON.parse(raw.toString("utf8"));
  const manifest = parseDeploymentManifest(parsed);
  return {
    path: resolvedOutputPath,
    manifestId: manifest.manifestId,
    contractDeploymentInfoSha256: createHash("sha256")
      .update(raw)
      .digest("hex"),
    manifest,
  };
};

export const verifyDeploymentManifestAgainstConfig = (
  manifest: DeploymentManifest,
  context: {
    readonly network: string;
    readonly referenceScriptDeployAddress: string;
    readonly hubOracleOneShotTxHash: string;
    readonly hubOracleOneShotOutputIndex: number;
    readonly economicsProfile: DeploymentManifestEconomicsProfile;
    readonly path?: string;
  },
): DeploymentManifestVerificationReport => {
  const mismatches: string[] = [];
  if (manifest.network !== context.network) {
    mismatches.push(
      `network manifest=${manifest.network} config=${context.network}`,
    );
  }
  if (
    manifest.referenceScriptDeployAddress !==
    context.referenceScriptDeployAddress
  ) {
    mismatches.push(
      `referenceScriptDeployAddress manifest=${manifest.referenceScriptDeployAddress} config=${context.referenceScriptDeployAddress}`,
    );
  }
  if (
    manifest.hubOracleOneShot.txHash !==
    context.hubOracleOneShotTxHash.toLowerCase()
  ) {
    mismatches.push(
      `hubOracleOneShot.txHash manifest=${manifest.hubOracleOneShot.txHash} config=${context.hubOracleOneShotTxHash}`,
    );
  }
  if (
    manifest.hubOracleOneShot.outputIndex !==
    context.hubOracleOneShotOutputIndex
  ) {
    mismatches.push(
      `hubOracleOneShot.outputIndex manifest=${manifest.hubOracleOneShot.outputIndex.toString()} config=${context.hubOracleOneShotOutputIndex.toString()}`,
    );
  }
  if (manifest.economics.profile !== context.economicsProfile) {
    mismatches.push(
      `economics.profile manifest=${manifest.economics.profile} config=${context.economicsProfile}`,
    );
  }
  return {
    ok: mismatches.length === 0,
    manifestId: manifest.manifestId,
    path: context.path,
    mismatches,
    recommendation:
      mismatches.length === 0 ? "attach" : "correct_attach_config",
  };
};

export const configuredContractDeploymentInfoPath = (): string => {
  const configuredPath = contractDeploymentInfoPathOverride();
  return configuredPath === undefined
    ? defaultContractDeploymentInfoOutputPath()
    : normalizeOutputPath(configuredPath);
};

export const verifyConfiguredDeploymentManifestProgram: Effect.Effect<
  DeploymentManifestVerificationReport,
  Error,
  NodeConfig
> = Effect.gen(function* () {
  const nodeConfig = yield* NodeConfig;
  const path = configuredContractDeploymentInfoPath();
  const manifest = yield* Effect.try({
    try: () => readDeploymentManifestFile(path),
    catch: (cause) =>
      new Error(
        `Failed to read V1 deployment manifest at ${path}: ${String(cause)}`,
      ),
  });
  return verifyDeploymentManifestAgainstConfig(manifest, {
    network: nodeConfig.NETWORK,
    referenceScriptDeployAddress: nodeConfig.L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS,
    hubOracleOneShotTxHash: nodeConfig.HUB_ORACLE_ONE_SHOT_TX_HASH,
    hubOracleOneShotOutputIndex: nodeConfig.HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX,
    economicsProfile: nodeConfig.MIDGARD_DEPLOYMENT_ECONOMICS_PROFILE,
    path,
  });
});

export const verifyConfiguredDeploymentManifestIfPresentProgram: Effect.Effect<
  DeploymentManifestVerificationReport | null,
  Error,
  NodeConfig
> = Effect.gen(function* () {
  const path = configuredContractDeploymentInfoPath();
  if (!existsSync(path)) {
    return null;
  }
  return yield* verifyConfiguredDeploymentManifestProgram;
});

export const buildContractDeploymentInfoFromContracts = (
  contracts: SDK.MidgardValidators,
  referenceScriptAuthPolicy: ReferenceScriptAuthPolicyDeploymentInfo,
  referenceScriptOutRefs: ReadonlyMap<
    string,
    ContractDeploymentInfoRefScriptUTxO
  > = new Map(),
  fraudProofCatalogue?: SDK.FraudProofCatalogueDeploymentInfo,
): ContractDeploymentInfo =>
  Object.freeze({
    referenceScriptAuthPolicy,
    contracts: Object.fromEntries(
      collectScriptDescriptors(contracts, referenceScriptAuthPolicy).map(
        (descriptor) => {
          const history =
            descriptor.name === "fraudProofFabricatedDeposit"
              ? contracts.fraudProofContracts.fabricatedDeposit.history
              : descriptor.name === "fraudProofFabricatedWithdrawal"
                ? contracts.fraudProofContracts.fabricatedWithdrawal.history
                : descriptor.name === "fraudProofTransitionTrace"
                  ? contracts.fraudProofContracts.transitionTrace.history
                  : undefined;
          const historyRecipe =
            descriptor.name === "depositMint"
              ? SDK.requireEventHistoryContracts(contracts).deposit.recipe
              : descriptor.name === "withdrawalMint"
                ? SDK.requireEventHistoryContracts(contracts).withdrawal.recipe
                : undefined;
          return [
            descriptor.name,
            {
              refScriptUTxO:
                referenceScriptOutRefs.get(descriptor.name) ?? null,
              contract: descriptor.contract,
              scriptHash: descriptor.scriptHash,
              ...(historyRecipe === undefined
                ? {}
                : {
                    eventHistoryRecipe: {
                      kind: historyRecipe.kind,
                      hubPolicyId: historyRecipe.hubPolicyId,
                      initializationNonce: {
                        txHash: historyRecipe.initializationNonce.transactionId,
                        outputIndex: Number(
                          historyRecipe.initializationNonce.outputIndex,
                        ),
                      },
                      protectionDurationMs:
                        historyRecipe.protectionDurationMs.toString(),
                      bounds: {
                        inlineLimitBytes:
                          historyRecipe.inlineLimitBytes.toString(),
                        maxPayloadBytes:
                          historyRecipe.maxPayloadBytes.toString(),
                        maxPayloadNodes:
                          historyRecipe.maxPayloadNodes.toString(),
                      },
                    },
                  }),
              ...(history === undefined
                ? {}
                : {
                    ...("retentionAddresses" in history
                      ? {
                          eventHistoryRetentionAddresses:
                            history.retentionAddresses,
                        }
                      : {
                          eventHistoryRetentionAddress:
                            history.retentionAddress,
                        }),
                    eventHistoryBounds: {
                      inlineLimitBytes: history.inlineLimitBytes.toString(),
                      maxPayloadBytes: history.maxPayloadBytes.toString(),
                      maxPayloadNodes: history.maxPayloadNodes.toString(),
                    },
                  }),
              ...(descriptor.name === "fraudProofCatalogueMint" &&
              fraudProofCatalogue !== undefined
                ? { fraudProofCatalogue }
                : {}),
            } satisfies ContractDeploymentInfoEntry,
          ];
        },
      ),
    ),
  });

const resolveLiveContractDeploymentInfoProgram = (
  referenceScriptAuthPolicy: ReferenceScriptAuthPolicyDeploymentInfo,
): Effect.Effect<ContractDeploymentInfo, Error, Lucid | MidgardContracts> =>
  Effect.gen(function* () {
    const contracts = yield* MidgardContracts;
    const descriptors = collectScriptDescriptors(
      contracts,
      referenceScriptAuthPolicy,
    );
    const referenceScriptWalletUtxos = yield* fetchLiveReferenceScriptUtxos();
    const referenceScriptOutRefs = buildReferenceScriptOutRefMap(
      referenceScriptWalletUtxos,
      descriptors,
      referenceScriptAuthPolicy,
    );
    const fraudProofCatalogue = yield* buildFraudProofCatalogueDeploymentInfo(
      fraudProofsToIndexedValidators(contracts.fraudProofs),
    );
    return buildContractDeploymentInfoFromContracts(
      contracts,
      referenceScriptAuthPolicy,
      referenceScriptOutRefs,
      fraudProofCatalogue,
    );
  });

export const buildContractDeploymentInfoProgram = (
  contracts: SDK.MidgardValidators,
  referenceScriptUtxos: readonly UTxO[],
  referenceScriptAuthPolicy: ReferenceScriptAuthPolicyDeploymentInfo,
): Effect.Effect<ContractDeploymentInfo, Error> =>
  Effect.gen(function* () {
    const descriptors = collectScriptDescriptors(
      contracts,
      referenceScriptAuthPolicy,
    );
    const referenceScriptOutRefs = buildReferenceScriptOutRefMap(
      referenceScriptUtxos,
      descriptors,
      referenceScriptAuthPolicy,
    );
    const fraudProofCatalogue = yield* buildFraudProofCatalogueDeploymentInfo(
      fraudProofsToIndexedValidators(contracts.fraudProofs),
    );
    return buildContractDeploymentInfoFromContracts(
      contracts,
      referenceScriptAuthPolicy,
      referenceScriptOutRefs,
      fraudProofCatalogue,
    );
  });

export const defaultContractDeploymentInfoOutputPath = (): string =>
  resolvePath(
    resolvePackageRootFromModuleUrl(import.meta.url),
    DEFAULT_CONTRACT_DEPLOYMENT_INFO_DIRECTORY_NAME,
    DEFAULT_CONTRACT_DEPLOYMENT_INFO_FILENAME,
  );

const normalizeOutputPath = (outputPath: string): string => {
  const normalized = outputPath.trim();
  if (normalized.length === 0) {
    throw new Error("Contract deployment info output path must not be empty.");
  }
  return resolvePath(normalized);
};

const readReferenceScriptAuthPolicyForLiveWrite = async (
  outputPath: string,
): Promise<ReferenceScriptAuthPolicyDeploymentInfo> => {
  const resolvedOutputPath = normalizeOutputPath(outputPath);
  if (existsSync(resolvedOutputPath)) {
    return readDeploymentManifestFile(resolvedOutputPath)
      .referenceScriptAuthPolicy;
  }
  const runStatePath = defaultDeploymentRunStatePath();
  const runState = await loadDeploymentRunState(runStatePath);
  const policy = runState?.identity.referenceScriptAuthPolicy;
  if (policy === undefined) {
    throw new Error(
      `Deployment run state at "${runStatePath}" is missing identity.referenceScriptAuthPolicy`,
    );
  }
  return SDK.referenceScriptAuthPolicyDeploymentInfo(
    SDK.referenceScriptAuthPolicyFromDeploymentInfo(policy),
  );
};

export const writeContractDeploymentInfoFileProgram = (
  outputPath: string,
  deploymentInfo: ContractDeploymentInfo,
): Effect.Effect<string, Error> =>
  Effect.tryPromise({
    try: async () => {
      const resolvedOutputPath = normalizeOutputPath(outputPath);
      await writeJsonFileAtomic(resolvedOutputPath, deploymentInfo);
      return resolvedOutputPath;
    },
    catch: (cause) =>
      new Error(
        `Failed to write contract deployment info file: ${String(cause)}`,
      ),
  });

export type LiveContractDeploymentInfoWriteOptions = {
  readonly steps?: Partial<DeploymentManifest["steps"]>;
  readonly hubOracleOneShotStatus?: DeploymentManifest["hubOracleOneShot"]["status"];
};

const formatDeploymentManifestVerificationReport = (
  report: DeploymentManifestVerificationReport,
): string =>
  `recommendation=${report.recommendation}; mismatches=[${report.mismatches.join(
    "; ",
  )}]`;

const buildLiveDeploymentManifestProgram = (
  outputPath: string,
  options: LiveContractDeploymentInfoWriteOptions = {},
): Effect.Effect<
  DeploymentManifest,
  Error,
  Lucid | MidgardContracts | NodeConfig
> =>
  Effect.gen(function* () {
    const nodeConfig = yield* NodeConfig;
    const identityContext =
      yield* buildDeploymentManifestIdentityContextProgram;
    const referenceScriptAuthPolicy = yield* Effect.tryPromise({
      try: () => readReferenceScriptAuthPolicyForLiveWrite(outputPath),
      catch: (cause) =>
        new Error(
          `Failed to read existing reference-script auth policy metadata: ${String(cause)}`,
        ),
    });
    const deploymentInfo = yield* resolveLiveContractDeploymentInfoProgram(
      referenceScriptAuthPolicy,
    );
    const existingManifest = yield* Effect.sync(() => {
      try {
        return readDeploymentManifestFile(outputPath);
      } catch {
        return undefined;
      }
    });
    const finalizationRequested =
      options.hubOracleOneShotStatus === "consumed_by_init" &&
      options.steps?.initProtocol?.status === "complete";
    if (existingManifest === undefined && !finalizationRequested) {
      return yield* Effect.fail(
        new Error(
          "A first DeploymentManifestV1 may be created only after initialization and reference-script publication are complete",
        ),
      );
    }
    const lucidService = yield* Lucid;
    const liveContracts = yield* MidgardContracts;
    const availabilityAccounts = yield* Effect.forEach(
      Object.values(liveContracts.availabilityChallenge.yields),
      (validator) =>
        queryScriptRewardRegistrationProgram(
          lucidService.api,
          validator.withdrawalScript,
        ),
    );
    const availabilityRegistered = availabilityAccounts.every(
      (account) => account.registered,
    );
    if (finalizationRequested && !availabilityRegistered) {
      return yield* Effect.fail(
        new Error(
          "Cannot finalize deployment manifest before all availability yield reward accounts are registered",
        ),
      );
    }
    const requestedSteps = finalizationRequested
      ? {
          prepareHubOracleNonce: {
            status: "complete" as const,
            txHash: nodeConfig.HUB_ORACLE_ONE_SHOT_TX_HASH.toLowerCase(),
          },
          deployNodeRuntimeReferenceScripts: {
            status: "complete" as const,
          },
          ...options.steps,
          availabilityRegistration: { status: "complete" as const },
        }
      : {
          ...options.steps,
          availabilityRegistration: {
            status: availabilityRegistered
              ? ("complete" as const)
              : ("pending" as const),
          },
        };
    const deploymentManifest = buildDeploymentManifest(deploymentInfo, {
      network: nodeConfig.NETWORK,
      ...identityContext,
      referenceScriptDeployAddress:
        nodeConfig.L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS,
      hubOracleOneShotTxHash: nodeConfig.HUB_ORACLE_ONE_SHOT_TX_HASH,
      hubOracleOneShotOutputIndex: nodeConfig.HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX,
      existingManifest,
      steps: requestedSteps,
      hubOracleOneShotStatus: options.hubOracleOneShotStatus,
    });
    const verification = verifyDeploymentManifestAgainstConfig(
      deploymentManifest,
      {
        network: nodeConfig.NETWORK,
        referenceScriptDeployAddress:
          nodeConfig.L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS,
        hubOracleOneShotTxHash: nodeConfig.HUB_ORACLE_ONE_SHOT_TX_HASH,
        hubOracleOneShotOutputIndex:
          nodeConfig.HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX,
        economicsProfile: nodeConfig.MIDGARD_DEPLOYMENT_ECONOMICS_PROFILE,
        path: outputPath,
      },
    );
    if (!verification.ok) {
      return yield* Effect.fail(
        new Error(
          `Refusing to write deployment manifest with configuration drift: ${formatDeploymentManifestVerificationReport(
            verification,
          )}`,
        ),
      );
    }
    return deploymentManifest;
  });

export const writeLiveContractDeploymentInfoProgram = (
  outputPath: string,
  options: LiveContractDeploymentInfoWriteOptions = {},
): Effect.Effect<string, Error, Lucid | MidgardContracts | NodeConfig> =>
  Effect.gen(function* () {
    const deploymentManifest = yield* buildLiveDeploymentManifestProgram(
      outputPath,
      options,
    );
    const marker = makeDeploymentMarker(deploymentManifest.manifestId);
    const runStatePath = defaultDeploymentRunStatePath();
    const runState = yield* Effect.tryPromise({
      try: () => loadDeploymentRunState(runStatePath),
      catch: (cause) =>
        new Error(`Failed to inspect deployment run-state identity`, {
          cause,
        }),
    });
    if (
      runState?.identity.deploymentMarker !== undefined &&
      runState.identity.deploymentMarker.manifestId !== marker.manifestId
    ) {
      return yield* Effect.fail(
        new Error(
          `Refusing to replace final deployment manifest ${runState.identity.deploymentMarker.manifestId} with ${marker.manifestId}; start an explicit fresh deployment run instead`,
        ),
      );
    }
    const manifestPath = yield* writeContractDeploymentInfoFileProgram(
      outputPath,
      deploymentManifest,
    );
    if (runState !== null) {
      const manifestSha256 = yield* Effect.tryPromise({
        try: () => sha256File(manifestPath),
        catch: (cause) =>
          new Error(`Failed to hash final deployment manifest`, { cause }),
      });
      yield* Effect.tryPromise({
        try: () =>
          mutateDeploymentRunState(
            runStatePath,
            () => {
              throw new Error(
                "Deployment run state disappeared before final marker binding",
              );
            },
            (current) =>
              bindDeploymentRunStateToMarker(current, {
                marker,
                manifestPath,
                manifestSha256,
              }),
          ),
        catch: (cause) =>
          new Error(`Failed to bind deployment run state to final manifest`, {
            cause,
          }),
      });
    }
    return manifestPath;
  });

export type ReconcileInitializedDeploymentManifestOptions = {
  readonly outputPath: string;
  readonly initTxHash: string;
};

export type ReconcileInitializedDeploymentManifestSummary = {
  readonly status: "complete";
  readonly path: string;
  readonly manifestId: string;
  readonly initTxHash: string;
  readonly hubOracleOutRef: string;
  readonly referenceScriptsConfirmed: number;
};

export const reconcileInitializedDeploymentManifestProgram = ({
  outputPath,
  initTxHash,
}: ReconcileInitializedDeploymentManifestOptions): Effect.Effect<
  ReconcileInitializedDeploymentManifestSummary,
  Error,
  Lucid | MidgardContracts | NodeConfig
> =>
  Effect.gen(function* () {
    const lucidService = yield* Lucid;
    const contracts = yield* MidgardContracts;
    const normalizedInitTxHash = initTxHash.toLowerCase();
    const deploymentStatus = yield* fetchProtocolDeploymentStatus(
      lucidService.api,
      contracts,
    ).pipe(
      Effect.mapError(
        (cause) =>
          new Error("Failed to inspect live protocol deployment status", {
            cause,
          }),
      ),
    );
    if (!deploymentStatus.complete) {
      return yield* Effect.fail(
        new Error(
          `Cannot reconcile deployment manifest for an incomplete protocol deployment: missing_components=[${deploymentStatus.missingComponents.join(
            ",",
          )}],state_queue_healthy=${deploymentStatus.stateQueueTopology.healthy.toString()}`,
        ),
      );
    }
    const hubOracleWitness = deploymentStatus.hubOracleWitness;
    if (hubOracleWitness === null) {
      return yield* Effect.fail(
        new Error(
          "Cannot reconcile deployment manifest without hub-oracle witness",
        ),
      );
    }
    const liveInitTxHash = hubOracleWitness.txHash.toLowerCase();
    if (liveInitTxHash !== normalizedInitTxHash) {
      return yield* Effect.fail(
        new Error(
          `Init transaction mismatch: live hub-oracle witness was created by ${liveInitTxHash}, expected ${normalizedInitTxHash}`,
        ),
      );
    }

    const path = yield* writeLiveContractDeploymentInfoProgram(outputPath, {
      hubOracleOneShotStatus: "consumed_by_init",
      steps: {
        initProtocol: {
          status: "complete",
          txHash: normalizedInitTxHash,
        },
      },
    });
    const manifest = yield* Effect.try({
      try: () => readDeploymentManifestFile(path),
      catch: (cause) =>
        new Error(
          `Failed to read reconciled deployment manifest: ${String(cause)}`,
        ),
    });
    return {
      status: "complete" as const,
      path,
      manifestId: manifest.manifestId,
      initTxHash: normalizedInitTxHash,
      hubOracleOutRef: `${hubOracleWitness.txHash}#${hubOracleWitness.outputIndex.toString()}`,
      referenceScriptsConfirmed: Object.values(
        manifest.referenceScripts,
      ).filter((record) => record.status === "confirmed").length,
    };
  });
