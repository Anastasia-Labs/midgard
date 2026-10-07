import {
  DA_RUNTIME_MANIFEST_SCHEMA_VERSION,
  DA_TRANSPORT_LIMITS,
  DA_TRANSPORT_PROTOCOL_VERSION,
} from "@al-ft/midgard-core/da-transport";
import {
  type DeploymentManifest,
  type DeploymentManifestCanonicalRational,
  type DeploymentManifestCardanoProtocolParameters,
  deriveDeploymentManifestCardanoProtocolParametersFromOgmios,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import { normalizeOgmiosHttpUrl } from "@al-ft/midgard-core/ogmios-slot";
import { MIDGARD_RETENTION_WINDOW } from "@al-ft/midgard-core/retention-window";
import { GENESIS_HEADER_HASH } from "@al-ft/midgard-sdk";
import { type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  computeDeploymentManifestDaCommitteeSignersHash,
  computeDeploymentManifestJsonDigest,
  normalizeDeploymentManifestJsonValue,
} from "../deployment-manifest.js";
import { deploymentEconomicsFromEnvironment } from "../environment.js";
import {
  loadRealBlueprintSha256,
  Lucid,
  NodeConfig,
  type NodeConfigDep,
} from "../services/index.js";
import { deriveOperatorDaParams } from "../transactions/initialization.js";
import { compareOutRefs } from "../tx-context.js";
import {
  configuredAvailabilityChallenge,
  type DeploymentManifestIdentityContext,
  protocolGcd,
  protocolNatural,
  protocolRecord,
} from "./contract-deployment-info.build-reference-script-out-ref-map.js";

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
    economics: deploymentEconomicsFromEnvironment(),
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

export const assertOutRefFields = (
  txHash: string,
  outputIndex: number,
): void => {
  if (!/^[0-9a-fA-F]{64}$/.test(txHash)) {
    throw new Error("hubOracleOneShot.txHash must be 32 bytes of hex");
  }
  if (!Number.isSafeInteger(outputIndex) || outputIndex < 0) {
    throw new Error(
      "hubOracleOneShot.outputIndex must be a safe non-negative integer",
    );
  }
};
