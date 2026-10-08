import {
  DA_RUNTIME_MANIFEST_SCHEMA_VERSION,
  DA_TRANSPORT_LIMITS,
  DA_TRANSPORT_PROTOCOL_VERSION,
} from "@al-ft/midgard-core/da-transport";
import {
  type DeploymentManifest,
  type DeploymentManifestCardanoProtocolParameters,
  deriveDeploymentManifestCardanoProtocolParametersFromLedger,
} from "@al-ft/midgard-core/deployment-manifest-identity";
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
} from "./contract-deployment-info.build-reference-script-out-ref-map.js";

/**
 * The manifest's protocol-parameter identity: the exact snapshot and its
 * digest.
 */
export const cardanoProtocolParametersIdentity = (
  snapshot: DeploymentManifestCardanoProtocolParameters,
): DeploymentManifest["cardanoProtocolParameters"] => ({
  snapshot,
  digest: computeDeploymentManifestJsonDigest(snapshot),
});

/**
 * The protocol-parameter identity from the local node's ledger: its raw
 * `protocol_params` answer, read once, with exact rationals.
 */
export const cardanoProtocolParametersIdentityFromLedger = async (
  readProtocolParametersCbor: () => Promise<Uint8Array>,
): Promise<DeploymentManifest["cardanoProtocolParameters"]> =>
  cardanoProtocolParametersIdentity(
    deriveDeploymentManifestCardanoProtocolParametersFromLedger(
      await readProtocolParametersCbor(),
    ),
  );

/** The Lucid service's reader of the ledger's raw protocol parameters. */
export const ledgerProtocolParametersReader = (
  lucidService: Pick<Lucid, "l1ProtocolParametersCbor">,
): (() => Promise<Uint8Array>) => {
  const read = lucidService.l1ProtocolParametersCbor;
  if (read === undefined)
    throw new Error(
      "The Lucid service has no reader of the local node's protocol parameters",
    );
  return read;
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
    try: () =>
      cardanoProtocolParametersIdentityFromLedger(
        ledgerProtocolParametersReader(lucidService),
      ),
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
