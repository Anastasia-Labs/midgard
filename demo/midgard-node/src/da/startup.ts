import { runDaZstdStartupSelfTest } from "@al-ft/midgard-core/da-compression";
import {
  assertDeploymentMarkerV1Matches,
  makeDeploymentMarkerV1,
} from "@al-ft/midgard-core/deployment-manifest-identity-v1";
import { Effect } from "effect";

import {
  ContractDeploymentIdentity,
  type ContractDeploymentIdentityValue,
  DatabaseInitializationError,
  Lucid,
  MidgardContracts,
  NodeConfig,
} from "../services/index.js";
import { fetchDaParamsUtxo } from "../transactions/da-attestation.js";
import {
  assertDaEnvelopeCapabilityQuorum,
  loadDaProducerPublicationManifestFromEnv,
} from "./libp2p-producer.js";

export const assertDaThresholdCompatible = (
  transportThreshold: number,
  onChainThreshold: bigint,
): void => {
  if (BigInt(transportThreshold) < onChainThreshold) {
    throw new DatabaseInitializationError({
      message:
        "DA transport threshold is lower than the on-chain attestation threshold",
      cause: `transport_threshold=${transportThreshold.toString()},on_chain_da_threshold=${onChainThreshold.toString()}`,
    });
  }
};

export const assertDaDeploymentIdentityCompatible = (
  daManifestId: string,
  contractIdentity: ContractDeploymentIdentityValue,
): void => {
  if (
    contractIdentity.kind !== "manifest" ||
    contractIdentity.manifestId === undefined ||
    contractIdentity.deploymentMarker === undefined
  ) {
    throw new DatabaseInitializationError({
      message:
        "DA publication requires a verified deployment-manifest contract source",
      cause: "selected contract source has no deployment manifest identity",
    });
  }
  try {
    assertDeploymentMarkerV1Matches(
      contractIdentity.deploymentMarker,
      makeDeploymentMarkerV1(daManifestId),
      "DA runtime manifest",
    );
  } catch {
    throw new DatabaseInitializationError({
      message: "DA and contract deployment manifest identities do not match",
      cause: `da_manifest_id=${daManifestId},contract_manifest_id=${contractIdentity.manifestId}`,
    });
  }
};

export type DaHardeningStartupPreflight = {
  readonly envelopeMode: "identity" | "zstd";
  readonly manifest: Awaited<
    ReturnType<typeof loadDaProducerPublicationManifestFromEnv>
  >;
};

/** Performs every local fail-closed check before startup may touch L1. */
export const prepareDaHardeningStartup = Effect.gen(function* () {
  const config = yield* NodeConfig;
  const envelopeMode = config.MIDGARD_DA_PAYLOAD_ENVELOPE;
  if (envelopeMode === "zstd") {
    yield* Effect.tryPromise({
      try: runDaZstdStartupSelfTest,
      catch: (cause) =>
        new DatabaseInitializationError({
          message:
            "DA zstd startup capability assertion failed; Node.js >=22.15.0 is required",
          cause,
        }),
    });
  }
  const manifest = yield* Effect.tryPromise({
    try: () => loadDaProducerPublicationManifestFromEnv(),
    catch: (cause) =>
      new DatabaseInitializationError({
        message: "Failed to load DA manifest for startup threshold assertion",
        cause,
      }),
  });
  const contractIdentity = yield* ContractDeploymentIdentity;
  yield* Effect.try({
    try: () =>
      assertDaDeploymentIdentityCompatible(
        manifest.contractDeploymentManifestId,
        contractIdentity,
      ),
    catch: (cause) => cause as DatabaseInitializationError,
  });
  return { envelopeMode, manifest } satisfies DaHardeningStartupPreflight;
});

/** Runs provider-backed DA checks using the manifest retained by preflight. */
export const assertDaHardeningProviderStartup = ({
  envelopeMode,
  manifest,
}: DaHardeningStartupPreflight) =>
  Effect.gen(function* () {
    const lucid = yield* Lucid;
    const contracts = yield* MidgardContracts;
    const daParams = yield* fetchDaParamsUtxo(lucid.api, contracts).pipe(
      Effect.mapError(
        (cause) =>
          new DatabaseInitializationError({
            message: "Failed to read on-chain DA threshold during startup",
            cause,
          }),
      ),
    );
    yield* Effect.try({
      try: () =>
        assertDaThresholdCompatible(
          manifest.threshold,
          daParams.datum.da_threshold,
        ),
      catch: (cause) => cause as DatabaseInitializationError,
    });
    yield* Effect.tryPromise({
      try: () =>
        assertDaEnvelopeCapabilityQuorum({
          manifest,
          mode: envelopeMode,
        }),
      catch: (cause) =>
        new DatabaseInitializationError({
          message: `DA ${envelopeMode} envelope capability quorum failed at startup`,
          cause,
        }),
    });
  });

/**
 * Enforces local deployment identity before database, protocol, or provider
 * startup effects can execute.
 */
export const runDaIdentityGatedStartupSequence = <
  E1,
  R1,
  E2,
  R2,
  E3,
  R3,
  E4,
  R4,
>({
  localPreflight,
  initializeDatabase,
  initializeProtocol,
  providerAssertions,
}: {
  readonly localPreflight: Effect.Effect<DaHardeningStartupPreflight, E1, R1>;
  readonly initializeDatabase: Effect.Effect<void, E2, R2>;
  readonly initializeProtocol: Effect.Effect<void, E3, R3>;
  readonly providerAssertions: (
    preflight: DaHardeningStartupPreflight,
  ) => Effect.Effect<void, E4, R4>;
}): Effect.Effect<void, E1 | E2 | E3 | E4, R1 | R2 | R3 | R4> =>
  Effect.gen(function* () {
    const preflight = yield* localPreflight;
    yield* initializeDatabase;
    yield* initializeProtocol;
    yield* providerAssertions(preflight);
  });
