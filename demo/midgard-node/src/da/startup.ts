import { runDaZstdStartupSelfTest } from "@al-ft/midgard-core/da-compression";
import {
  assertDeploymentMarkerMatches,
  makeDeploymentMarker,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import { Effect } from "effect";

import {
  ContractDeploymentIdentity,
  type ContractDeploymentIdentityValue,
  DatabaseInitializationError,
  Lucid,
  MidgardContracts,
  NodeConfig,
} from "../services/index.js";
import {
  DA_CAPABILITY_QUORUM_PENDING,
  DA_PROVIDER_ASSERTIONS_UNAVAILABLE,
} from "../services/startup-waiting.js";
import { fetchDaParamsUtxo } from "../transactions/da-attestation.js";
import {
  createDaLibp2pProducerProbeTransport,
  type DaEnvelopeCapabilityMode,
  type DaEnvelopeCapabilityPeerResult,
  type DaProducerPublicationManifest,
  loadDaProducerPublicationManifestFromEnv,
  probeDaEnvelopeCapabilities,
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
    assertDeploymentMarkerMatches(
      contractIdentity.deploymentMarker,
      makeDeploymentMarker(daManifestId),
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
    yield* assertDaEnvelopeCapabilityQuorumOnStartup(manifest, envelopeMode);
  });

/**
 * Too few committee peers answered capably yet, but enough have not answered
 * at all that the quorum can still form: peers starting, restarting or not
 * yet dialable. The startup provider retry waits and probes again.
 */
export class DaCapabilityQuorumPendingError extends Error {
  readonly retryable = true;
  override readonly name = "DaCapabilityQuorumPendingError";
}

/**
 * The reason the startup DA provider assertions wait under after `error`:
 * `da_capability_quorum_pending` while the quorum is still forming,
 * `da_provider_assertions_unavailable` for any other retryable failure.
 */
export const daProviderAssertionsWaitReason = (error: unknown): string => {
  let link: unknown = error;
  // A bounded walk down the cause chain.
  for (
    let depth = 0;
    depth < 16 && typeof link === "object" && link !== null;
    depth += 1
  ) {
    if (link instanceof DaCapabilityQuorumPendingError)
      return DA_CAPABILITY_QUORUM_PENDING;
    link = (link as { cause?: unknown }).cause;
  }
  return DA_PROVIDER_ASSERTIONS_UNAVAILABLE;
};

/**
 * Enough committee signers answered and rejected this node's DA envelope
 * capabilities that no quorum can form: a configuration disagreement no
 * waiting clears.
 */
export class DaCapabilityMismatchError extends Error {
  readonly retryable = false;
  override readonly name = "DaCapabilityMismatchError";
}

/**
 * Judges one capability probe round. A signer counts against the quorum only
 * when every one of its peers answered and rejected; an unreachable or
 * undecodable peer may still answer capably later.
 */
export const classifyDaEnvelopeCapabilityQuorum = (
  manifest: Pick<DaProducerPublicationManifest, "threshold">,
  mode: DaEnvelopeCapabilityMode,
  results: readonly DaEnvelopeCapabilityPeerResult[],
): DaCapabilityQuorumPendingError | DaCapabilityMismatchError | undefined => {
  const signers = new Set(results.map((result) => result.signerIndex));
  const capable = new Set(
    results.filter((result) => result.capable).map((r) => r.signerIndex),
  );
  if (capable.size >= manifest.threshold) {
    return undefined;
  }
  const rejected = results.filter(
    (result) => !result.capable && result.capabilities !== undefined,
  );
  const unanswered = results.filter(
    (result) => !result.capable && result.capabilities === undefined,
  );
  const rejectingSigners = [...signers].filter((signer) =>
    results
      .filter((result) => result.signerIndex === signer)
      .every((result) => !result.capable && result.capabilities !== undefined),
  );
  const describe = (peers: readonly DaEnvelopeCapabilityPeerResult[]) =>
    peers
      .map(
        (result) =>
          `${result.peerId}[${result.signerIndex.toString()}]=${result.error ?? "incapable"}`,
      )
      .join(",");
  const counts = `capable_signers=${capable.size.toString()},threshold=${manifest.threshold.toString()},rejecting_signers=${rejectingSigners.length.toString()},unanswered_peers=${unanswered.length.toString()}`;
  if (signers.size - rejectingSigners.length < manifest.threshold) {
    // Only the rejections are named: an unreachable peer's transport error
    // text must not make this refusal look transient.
    return new DaCapabilityMismatchError(
      `DA ${mode} envelope capabilities rejected by the committee: ${counts},rejections=${describe(rejected)}`,
    );
  }
  return new DaCapabilityQuorumPendingError(
    `DA ${mode} envelope capability quorum not yet reached: ${counts},peers=${describe([...rejected, ...unanswered])}`,
  );
};

const probeOverDialOnlyTransport = async (
  manifest: DaProducerPublicationManifest,
  mode: DaEnvelopeCapabilityMode,
): Promise<readonly DaEnvelopeCapabilityPeerResult[]> => {
  const transport = await createDaLibp2pProducerProbeTransport(manifest, {
    mode: "dial-only",
  });
  try {
    return await probeDaEnvelopeCapabilities({ manifest, mode, transport });
  } finally {
    await transport.close?.();
  }
};

/**
 * One startup capability-quorum round: a quorum still forming fails with a
 * retryable cause the startup provider retry waits out; a committee that
 * answered and rejected fails terminally.
 */
export const assertDaEnvelopeCapabilityQuorumOnStartup = (
  manifest: DaProducerPublicationManifest,
  mode: DaEnvelopeCapabilityMode,
  probe: (
    manifest: DaProducerPublicationManifest,
    mode: DaEnvelopeCapabilityMode,
  ) => Promise<
    readonly DaEnvelopeCapabilityPeerResult[]
  > = probeOverDialOnlyTransport,
): Effect.Effect<void, DatabaseInitializationError> =>
  Effect.tryPromise({
    try: async () => {
      const verdict = classifyDaEnvelopeCapabilityQuorum(
        manifest,
        mode,
        await probe(manifest, mode),
      );
      if (verdict !== undefined) {
        throw verdict;
      }
    },
    catch: (cause) =>
      new DatabaseInitializationError({
        message: `DA ${mode} envelope capability quorum failed at startup`,
        cause,
      }),
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
