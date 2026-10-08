import { parseContractDeploymentInfo } from "@al-ft/midgard-fault-proofs";
import { type LucidEvolution } from "@lucid-evolution/lucid";

import type {
  WatcherConfig,
  WatcherWalletKeySource,
} from "../runtime/config.js";
import { type WatcherWorkflowInfrastructure } from "../storage/retained-da-runtime.js";
import {
  canonicalAbsolutePath,
  exactKeys,
  plainRecord,
  type WatcherFaultProofApplicationDependencies,
  type WatcherFaultProofInfrastructureAuthority,
  type WatcherFaultProofL1,
  type WatcherHistoricalNativeScriptHistoryOverlay,
} from "./fault-proof-application.production-dependencies.js";

const historicalProviderEndpoint = (value: unknown): string => {
  if (typeof value !== "string" || value.trim() !== value) {
    throw new Error("historical native-script provider endpoint is invalid");
  }
  let endpoint: URL;
  try {
    endpoint = new URL(value);
  } catch {
    throw new Error("historical native-script provider endpoint is invalid");
  }
  endpoint.pathname = endpoint.pathname.replace(/\/+$/u, "") || "/";
  if (
    endpoint.protocol !== "https:" ||
    endpoint.username.length !== 0 ||
    endpoint.password.length !== 0 ||
    endpoint.search.length !== 0 ||
    endpoint.hash.length !== 0 ||
    ["127.0.0.1", "localhost", "::1", "[::1]"].includes(
      endpoint.hostname.toLowerCase(),
    )
  ) {
    throw new Error(
      "historical native-script provider endpoint must be fixed external HTTPS",
    );
  }
  return endpoint.toString().replace(/\/$/u, "");
};

const admitHistoricalNativeScriptHistory = (
  value: unknown,
): WatcherHistoricalNativeScriptHistoryOverlay => {
  const input = plainRecord(value, "historical native-script history overlay");
  exactKeys(
    input,
    ["sourceMode", "consistencyPolicy", "providers"],
    "historical native-script history overlay",
  );
  if (
    input.sourceMode !== "external_provider_quorum" ||
    input.consistencyPolicy !== "exact_bytes_all_providers_v1" ||
    !Array.isArray(input.providers) ||
    input.providers.length < 2 ||
    input.providers.length > 4
  ) {
    throw new Error("historical native-script history overlay is invalid");
  }
  const sourceIds = new Set<string>();
  const operators = new Set<string>();
  const endpoints = new Set<string>();
  const providers = input.providers.map((value, index) => {
    const provider = plainRecord(
      value,
      `historical native-script provider ${index.toString()}`,
    );
    exactKeys(
      provider,
      ["sourceId", "operatorIdentitySha256", "authorityEndpoint"],
      `historical native-script provider ${index.toString()}`,
    );
    const endpoint = historicalProviderEndpoint(provider.authorityEndpoint);
    if (
      typeof provider.sourceId !== "string" ||
      provider.sourceId.trim() !== provider.sourceId ||
      provider.sourceId.length === 0 ||
      typeof provider.operatorIdentitySha256 !== "string" ||
      !/^[0-9a-f]{64}$/u.test(provider.operatorIdentitySha256) ||
      sourceIds.has(provider.sourceId) ||
      operators.has(provider.operatorIdentitySha256) ||
      endpoints.has(endpoint)
    ) {
      throw new Error(
        "historical native-script providers must have distinct canonical identities and endpoints",
      );
    }
    sourceIds.add(provider.sourceId);
    operators.add(provider.operatorIdentitySha256);
    endpoints.add(endpoint);
    return Object.freeze({
      sourceId: provider.sourceId,
      operatorIdentitySha256: provider.operatorIdentitySha256,
      authorityEndpoint: endpoint,
    });
  });
  return Object.freeze({
    sourceMode: "external_provider_quorum",
    consistencyPolicy: "exact_bytes_all_providers_v1",
    providers: Object.freeze(providers),
  });
};

export const admitInfrastructure = (
  value: unknown,
): WatcherFaultProofInfrastructureAuthority => {
  const input = plainRecord(value, "fault-proof infrastructure authority");
  exactKeys(
    input,
    [
      "manifestPath",
      "blueprintPath",
      "deploymentInfoPath",
      "historicalNativeScriptHistory",
    ],
    "fault-proof infrastructure authority",
  );
  return Object.freeze({
    manifestPath: canonicalAbsolutePath(input.manifestPath, "manifestPath"),
    blueprintPath: canonicalAbsolutePath(input.blueprintPath, "blueprintPath"),
    deploymentInfoPath: canonicalAbsolutePath(
      input.deploymentInfoPath,
      "deploymentInfoPath",
    ),
    historicalNativeScriptHistory: admitHistoricalNativeScriptHistory(
      input.historicalNativeScriptHistory,
    ),
  });
};

export const requireCanonicalFile = async (
  path: string,
  dependencies: WatcherFaultProofApplicationDependencies,
): Promise<string> => {
  const canonical = await dependencies.canonicalPath(path);
  if (canonical !== path) {
    throw new Error(
      `production fault-proof authority path must not traverse a symlink or non-canonical segment: ${path}`,
    );
  }
  return path;
};

export const readSecret = async ({
  source,
  dependencies,
  environment,
  label,
}: {
  readonly source: WatcherWalletKeySource;
  readonly dependencies: WatcherFaultProofApplicationDependencies;
  readonly environment: NodeJS.ProcessEnv;
  readonly label: string;
}): Promise<string> => {
  const raw =
    source.kind === "environment"
      ? environment[source.variable]
      : await dependencies.readText(
          await requireCanonicalFile(source.path, dependencies),
        );
  const secret = raw?.trim() ?? "";
  if (secret.length === 0) {
    throw new Error(`${label} secret source is empty`);
  }
  return secret;
};

/**
 * What binds the invocation to the verified deployment and reaches its
 * published scripts: the admitted local-node authority, the manifest,
 * blueprint and deployment info, a Lucid instance over the chain follower and the
 * resolver a family's roster is resolved through. It reads no secret, so the
 * startup-readiness path can prove the deployment's published scripts were
 * found without holding the prover wallet. The
 * invocation's deployment fingerprint is checked by the runtime-config reader
 * both callers admit their configuration through.
 */
type WatcherDeploymentBinding = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  localL1Source: WatcherConfig["l1"]["source"];
  lucid: LucidEvolution;
  resolveReferenceScript: WatcherWorkflowInfrastructure["resolveReferenceScript"];
}>;

export const bindWatcherDeploymentAuthority = async ({
  watcherConfig,
  infrastructure,
  l1,
  dependencies,
}: {
  readonly watcherConfig: WatcherConfig;
  readonly l1: WatcherFaultProofL1;
  readonly infrastructure: WatcherFaultProofInfrastructureAuthority;
  readonly dependencies: WatcherFaultProofApplicationDependencies;
}): Promise<WatcherDeploymentBinding> => {
  const localL1Source = watcherConfig.l1.source;
  const [manifestPath, blueprintPath, deploymentInfoPath] = await Promise.all([
    requireCanonicalFile(infrastructure.manifestPath, dependencies),
    requireCanonicalFile(infrastructure.blueprintPath, dependencies),
    requireCanonicalFile(infrastructure.deploymentInfoPath, dependencies),
  ]);
  const [manifestJson, blueprintJson, deploymentInfoJson] = await Promise.all([
    dependencies.readText(manifestPath),
    dependencies.readText(blueprintPath),
    dependencies.readText(deploymentInfoPath),
  ]);
  let manifest: unknown;
  let deploymentInfoValue: unknown;
  try {
    manifest = JSON.parse(manifestJson) as unknown;
    deploymentInfoValue = JSON.parse(deploymentInfoJson) as unknown;
  } catch {
    throw new Error("watcher manifest/deployment-info input is not JSON");
  }
  const manifestRecord = plainRecord(manifest, "deployment manifest");
  const manifestFinality = plainRecord(
    manifestRecord.l1Finality,
    "deployment manifest l1Finality",
  );
  if (manifestFinality.confirmationDepth !== watcherConfig.l1.finality.depth) {
    throw new Error(
      "watcher configured finality differs from the deployment manifest",
    );
  }
  const deploymentInfo = parseContractDeploymentInfo(deploymentInfoValue);
  const lucid = await dependencies.makeLucid({
    network: watcherConfig.targetNetwork,
    slotConfig: watcherConfig.customNetwork?.slotConfig,
    provider: l1.provider,
  });
  return Object.freeze({
    manifest,
    blueprintJson,
    deploymentInfo: deploymentInfoValue,
    localL1Source,
    lucid,
    resolveReferenceScript: async ({ contractName }) =>
      await dependencies.resolveReferenceScript({
        lucid,
        deploymentInfo,
        contractName,
      }),
  });
};
