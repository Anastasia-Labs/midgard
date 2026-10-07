import { createHash, X509Certificate } from "node:crypto";
import { join } from "node:path";

import {
  computeDeploymentManifestJsonDigest,
  verifyFinalizedDeploymentManifest,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  computeWatcherRuleBundleCommitment,
  parseWatcherConfig,
  parseWatcherProcessConfig,
  parseWatcherRuleBundle,
  parseWatcherStrictJsonValue,
} from "midgard-watcher";

import {
  HistoryConfigurationRefusal,
  HistoryEvidenceContradiction,
} from "./history-configuration-refusal.js";
import type { HistoryPinnedListener } from "./history-pinned-listener.js";
import {
  historyPublicBudget,
  historyPublicFile,
} from "./history-public-file.js";
import { type Layout, type RunEnv, servicePorts } from "./layout.js";
import { HISTORY_ROLES, historyHost } from "./watcher-history.js";
import { loadWatcherModule, releasePaths } from "./watcher-release.js";

const refuse = (message: string): never => {
  throw new HistoryEvidenceContradiction(message);
};
const object = (value: unknown): value is Record<string, unknown> =>
  value !== null && typeof value === "object" && !Array.isArray(value);
const hex = (value: unknown, bytes = 32): value is string =>
  typeof value === "string" &&
  new RegExp(`^[0-9a-f]{${bytes * 2}}$`, "u").test(value);
/** A synchronous pure decoder may classify contradictions; I/O stays outside. */
const decoded = <T>(text: string, parse: (value: unknown) => T): T => {
  try {
    return parse(parseWatcherStrictJsonValue(text));
  } catch (error) {
    throw new HistoryConfigurationRefusal(
      error instanceof Error
        ? error.message
        : "history recorded public configuration is invalid",
    );
  }
};
const releaseMarker = (value: unknown) => {
  if (
    !object(value) ||
    !object(value.signedIdentity) ||
    !object(value.signedIdentity.attestation) ||
    !object(value.signedIdentity.manifest) ||
    !object(value.signedIdentity.releaseBindings) ||
    !object(value.signedIdentity.releaseBindings.artifacts)
  )
    return refuse("history recorded release identifiers are absent");
  const { attestation, manifest, releaseBindings } = value.signedIdentity;
  const artifacts = releaseBindings.artifacts;
  if (!object(artifacts))
    return refuse("history recorded release artifacts are absent");
  if (
    !hex(attestation.trustRootId) ||
    !hex(attestation.signature, 64) ||
    !hex(manifest.manifestId) ||
    !hex(releaseBindings.ruleBundleCommitment) ||
    !hex(artifacts.blueprintHash)
  )
    return refuse("history recorded public release identifiers are malformed");
  // Public identifiers only. Verification still freshly loads the signed
  // authority/rules; this descriptor cannot grant deployment authority.
  return {
    trustRootId: attestation.trustRootId,
    signature: attestation.signature,
    manifestId: manifest.manifestId,
    ruleBundleCommitment: releaseBindings.ruleBundleCommitment,
    blueprintHash: artifacts.blueprintHash,
  };
};
export const historyRecordedBinding = (
  layout: Layout,
  run: RunEnv,
  expectedNetwork: "Custom" | "Preprod",
  deadline?: number,
) => {
  const readPublic = (path: string) => historyPublicFile(path, deadline).text;
  const config = decoded(
    readPublic(layout.watcherProcessConfig),
    parseWatcherProcessConfig,
  );
  const paths = releasePaths(layout);
  if (
    config.deploymentAuthorityPath !== paths.authority ||
    config.ruleBundlePath !== paths.rules ||
    config.watcherRuntimeConfigPath !== layout.watcherRuntimeConfig ||
    config.faultProofInfrastructure.manifestPath !== paths.manifest ||
    config.fundingProfileBundlePath !== paths.fundingProfiles ||
    config.nativeChainSyncBinaryPath !==
      join(layout.bin, "midgard-l1-node-transport")
  )
    refuse("history configuration does not name this run's recorded release");
  if (
    config.watcherConfig.targetNetwork !== expectedNetwork ||
    (expectedNetwork === "Custom" &&
      config.watcherConfig.customNetwork?.networkMagic !== run.networkMagic)
  )
    refuse("history configured network differs from recorded run");
  const runtime = decoded(
    readPublic(layout.watcherRuntimeConfig),
    parseWatcherConfig,
  );
  if (
    computeDeploymentManifestJsonDigest(runtime) !==
    computeDeploymentManifestJsonDigest(config.watcherConfig)
  )
    refuse("history process/runtime configurations disagree");
  const manifest = decoded(
    readPublic(layout.contractManifest),
    verifyFinalizedDeploymentManifest,
  );
  const releaseManifest = decoded(
    readPublic(paths.manifest),
    verifyFinalizedDeploymentManifest,
  );
  if (
    manifest.network !== expectedNetwork ||
    computeDeploymentManifestJsonDigest(manifest) !==
      computeDeploymentManifestJsonDigest(releaseManifest)
  )
    refuse("history recorded finalized deployment differs from release");
  const authority = decoded(readPublic(paths.authority), (document) => ({
    document,
    marker: releaseMarker(document),
  }));
  const marker = authority.marker;
  const rules = decoded(readPublic(paths.rules), parseWatcherRuleBundle);
  if (computeWatcherRuleBundleCommitment(rules) !== marker.ruleBundleCommitment)
    refuse(
      "history recorded rule bundle differs from signed release commitment",
    );
  if (
    marker.manifestId !== manifest.manifestId ||
    marker.blueprintHash !== manifest.artifacts.blueprintHash
  )
    refuse("history recorded release identifiers differ from deployment");
  const history = config.faultProofInfrastructure.historicalNativeScriptHistory;
  const roster = decoded(
    readPublic(layout.watcherHistoryProviders),
    (value) => value,
  );
  if (
    computeDeploymentManifestJsonDigest(roster) !==
      computeDeploymentManifestJsonDigest(history) ||
    history.providers.length !== HISTORY_ROLES.length
  )
    refuse("history recorded provider roster differs from configured roster");
  const certificates = HISTORY_ROLES.map((role) => {
    const text = readPublic(
      join(layout.watcherHistoryArchive(role), "certificate.pem"),
    );
    try {
      return new X509Certificate(text);
    } catch {
      return refuse("history recorded public certificate is invalid");
    }
  });
  const ca = readPublic(layout.watcherHistoryCa);
  const blocks =
    ca.match(
      /-----BEGIN CERTIFICATE-----[\s\S]*?-----END CERTIFICATE-----/gu,
    ) ?? [];
  if (
    ca
      .replace(
        /-----BEGIN CERTIFICATE-----[\s\S]*?-----END CERTIFICATE-----/gu,
        "",
      )
      .trim() !== "" ||
    blocks.length !== certificates.length
  )
    refuse("history recorded CA bundle differs from provider certificates");
  const authorities = HISTORY_ROLES.map((role) =>
    decoded(
      readPublic(join(layout.watcherHistoryArchive(role), "authority.json")),
      (value) => value,
    ),
  );
  const providers: HistoryPinnedListener[] = HISTORY_ROLES.map(
    (role, index) => {
      const provider = history.providers[index];
      const certificate = certificates[index];
      const caBlock = blocks[index];
      const authority = authorities[index];
      if (
        provider === undefined ||
        certificate === undefined ||
        caBlock === undefined ||
        !object(authority) ||
        !object(authority.releaseFinality)
      )
        return refuse("history recorded provider authority is absent");
      const host = historyHost(run, role);
      const endpoint = new URL(provider.authorityEndpoint);
      const pin = createHash("sha256")
        .update(certificate.publicKey.export({ type: "spki", format: "der" }))
        .digest("hex");
      let trusted: X509Certificate;
      try {
        trusted = new X509Certificate(caBlock);
      } catch {
        return refuse("history recorded CA certificate is invalid");
      }
      if (
        !certificate.checkHost(host) ||
        !certificate.raw.equals(trusted.raw) ||
        provider.sourceId !== `devnet-history-${role}` ||
        provider.operatorIdentitySha256 !== pin ||
        endpoint.hostname !== host ||
        endpoint.protocol !== "https:" ||
        !["", "443"].includes(endpoint.port) ||
        endpoint.pathname !== "/" ||
        endpoint.search !== "" ||
        endpoint.hash !== "" ||
        authority.sourceId !== provider.sourceId ||
        authority.operatorIdentitySha256 !== pin ||
        authority.releaseFinality.deploymentIdentityDigest !==
          manifest.manifestId ||
        authority.releaseFinality.blueprintHash !== marker.blueprintHash ||
        !hex(authority.releaseFinality.policyDigest)
      )
        return refuse(
          "history recorded provider identity/release binding disagrees",
        );
      return {
        hostname: host,
        port: servicePorts(run).historyArchive(index),
        ca,
        binding: {
          sourceId: provider.sourceId,
          operatorIdentitySha256: pin,
          deploymentIdentityDigest: manifest.manifestId,
          blueprintHash: marker.blueprintHash,
          policyDigest: authority.releaseFinality.policyDigest,
        },
      };
    },
  );
  const digest = computeDeploymentManifestJsonDigest({
    runId: run.runId,
    networkMagic: run.networkMagic,
    portOffset: run.portOffset,
    processConfig: config,
    runtime,
    manifest,
    release: authority.document,
    rules,
    roster: history,
    certificates: certificates.map((certificate) =>
      certificate.raw.toString("hex"),
    ),
    authorityPins: providers.map((provider) => provider.binding),
    providerAuthorities: authorities,
  });
  return { digest, config, manifest, marker, authorities, providers };
};
/** Fresh signed release/policy admission; the descriptor alone never authenticates it. */
export const loadHistoryRoleAdmission = async (
  layout: Layout,
  run: RunEnv,
  expected: string,
  deadline: number,
  expectedNetwork: "Custom" | "Preprod",
) => {
  const inBudget = () => historyPublicBudget(deadline);
  const recorded = () => {
    inBudget();
    try {
      const result = historyRecordedBinding(
        layout,
        run,
        expectedNetwork,
        deadline,
      );
      inBudget();
      return result;
    } catch (error) {
      // Only a decoder overrun is uncertain; raw contradictions and I/O stay known.
      if (
        error instanceof HistoryConfigurationRefusal &&
        !(error instanceof HistoryEvidenceContradiction)
      )
        inBudget();
      throw error;
    }
  };
  const before = recorded();
  inBudget();
  if (before.digest !== expected)
    refuse(
      "history recorded configuration changed from this service generation",
    );
  const watcher = await loadWatcherModule(layout);
  inBudget();
  let verified: Awaited<
    ReturnType<typeof watcher.loadWatcherVerifiedDeploymentAuthority>
  >;
  try {
    verified = await watcher.loadWatcherVerifiedDeploymentAuthority({
      path: before.config.deploymentAuthorityPath,
      ruleBundlePath: before.config.ruleBundlePath,
    });
  } catch (error) {
    if (error instanceof watcher.WatcherDeploymentIdentityError)
      throw new HistoryConfigurationRefusal(error.message);
    throw error;
  }
  inBudget();
  const release = await watcher
    .watcherDeploymentReleaseFinalityAuthority(verified.deploymentIdentity)
    .verifyForWorkflow({ deploymentFingerprint: before.manifest.manifestId });
  inBudget();
  if (
    release.blueprintHash !== before.marker.blueprintHash ||
    verified.deploymentIdentity.trustRootId !== before.marker.trustRootId ||
    verified.ruleBundle.ruleBundleCommitment !==
      before.marker.ruleBundleCommitment ||
    before.authorities.some(
      (authority) =>
        !object(authority) ||
        computeDeploymentManifestJsonDigest(authority.releaseFinality) !==
          computeDeploymentManifestJsonDigest(release),
    )
  )
    refuse(
      "history authenticated release differs from recorded provider authorities",
    );
  const after = recorded();
  inBudget();
  if (after.digest !== expected || after.digest !== before.digest)
    refuse("history recorded configuration changed during signed admission");
  return { ...after, watcher, release };
};
