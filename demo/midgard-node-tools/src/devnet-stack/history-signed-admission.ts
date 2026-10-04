import { createHash } from "node:crypto";
import { realpathSync } from "node:fs";
import { join } from "node:path";
import { pathToFileURL } from "node:url";

import { HistoryConfigurationRefusal } from "./history-configuration-refusal.js";
import {
  historyPublicBudget,
  historyPublicFile,
} from "./history-public-file.js";
import { loadHistoryRoleAdmission } from "./history-recorded-binding.js";
import type { Layout, RunEnv } from "./layout.js";
import { HISTORY_ROLES } from "./watcher-history.js";
import { releasePaths } from "./watcher-release.js";

export type HistoryAdmissionScope = Readonly<{
  codeStamp: string;
  serviceSpecsDigest: string;
  incarnation: string;
}>;
/** Exactly the public files read by the recorded descriptor and signed loader. */
export const historyAdmissionPublicPaths = (
  layout: Layout,
): readonly string[] => {
  const release = releasePaths(layout);
  return [
    layout.watcherProcessConfig,
    layout.watcherRuntimeConfig,
    layout.contractManifest,
    release.manifest,
    release.authority,
    release.rules,
    layout.watcherHistoryProviders,
    ...HISTORY_ROLES.map((role) =>
      join(layout.watcherHistoryArchive(role), "certificate.pem"),
    ),
    layout.watcherHistoryCa,
    ...HISTORY_ROLES.map((role) =>
      join(layout.watcherHistoryArchive(role), "authority.json"),
    ),
  ];
};
const freezeMetadata = (value: unknown): void => {
  if (value === null || typeof value !== "object") return;
  for (const child of Object.values(value)) freezeMetadata(child);
  Object.freeze(value);
};
/** One process incarnation. Reuse authentic static admission, never live readiness. */
export const makeHistorySignedAdmission = (input: {
  layout: Layout;
  run: RunEnv;
  publicBindingDigest: string;
  deploymentFingerprint: string;
  expectedNetwork: "Custom" | "Preprod";
  expectedScope: HistoryAdmissionScope;
  currentScope: () => HistoryAdmissionScope;
}) => {
  const {
    layout,
    run,
    expectedNetwork,
    publicBindingDigest,
    deploymentFingerprint,
    currentScope,
  } = input;
  const ownedLayout = Object.freeze({ ...layout });
  const ownedRun = Object.freeze({ ...run });
  const paths = historyAdmissionPublicPaths(ownedLayout);
  // Own primitive values; never retain a caller's mutable scope or run object.
  const fixedRun = {
    runId: run.runId,
    networkMagic: run.networkMagic,
    portOffset: run.portOffset,
  };
  const firstScope = input.expectedScope;
  const initialScope = {
    codeStamp: firstScope.codeStamp,
    serviceSpecsDigest: firstScope.serviceSpecsDigest,
    incarnation: firstScope.incarnation,
  };
  const scopeKey = (scope: HistoryAdmissionScope) =>
    JSON.stringify([
      scope.codeStamp,
      scope.serviceSpecsDigest,
      scope.incarnation,
      fixedRun.runId,
      fixedRun.networkMagic,
      fixedRun.portOffset,
    ]);
  const initialScopeKey = scopeKey(initialScope);
  const modulePath = join(layout.watcherRoot, "dist/index.js");
  const fixedPaths = JSON.stringify([layout.runDir, modulePath, ...paths]);
  let refusal: HistoryConfigurationRefusal | undefined;
  const drift = (): never => {
    refusal ??= new HistoryConfigurationRefusal(
      "history signed admission inputs changed; this process generation cannot adopt replacements",
    );
    throw refusal;
  };
  let initialKey: string | undefined;
  const key = (deadline: number): string => {
    if (refusal !== undefined) throw refusal;
    historyPublicBudget(deadline);
    try {
      if (
        scopeKey(currentScope()) !== initialScopeKey ||
        run.runId !== fixedRun.runId ||
        run.networkMagic !== fixedRun.networkMagic ||
        run.portOffset !== fixedRun.portOffset ||
        JSON.stringify([
          layout.runDir,
          join(layout.watcherRoot, "dist/index.js"),
          ...historyAdmissionPublicPaths(layout),
        ]) !== fixedPaths ||
        realpathSync(layout.runDir) !== layout.runDir ||
        realpathSync(modulePath) !== modulePath
      )
        drift();
      const hash = createHash("sha256");
      const field = (bytes: Buffer) => {
        hash.update(`${bytes.length}:`);
        hash.update(bytes);
      };
      field(
        Buffer.from(
          JSON.stringify([
            fixedRun,
            expectedNetwork,
            publicBindingDigest,
            deploymentFingerprint,
            initialScope,
            pathToFileURL(modulePath).href,
          ]),
        ),
      );
      for (const path of paths) {
        const file = historyPublicFile(path, deadline);
        field(Buffer.from(path));
        field(Buffer.from(file.identity));
        field(file.bytes);
      }
      if (scopeKey(currentScope()) !== initialScopeKey) drift();
      const captured = hash.digest("hex");
      if (initialKey !== undefined && captured !== initialKey) drift();
      historyPublicBudget(deadline);
      return captured;
    } catch (error) {
      if (
        error instanceof Error &&
        "code" in error &&
        ["ENOENT", "ENOTDIR", "ELOOP"].some((code) => error.code === code)
      )
        drift();
      if (error instanceof HistoryConfigurationRefusal) refusal ??= error;
      throw error;
    }
  };
  let admission:
    | Awaited<ReturnType<typeof loadHistoryRoleAdmission>>
    | undefined;
  let admitting = false;
  const sameGeneration = (deadline: number, establish = false) => {
    const captured = key(deadline);
    // No partial key survives an expired capture; a complete key never changes.
    if (establish) initialKey ??= captured;
  };
  const current = (deadline: number) => {
    if (refusal !== undefined) throw refusal;
    historyPublicBudget(deadline);
    if (admission === undefined)
      throw new Error("history signed admission has not completed");
    sameGeneration(deadline);
    return admission;
  };
  return Object.freeze({
    current,
    admit: async (deadline: number) => {
      sameGeneration(deadline, true);
      if (admission !== undefined) return admission;
      if (admitting)
        throw new Error("history signed admission is already in progress");
      admitting = true;
      try {
        const candidate = await loadHistoryRoleAdmission(
          ownedLayout,
          ownedRun,
          publicBindingDigest,
          deadline,
          expectedNetwork,
        );
        historyPublicBudget(deadline);
        if (candidate.manifest.manifestId !== deploymentFingerprint) drift();
        // Freeze only our parsed public metadata. Preserve genuine SDK/module identity.
        for (const value of [
          candidate.config,
          candidate.manifest,
          candidate.marker,
          candidate.authorities,
          candidate.providers,
        ])
          freezeMetadata(value);
        Object.freeze(candidate);
        // Freshly fence the loader await and pure freezing before publication.
        sameGeneration(deadline);
        admission = candidate;
        return candidate;
      } catch (error) {
        if (error instanceof HistoryConfigurationRefusal) refusal ??= error;
        throw error;
      } finally {
        admitting = false;
      }
    },
  });
};
