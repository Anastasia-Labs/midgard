import { mkdir, readFile, realpath } from "node:fs/promises";

import { FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER } from "@al-ft/midgard-sdk";

import { type WatcherAvailabilityRuntime } from "../availability/runtime.js";
import {
  type WatcherFaultProofApplication,
  type WatcherFaultProofStartupReadiness,
} from "../fault-proofs/fault-proof-application.js";
import { type WatcherFaultProofSupervisor } from "../fault-proofs/fault-proof-supervisor.js";
import { type WatcherFollowerRuntime } from "../l1-follower/follower-runtime.js";
import { watcherCanonicalJson } from "../storage/durable-store.js";
import { parseWatcherConfigJson } from "./config.js";
import { type VerifiedWatcherDeploymentAuthority } from "./deployment-authority.js";
import { type WatcherOperationsObservability } from "./operations-observability.js";
import { type WatcherProcessConfig } from "./process-config.js";
import {
  type WatcherDecisionDriver,
  type WatcherDecisionReadiness,
} from "./watcher-runtime.decision-driver.js";

export const WATCHER_RUNTIME_SCHEMA_VERSION =
  "midgard-watcher-production-runtime-v1" as const;

export type WatcherRuntime = Readonly<{
  schemaVersion: typeof WATCHER_RUNTIME_SCHEMA_VERSION;
  deploymentAuthority: VerifiedWatcherDeploymentAuthority;
  faultProofApplication: WatcherFaultProofApplication;
  faultProofReadiness: readonly WatcherFaultProofStartupReadiness[];
  faultProofSupervisor: WatcherFaultProofSupervisor;
  operations: WatcherOperationsObservability;
  operationsEndpoint: string;
  recoveredFaultProofWorkflowCount: number;
  availability: WatcherAvailabilityRuntime;
  /** The chain follower every L1 read goes through. */
  follower: WatcherFollowerRuntime;
  /** Decides faults, availability and history advances from the follower's facts. */
  decisionDriver: WatcherDecisionDriver;
  done: Promise<void>;
  caughtUp: Promise<void>;
  status(): Readonly<{
    phase: "live" | "closing" | "closed" | "failed";
    liveness: boolean;
    readiness: boolean;
    caughtUp: boolean;
    /** Every L1 reason the watcher is not ready (follower and decisions). */
    l1Readiness: readonly WatcherDecisionReadiness[];
    proofSupervisor: ReturnType<WatcherFaultProofSupervisor["status"]>;
    availability: ReturnType<WatcherAvailabilityRuntime["status"]>;
  }>;
  close(): Promise<void>;
}>;

/**
 * A partial replay/runner union is not a production classifier: an omitted
 * family could otherwise be misreported as healthy. Launch therefore requires
 * the exact canonical catalogue, in canonical order, before L1 intake.
 */
export const assertWatcherFaultProofLaunchScope = (
  categories: readonly string[],
): void => {
  if (
    categories.length !== FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.length ||
    FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.some(
      (category, index) => categories[index] !== category,
    )
  ) {
    throw new Error(
      "watcher production fault-proof application does not cover the exact canonical catalogue",
    );
  }
};

export const requireWatcherRuntimeConfig = async (
  config: WatcherProcessConfig,
): Promise<void> => {
  if (
    (await realpath(config.watcherRuntimeConfigPath)) !==
    config.watcherRuntimeConfigPath
  ) {
    throw new Error("watcher workflow runtime config traverses a symlink");
  }
  const raw = await readFile(config.watcherRuntimeConfigPath, "utf8");
  const parsed = parseWatcherConfigJson(raw);
  if (
    watcherCanonicalJson(parsed) !== watcherCanonicalJson(config.watcherConfig)
  ) {
    throw new Error(
      "watcher process and workflow runtime configurations differ",
    );
  }
};

export const prepareJournalDirectory = async (path: string): Promise<void> => {
  await mkdir(path, { recursive: true, mode: 0o700 });
  if ((await realpath(path)) !== path) {
    throw new Error("watcher workflow journal directory traverses a symlink");
  }
};
