import { createHash } from "node:crypto";
import {
  mkdirSync,
  readFileSync,
  renameSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { join } from "node:path";

import {
  allowListAdmits,
  deploymentAllowList,
  type DeploymentProfile,
  SELECTED_DEPLOYMENT_PROFILE,
} from "@al-ft/midgard-core/deployment-profile";
import { paymentCredentialOf } from "@lucid-evolution/lucid";

import type { WatcherOperationsSink } from "./operations-observability.js";

/**
 * How the watcher acts on a fault it proves. `submit`: it initialises the
 * computation thread or opens the availability challenge itself.
 * `observe_and_prepare`: its key is not on the deployment's allow-list, so
 * the validators would refuse its transaction; it submits nothing, writes a
 * prepared-proof bundle and pages, and an allow-listed key submits it.
 */
export type WatcherSubmissionMode = "submit" | "observe_and_prepare";

export type WatcherSubmissionModes = Readonly<{
  /** The prover key against the fraud-prover list (Computation-Thread Init). */
  faultProofs: WatcherSubmissionMode;
  /** The availability key against the DA challenger list (challenge Open). */
  availability: WatcherSubmissionMode;
}>;

/** The modes the profile's lists give the watcher's two payment key hashes. */
export const watcherSubmissionModes = (
  keys: Readonly<{ prover: string; availability: string }>,
  profile: DeploymentProfile = SELECTED_DEPLOYMENT_PROFILE,
): WatcherSubmissionModes =>
  Object.freeze({
    faultProofs: allowListAdmits(
      deploymentAllowList("fraud_provers", profile),
      keys.prover,
    )
      ? "submit"
      : "observe_and_prepare",
    availability: allowListAdmits(
      deploymentAllowList("da_challengers", profile),
      keys.availability,
    )
      ? "submit"
      : "observe_and_prepare",
  });

export const WATCHER_PREPARED_PROOF_FORMAT =
  "midgard-watcher-prepared-proof-v1";

/**
 * What an allow-listed key needs to act on a fault this watcher found: the
 * objective, never evidence. The submitter re-derives every input from
 * authenticated L1 and public DA, so a bundle cannot steer a proof.
 */
export type WatcherPreparedProof =
  | Readonly<{
      format: typeof WATCHER_PREPARED_PROOF_FORMAT;
      kind: "fault_proof";
      deploymentFingerprint: string;
      category: string;
      headerHash: string;
      decisionDigest: string;
      rollbackGeneration: string;
      /** The unlisted prover key that found the fault. */
      observerKeyHash: string;
      preparedAtMs: string;
    }>
  | Readonly<{
      format: typeof WATCHER_PREPARED_PROOF_FORMAT;
      kind: "availability_open";
      deploymentFingerprint: string;
      headerHash: string;
      /** The last moment an Open can land, when the watcher knows it. */
      openDeadlineEpochMs: string | null;
      /** The unlisted availability key that found the withheld payload. */
      observerKeyHash: string;
      preparedAtMs: string;
    }>;

type PreparedProofIdentity = Readonly<{
  kind: WatcherPreparedProof["kind"];
  headerHash: string;
  category?: string;
}>;

export type WatcherPreparedProofStore = Readonly<{
  directory: string;
  /**
   * Writes the bundle atomically and returns its path. A bundle already on
   * disk for the same objective keeps its first `preparedAtMs`.
   */
  write(bundle: WatcherPreparedProof): string;
  /** Removes the objective's bundle once nothing is left to submit. */
  remove(identity: PreparedProofIdentity): void;
}>;

const HEX_28 = /^[0-9a-f]{56}$/u;

const bundleName = ({ kind, headerHash, category }: PreparedProofIdentity) => {
  if (!HEX_28.test(headerHash))
    throw new Error("prepared proof header hash must be 28-byte lowercase hex");
  if (category !== undefined && !/^[A-Za-z0-9]+$/u.test(category))
    throw new Error("prepared proof category must be alphanumeric");
  return `${kind}${category === undefined ? "" : `-${category}`}-${headerHash}.json`;
};

const identityOf = (bundle: WatcherPreparedProof): PreparedProofIdentity =>
  bundle.kind === "fault_proof"
    ? {
        kind: bundle.kind,
        headerHash: bundle.headerHash,
        category: bundle.category,
      }
    : { kind: bundle.kind, headerHash: bundle.headerHash };

const withoutTime = ({ preparedAtMs: _, ...rest }: WatcherPreparedProof) =>
  JSON.stringify(rest);

/** The prepared-proof directory, created owner-only on first write. */
export const openWatcherPreparedProofStore = (
  directory: string,
): WatcherPreparedProofStore =>
  Object.freeze({
    directory,
    write: (bundle) => {
      const path = join(directory, bundleName(identityOf(bundle)));
      let existing: WatcherPreparedProof | undefined;
      try {
        existing = JSON.parse(
          readFileSync(path, "utf8"),
        ) as WatcherPreparedProof;
      } catch {
        existing = undefined;
      }
      if (
        existing !== undefined &&
        withoutTime(existing) === withoutTime(bundle)
      )
        return path;
      mkdirSync(directory, { recursive: true, mode: 0o700 });
      const temporary = `${path}.${process.pid}.tmp`;
      writeFileSync(temporary, `${JSON.stringify(bundle, null, 2)}\n`, {
        mode: 0o600,
      });
      renameSync(temporary, path);
      return path;
    },
    remove: (identity) =>
      rmSync(join(directory, bundleName(identity)), { force: true }),
  });

const PREPARED_PROOF_ALERT = "prepared_proof_awaiting_submission" as const;

type AlertSink = () => Pick<WatcherOperationsSink, "setAlert">;

/**
 * The fault-proof side of `observe_and_prepare`: each admitted objective gets
 * a bundle and an active alert (subject: its decision digest) in place of a
 * submission; both go once the objective is settled.
 */
export type WatcherFaultProofPreparation = Readonly<{
  prepare(
    job: Readonly<{
      category: string;
      headerHash: string;
      decisionDigest: string;
      rollbackGeneration: string;
    }>,
  ): string;
  settled(objective: Readonly<{ category: string; headerHash: string }>): void;
}>;

export const createWatcherFaultProofPreparation = (input: {
  readonly store: WatcherPreparedProofStore;
  readonly observerKeyHash: string;
  readonly deploymentFingerprint: string;
  readonly operationsSink: AlertSink;
  readonly nowMs?: () => number;
}): WatcherFaultProofPreparation => {
  const nowMs = input.nowMs ?? Date.now;
  const prepared = new Map<string, string>();
  const objectiveOf = (o: Readonly<{ category: string; headerHash: string }>) =>
    `${o.category}/${o.headerHash}`;
  return Object.freeze({
    prepare: ({ category, headerHash, decisionDigest, rollbackGeneration }) => {
      const at = nowMs().toString();
      const path = input.store.write({
        format: WATCHER_PREPARED_PROOF_FORMAT,
        kind: "fault_proof",
        deploymentFingerprint: input.deploymentFingerprint,
        category,
        headerHash,
        decisionDigest,
        rollbackGeneration,
        observerKeyHash: input.observerKeyHash,
        preparedAtMs: at,
      });
      const previous = prepared.get(objectiveOf({ category, headerHash }));
      if (previous !== decisionDigest) {
        if (previous !== undefined) clear(previous, at);
        input.operationsSink().setAlert({
          code: PREPARED_PROOF_ALERT,
          subjectDigest: decisionDigest,
          active: true,
          observedAtMs: at,
        });
        prepared.set(objectiveOf({ category, headerHash }), decisionDigest);
      }
      return path;
    },
    settled: (objective) => {
      const digest = prepared.get(objectiveOf(objective));
      if (digest === undefined) return;
      input.store.remove({ kind: "fault_proof", ...objective });
      clear(digest, nowMs().toString());
      prepared.delete(objectiveOf(objective));
    },
  });
  function clear(subjectDigest: string, observedAtMs: string) {
    input.operationsSink().setAlert({
      code: PREPARED_PROOF_ALERT,
      subjectDigest,
      active: false,
      observedAtMs,
    });
  }
};

/** A due availability Open the watcher's key may not submit. */
export type WatcherPreparedOpen = Readonly<{
  headerHash: string;
  openDeadlineEpochMs: number | null;
}>;

export const watcherPreparedOpenAlertSubject = (headerHash: string): string =>
  createHash("sha256")
    .update("midgard-watcher-prepared-open-alert-v1\u0000")
    .update(headerHash.toLowerCase())
    .digest("hex");

/**
 * The availability side of `observe_and_prepare`, called with every due Open
 * of a reconciliation: new ones get a bundle and an active alert; ones no
 * longer due (opened by a listed challenger, published, or past their
 * deadline) lose both.
 */
export const createWatcherPreparedOpenReporter = (input: {
  readonly store: WatcherPreparedProofStore;
  readonly observerKeyHash: string;
  readonly deploymentFingerprint: string;
  readonly operationsSink: AlertSink;
  readonly nowMs?: () => number;
}): ((opens: readonly WatcherPreparedOpen[]) => void) => {
  const nowMs = input.nowMs ?? Date.now;
  let reported = new Set<string>();
  return (opens) => {
    const at = nowMs().toString();
    const due = new Set(opens.map(({ headerHash }) => headerHash));
    for (const headerHash of reported) {
      if (due.has(headerHash)) continue;
      input.store.remove({ kind: "availability_open", headerHash });
      input.operationsSink().setAlert({
        code: PREPARED_PROOF_ALERT,
        subjectDigest: watcherPreparedOpenAlertSubject(headerHash),
        active: false,
        observedAtMs: at,
      });
    }
    for (const { headerHash, openDeadlineEpochMs } of opens) {
      input.store.write({
        format: WATCHER_PREPARED_PROOF_FORMAT,
        kind: "availability_open",
        deploymentFingerprint: input.deploymentFingerprint,
        headerHash,
        openDeadlineEpochMs:
          openDeadlineEpochMs === null ? null : openDeadlineEpochMs.toString(),
        observerKeyHash: input.observerKeyHash,
        preparedAtMs: at,
      });
      if (!reported.has(headerHash))
        input.operationsSink().setAlert({
          code: PREPARED_PROOF_ALERT,
          subjectDigest: watcherPreparedOpenAlertSubject(headerHash),
          active: true,
          observedAtMs: at,
        });
    }
    reported = due;
  };
};

/**
 * The runtime's wiring: the modes its two wallets get under the compiled
 * deployment profile, and for each key in `observe_and_prepare` the hook that
 * prepares in place of submitting. Bundles go to `<storage>.prepared-proofs`.
 */
export const watcherObserveAndPrepare = (input: {
  readonly proverWalletAddress: string;
  readonly availabilityWalletAddress: string;
  readonly storagePath: string;
  readonly deploymentFingerprint: string;
  readonly operationsSink: AlertSink;
  readonly profile?: DeploymentProfile;
}) => {
  const prover = paymentCredentialOf(input.proverWalletAddress).hash;
  const availability = paymentCredentialOf(
    input.availabilityWalletAddress,
  ).hash;
  const modes = watcherSubmissionModes({ prover, availability }, input.profile);
  const store = openWatcherPreparedProofStore(
    `${input.storagePath}.prepared-proofs`,
  );
  const shared = {
    store,
    deploymentFingerprint: input.deploymentFingerprint,
    operationsSink: input.operationsSink,
  };
  return Object.freeze({
    modes,
    preparation:
      modes.faultProofs === "submit"
        ? undefined
        : createWatcherFaultProofPreparation({
            ...shared,
            observerKeyHash: prover,
          }),
    onOpensPrepared:
      modes.availability === "submit"
        ? undefined
        : createWatcherPreparedOpenReporter({
            ...shared,
            observerKeyHash: availability,
          }),
  });
};
