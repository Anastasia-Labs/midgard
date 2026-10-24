import { createHash } from "node:crypto";
import path from "node:path";
import { isDeepStrictEqual } from "node:util";

export const PHASE4_PROCESS_SUMMARY_SCHEMA =
  "midgard-phase4-journal-kill-recovery-acceptance-v1";

export const PHASE4_PROCESS_SUMMARY_MODE =
  "attach-resume-matched-local-devnet-snapshot";

/**
 * The default-path log lines the surviving node must print, in this order:
 * the lease is busy while the killed winner holds it, the survivor abandons
 * the winner's unsubmitted journal once the lease expires, and the survivor
 * then submits its own block.
 */
export const JOURNAL_KILL_SURVIVOR_MARKERS = [
  "Skipping block commitment trigger because the state-queue mutation lease is busy",
  "abandoning unsubmitted journal and recovering canonical state_queue tip",
  "Block submitted; local finalization is intentionally deferred until L1 confirmation.",
];

export const JOURNAL_KILL_CHECKPOINT_MARKER =
  "pipeline_trace phase=e2e_crash_checkpoint checkpoint=journal_prepared_before_submit";

/** True when every marker appears in `log`, each after the one before it. */
export const markersAppearInOrder = (log, markers) => {
  let from = 0;
  for (const marker of markers) {
    const index = log.indexOf(marker, from);
    if (index < 0) return false;
    from = index + marker.length;
  }
  return true;
};

export const L2_HEADER_HASH = /^[a-f0-9]{56}$/u;

export const HASH_32 = /^[a-f0-9]{64}$/u;

export const CARDANO_OUT_REF = /^[a-f0-9]{64}#[0-9]+$/u;

export const ISOLATED_PREFIX = "midgard_phase4_process_";

export const SUPERVISOR_SCHEMA = "midgard-e2e-service-supervisor-v1";

export const ACTIVE_JOURNAL_STATUSES = new Set([
  "pending_submission",
  "submitted_local_finalization_pending",
  "submitted_unconfirmed",
  "observed_waiting_stability",
]);

export const LEASE_STATUSES = new Set(["active", "released", "failed"]);

export const object = (value) =>
  typeof value === "object" && value !== null && !Array.isArray(value);

export const safeNonnegative = (value) =>
  Number.isSafeInteger(value) && value >= 0;

export const safePositive = (value) => Number.isSafeInteger(value) && value > 0;

export const isoTimestamp = (value) =>
  typeof value === "string" &&
  Number.isFinite(Date.parse(value)) &&
  new Date(value).toISOString() === value;

export const hexBytes = (value) =>
  typeof value === "string" &&
  value.length > 0 &&
  value.length % 2 === 0 &&
  /^[a-f0-9]+$/u.test(value);

export const sha256 = (bytes) =>
  createHash("sha256").update(bytes).digest("hex");

export const check = (reasons, condition, message) => {
  if (!condition) reasons.push(message);
  return condition;
};

export const exactKeys = (reasons, value, expected, label) => {
  if (!object(value)) {
    reasons.push(`${label} must be an object`);
    return false;
  }
  const actual = Object.keys(value).sort((left, right) =>
    left.localeCompare(right),
  );
  const wanted = [...expected].sort((left, right) => left.localeCompare(right));
  return check(
    reasons,
    isDeepStrictEqual(actual, wanted),
    `${label} fields do not match the exact schema`,
  );
};

export const pathWithin = (parent, child) => {
  if (typeof parent !== "string" || typeof child !== "string") return false;
  if (!path.isAbsolute(parent) || !path.isAbsolute(child)) return false;
  const relative = path.relative(parent, child);
  return (
    relative.length > 0 &&
    relative !== ".." &&
    !relative.startsWith(`..${path.sep}`) &&
    !path.isAbsolute(relative)
  );
};

export const validateCleanup = (reasons, value, label) => {
  if (value === null) return;
  const baseKeys = ["attempted", "pid", "target", "signal", "success", "error"];
  const keys = Object.hasOwn(value ?? {}, "ownershipValidation")
    ? [...baseKeys, "ownershipValidation"]
    : baseKeys;
  if (!exactKeys(reasons, value, keys, label)) return;
  check(
    reasons,
    typeof value.attempted === "boolean",
    `${label}.attempted is invalid`,
  );
  check(
    reasons,
    value.pid === null || safePositive(value.pid),
    `${label}.pid is invalid`,
  );
  check(
    reasons,
    ["process_group", "process", "none"].includes(value.target),
    `${label}.target is invalid`,
  );
  check(
    reasons,
    typeof value.signal === "string" && value.signal.startsWith("SIG"),
    `${label}.signal is invalid`,
  );
  check(
    reasons,
    typeof value.success === "boolean",
    `${label}.success is invalid`,
  );
  check(
    reasons,
    value.error === null || typeof value.error === "string",
    `${label}.error is invalid`,
  );
  if (Object.hasOwn(value, "ownershipValidation")) {
    if (
      exactKeys(
        reasons,
        value.ownershipValidation,
        ["valid", "reason"],
        `${label}.ownershipValidation`,
      )
    ) {
      check(
        reasons,
        typeof value.ownershipValidation.valid === "boolean" &&
          typeof value.ownershipValidation.reason === "string" &&
          value.ownershipValidation.reason.length > 0,
        `${label}.ownershipValidation is invalid`,
      );
    }
  }
};

export const validateClassification = (reasons, value, label) => {
  if (!exactKeys(reasons, value, ["class", "reason", "restartable"], label)) {
    return;
  }
  check(reasons, typeof value.class === "string", `${label}.class is invalid`);
  check(
    reasons,
    typeof value.reason === "string" && value.reason.length > 0,
    `${label}.reason is invalid`,
  );
  check(
    reasons,
    typeof value.restartable === "boolean",
    `${label}.restartable is invalid`,
  );
};

export const validateTermination = (reasons, value, label, kind) => {
  if (value === null) return;
  const keys =
    kind === "output"
      ? ["marker", "occurrence", "signal", "at"]
      : ["path", "signal", "at"];
  if (!exactKeys(reasons, value, keys, label)) return;
  if (kind === "output") {
    check(
      reasons,
      typeof value.marker === "string" && value.marker.length > 0,
      `${label}.marker is invalid`,
    );
    check(
      reasons,
      safePositive(value.occurrence),
      `${label}.occurrence is invalid`,
    );
  } else {
    check(
      reasons,
      typeof value.path === "string" && path.isAbsolute(value.path),
      `${label}.path must be absolute`,
    );
  }
  check(
    reasons,
    typeof value.signal === "string" && value.signal.startsWith("SIG"),
    `${label}.signal is invalid`,
  );
  check(reasons, isoTimestamp(value.at), `${label}.at is invalid`);
};

export const requireOutputTermination = (
  reasons,
  summary,
  marker,
  signal,
  label,
) => {
  const matching = Array.isArray(summary?.attempts)
    ? summary.attempts.filter(
        (attempt) => attempt?.outputTermination?.marker === marker,
      )
    : [];
  check(
    reasons,
    matching.length === 1,
    `${label} must contain exactly one ${marker} termination`,
  );
  const attempt = matching[0];
  check(
    reasons,
    attempt?.outputTermination?.signal === signal && attempt?.signal === signal,
    `${label} must terminate with ${signal} at ${marker}`,
  );
};

export const requireFileTermination = (reasons, summary, signal, label) => {
  const matching = Array.isArray(summary?.attempts)
    ? summary.attempts.filter((attempt) => attempt?.fileTermination !== null)
    : [];
  check(
    reasons,
    matching.length === 1 &&
      matching[0]?.fileTermination?.signal === signal &&
      matching[0]?.signal === signal,
    `${label} must terminate with ${signal} through its stop file`,
  );
};

export const validateRoots = (reasons, value, keys, label) => {
  if (!exactKeys(reasons, value, keys, label)) return;
  for (const key of keys) {
    check(reasons, HASH_32.test(value[key]), `${label}.${key} is invalid`);
  }
};

export const canonicalJsonOrder = (value) =>
  isDeepStrictEqual(
    value,
    [...value].sort((left, right) =>
      JSON.stringify(left).localeCompare(JSON.stringify(right)),
    ),
  );

export const validateJournalMembers = (reasons, value, label) => {
  if (!Array.isArray(value)) {
    reasons.push(`${label} must be an array`);
    return;
  }
  check(
    reasons,
    canonicalJsonOrder(value),
    `${label} is not canonically sorted`,
  );
  const memberIds = new Set();
  const ordinals = new Set();
  value.forEach((member, index) => {
    const itemLabel = `${label}[${index.toString()}]`;
    if (
      !exactKeys(
        reasons,
        member,
        ["memberId", "ordinal", "payloadSha256", "sourceTable", "sourceId"],
        itemLabel,
      )
    ) {
      return;
    }
    check(
      reasons,
      HASH_32.test(member.memberId) &&
        safeNonnegative(member.ordinal) &&
        HASH_32.test(member.payloadSha256) &&
        typeof member.sourceTable === "string" &&
        /^[a-z][a-z0-9_]*$/u.test(member.sourceTable) &&
        (member.sourceId === null || hexBytes(member.sourceId)),
      `${itemLabel} contains a noncanonical value`,
    );
    check(
      reasons,
      !memberIds.has(member.memberId) && !ordinals.has(member.ordinal),
      `${itemLabel} duplicates a member identity or ordinal`,
    );
    memberIds.add(member.memberId);
    ordinals.add(member.ordinal);
  });
};
