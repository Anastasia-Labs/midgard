import { execFileSync, spawnSync } from "node:child_process";
import {
  copyFileSync,
  existsSync,
  mkdirSync,
  readdirSync,
  readFileSync,
  realpathSync,
  renameSync,
} from "node:fs";
import { randomUUID } from "node:crypto";
import { dirname, isAbsolute, join, relative, resolve, sep } from "node:path";

import { blueprintReadiness, deploymentBuild } from "./blueprint.mjs";
import { buildPackage, checkBuild } from "./build.mjs";
import { atomicJson, json, runtimeBuildClosure, sha256 } from "./files.mjs";
import { buildNative, checkNative } from "./native.mjs";
import { runProcess } from "./process.mjs";
import { pinnedPnpm } from "./pnpm.mjs";
import { withResource } from "./resources.mjs";

// A devnet campaign runs an ordered list of devnet stages (devnet-stack
// commands, phase4 L1 services up and down, watcher journeys) against one
// pinned commit: it checks the checkout is that commit, builds once, freezes
// the blueprint, runs each stage under its own deadline and log, and keeps
// one receipt in the campaign directory. Rerunning the same file resumes:
// stages the receipt records as passed with the same definition are skipped.
// It never moves HEAD and never deletes a run's state.

export const CAMPAIGN_SCHEMA = "midgard-devnet-campaign/v1";
export const CAMPAIGN_RECEIPT_SCHEMA = "midgard-devnet-campaign-receipt/v1";
export const CAMPAIGN_PLAN_SCHEMA = "midgard-devnet-campaign-plan/v1";

const TOOLS = "demo/midgard-node-tools";
const JOURNEYS_CONFIG = "vitest.watcher-journeys.config.ts";
const BLUEPRINT = "onchain/aiken/plutus.json";
// The node reads the blueprint's build record beside it.
const RECORD_SUFFIX = ".deployment.json";
// `deployment:build <profile>` rewrites this tracked file to select the
// profile; that one change is the campaign's own build output.
const GENERATED_PROFILES =
  "demo/midgard-core/src/generated-deployment-profiles.ts";
// The entry points devnet-stack and the journeys start; their runtime
// closures cover every dist the stack executes (dist-freshness.ts).
const RUNTIME_PACKAGES = [
  "midgard-node-tools",
  "midgard-node",
  "da-committee-node",
  "midgard-watcher",
];
const NATIVE_PACKAGES = ["midgard-node", "@al-ft/l1-node-transport"];
// Node timers fire at once beyond 2^31-1 ms.
const MAX_TIMEOUT_SECONDS = 2_147_483;
// A stage past its deadline gets SIGTERM, then SIGKILL this much later.
const GRACE_MS = 120_000;
// A stage whose output passes this is stopped rather than filling the disk.
const MAX_LOG_BYTES = 1 << 30;
const PHASE4_ACTIONS = ["up", "down"];
// What the campaign itself sets for a watcher-journeys stage.
const JOURNEY_ENV_KEYS = [
  "MIDGARD_DEPLOYMENT_PROFILE",
  "NETWORK",
  "MIDGARD_DOTENV_MODE",
  "MIDGARD_WATCHER_JOURNEY_RUN_DIR",
  "MIDGARD_REAL_BLUEPRINT_PATH",
  "MIDGARD_CONTRIB_VERBOSE",
];

const ID = /^[a-z0-9][a-z0-9-]{0,63}$/u;
const COMMIT = /^[0-9a-f]{40}$/u;
const PROFILE = /^[a-z0-9][a-z0-9-]{0,63}$/u;
const ENV_KEY = /^[A-Za-z_][A-Za-z0-9_]*$/u;
const JOURNEY_FILE =
  /^devnet\/watcher-journeys\/(?:[\w-]+\/)*[\w.-]+\.test\.ts$/u;

const TOP_FIELDS = [
  "schema",
  "id",
  "commit",
  "deploymentProfile",
  "directory",
  "stages",
];
const STAGE_FIELDS = {
  "devnet-stack": { required: ["args"], optional: [] },
  phase4: { required: ["action", "runDir"], optional: [] },
  "watcher-journeys": { required: ["runDir", "files"], optional: ["env"] },
};
const COMMON_STAGE_FIELDS = ["id", "kind", "timeoutSeconds"];

const isObject = (value) =>
  Boolean(value) && typeof value === "object" && !Array.isArray(value);
const isAbsolutePath = (value) =>
  typeof value === "string" && isAbsolute(value) && resolve(value) === value;
const isText = (value) =>
  typeof value === "string" && value.length > 0 && !value.includes("\0");

const validateStage = (stage, where, problems) => {
  if (!isObject(stage)) {
    problems.push(`${where} must be an object`);
    return;
  }
  const fields = STAGE_FIELDS[stage.kind];
  if (!fields) {
    problems.push(
      `${where}.kind must be one of ${Object.keys(STAGE_FIELDS).join(", ")}`,
    );
    return;
  }
  const allowed = [
    ...COMMON_STAGE_FIELDS,
    ...fields.required,
    ...fields.optional,
  ];
  for (const key of Object.keys(stage))
    if (!allowed.includes(key))
      problems.push(`${where} (${stage.kind}) has unknown field ${key}`);
  for (const key of fields.required)
    if (stage[key] === undefined) problems.push(`${where}.${key} is required`);
  if (typeof stage.id !== "string" || !ID.test(stage.id))
    problems.push(
      `${where}.id must be a lowercase identifier of 1..64 characters`,
    );
  if (
    !Number.isSafeInteger(stage.timeoutSeconds) ||
    stage.timeoutSeconds <= 0 ||
    stage.timeoutSeconds > MAX_TIMEOUT_SECONDS
  )
    problems.push(
      `${where}.timeoutSeconds must be a whole number of seconds from 1 to ${MAX_TIMEOUT_SECONDS}`,
    );
  if (stage.kind === "devnet-stack") {
    if (
      stage.args !== undefined &&
      (!Array.isArray(stage.args) ||
        stage.args.length === 0 ||
        !stage.args.every(isText))
    )
      problems.push(`${where}.args must be a nonempty array of strings`);
  }
  if (stage.kind === "phase4") {
    if (stage.action !== undefined && !PHASE4_ACTIONS.includes(stage.action))
      problems.push(
        `${where}.action must be one of ${PHASE4_ACTIONS.join(", ")}`,
      );
  }
  if (stage.runDir !== undefined && !isAbsolutePath(stage.runDir))
    problems.push(`${where}.runDir must be an absolute normalized path`);
  if (stage.kind === "watcher-journeys") {
    if (
      stage.files !== undefined &&
      (!Array.isArray(stage.files) ||
        stage.files.length === 0 ||
        new Set(stage.files).size !== stage.files.length ||
        !stage.files.every(
          (file) => typeof file === "string" && JOURNEY_FILE.test(file),
        ))
    )
      problems.push(
        `${where}.files must be distinct devnet/watcher-journeys/**/*.test.ts paths, relative to ${TOOLS}`,
      );
    if (stage.env !== undefined) {
      if (!isObject(stage.env)) problems.push(`${where}.env must be an object`);
      else
        for (const [key, value] of Object.entries(stage.env)) {
          if (!ENV_KEY.test(key))
            problems.push(`${where}.env key ${key} is not a variable name`);
          else if (
            JOURNEY_ENV_KEYS.includes(key) ||
            key.startsWith("MIDGARD_CONTRIB_")
          )
            problems.push(
              `${where}.env may not set ${key}: the campaign sets it`,
            );
          if (typeof value !== "string" || value.includes("\0"))
            problems.push(`${where}.env.${key} must be a string`);
        }
    }
  }
};

/** The campaign, or an error naming every problem with it. */
export const parseCampaign = (value) => {
  const problems = [];
  if (!isObject(value)) throw new Error("invalid campaign: must be an object");
  for (const key of Object.keys(value))
    if (!TOP_FIELDS.includes(key))
      problems.push(`campaign has unknown field ${key}`);
  if (value.schema !== CAMPAIGN_SCHEMA)
    problems.push(`campaign.schema must be ${CAMPAIGN_SCHEMA}`);
  if (typeof value.id !== "string" || !ID.test(value.id))
    problems.push(
      "campaign.id must be a lowercase identifier of 1..64 characters",
    );
  if (typeof value.commit !== "string" || !COMMIT.test(value.commit))
    problems.push("campaign.commit must be a full 40-character commit sha");
  if (
    typeof value.deploymentProfile !== "string" ||
    !PROFILE.test(value.deploymentProfile)
  )
    problems.push(
      "campaign.deploymentProfile must name a deployment profile, such as local-devnet-testing",
    );
  if (!isAbsolutePath(value.directory))
    problems.push("campaign.directory must be an absolute normalized path");
  if (!Array.isArray(value.stages) || value.stages.length === 0)
    problems.push("campaign.stages must be a nonempty array");
  else {
    value.stages.forEach((stage, index) =>
      validateStage(stage, `campaign.stages[${index}]`, problems),
    );
    const ids = value.stages.map((stage) => stage?.id);
    const repeated = ids.filter(
      (id, index) => typeof id === "string" && ids.indexOf(id) !== index,
    );
    if (repeated.length)
      problems.push(
        `campaign stage ids repeat: ${[...new Set(repeated)].join(", ")}`,
      );
  }
  if (problems.length)
    throw new Error(`invalid campaign: ${problems.join("; ")}`);
  return value;
};

export const readCampaign = (path) => parseCampaign(json(resolve(path)));

const canonical = (value) =>
  Array.isArray(value)
    ? `[${value.map(canonical).join(",")}]`
    : isObject(value)
      ? `{${Object.keys(value)
          .sort()
          .map((key) => `${JSON.stringify(key)}:${canonical(value[key])}`)
          .join(",")}}`
      : JSON.stringify(value);

/** A digest of everything a stage's definition says; a change reruns it. */
export const definitionDigest = (stage) => sha256(canonical(stage));

export const receiptPath = (campaign) =>
  resolve(campaign.directory, "campaign-receipt.json");
export const frozenBlueprintPath = (campaign) =>
  resolve(campaign.directory, "plutus.json");

/**
 * The argv, working directory and campaign-set environment of one attempt
 * of a stage. `name` is the attempt's file stem under logs/ and receipts/.
 */
export const stageCommand = (root, campaign, stage, name = stage.id) => {
  const tools = resolve(root, TOOLS);
  if (stage.kind === "devnet-stack")
    return {
      argv: [
        process.execPath,
        resolve(tools, "dist/devnet-stack.js"),
        ...stage.args,
      ],
      cwd: tools,
      env: {},
    };
  if (stage.kind === "phase4")
    return {
      argv: [
        "sh",
        resolve(tools, `devnet/phase4-process/scripts/${stage.action}.sh`),
        stage.runDir,
      ],
      cwd: tools,
      env: {},
    };
  const output = resolve(campaign.directory, "receipts", `${name}.json`);
  return {
    argv: [
      process.execPath,
      resolve(root, "scripts/contrib.mjs"),
      "--root",
      root,
      "test",
      "--package",
      "midgard-node-tools",
      "--config",
      JOURNEYS_CONFIG,
      "--timeout-seconds",
      String(stage.timeoutSeconds),
      "--output",
      output,
      "--file",
      ...stage.files,
    ],
    cwd: root,
    env: {
      ...stage.env,
      MIDGARD_DEPLOYMENT_PROFILE: campaign.deploymentProfile,
      NETWORK: "Custom",
      MIDGARD_DOTENV_MODE: "disabled",
      MIDGARD_WATCHER_JOURNEY_RUN_DIR: stage.runDir,
      MIDGARD_REAL_BLUEPRINT_PATH: frozenBlueprintPath(campaign),
      // The journeys' own output goes to the stage log, not only to the
      // contrib test log in the run registry.
      MIDGARD_CONTRIB_VERBOSE: "1",
    },
    output,
  };
};

// A hook exports GIT_DIR and friends; with them git would act on the hook's
// repository instead of the checkout.
const gitOutput = (root, args) =>
  execFileSync("git", args, {
    cwd: root,
    env: Object.fromEntries(
      Object.entries(process.env).filter(([key]) => !key.startsWith("GIT_")),
    ),
    encoding: "utf8",
  });

/** Whether the generated deployment files are exactly those of `profile`. */
export const deploymentProfileCurrent = (root, profile) =>
  spawnSync(
    process.execPath,
    ["scripts/deployment-profiles.mjs", "check", profile],
    { cwd: resolve(root, "demo"), stdio: "ignore", timeout: 300_000 },
  ).status === 0;

/**
 * `{ head, refusal }`: the refusal says why this checkout cannot run the
 * campaign (HEAD is another commit, or the tree has changes), or is absent.
 */
export const pinCheck = (root, campaign, { git, profileCheck }) => {
  const head = git(root, ["rev-parse", "HEAD"]).trim();
  const fix = `create a checkout at the commit and run the campaign there: node scripts/contrib.mjs worktree create --branch <name> --base ${campaign.commit} --package midgard-node-tools`;
  if (head !== campaign.commit)
    return {
      head,
      refusal: `this checkout's HEAD is ${head}, not the campaign commit ${campaign.commit}; a campaign never moves HEAD: ${fix}`,
    };
  const changes = git(root, [
    "status",
    "--porcelain=v1",
    "--untracked-files=all",
  ])
    .split("\n")
    .filter((line) => line && line !== `?? ${BLUEPRINT}`);
  const generated = ` M ${GENERATED_PROFILES}`;
  const others = changes.filter((line) => line !== generated);
  if (others.length)
    return {
      head,
      refusal: `the checkout has ${others.length} change(s) or untracked file(s), first: ${others[0].trim()}; a campaign runs a clean commit: ${fix}`,
    };
  if (
    changes.includes(generated) &&
    !profileCheck(root, campaign.deploymentProfile)
  )
    return {
      head,
      refusal: `${GENERATED_PROFILES} is modified and is not the generated file of profile ${campaign.deploymentProfile}; a campaign runs a clean commit: ${fix}`,
    };
  return { head };
};

const insidePath = (base, path) =>
  path === base || path.startsWith(`${base}${sep}`);

// The campaign directory holds logs and the frozen blueprint: inside the
// checkout they would be untracked changes, so the next resume would refuse.
const checkDirectory = (root, campaign, inputPath) => {
  let existing = campaign.directory;
  while (!existsSync(existing)) existing = dirname(existing);
  const real = join(
    realpathSync(existing),
    relative(existing, campaign.directory),
  );
  if (insidePath(realpathSync(root), real))
    throw new Error(
      `campaign directory ${campaign.directory} is inside the checkout; choose a directory outside it`,
    );
  if (existsSync(campaign.directory) && !existsSync(receiptPath(campaign))) {
    const strays = readdirSync(campaign.directory).filter(
      (name) => resolve(campaign.directory, name) !== resolve(inputPath),
    );
    if (strays.length)
      throw new Error(
        `${campaign.directory} holds ${strays[0]} but no campaign receipt; choose a new campaign directory`,
      );
  }
};

/** The directory's receipt, refused when it belongs to another campaign. */
export const loadReceipt = (campaign) => {
  const path = receiptPath(campaign);
  if (!existsSync(path)) return undefined;
  const receipt = json(path);
  if (receipt.schema !== CAMPAIGN_RECEIPT_SCHEMA)
    throw new Error(
      `${path} is not a campaign receipt; choose a new campaign directory`,
    );
  const drift = ["id", "commit", "deploymentProfile"].filter(
    (key) => receipt[key] !== campaign[key],
  );
  if (drift.length)
    throw new Error(
      `${campaign.directory} holds campaign ${receipt.id} at commit ${receipt.commit} with profile ${receipt.deploymentProfile}; this campaign has another ${drift.join(" and ")}. A code change needs a new campaign directory: a run's stages never mix two builds`,
    );
  return receipt;
};

const latestEntry = (receipt, id) =>
  receipt?.stages.filter((entry) => entry.id === id).at(-1);

/** Index of the first stage not recorded as passed, or -1 when all are. */
export const resumePoint = (campaign, receipt) =>
  campaign.stages.findIndex((stage) => {
    const entry = latestEntry(receipt, stage.id);
    return !(
      entry?.status === "passed" &&
      entry.definitionSha256 === definitionDigest(stage)
    );
  });

// Each attempt of a stage gets its own log; a rerun never truncates the log
// of the attempt it replaces.
const attemptName = (campaign, receipt, stage) => {
  let attempt =
    (receipt?.stages.filter((entry) => entry.id === stage.id).length ?? 0) + 1;
  const name = () => (attempt === 1 ? stage.id : `${stage.id}.${attempt}`);
  while (existsSync(resolve(campaign.directory, "logs", `${name()}.log`)))
    attempt += 1;
  return { attempt, name: name() };
};

/** The build steps a campaign's stages need, in order. */
export const buildSteps = (root, campaign) => {
  if (campaign.stages.every((stage) => stage.kind === "phase4")) return [];
  const packages = [
    ...new Set(
      RUNTIME_PACKAGES.flatMap((name) =>
        runtimeBuildClosure(root, name).map((pkg) => pkg.name),
      ),
    ),
  ];
  return [
    { step: "install" },
    { step: "deployment-build", profile: campaign.deploymentProfile },
    ...packages.map((name) => ({ step: "build", package: name })),
    ...NATIVE_PACKAGES.map((name) => ({ step: "native", package: name })),
  ];
};

// pnpm keeps a copy of the lockfile it installed from (as devnet-stack's
// own build check reads it).
const installFresh = (root) => {
  const installed = resolve(root, "demo/node_modules/.pnpm/lock.yaml");
  return (
    existsSync(installed) &&
    readFileSync(installed).equals(
      readFileSync(resolve(root, "demo/pnpm-lock.yaml")),
    )
  );
};

/** `{ status: "fresh" }` or `{ status: "needed", reason }` for one step. */
export const stepFreshness = (root, step, { profileCheck }) => {
  try {
    if (step.step === "install")
      return installFresh(root)
        ? { status: "fresh" }
        : {
            status: "needed",
            reason:
              "the installed dependencies do not match demo/pnpm-lock.yaml",
          };
    if (step.step === "deployment-build") {
      if (!existsSync(resolve(root, "demo/node_modules")))
        return { status: "needed", reason: "the workspace is not installed" };
      if (!profileCheck(root, step.profile))
        return {
          status: "needed",
          reason: `the generated deployment files are not those of ${step.profile}`,
        };
      const readiness = blueprintReadiness(root);
      return readiness.ready
        ? { status: "fresh" }
        : { status: "needed", reason: readiness.reason };
    }
    const verdict =
      step.step === "build"
        ? checkBuild(root, step.package)
        : checkNative(root, step.package);
    return verdict.status === "fresh"
      ? { status: "fresh" }
      : { status: "needed", reason: verdict.reason };
  } catch (error) {
    return { status: "needed", reason: error.message };
  }
};

/**
 * Run every build step that is not already fresh, in order, stopping at the
 * first failure. Package and native builds are contrib's own guarded builds
 * with their receipts; logs of the others go to the campaign's logs/.
 */
export const runBuild = async (
  root,
  campaign,
  {
    signal,
    env,
    logPath,
    profileCheck,
    freshness = stepFreshness,
    steps: planned = buildSteps(root, campaign),
  },
) => {
  const steps = [];
  for (const step of planned) {
    const verdict = freshness(root, step, { profileCheck });
    if (verdict.status === "fresh") {
      steps.push({ ...step, status: "fresh" });
      continue;
    }
    const record = { ...step, status: "built", why: verdict.reason };
    let failure;
    if (step.step === "install") {
      const result = await runProcess({
        ...pinnedPnpm(resolve(root, "demo"), ["install", "--frozen-lockfile"]),
        env,
        signal,
        logPath: logPath("build-install"),
        timeoutMs: 3_600_000,
      });
      record.logPath = result.logPath;
      if (result.exitCode !== 0 || result.reason)
        failure = `pnpm install --frozen-lockfile failed${result.reason ? `: ${result.reason}` : ""}`;
    } else if (step.step === "deployment-build") {
      const result = await deploymentBuild(root, step.profile, {
        signal,
        env,
        logPath: logPath("build-deployment"),
      });
      record.logPath = result.logPath;
      if (result.exitCode !== 0)
        failure = `pnpm --dir demo deployment:build ${step.profile} exited ${result.exitCode}`;
      else if (freshness(root, step, { profileCheck }).status !== "fresh")
        failure = `deployment:build ${step.profile} did not leave a ready blueprint of that profile`;
    } else {
      const result =
        step.step === "build"
          ? await buildPackage(root, step.package, { signal, env })
          : await buildNative(root, step.package, { signal, env });
      record.receipt = result.path;
      if (result.exitCode !== 0)
        failure = `${step.step} of ${step.package} failed${result.reason ? `: ${result.reason}` : ""}`;
    }
    if (failure) {
      steps.push({ ...record, status: "failed", reason: failure });
      return { status: "failed", steps };
    }
    steps.push(record);
  }
  return { status: "passed", steps };
};

const fileSha256 = (path) => sha256(readFileSync(path));

const copyAtomically = (source, destination) => {
  const temporary = `${destination}.${randomUUID()}.tmp`;
  copyFileSync(source, temporary);
  renameSync(temporary, destination);
};

/**
 * Freeze the checkout's blueprint and its build record into the campaign
 * directory once; on a resume, refuse a frozen copy or a checkout blueprint
 * that no longer matches what was frozen.
 */
export const freezeBlueprint = (root, campaign, frozen) => {
  const source = resolve(root, BLUEPRINT);
  const destination = frozenBlueprintPath(campaign);
  const pairs = [
    [source, destination],
    [`${source}${RECORD_SUFFIX}`, `${destination}${RECORD_SUFFIX}`],
  ];
  for (const [from] of pairs)
    if (!existsSync(from))
      throw new Error(
        `no blueprint to freeze: ${from} is missing after the build`,
      );
  if (frozen) {
    const recorded = [frozen.sha256, frozen.record.sha256];
    pairs.forEach(([from, to], index) => {
      if (!existsSync(to) || fileSha256(to) !== recorded[index])
        throw new Error(
          `the frozen ${to} is missing or changed since the campaign froze it; a campaign never reuses another blueprint: start a new campaign directory`,
        );
      if (fileSha256(from) !== recorded[index])
        throw new Error(
          `${from} differs from the blueprint this campaign froze; a changed blueprint needs a new campaign directory`,
        );
    });
    return frozen;
  }
  for (const [from, to] of pairs) copyAtomically(from, to);
  return {
    path: destination,
    sha256: fileSha256(destination),
    record: {
      path: `${destination}${RECORD_SUFFIX}`,
      sha256: fileSha256(`${destination}${RECORD_SUFFIX}`),
    },
    frozenAt: new Date().toISOString(),
  };
};

const DEFAULTS = {
  run: runProcess,
  build: runBuild,
  git: gitOutput,
  profileCheck: deploymentProfileCurrent,
  freshness: stepFreshness,
  buildSteps,
  graceMs: GRACE_MS,
};

/** What `run` would do, executing nothing. */
export const campaignPlan = (root, inputPath, overrides = {}) => {
  const deps = { ...DEFAULTS, ...overrides };
  const campaign = readCampaign(inputPath);
  checkDirectory(root, campaign, inputPath);
  const receipt = loadReceipt(campaign);
  const { head, refusal } = pinCheck(root, campaign, deps);
  const start = resumePoint(campaign, receipt);
  return {
    schema: CAMPAIGN_PLAN_SCHEMA,
    id: campaign.id,
    root,
    commit: campaign.commit,
    head,
    deploymentProfile: campaign.deploymentProfile,
    directory: campaign.directory,
    receipt: receiptPath(campaign),
    ...(refusal ? { refusal } : {}),
    build:
      start === -1
        ? []
        : deps.buildSteps(root, campaign).map((step) => ({
            ...step,
            ...deps.freshness(root, step, deps),
          })),
    blueprint: receipt?.blueprint
      ? {
          path: receipt.blueprint.path,
          status: `frozen ${receipt.blueprint.sha256}`,
        }
      : {
          path: frozenBlueprintPath(campaign),
          status: "frozen after the build",
        },
    stages: campaign.stages.map((stage, index) => {
      const skip = start === -1 || index < start;
      const { name } = attemptName(campaign, receipt, stage);
      const command = stageCommand(root, campaign, stage, name);
      return {
        id: stage.id,
        kind: stage.kind,
        action: skip ? "skip: passed" : "run",
        timeoutSeconds: stage.timeoutSeconds,
        argv: command.argv,
        cwd: command.cwd,
        envKeys: Object.keys(command.env).sort(),
        logPath: resolve(campaign.directory, "logs", `${name}.log`),
      };
    }),
    exitCode: refusal ? 1 : 0,
  };
};

const save = (receipt) => {
  receipt.updatedAt = new Date().toISOString();
  atomicJson(receipt.path, receipt);
};

const childResult = (output) => {
  if (!output || !existsSync(output)) return undefined;
  try {
    const result = json(output);
    return {
      output,
      receipt: result.path,
      status: result.status,
      exitCode: result.exitCode,
      counts: result.counts,
    };
  } catch (error) {
    return { output, error: error.message };
  }
};

const stageStatus = (stage, result, child) => {
  if (result.reason === "cancelled") return "cancelled";
  if (result.reason || result.signal) return "failed";
  if (stage.kind === "watcher-journeys") {
    if (!child?.status) return "failed";
    if (result.exitCode === 3 && child.status === "incomplete")
      return "incomplete";
    return result.exitCode === 0 && child.status === "passed"
      ? "passed"
      : "failed";
  }
  return result.exitCode === 0 ? "passed" : "failed";
};

const runStage = async (
  root,
  campaign,
  stage,
  receipt,
  { run, env, signal, graceMs },
) => {
  const { attempt, name } = attemptName(campaign, receipt, stage);
  const command = stageCommand(root, campaign, stage, name);
  const entry = {
    id: stage.id,
    kind: stage.kind,
    attempt,
    definitionSha256: definitionDigest(stage),
    status: "running",
    argv: command.argv,
    cwd: command.cwd,
    // Keys only: values may be credentials. The stage inherits the
    // campaign's own environment beneath them.
    envKeys: Object.keys(command.env).sort(),
    timeoutSeconds: stage.timeoutSeconds,
    logPath: resolve(campaign.directory, "logs", `${name}.log`),
    startedAt: new Date().toISOString(),
  };
  receipt.stages.push(entry);
  save(receipt);
  let result;
  try {
    result = await run({
      argv: command.argv,
      cwd: command.cwd,
      env: { ...env, ...command.env },
      signal,
      logPath: entry.logPath,
      timeoutMs: stage.timeoutSeconds * 1000,
      graceMs,
      maxBytes: MAX_LOG_BYTES,
    });
  } catch (error) {
    result = {
      exitCode: null,
      signal: null,
      reason: signal?.aborted ? "cancelled" : error.message,
      endedAt: new Date().toISOString(),
    };
  }
  const child = childResult(command.output);
  Object.assign(entry, {
    startedAt: result.startedAt ?? entry.startedAt,
    endedAt: result.endedAt ?? new Date().toISOString(),
    durationMs: result.durationMs,
    exitCode: result.exitCode,
    signal: result.signal ?? undefined,
    reason: result.reason,
    timedOut: /^deadline exceeded/u.test(result.reason ?? ""),
    logSha256: existsSync(entry.logPath)
      ? fileSha256(entry.logPath)
      : undefined,
    ...(command.output
      ? { childReceipt: child ?? { output: command.output, status: "absent" } }
      : {}),
  });
  entry.status = stageStatus(stage, result, child);
  save(receipt);
  return entry;
};

// `outcome` is "failed" when this invocation's build or a stage failed; a
// campaign with stages left and no failure is incomplete (cancelled, or a
// journey stage that skipped tests).
const finish = (receipt, campaign, outcome) => {
  receipt.status =
    resumePoint(campaign, receipt) === -1
      ? "passed"
      : outcome === "failed"
        ? "failed"
        : "incomplete";
  receipt.exitCode =
    receipt.status === "passed" ? 0 : receipt.status === "failed" ? 1 : 3;
  const invocation = receipt.invocations.at(-1);
  if (invocation) {
    invocation.endedAt = new Date().toISOString();
    invocation.status = receipt.status;
  }
  save(receipt);
  return receipt;
};

/**
 * Run (or resume) a campaign: refuse a checkout that is not the pinned
 * commit, build what the stages need once, freeze the blueprint, then run
 * each stage not yet passed in order, stopping at the first that does not
 * pass. Exit 0 when every stage passed, 1 on a failure, 3 when incomplete
 * (cancelled, or a journey that skipped tests).
 */
export const runCampaign = async (
  root,
  inputPath,
  { signal, env = process.env, ...overrides } = {},
) => {
  const deps = { ...DEFAULTS, ...overrides };
  const campaign = readCampaign(inputPath);
  checkDirectory(root, campaign, inputPath);
  return withResource(
    `devnet-campaign:${campaign.directory}`,
    (campaignEnv) =>
      withResource(
        `workspace:${realpathSync(root)}`,
        async (ownedEnv) => {
          const receipt = loadReceipt(campaign) ?? {
            schema: CAMPAIGN_RECEIPT_SCHEMA,
            kind: "devnet-campaign",
            id: campaign.id,
            root: realpathSync(root),
            commit: campaign.commit,
            deploymentProfile: campaign.deploymentProfile,
            directory: campaign.directory,
            status: "running",
            blueprint: undefined,
            builds: [],
            invocations: [],
            stages: [],
            createdAt: new Date().toISOString(),
          };
          receipt.path = receiptPath(campaign);
          // The lease is ours, so an entry still running belongs to a
          // campaign process that died; it did not pass.
          for (const entry of receipt.stages)
            if (entry.status === "running") entry.status = "interrupted";
          const start = resumePoint(campaign, receipt);
          if (start === -1) return finish(receipt, campaign);
          const pin = pinCheck(root, campaign, deps);
          if (pin.refusal) throw new Error(pin.refusal);
          const invocation = {
            startedAt: new Date().toISOString(),
            input: resolve(inputPath),
            inputSha256: fileSha256(resolve(inputPath)),
            skipped: campaign.stages.slice(0, start).map((stage) => stage.id),
            stagesRun: [],
          };
          receipt.invocations.push(invocation);
          mkdirSync(resolve(campaign.directory, "logs"), { recursive: true });
          save(receipt);
          const logPath = (stem) => {
            let index = 1;
            const path = () =>
              resolve(
                campaign.directory,
                "logs",
                `${stem}${index === 1 ? "" : `.${index}`}.log`,
              );
            while (existsSync(path())) index += 1;
            return path();
          };
          const steps = deps.buildSteps(root, campaign);
          if (steps.length) {
            const startedAt = new Date().toISOString();
            const build = await deps.build(root, campaign, {
              signal,
              env: ownedEnv,
              logPath,
              profileCheck: deps.profileCheck,
              freshness: deps.freshness,
              steps,
            });
            receipt.builds.push({
              startedAt,
              endedAt: new Date().toISOString(),
              ...build,
            });
            if (build.status !== "passed")
              return finish(
                receipt,
                campaign,
                signal?.aborted ? "incomplete" : "failed",
              );
            receipt.blueprint = freezeBlueprint(
              root,
              campaign,
              receipt.blueprint,
            );
            save(receipt);
          }
          for (const stage of campaign.stages.slice(start)) {
            if (signal?.aborted) break;
            // HEAD or the tree may have moved under a long campaign.
            const again = pinCheck(root, campaign, deps);
            if (again.refusal) {
              finish(receipt, campaign, "failed");
              throw new Error(again.refusal);
            }
            invocation.stagesRun.push(stage.id);
            const entry = await runStage(root, campaign, stage, receipt, {
              run: deps.run,
              env: ownedEnv,
              signal,
              graceMs: deps.graceMs,
            });
            if (entry.status !== "passed")
              return finish(
                receipt,
                campaign,
                entry.status === "failed" ? "failed" : "incomplete",
              );
          }
          return finish(receipt, campaign);
        },
        { signal, env: campaignEnv },
      ),
    { signal, env },
  );
};

/** The lines a person acts on; --output keeps the whole receipt or plan. */
export const campaignView = (result) => {
  if (result?.schema !== CAMPAIGN_RECEIPT_SCHEMA) return result;
  return {
    status: result.status,
    id: result.id,
    commit: result.commit,
    blueprint: result.blueprint?.sha256,
    build: result.builds
      .at(-1)
      ?.steps.map((step) =>
        [step.step, step.package ?? step.profile, step.status, step.reason]
          .filter(Boolean)
          .join(" "),
      ),
    stages: result.stages.slice(-20).map((entry) => ({
      id: entry.id,
      attempt: entry.attempt,
      status: entry.status,
      exitCode: entry.exitCode,
      reason: entry.reason,
      log: entry.logPath,
      ...(entry.childReceipt
        ? { testReceipt: entry.childReceipt.receipt }
        : {}),
    })),
    receipt: result.path,
    exitCode: result.exitCode,
  };
};
