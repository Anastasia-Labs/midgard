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
import { dirname, join, relative, resolve, sep } from "node:path";

import { blueprintReadiness, deploymentBuild } from "./blueprint.mjs";
import { buildPackage, checkBuild } from "./build.mjs";
import { atomicJson, json, runtimeBuildClosure, sha256 } from "./files.mjs";
import { buildNative, checkNative } from "./native.mjs";
import { runProcess } from "./process.mjs";
import { pinnedPnpm } from "./pnpm.mjs";
import { withResource } from "./resources.mjs";
import {
  definitionDigest,
  frozenBlueprintPath,
  readCampaign,
  receiptPath,
  stageCommand,
} from "./campaign-definition.mjs";

// A devnet campaign runs an ordered list of devnet stages (devnet-stack
// commands, phase4 L1 services up and down, watcher journeys) against one
// pinned commit: it checks the checkout is that commit, builds once, freezes
// the blueprint, runs each stage under its own deadline and log, and keeps
// one receipt in the campaign directory. Rerunning the same file resumes:
// stages the receipt records as passed with the same definition are skipped.
// It never moves HEAD and never deletes a run's state.

export const CAMPAIGN_RECEIPT_SCHEMA = "midgard-devnet-campaign-receipt/v1";
export const CAMPAIGN_PLAN_SCHEMA = "midgard-devnet-campaign-plan/v1";
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
// A stage past its deadline gets SIGTERM, then SIGKILL this much later.
const GRACE_MS = 120_000;
// A stage whose output passes this is stopped rather than filling the disk.
const MAX_LOG_BYTES = 1 << 30;

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
