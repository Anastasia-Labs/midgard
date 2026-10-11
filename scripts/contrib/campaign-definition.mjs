import { isAbsolute, join, resolve } from "node:path";

import { json, sha256 } from "./files.mjs";
import { NATIVE_RECIPES } from "./native-recipes.mjs";

// The definition of a devnet campaign (midgard-devnet-campaign/v1): its
// validation, the digest and receipt reading that decide which stages a
// resume runs, the command each stage runs and the run-directory inputs a
// journey stage needs. Pure: running a campaign is campaign.mjs.

export const CAMPAIGN_SCHEMA = "midgard-devnet-campaign/v1";

const TOOLS = "demo/midgard-node-tools";
const JOURNEYS_CONFIG = "vitest.watcher-journeys.config.ts";
// Node timers fire at once beyond 2^31-1 ms.
const MAX_TIMEOUT_SECONDS = 2_147_483;
const PHASE4_ACTIONS = ["up", "down"];
// A watcher-journeys run directory as its generator leaves it: the phase4
// run environment, and the node config with host genesis paths plus the
// compose overlay that only the watcher-journeys wrapper writes.
const JOURNEY_RUN_FILES = [
  "run.env",
  "config/config.json",
  "compose.host-paths.json",
];
const JOURNEY_GENERATOR = `${TOOLS}/devnet/watcher-journeys/scripts/generate.sh`;
// The journeys start the run's copy of the campaign-built transport binary.
const TRANSPORT_PACKAGE = "@al-ft/l1-node-transport";
const TRANSPORT_BUILD = join(
  "demo/l1-node-transport",
  NATIVE_RECIPES[TRANSPORT_PACKAGE].output,
);
const JOURNEY_TRANSPORT = "work/midgard-l1-node-transport";
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
  "devnet-stack": { required: ["args"], optional: ["always"] },
  phase4: { required: ["action", "runDir"], optional: ["always"] },
  "watcher-journeys": { required: ["runDir", "files"], optional: ["env"] },
};
const COMMON_STAGE_FIELDS = ["id", "kind", "timeoutSeconds"];

/** Whether a stage starts services: phase4 up, or devnet-stack up. */
export const startsServices = (stage) =>
  stage.kind === "phase4"
    ? stage.action === "up"
    : stage.kind === "devnet-stack" && stage.args?.[0] === "up";

/** Whether a stage stops services (keeping their state): a teardown. */
export const stopsServices = (stage) =>
  stage.kind === "phase4"
    ? stage.action === "down"
    : stage.kind === "devnet-stack" && stage.args?.[0] === "down";

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
  // An always-stage runs even after an earlier stage did not pass; only a
  // teardown may, so a failure never starts anything.
  if (stage.always !== undefined && fields.optional.includes("always")) {
    if (stage.always !== true)
      problems.push(
        `${where}.always must be true; leave it out for an ordinary stage`,
      );
    else if (!stopsServices(stage))
      problems.push(
        `${where}.always is allowed only on a teardown stage (phase4 action down, or devnet-stack down)`,
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

const latestIndex = (receipt, predicate) =>
  receipt?.stages.findLastIndex(predicate) ?? -1;

/** Index of the first stage not recorded as passed, or -1 when all are. */
export const resumePoint = (campaign, receipt) =>
  campaign.stages.findIndex((stage) => {
    const entry =
      receipt?.stages[latestIndex(receipt, (entry) => entry.id === stage.id)];
    return !(
      entry?.status === "passed" &&
      entry.definitionSha256 === definitionDigest(stage)
    );
  });

/**
 * Indices of the always-stages a non-pass at stage `index` still runs: the
 * rest of its block when it is itself an always-stage, else the block of
 * consecutive always-stages that closes its segment. Later segments started
 * nothing, so their teardowns do not run.
 */
export const teardownAfter = (campaign, index) => {
  const { stages } = campaign;
  let next = index + 1;
  if (!stages[index].always)
    while (next < stages.length && !stages[next].always) next += 1;
  const block = [];
  while (next < stages.length && stages[next].always) block.push(next++);
  return block;
};

/**
 * The stages a run or resume runs, in order, as `{ index, stage, restart }`:
 * every stage from the first not passed, after the service-starting stages
 * of its segment (the stages since the previous always-stage) that the
 * always-stages closing the segment ran after: they stopped those services,
 * so they start again, while passed work, whose state a teardown keeps, is
 * skipped. A first stage that is itself an always-stage restarts nothing.
 */
export const resumeStages = (campaign, receipt) => {
  const { stages } = campaign;
  const start = resumePoint(campaign, receipt);
  if (start === -1) return [];
  const restarts = new Set();
  if (!stages[start].always) {
    let first = start;
    while (first > 0 && !stages[first - 1].always) first -= 1;
    const block = teardownAfter(campaign, start).map(
      (index) => stages[index].id,
    );
    const tornDown = latestIndex(receipt, (entry) => block.includes(entry.id));
    for (let index = first; index < start; index += 1)
      if (
        startsServices(stages[index]) &&
        latestIndex(
          receipt,
          (entry) => entry.id === stages[index].id && entry.status === "passed",
        ) < tornDown
      )
        restarts.add(index);
  }
  return stages.flatMap((stage, index) =>
    index >= start || restarts.has(index)
      ? [{ index, stage, restart: restarts.has(index) }]
      : [],
  );
};

/**
 * What a watcher-journeys stage needs in its run directory: the files only
 * the watcher-journeys generator writes (a person runs it: `generate`), and
 * the transport binary the journeys start, which the stage copies from the
 * campaign's build.
 */
export const journeyInputs = (root, stage) => ({
  generated: JOURNEY_RUN_FILES.map((file) => join(stage.runDir, file)),
  generate: `MIDGARD_WATCHER_JOURNEY_RUN_DIR=${stage.runDir} sh ${JOURNEY_GENERATOR}`,
  transport: {
    source: resolve(root, TRANSPORT_BUILD),
    target: join(stage.runDir, JOURNEY_TRANSPORT),
  },
});

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
