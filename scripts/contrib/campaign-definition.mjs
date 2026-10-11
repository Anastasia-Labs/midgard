import { isAbsolute, join, relative, resolve } from "node:path";

import { json, sha256 } from "./files.mjs";

// The definition of a devnet campaign (midgard-devnet-campaign/v1): its
// validation, the digest that decides whether a stage reruns on resume, and
// the command each stage runs. Pure: running a campaign is campaign.mjs.

export const CAMPAIGN_SCHEMA = "midgard-devnet-campaign/v1";

const TOOLS = "demo/midgard-node-tools";
const JOURNEYS_CONFIG = "vitest.watcher-journeys.config.ts";
// Node timers fire at once beyond 2^31-1 ms.
const MAX_TIMEOUT_SECONDS = 2_147_483;
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
