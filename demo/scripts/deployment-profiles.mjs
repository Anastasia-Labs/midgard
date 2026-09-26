import { createHash } from "node:crypto";
import { existsSync, readFileSync, unlinkSync, writeFileSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { parseDocument } from "yaml";
import { format } from "prettier";

const root = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
export const profileNames = [
  "mainnet",
  "preprod-public",
  "preprod-testing",
  "local-devnet-testing",
];
const networks = ["Mainnet", "Preprod", "Preprod", "Custom"];
const timingConstants = {
  block_maturity_ms: "block_maturity_duration_v1",
  dispute_response_window_ms: "response_window_milliseconds",
  operator_shift_ms: "shift_duration",
  registration_ms: "registration_duration",
  event_wait_ms: "event_wait_duration",
  user_events_negligence_timeout_ms: "user_events_negligence_timeout",
  max_inactivity_between_block_commitments_ms:
    "max_inactivity_between_block_commitments",
  new_shift_inactivity_grace_period_ms: "new_shift_inactivity_grace_period",
  max_validity_range_ms: "max_validity_range_length",
  da_attestation_timeout_ms: "da_attestation_timeout_v1",
  da_small_response_window_ms: "small_response_window_ms_v1",
  da_full_response_window_ms: "full_response_window_ms_v1",
};
const economicsConstants = {
  requiredBondLovelace: "required_bond",
  slashingPenaltyLovelace: "slashing_penalty",
  inactivitySlashingPenaltyLovelace: "inactivity_slashing_penalty",
  fraudProverRewardLovelace: "fraud_prover_reward",
  proverCollateralFloorLovelace: "prover_collateral_floor",
};
const limitConstants = {
  max_bisection_rounds: "max_bisection_rounds",
  max_inactivity_strikes: "max_inactivity_strikes",
  coins_per_utxo_byte: "coins_per_utxo_byte",
};

export const canonicalJson = (value) =>
  JSON.stringify(
    value !== null && typeof value === "object"
      ? Object.fromEntries(
          Object.keys(value)
            .sort()
            .map((key) => [key, JSON.parse(canonicalJson(value[key]))]),
        )
      : value,
  );
export const profileDigest = (profile) =>
  createHash("sha256").update(canonicalJson(profile)).digest("hex");

const exactKeys = (value, keys, field) => {
  if (
    value === null ||
    typeof value !== "object" ||
    Array.isArray(value) ||
    Object.keys(value).length !== keys.length ||
    keys.some((key) => !Object.hasOwn(value, key))
  ) {
    throw new Error(`${field} must contain exactly ${keys.join(", ")}`);
  }
};
const positiveIntegers = (value, keys, field) => {
  for (const key of keys) {
    if (!Number.isSafeInteger(value[key]) || value[key] <= 0) {
      throw new Error(`${field}.${key} must be a positive safe integer`);
    }
  }
};

export const validateProfile = (profile, name) => {
  exactKeys(
    profile,
    ["name", "network", "l1_finality", "timing", "limits", "economics"],
    "profile",
  );
  const index = profileNames.indexOf(name);
  if (
    index < 0 ||
    profile.name !== name ||
    profile.network !== networks[index]
  ) {
    throw new Error(`Profile name/network must match ${name}`);
  }
  exactKeys(profile.l1_finality, ["confirmation_depth"], "l1_finality");
  positiveIntegers(profile.l1_finality, ["confirmation_depth"], "l1_finality");
  exactKeys(profile.timing, Object.keys(timingConstants), "timing");
  positiveIntegers(profile.timing, Object.keys(timingConstants), "timing");
  exactKeys(profile.limits, Object.keys(limitConstants), "limits");
  positiveIntegers(profile.limits, Object.keys(limitConstants), "limits");
  exactKeys(
    profile.economics,
    ["profile", ...Object.keys(economicsConstants)],
    "economics",
  );
  positiveIntegers(
    profile.economics,
    Object.keys(economicsConstants),
    "economics",
  );
  const expectedEconomics = name.endsWith("testing")
    ? "bounded-acceptance-v1"
    : "public-preprod-launch-v1";
  if (profile.economics.profile !== expectedEconomics)
    throw new Error(`economics.profile must equal ${expectedEconomics}`);
  const timing = profile.timing;
  const maturity = BigInt(timing.block_maturity_ms);
  const disputeDuration =
    (2n * BigInt(profile.limits.max_bisection_rounds) + 2n) *
    BigInt(timing.dispute_response_window_ms);
  const nonInteractiveTesting =
    name === "preprod-testing" || name === "local-devnet-testing";
  if (nonInteractiveTesting && disputeDuration <= maturity)
    throw new Error(
      `Non-interactive ${name} must exclude interactive dispute opening`,
    );
  if (!nonInteractiveTesting && 2n * disputeDuration > maturity)
    throw new Error("Dispute schedule must fit in half of block maturity");
  if (
    timing.da_attestation_timeout_ms >= timing.block_maturity_ms ||
    timing.da_full_response_window_ms >= timing.block_maturity_ms ||
    timing.da_small_response_window_ms > timing.da_full_response_window_ms
  ) {
    throw new Error(
      "DA response windows must be ordered and shorter than block maturity",
    );
  }
  if (
    timing.new_shift_inactivity_grace_period_ms > timing.operator_shift_ms ||
    timing.max_validity_range_ms > timing.operator_shift_ms
  ) {
    throw new Error(
      "Operator shift must cover grace and maximum validity range",
    );
  }
  const economics = profile.economics;
  if (
    BigInt(economics.requiredBondLovelace) !==
      BigInt(economics.slashingPenaltyLovelace) +
        BigInt(economics.fraudProverRewardLovelace) ||
    economics.inactivitySlashingPenaltyLovelace >=
      economics.slashingPenaltyLovelace
  ) {
    throw new Error(
      "Bond must equal slash plus reward; inactivity penalty must be smaller than slash",
    );
  }
  return profile;
};

export const readProfiles = () =>
  Object.fromEntries(
    profileNames.map((name) => {
      const document = parseDocument(
        readFileSync(resolve(root, `config/deployments/${name}.yaml`), "utf8"),
        { uniqueKeys: true },
      );
      if (document.errors.length || document.warnings.length)
        throw new Error(
          `Invalid YAML ${name}: ${[...document.errors, ...document.warnings].join("; ")}`,
        );
      return [name, validateProfile(document.toJS({ maxAliasCount: 0 }), name)];
    }),
  );

export const renderAiken = (profile, template) => {
  const constants = [
    [profile.timing, timingConstants],
    [profile.limits, limitConstants],
    [profile.economics, economicsConstants],
  ].flatMap(([values, names]) =>
    Object.entries(names).map(
      ([key, name]) =>
        `pub const ${name}: Int = ${values[key].toLocaleString("en-US").replaceAll(",", "_")}`,
    ),
  );
  return `${template.trimEnd()}\n\n${constants.join("\n\n")}\n`;
};

export const generateProfiles = async (selected, check = false) => {
  if (!profileNames.includes(selected))
    throw new Error(`Select one of: ${profileNames.join(", ")}`);
  const profiles = readProfiles();
  const economics = {};
  for (const profile of Object.values(profiles)) {
    const key = profile.economics.profile;
    if (
      economics[key] &&
      canonicalJson(economics[key]) !== canonicalJson(profile.economics)
    )
      throw new Error(`Conflicting economics schedule ${key}`);
    economics[key] = profile.economics;
  }
  const template = readFileSync(
    resolve(root, "config/deployments/env.ak.template"),
    "utf8",
  );
  const outputs = Object.fromEntries(
    Object.entries(profiles).map(([name, profile]) => [
      `onchain/aiken/env/${name}.ak`,
      renderAiken(profile, template),
    ]),
  );
  outputs["onchain/aiken/env/default.ak"] =
    outputs["onchain/aiken/env/mainnet.ak"];
  outputs["onchain/aiken/env/testnet.ak"] =
    outputs["onchain/aiken/env/preprod-testing.ak"];
  outputs["demo/midgard-core/src/generated-deployment-profiles.ts"] =
    `export const DEPLOYMENT_PROFILES = ${JSON.stringify(profiles, null, 2)} as const;\n\n` +
    `export const DEPLOYMENT_PROFILE_DIGESTS = ${JSON.stringify(Object.fromEntries(Object.entries(profiles).map(([name, profile]) => [name, profileDigest(profile)])), null, 2)} as const;\n\n` +
    `export const DEPLOYMENT_MANIFEST_ECONOMICS_BY_PROFILE = ${JSON.stringify(economics, null, 2)} as const;\n\n` +
    `export const SELECTED_DEPLOYMENT_PROFILE = DEPLOYMENT_PROFILES[${JSON.stringify(selected)}];\n` +
    `export const SELECTED_DEPLOYMENT_PROFILE_DIGEST = DEPLOYMENT_PROFILE_DIGESTS[${JSON.stringify(selected)}];\n` +
    `for (const profile of Object.values(DEPLOYMENT_PROFILES)) {\n` +
    `  Object.freeze(profile.l1_finality);\n  Object.freeze(profile.timing);\n  Object.freeze(profile.limits);\n  Object.freeze(profile.economics);\n  Object.freeze(profile);\n}\n` +
    `Object.freeze(DEPLOYMENT_PROFILES);\nObject.freeze(DEPLOYMENT_PROFILE_DIGESTS);\n` +
    `for (const economics of Object.values(DEPLOYMENT_MANIFEST_ECONOMICS_BY_PROFILE)) Object.freeze(economics);\n` +
    `Object.freeze(DEPLOYMENT_MANIFEST_ECONOMICS_BY_PROFILE);\n`;
  for (const [path, content] of Object.entries(outputs)) {
    const formatted = path.endsWith(".ts")
      ? await format(content, { parser: "typescript" })
      : content;
    if (check) {
      if (readFileSync(resolve(root, path), "utf8") !== formatted)
        throw new Error(`Stale generated deployment file: ${path}`);
    } else writeFileSync(resolve(root, path), formatted);
  }
  return profiles[selected];
};

if (
  process.argv[1] &&
  resolve(process.argv[1]) === fileURLToPath(import.meta.url)
) {
  const [command, requested, ...extra] = process.argv.slice(2);
  const selected =
    requested ??
    (command === "check"
      ? readFileSync(
          resolve(
            root,
            "demo/midgard-core/src/generated-deployment-profiles.ts",
          ),
          "utf8",
        ).match(
          /SELECTED_DEPLOYMENT_PROFILE\s*=\s*DEPLOYMENT_PROFILES\[\s*"([^"]+)"\s*\]/u,
        )?.[1]
      : undefined);
  if (!["generate", "check", "build"].includes(command) || extra.length)
    throw new Error(
      "Usage: deployment-profiles.mjs <generate|check|build> <profile>",
    );
  const profile = await generateProfiles(selected, command === "check");
  if (command === "build") {
    const expectedCompiler = readFileSync(
      resolve(root, ".github/workflows/aiken-ci.yml"),
      "utf8",
    ).match(/AIKEN_FORK_VERSION: (.+)/u)?.[1];
    const compiler = spawnSync("aiken", ["--version"], { encoding: "utf8" });
    if (compiler.error) throw compiler.error;
    if (compiler.status !== 0 || compiler.stdout.trim() !== expectedCompiler)
      throw new Error(
        `Deployment builds require ${expectedCompiler}; found ${compiler.stdout.trim()}`,
      );
    const blueprintPath = resolve(root, "onchain/aiken/plutus.json");
    if (existsSync(`${blueprintPath}.deployment.json`))
      unlinkSync(`${blueprintPath}.deployment.json`);
    const result = spawnSync(
      "aiken",
      ["build", "--env", selected.replaceAll("-", "_")],
      { cwd: resolve(root, "onchain/aiken"), stdio: "inherit" },
    );
    if (result.error) throw result.error;
    if (result.status !== 0) process.exit(result.status ?? 1);
    const blueprintHash = createHash("sha256")
      .update(readFileSync(blueprintPath))
      .digest("hex");
    writeFileSync(
      `${blueprintPath}.deployment.json`,
      JSON.stringify(
        { profile, profileDigest: profileDigest(profile), blueprintHash },
        null,
        2,
      ) + "\n",
    );
  }
}
