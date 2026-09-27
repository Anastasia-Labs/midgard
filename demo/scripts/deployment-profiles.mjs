import { createHash } from "node:crypto";
import { existsSync, readFileSync, unlinkSync, writeFileSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { parseDocument } from "yaml";
import { format } from "prettier";

import {
  assertPinnedAiken,
  defaultAikenBinary,
} from "../../onchain/aiken/scripts/pinned-compiler.mjs";
import {
  blueprintHash,
  blueprintSourceHash,
  buildRecordPath,
} from "./lib/blueprint-stamp.mjs";

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

// Minimum DA response budget. Before either response window closes, the
// committee must see the open at the profile's confirmation depth, then land
// every chained publication of the largest small-class payload, and it gets one
// more block for its poll and submission. Publications are chained: each one
// spends the previous carrier, so they land one per L1 block. The payload and
// chunk sizes copy demo/midgard-sdk/src/availability-challenge.ts
// DA_AVAILABILITY_SMALL_PAYLOAD_MAX_BYTES and
// DA_AVAILABILITY_RESPONSE_GEOMETRY_MEASUREMENT_CANDIDATE.chunkByteLength; this
// script runs before any package builds, so it cannot import them, and
// midgard-sdk/tests/availability-challenge.test.ts pins the copies.
//
// Every "d blocks of time" budget in this file uses Cardano's 20 s mean block
// interval; production is Poisson, so each budget doubles it.
export const L1_MEAN_BLOCK_MS = 20_000;
export const L1_BLOCK_SAFETY_FACTOR = 2;
export const l1BlocksBudgetMs = (blocks) =>
  blocks * L1_MEAN_BLOCK_MS * L1_BLOCK_SAFETY_FACTOR;
export const DA_SMALL_PAYLOAD_MAX_BYTES = 65_536;
export const DA_RESPONSE_CHUNK_BYTES = 14_020;
export const DA_SMALL_PAYLOAD_CHAINED_PUBLICATIONS = Math.ceil(
  DA_SMALL_PAYLOAD_MAX_BYTES / DA_RESPONSE_CHUNK_BYTES,
);
export const DA_RESPONSE_POLL_AND_SUBMIT_BLOCKS = 1;
export const minimumDaResponseBudgetMs = (profile) =>
  l1BlocksBudgetMs(
    profile.l1_finality.confirmation_depth +
      DA_SMALL_PAYLOAD_CHAINED_PUBLICATIONS +
      DA_RESPONSE_POLL_AND_SUBMIT_BLOCKS,
  );

// Minimum event wait of a public profile. An event's inclusion time is its
// transaction's validity upper bound plus the event wait (user-events.ak,
// order-facts.ak), and a block header's end time is its commit's validity
// upper bound, at most the maximum validity range after the commit's lower
// bound. So every event a header must include was on L1 at least (event wait -
// maximum validity range) before that commit became valid. The floor sizes
// that span as confirmation-depth blocks at twice the mean interval, so an
// honest operator can omit a required event only if L1 rolls back past its
// finality depth or produces under half its mean block rate for the whole
// span. That margin is probabilistic, and weaker than the 3N/f worst case
// config/deployments/README.md uses to size the event wait itself.
export const minimumPublicEventWaitMs = (profile) =>
  profile.timing.max_validity_range_ms +
  l1BlocksBudgetMs(profile.l1_finality.confirmation_depth);

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
  // The ordering above keeps the full window at least the small one, so the
  // small window reaching the budget carries both.
  const responseBudget = minimumDaResponseBudgetMs(profile);
  if (timing.da_small_response_window_ms < responseBudget) {
    throw new Error(
      `DA response windows must each cover the minimum response budget of ${responseBudget} ms`,
    );
  }
  // Fraud must stay provable after the latest DA response: the latest
  // attestation, then an open that front-runs the honest one with the widest
  // validity range (its window starts at the upper bound), then the whole full
  // window, then the whole validation-dispute schedule, all before maturity.
  // The last term is what can_open_before_maturity (validation-dispute-v1.ak)
  // requires of a dispute opened when the response lands: opening upper +
  // max dispute duration <= end_time + maturity. The comparison is strict
  // because the dispute opening lands after the response it depends on.
  // Fifteen-minute maturity cannot hold this, so the non-interactive testing
  // profiles are exempt: they are not fault-proof security configurations.
  if (
    !nonInteractiveTesting &&
    BigInt(timing.da_attestation_timeout_ms) +
      BigInt(timing.max_validity_range_ms) +
      BigInt(timing.da_full_response_window_ms) +
      disputeDuration >=
      maturity
  ) {
    throw new Error(
      "DA attestation timeout, maximum validity range, full response window and dispute schedule must end before block maturity",
    );
  }
  // The testing profiles' short event wait is shorter than the maximum
  // validity range, so there a short L1 fork can make an honest block omit an
  // event. That is accepted: they are not public security configurations.
  const eventWaitFloor = minimumPublicEventWaitMs(profile);
  if (!nonInteractiveTesting && timing.event_wait_ms < eventWaitFloor) {
    throw new Error(
      `Event wait must cover the maximum validity range plus confirmation depth, at least ${eventWaitFloor} ms`,
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
  // The scheduler's neglected-event guard (scheduler.ak,
  // inactivity_threshold_from_user_event_inclusion_time) accepts an event whose
  // inclusion time equals the tail block's end time, which that block already
  // includes. The commitment-gap threshold (tail end + max inactivity) must
  // therefore never be later than a neglected event's threshold (inclusion +
  // negligence), or an operator could be struck for an event it included.
  // The operator watchdog relies on the same ordering.
  if (
    timing.user_events_negligence_timeout_ms <
    timing.max_inactivity_between_block_commitments_ms
  ) {
    throw new Error(
      "User-event negligence timeout must be at least the maximum inactivity between block commitments",
    );
  }
  // A strike needs its threshold, at least the tail's end time plus the
  // commitment gap, strictly before the convicted shift ends (scheduler.ak,
  // validate_operator_inactivity_and_get_its_link). A shift that starts at the
  // tail's end time (the last commit's validity upper bound) is strikable only
  // when the gap is shorter than the shift; otherwise a dead operator following
  // a live one rotates out unstruck. This assumes a predecessor that respects
  // the end-time cap; one that commits past its shift end shortens the window.
  if (
    timing.max_inactivity_between_block_commitments_ms >=
    timing.operator_shift_ms
  ) {
    throw new Error(
      "Maximum inactivity between block commitments must be shorter than the operator shift",
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
    const aikenBinary = defaultAikenBinary();
    const compiler = assertPinnedAiken(aikenBinary);
    const blueprintPath = resolve(root, "onchain/aiken/plutus.json");
    if (existsSync(buildRecordPath(blueprintPath)))
      unlinkSync(buildRecordPath(blueprintPath));
    // Hashed before the build: `aiken build` only reads these, and a record
    // must describe the inputs the compiler actually saw.
    const sourceHash = blueprintSourceHash(root);
    const result = spawnSync(
      aikenBinary,
      ["build", "--env", selected.replaceAll("-", "_")],
      { cwd: resolve(root, "onchain/aiken"), stdio: "inherit" },
    );
    if (result.error) throw result.error;
    if (result.status !== 0) process.exit(result.status ?? 1);
    writeFileSync(
      buildRecordPath(blueprintPath),
      JSON.stringify(
        {
          profile,
          profileDigest: profileDigest(profile),
          blueprintHash: blueprintHash(blueprintPath),
          sourceHash,
          compiler,
        },
        null,
        2,
      ) + "\n",
    );
  }
}
