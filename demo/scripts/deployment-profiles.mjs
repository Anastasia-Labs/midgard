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

import {
  allowListConstants,
  daBondConstants,
  economicsConstants,
  limitConstants,
  renderAiken,
  timingConstants,
} from "./deployment-profile-aiken.mjs";

export { renderAiken } from "./deployment-profile-aiken.mjs";
import {
  canonicalJson,
  profileDigest,
} from "./deployment-profile-identity.mjs";
export {
  canonicalJson,
  profileDigest,
} from "./deployment-profile-identity.mjs";

import {
  COMMIT_TTL_FUTURE_BUFFER_MS,
  daBondWithdrawDelayFloorMs,
  exactDecimal,
  largestFeasibleCommitEventDepth,
  largestNoStrikeCommitEventDepth,
  largestProductionCommitEventDepth,
  maximumTransitionTraceThreadBudgetMs,
  maximumTransitionTraceThreadSteps,
  minimumDaResponseBudgetMs,
  minimumPublicEventWaitMs,
  minimumPublicOpenAfterApplyMarginMs,
  specDaBondWithdrawDelayFloorMs,
} from "./deployment-profile-budgets.mjs";
export {
  COMMIT_TTL_FUTURE_BUFFER_MS,
  DA_RESPONSE_CHUNK_BYTES,
  DA_RESPONSE_POLL_AND_SUBMIT_BLOCKS,
  DA_SMALL_PAYLOAD_CHAINED_PUBLICATIONS,
  DA_SMALL_PAYLOAD_MAX_BYTES,
  L1_BLOCK_SAFETY_FACTOR,
  L1_MEAN_BLOCK_MS,
  daBondWithdrawDelayFloorMs,
  exactDecimal,
  guaranteedBlocksWithinMs,
  l1BlocksBudgetMs,
  largestFeasibleCommitEventDepth,
  largestNoStrikeCommitEventDepth,
  largestProductionCommitEventDepth,
  maximumTransitionTraceThreadBudgetMs,
  maximumTransitionTraceThreadSteps,
  minimumDaResponseBudgetMs,
  minimumPublicEventWaitMs,
  minimumPublicOpenAfterApplyMarginMs,
  specDaBondWithdrawDelayFloorMs,
} from "./deployment-profile-budgets.mjs";

const root = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
export const profileNames = [
  "mainnet",
  "preprod-public",
  "preprod-testing",
  "local-devnet-testing",
  "preprod-emulator-testing",
  "preprod-emulator-curated",
];
const networks = [
  "Mainnet",
  "Preprod",
  "Preprod",
  "Custom",
  "Preprod",
  "Preprod",
];
// Generated and compiled for the interactive Vitest projects only; never the
// checkout's selected deployment, and never built as the default blueprint.
export const emulatorOnlyProfileNames = [
  "preprod-emulator-testing",
  "preprod-emulator-curated",
];
// Stand-in keys until the owner supplies the real ones: the ASCII prefix
// "placeholder:" marks them. Nothing yet marks a profile for release, so no
// check refuses them; a release gate must.
export const ALLOW_LIST_PLACEHOLDER_KEY_PREFIX = Buffer.from(
  "placeholder:",
  "ascii",
).toString("hex");
export const isAllowListPlaceholderKey = (key) =>
  key.startsWith(ALLOW_LIST_PLACEHOLDER_KEY_PREFIX);
const verificationKeyHash = /^[0-9a-f]{56}$/u;
const validateAllowLists = (allowLists, name) => {
  exactKeys(allowLists, Object.keys(allowListConstants), "allow_lists");
  for (const key of Object.keys(allowListConstants)) {
    const keys = allowLists[key];
    if (keys === null) {
      if (productionProfileNames.includes(name))
        throw new Error(
          `allow_lists.${key} must list the admitted keys on a production profile`,
        );
      continue;
    }
    if (
      !Array.isArray(keys) ||
      keys.length === 0 ||
      keys.some((entry) => !verificationKeyHash.test(entry))
    )
      throw new Error(
        `allow_lists.${key} must be null or a non-empty list of 28-byte lowercase hex key hashes`,
      );
    if (new Set(keys).size !== keys.length)
      throw new Error(`allow_lists.${key} must not repeat a key`);
  }
};

// Fraud-proof catalogue categories the deployment leaves out (P1: the
// families that read validation-trace states). The atomic init builds the
// catalogue MPF without them, so Computation-Thread Init has no membership
// proof for them. midgard-core checks each name against the catalogue.
const catalogueCategoryName = /^[a-z][A-Za-z0-9]*$/u;
const validateExcludedFraudProofCategories = (categories) => {
  if (
    !Array.isArray(categories) ||
    categories.some(
      (entry) =>
        typeof entry !== "string" || !catalogueCategoryName.test(entry),
    )
  )
    throw new Error(
      "excluded_fraud_proof_categories must be a list of catalogue category names",
    );
  if (new Set(categories).size !== categories.length)
    throw new Error(
      "excluded_fraud_proof_categories must not repeat a category",
    );
};

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

// The commit-event depth d (`l1_finality.commit_event_depth`): an event is
// committed only below the commit anchor, the follower block d under the view
// a commit is planned at. Under the guaranteed chain-growth bound, n L1 blocks
// take at most 3n/f slots, so the bounds below count blocks in a span exactly:
// active_slot_coeff is a decimal string, because the deployment identity's
// canonical JSON admits only safe-integer numbers; it becomes a rational
// ("0.05" is 5/100) and every comparison is BigInt, never a float division.
export const productionProfileNames = ["mainnet", "preprod-public"];
const l1FinalityKeys = [
  "confirmation_depth",
  "commit_event_depth",
  "security_parameter",
  "active_slot_coeff",
  "slot_length_ms",
];

export const validateProfile = (profile, name) => {
  exactKeys(
    profile,
    [
      "name",
      "network",
      "l1_finality",
      "timing",
      "da_bond",
      "limits",
      "economics",
      "allow_lists",
      "excluded_fraud_proof_categories",
    ],
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
  exactKeys(profile.l1_finality, l1FinalityKeys, "l1_finality");
  positiveIntegers(
    profile.l1_finality,
    ["confirmation_depth", "security_parameter", "slot_length_ms"],
    "l1_finality",
  );
  const commitEventDepth = profile.l1_finality.commit_event_depth;
  if (!Number.isSafeInteger(commitEventDepth) || commitEventDepth < 0)
    throw new Error(
      "l1_finality.commit_event_depth must be a non-negative safe integer",
    );
  const activeSlotCoeff = exactDecimal(profile.l1_finality.active_slot_coeff);
  if (
    activeSlotCoeff === undefined ||
    activeSlotCoeff.numerator === 0n ||
    activeSlotCoeff.numerator > activeSlotCoeff.denominator
  )
    throw new Error(
      "l1_finality.active_slot_coeff must be a canonical decimal string in (0, 1]",
    );
  exactKeys(profile.timing, Object.keys(timingConstants), "timing");
  positiveIntegers(profile.timing, Object.keys(timingConstants), "timing");
  exactKeys(profile.da_bond, Object.keys(daBondConstants), "da_bond");
  positiveIntegers(profile.da_bond, Object.keys(daBondConstants), "da_bond");
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
  // Emulator-only profiles are test deployments: bounded economics.
  const expectedEconomics =
    name.endsWith("testing") || emulatorOnlyProfileNames.includes(name)
      ? "bounded-acceptance-v1"
      : "public-preprod-launch-v1";
  if (profile.economics.profile !== expectedEconomics)
    throw new Error(`economics.profile must equal ${expectedEconomics}`);
  validateAllowLists(profile.allow_lists, name);
  validateExcludedFraudProofCategories(profile.excluded_fraud_proof_categories);
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
  // A block attested at the last moment can still be challenged, and a block
  // can never be both challengeable and mergeable.
  if (
    timing.da_challenge_window_ms <= timing.da_attestation_timeout_ms ||
    timing.da_challenge_window_ms > timing.block_maturity_ms
  ) {
    throw new Error(
      "DA challenge window must be longer than the attestation timeout and at most block maturity",
    );
  }
  const specWithdrawDelayFloor = specDaBondWithdrawDelayFloorMs(timing);
  if (timing.da_bond_withdraw_delay_ms < specWithdrawDelayFloor) {
    throw new Error(
      `DA bond withdrawal delay must cover the maximum validity range, challenge window, full response window and slash grace, at least ${specWithdrawDelayFloor} ms`,
    );
  }
  const withdrawDelayFloor = daBondWithdrawDelayFloorMs(timing);
  if (timing.da_bond_withdraw_delay_ms < withdrawDelayFloor) {
    throw new Error(
      `DA bond withdrawal delay must cover the maximum validity range, the later of the challenge response deadline and block maturity, and the slash grace, at least ${withdrawDelayFloor} ms`,
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
  // Fraud must stay provable after the latest DA response: the latest open,
  // landing at the end of the challenge window with the widest validity range
  // (its response window starts at the upper bound), then the whole full
  // window, then the whole validation-dispute schedule, all before maturity.
  // The last term is what can_open_before_maturity (validation-dispute-v1.ak)
  // requires of a dispute opened when the response lands: opening upper +
  // max dispute duration <= end_time + maturity. The comparison is strict
  // because the dispute opening lands after the response it depends on.
  // Fifteen-minute maturity cannot hold this, so the non-interactive testing
  // profiles are exempt: they are not fault-proof security configurations.
  if (
    !nonInteractiveTesting &&
    BigInt(timing.da_challenge_window_ms) +
      BigInt(timing.max_validity_range_ms) +
      BigInt(timing.da_full_response_window_ms) +
      disputeDuration >=
      maturity
  ) {
    throw new Error(
      "DA challenge window, maximum validity range, full response window and dispute schedule must end before block maturity",
    );
  }
  const threadBudget = maximumTransitionTraceThreadBudgetMs();
  if (!nonInteractiveTesting && threadBudget > timing.block_maturity_ms)
    throw new Error(
      `Block maturity must cover the longest transition-trace thread, ${maximumTransitionTraceThreadSteps()} steps (${threadBudget} ms)`,
    );
  // An Apply may land as late as end time plus the attestation timeout, and an
  // open must land before end time plus the challenge window. On a public
  // profile a challenger must see that latest Apply at confirmation depth and
  // then land an open whose validity range may be a full maximum range wide,
  // so the span between them is at least minimumPublicOpenAfterApplyMarginMs.
  // The testing profiles keep less and are exempt: they are not public
  // security configurations.
  const openAfterApplyMargin = minimumPublicOpenAfterApplyMarginMs(profile);
  if (
    !nonInteractiveTesting &&
    timing.da_challenge_window_ms - timing.da_attestation_timeout_ms <
      openAfterApplyMargin
  ) {
    throw new Error(
      `DA challenge window must exceed the attestation timeout by two maximum validity ranges plus confirmation depth, at least ${openAfterApplyMargin} ms`,
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
  // A tx-order's L1 validity range is closed and at most the event range
  // (the tx-order mint), so the orders due at one inclusion time were submitted
  // within that range. Deposits and withdrawals already get the shorter
  // maximum validity range from their history-list insertion.
  if (timing.max_validity_range_ms > timing.max_event_validity_range_ms)
    throw new Error(
      "Maximum event validity range must cover the maximum validity range",
    );
  if (
    timing.new_shift_inactivity_grace_period_ms > timing.operator_shift_ms ||
    timing.max_validity_range_ms > timing.operator_shift_ms
  ) {
    throw new Error(
      "Operator shift must cover grace and maximum validity range",
    );
  }
  // A strike needs its threshold, at least a neglected event's inclusion time
  // plus the negligence timeout, strictly before the convicted shift ends
  // (scheduler.ak, validate_operator_inactivity_and_get_its_link). An event
  // included as a shift starts is strikable within that shift only when the
  // timeout is shorter than the shift; otherwise a dead operator rotates out
  // unstruck while the event waits for the next shift.
  if (timing.user_events_negligence_timeout_ms >= timing.operator_shift_ms) {
    throw new Error(
      "User-event negligence timeout must be shorter than the operator shift",
    );
  }
  if (
    profile.da_bond.da_slash_penalty_lovelace >=
    profile.da_bond.da_bond_lovelace
  ) {
    throw new Error(
      "DA slash penalty must be smaller than the DA bond, leaving the challenger a reward",
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
  // The follower keeps k blocks of history, and the block just above the
  // commit anchor must not be final, so d <= k.
  const securityParameter = profile.l1_finality.security_parameter;
  // eslint-disable-next-line midgard/depth-through-heads -- Profile shape check between two depth parameters, counting no block depth; the rule does not list commit_event_depth.
  if (commitEventDepth > securityParameter)
    throw new Error(
      `l1_finality.commit_event_depth must be at most l1_finality.security_parameter (${securityParameter})`,
    );
  // No inactivity strike, every profile. A strike cites one undelivered event
  // (inclusion time I = valid_to v + event wait W, after the state queue's
  // tail end) and opens strictly after max(shift start + grace, I + N), N the
  // negligence timeout (scheduler.ak, validate_operator_inactivity_and_get_its_link).
  // With no such event there is nothing to bound. A commit covering the event
  // (header end E >= I) moves the tail end to E and retires the citation, so
  // it is enough that each event's covering commit lands by I + N, which that
  // max never precedes.
  // - Includable: E is capped at time(A) + W - 1 (commit-anchor.ts), so E >= I
  //   needs an anchor A dated after v, which needs d + 1 blocks after v, at
  //   most 3(d + 1) slot/f under the chain-growth bound.
  // - Landed: a commit lands by its TTL E, and E is capped at the submit
  //   slot's start + L - 61 s (commitValidityEndTimeCapMs), so it lands within
  //   one maximum validity range L of its planning.
  // Hence W + N >= 3(d + 1) slot/f + L here, and N >= L below.
  const noStrikeDepth = largestNoStrikeCommitEventDepth(profile);
  if (BigInt(commitEventDepth) > noStrikeDepth)
    throw new Error(
      `l1_finality.commit_event_depth must satisfy event_wait_ms + user_events_negligence_timeout_ms >= 3 (d + 1) slot_length_ms / active_slot_coeff + max_validity_range_ms; the largest such d is ${noStrikeDepth}`,
    );
  // An event admitted before it is due: no commit planned more than L - 61 s
  // before I can reach it (its E is capped below I), and the commit in flight
  // then lands by its own E < I, so the covering commit is planned by I and,
  // as above, lands within L of that: N >= L.
  if (timing.user_events_negligence_timeout_ms < timing.max_validity_range_ms)
    throw new Error(
      "User-event negligence timeout must cover the maximum validity range, the most a covering commit takes from planning to landing",
    );
  // An event already overdue at a shift boundary (I + N <= shift start): the
  // max above is then shift start + grace, and the successor, which can plan
  // the covering commit only once its shift starts, lands it within L of
  // that: grace >= L. After a fraud removal rolls the tail end back, an event
  // it re-exposes is held only to shift start + grace, by design.
  if (
    timing.new_shift_inactivity_grace_period_ms < timing.max_validity_range_ms
  )
    throw new Error(
      "New-shift inactivity grace period must cover the maximum validity range, the most a successor's covering commit takes from planning to landing",
    );
  // Every profile: the anchor cap must leave room for the commit's TTL. A d
  // past this bound holds every commit even while L1 produces blocks at the
  // guaranteed rate, so the operator is struck for inactivity.
  const feasibleDepth = largestFeasibleCommitEventDepth(profile);
  if (BigInt(commitEventDepth) > feasibleDepth)
    throw new Error(
      `l1_finality.commit_event_depth must satisfy event_wait_ms - ${COMMIT_TTL_FUTURE_BUFFER_MS} ms (the commit TTL floor) - slot_length_ms >= 3 (d + 1) slot_length_ms / active_slot_coeff; the largest such d is ${feasibleDepth}`,
    );
  // Production profiles: a commit lands no earlier than one maximum validity
  // range before its header end time, so an event the block must include is
  // at least W - L old when the commit lands, and d blocks deep by then.
  const productionDepth = largestProductionCommitEventDepth(profile);
  if (
    productionProfileNames.includes(name) &&
    BigInt(commitEventDepth) > productionDepth
  )
    throw new Error(
      `l1_finality.commit_event_depth must satisfy event_wait_ms - max_validity_range_ms >= 3 d slot_length_ms / active_slot_coeff on a production profile; the largest such d is ${productionDepth}`,
    );
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

export const generateProfiles = async (selected, check = false) => {
  if (!profileNames.includes(selected))
    throw new Error(`Select one of: ${profileNames.join(", ")}`);
  if (emulatorOnlyProfileNames.includes(selected))
    throw new Error(
      `${selected} is emulator-only; the interactive Vitest setup builds it separately`,
    );
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
    `/** The least span from planning a commit to its TTL (\`deployment-profiles.mjs\`). */\n` +
    `export const COMMIT_TTL_FUTURE_BUFFER_MS = ${COMMIT_TTL_FUTURE_BUFFER_MS};\n\n` +
    `export const SELECTED_DEPLOYMENT_PROFILE = DEPLOYMENT_PROFILES[${JSON.stringify(selected)}];\n` +
    `export const SELECTED_DEPLOYMENT_PROFILE_DIGEST = DEPLOYMENT_PROFILE_DIGESTS[${JSON.stringify(selected)}];\n` +
    `for (const profile of Object.values(DEPLOYMENT_PROFILES)) {\n` +
    `  Object.freeze(profile.l1_finality);\n  Object.freeze(profile.timing);\n  Object.freeze(profile.da_bond);\n  Object.freeze(profile.limits);\n  Object.freeze(profile.economics);\n  for (const keys of Object.values(profile.allow_lists)) if (keys !== null) Object.freeze(keys);\n  Object.freeze(profile.allow_lists);\n  Object.freeze(profile.excluded_fraud_proof_categories);\n  Object.freeze(profile);\n}\n` +
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
