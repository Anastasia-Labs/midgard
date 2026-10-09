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
  "preprod-emulator-testing",
];
const networks = ["Mainnet", "Preprod", "Preprod", "Custom", "Preprod"];
// Generated and compiled for the interactive Vitest projects only; never the
// checkout's selected deployment, and never built as the default blueprint.
export const emulatorOnlyProfileNames = ["preprod-emulator-testing"];
const timingConstants = {
  block_maturity_ms: "block_maturity_duration_v1",
  dispute_response_window_ms: "response_window_milliseconds",
  operator_shift_ms: "shift_duration",
  registration_ms: "registration_duration",
  event_wait_ms: "event_wait_duration",
  user_events_negligence_timeout_ms: "user_events_negligence_timeout",
  new_shift_inactivity_grace_period_ms: "new_shift_inactivity_grace_period",
  max_validity_range_ms: "max_validity_range_length",
  da_attestation_timeout_ms: "da_attestation_timeout_v1",
  da_small_response_window_ms: "small_response_window_ms_v1",
  da_full_response_window_ms: "full_response_window_ms_v1",
  da_challenge_window_ms: "da_challenge_window_ms_v1",
  da_slash_grace_ms: "da_slash_grace_ms_v1",
  da_bond_withdraw_delay_ms: "da_bond_withdraw_delay_ms_v1",
};
// The pooled DA committee bond (#685). The availability ParametersV1 carries
// these amounts on-chain; off-chain consumers must match the selected profile.
const daBondConstants = {
  da_bond_lovelace: "da_bond_lovelace_v1",
  da_slash_penalty_lovelace: "da_slash_penalty_lovelace_v1",
  da_bond_min_top_up_lovelace: "da_bond_min_top_up_lovelace_v1",
  da_bond_pool_floor_lovelace: "da_bond_pool_floor_lovelace_v1",
  challenge_record_lovelace: "challenge_record_lovelace_v1",
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
// A canonical decimal string (no sign, exponent, leading or trailing zero, so
// one value has one spelling and one digest) as an exact rational. Undefined
// for anything else.
export const exactDecimal = (value) => {
  const match =
    typeof value === "string"
      ? /^(0|[1-9]\d*)(?:\.(\d*[1-9]))?$/u.exec(value)
      : null;
  if (match === null) return undefined;
  const fraction = match[2] ?? "";
  return {
    numerator: BigInt(match[1] + fraction),
    denominator: 10n ** BigInt(fraction.length),
  };
};
// The most L1 blocks the chain-growth bound guarantees within spanMs:
// floor(spanMs * f / (3 * slot_length_ms)), rounded toward negative infinity.
export const guaranteedBlocksWithinMs = (l1Finality, spanMs) => {
  const f = exactDecimal(l1Finality.active_slot_coeff);
  const numerator = BigInt(spanMs) * f.numerator;
  const denominator = 3n * BigInt(l1Finality.slot_length_ms) * f.denominator;
  const quotient = numerator / denominator;
  return quotient * denominator > numerator ? quotient - 1n : quotient;
};
// Largest d with no inactivity strike: W + N >= 3(d + 1) slot/f + L.
export const largestNoStrikeCommitEventDepth = (profile) =>
  guaranteedBlocksWithinMs(
    profile.l1_finality,
    BigInt(profile.timing.event_wait_ms) +
      BigInt(profile.timing.user_events_negligence_timeout_ms) -
      BigInt(profile.timing.max_validity_range_ms),
  ) - 1n;
// The least span from planning a commit to its TTL, the header end time E:
// the node's history-commit planner refuses an E sooner than this after now.
// The node reads it from the generated profiles
// (HISTORY_COMMIT_MINIMUM_FUTURE_BUFFER_MS), so the planner and the bound
// below share one value.
export const COMMIT_TTL_FUTURE_BUFFER_MS = 30_000;
// Largest d at which a commit stays plannable: W - B - slot >= 3(d + 1) slot/f.
// The anchor caps E at time(A) + W - 1, and the planner needs E at least B
// after now, rounded up to the next slot boundary. When a commit is planned,
// the view's tip is d blocks above A and the next block has not arrived, so
// now is at most 3(d + 1) slot/f after A under the chain-growth bound.
export const largestFeasibleCommitEventDepth = (profile) =>
  guaranteedBlocksWithinMs(
    profile.l1_finality,
    BigInt(profile.timing.event_wait_ms) -
      BigInt(COMMIT_TTL_FUTURE_BUFFER_MS) -
      BigInt(profile.l1_finality.slot_length_ms),
  ) - 1n;
// Largest d a production profile admits: W - L >= 3d slot/f.
export const largestProductionCommitEventDepth = (profile) =>
  guaranteedBlocksWithinMs(
    profile.l1_finality,
    BigInt(profile.timing.event_wait_ms) -
      BigInt(profile.timing.max_validity_range_ms),
  );

// Minimum span a public profile keeps between the latest Apply (end time plus
// the attestation timeout) and the Open deadline (end time plus the challenge
// window). A challenger must first see a timeout-edge Apply at the profile's
// confirmation depth, then land an Open whose validity range may be a full
// maximum range wide (the record's opened_at is its upper bound). The second
// range covers clock skew and the poll-and-submit block.
export const minimumPublicOpenAfterApplyMarginMs = (profile) =>
  2 * profile.timing.max_validity_range_ms +
  l1BlocksBudgetMs(profile.l1_finality.confirmation_depth);

// Earliest a DA committee may complete a pool withdrawal after BeginWithdraw
// and still leave every block it applied slashable. The spec (#685 section 6)
// form: the last applied block ends at most one validity range after
// BeginWithdraw, the open lands inside the challenge window, the response
// deadline is one full window later and the timeout gets the slash grace.
export const specDaBondWithdrawDelayFloorMs = (timing) =>
  timing.max_validity_range_ms +
  timing.da_challenge_window_ms +
  timing.da_full_response_window_ms +
  timing.da_slash_grace_ms;
// The enforced form. An unavailable block is timed out only once it is the
// queue head, and its predecessors merge no earlier than end time plus block
// maturity, so the timeout can wait for maturity rather than the response
// deadline. The grace then starts at whichever is later.
export const daBondWithdrawDelayFloorMs = (timing) =>
  timing.max_validity_range_ms +
  Math.max(
    timing.da_challenge_window_ms + timing.da_full_response_window_ms,
    timing.block_maturity_ms,
  ) +
  timing.da_slash_grace_ms;

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

export const renderAiken = (profile, template) => {
  const constants = [
    [profile.timing, timingConstants],
    [profile.da_bond, daBondConstants],
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
    `  Object.freeze(profile.l1_finality);\n  Object.freeze(profile.timing);\n  Object.freeze(profile.da_bond);\n  Object.freeze(profile.limits);\n  Object.freeze(profile.economics);\n  Object.freeze(profile);\n}\n` +
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
