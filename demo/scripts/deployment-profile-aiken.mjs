/** The profile fields compiled into Aiken and their exact rendering. */
export const timingConstants = {
  block_maturity_ms: "block_maturity_duration_v1",
  dispute_response_window_ms: "response_window_milliseconds",
  dispute_side_clock_ms: "dispute_side_clock_milliseconds",
  dispute_max_validity_range_ms: "dispute_max_validity_range_milliseconds",
  dispute_resolution_allowance_ms: "dispute_resolution_allowance_milliseconds",
  operator_shift_ms: "shift_duration",
  registration_ms: "registration_duration",
  event_wait_ms: "event_wait_duration",
  user_events_negligence_timeout_ms: "user_events_negligence_timeout",
  new_shift_inactivity_grace_period_ms: "new_shift_inactivity_grace_period",
  max_validity_range_ms: "max_validity_range_length",
  max_event_validity_range_ms: "max_event_validity_range_length",
  da_attestation_timeout_ms: "da_attestation_timeout_v1",
  da_small_response_window_ms: "small_response_window_ms_v1",
  da_full_response_window_ms: "full_response_window_ms_v1",
  da_challenge_window_ms: "da_challenge_window_ms_v1",
  da_slash_grace_ms: "da_slash_grace_ms_v1",
  da_bond_withdraw_delay_ms: "da_bond_withdraw_delay_ms_v1",
};
// The pooled DA committee bond (#685). The availability ParametersV1 carries
// these amounts on-chain; off-chain consumers must match the selected profile.
export const daBondConstants = {
  da_bond_lovelace: "da_bond_lovelace_v1",
  da_slash_penalty_lovelace: "da_slash_penalty_lovelace_v1",
  da_bond_min_top_up_lovelace: "da_bond_min_top_up_lovelace_v1",
  da_bond_pool_floor_lovelace: "da_bond_pool_floor_lovelace_v1",
  challenge_record_lovelace: "challenge_record_lovelace_v1",
};
export const economicsConstants = {
  requiredBondLovelace: "required_bond",
  slashingPenaltyLovelace: "slashing_penalty",
  inactivitySlashingPenaltyLovelace: "inactivity_slashing_penalty",
  fraudProverRewardLovelace: "fraud_prover_reward",
  proverCollateralFloorLovelace: "prover_collateral_floor",
};
export const limitConstants = {
  max_bisection_rounds: "max_bisection_rounds",
  max_inactivity_strikes: "max_inactivity_strikes",
  coins_per_utxo_byte: "coins_per_utxo_byte",
  max_dispute_move_fee_lovelace: "max_dispute_move_fee_lovelace",
  fraud_proof_output_lovelace: "fraud_proof_output_lovelace",
};
// The stage-0 operator allow-list (P1): null (every key admitted) or the
// verification key hashes the deployment admits, fixed for its life. It
// renders as an `Option<List<ByteArray>>` env constant checked by
// `midgard/common/allow_list.admits`. Fault provers and DA challengers are
// permissionless.
export const allowListConstants = {
  operators: "operator_allow_list",
};

// The protocol treasury (pd-design §6.4) is a native script in the cardano-cli
// JSON form. The generator renders its CBOR, and the Aiken env derives the
// script hash, blake2b_224(0x00 || CBOR), at compile time: the hash is never
// passed.
const cborHead = (major, length) => {
  if (!Number.isSafeInteger(length) || length < 0)
    throw new Error(`Native script value ${length} is not a natural number`);
  const head = (bytes) => [
    (major << 5) | { 1: 24, 2: 25, 4: 26, 8: 27 }[bytes],
    ...Array.from({ length: bytes }, (_, index) =>
      Number((BigInt(length) >> BigInt(8 * (bytes - 1 - index))) & 0xffn),
    ),
  ];
  if (length < 24) return [(major << 5) | length];
  if (length < 0x100) return head(1);
  if (length < 0x10000) return head(2);
  if (length < 0x100000000) return head(4);
  return head(8);
};
const nativeScriptTags = {
  sig: 0,
  all: 1,
  any: 2,
  atLeast: 3,
  after: 4,
  before: 5,
};
const encodeNativeScriptBytes = (script) => {
  const tag = nativeScriptTags[script?.type];
  if (tag === undefined)
    throw new Error(`Unknown native script type ${script?.type}`);
  const list = (scripts) => {
    if (!Array.isArray(scripts))
      throw new Error(`Native script ${script.type} needs a scripts list`);
    return [
      ...cborHead(4, scripts.length),
      ...scripts.flatMap(encodeNativeScriptBytes),
    ];
  };
  switch (script.type) {
    case "sig": {
      if (!/^[0-9a-f]{56}$/u.test(script.keyHash ?? ""))
        throw new Error("Native script sig needs a 28-byte keyHash");
      return [
        ...cborHead(4, 2),
        tag,
        ...cborHead(2, 28),
        ...Buffer.from(script.keyHash, "hex"),
      ];
    }
    case "all":
    case "any":
      return [...cborHead(4, 2), tag, ...list(script.scripts)];
    case "atLeast":
      return [
        ...cborHead(4, 3),
        tag,
        ...cborHead(0, script.required),
        ...list(script.scripts),
      ];
    default:
      return [...cborHead(4, 2), tag, ...cborHead(0, script.slot)];
  }
};
/** The canonical (definite-length) CBOR of a cardano-cli JSON native script. */
export const encodeNativeScript = (script) =>
  Buffer.from(encodeNativeScriptBytes(script)).toString("hex");

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
  const allowLists = Object.entries(allowListConstants).map(([key, name]) => {
    const keys = profile.allow_lists[key];
    // The layout `aiken fmt` gives each form, so the generated env stays
    // format-clean.
    const head = `pub const ${name}: Option<List<ByteArray>> =`;
    if (keys === null) return `${head} None`;
    if (keys.length === 1) return `${head}\n  Some([#"${keys[0]}"])`;
    return `${head}\n  Some(\n    [\n${keys.map((entry) => `      #"${entry}",`).join("\n")}\n    ],\n  )`;
  });
  const treasury = [
    `pub const protocol_treasury_native_script_cbor: ByteArray =\n  #"${encodeNativeScript(profile.protocol_treasury_native_script)}"`,
    `/// The forfeit destination of the validation-dispute bond: the hash of\n/// \`protocol_treasury_native_script_cbor\`, never passed.\npub const protocol_treasury_script_hash: ScriptHash =\n  builtin.blake2b_224(\n    builtin.cons_bytearray(0, protocol_treasury_native_script_cbor),\n  )`,
  ];
  return `${template.trimEnd()}\n\n${[...constants, ...allowLists, ...treasury].join("\n\n")}\n`;
};

// The one value of `name` the template pins, in `#"<hex>"` form.
const templatePin = (template, name, type) => {
  const pins = [
    ...template.matchAll(
      new RegExp(`^pub const ${name}: ${type} =\\s*#"([0-9a-f]+)"`, "gmu"),
    ),
  ];
  if (pins.length !== 1)
    throw new Error(
      `config/deployments/env.ak.template must pin ${name} exactly once`,
    );
  return pins[0][1];
};

/**
 * TypeScript exports for the constants the template pins, the same in every
 * profile: the user-events witness prefix and the script hashes of the
 * verifiers other validators delegate to by withdrawal.
 */
export const renderTemplateExports = (template) => {
  const verifierHashes = Object.fromEntries(
    [
      "plutarch_phas_validator_hash",
      "plutarch_pexcludes_validator_hash",
      "mpf_chunked_verify_validator_hash",
    ].map((name) => [name, templatePin(template, name, "ScriptHash")]),
  );
  const witnessPrefix = templatePin(
    template,
    "user_events_witness_script_prefix",
    "ByteArray",
  );
  return (
    `/** \`user_events_witness_script_prefix\`, the same in every profile's Aiken env. */\n` +
    `export const USER_EVENT_WITNESS_SCRIPT_PREFIX = ${JSON.stringify(witnessPrefix)};\n\n` +
    `/** The delegated verifier script hashes pinned in every profile's Aiken env. */\n` +
    `export const VERIFIER_SCRIPT_HASHES = Object.freeze(${JSON.stringify(verifierHashes, null, 2)} as const);\n\n`
  );
};
