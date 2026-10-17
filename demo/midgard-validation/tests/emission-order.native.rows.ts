import { encodeMidgardTxOutput } from "@al-ft/midgard-core";
import { encodeMidgardDefiniteBytes } from "@al-ft/midgard-core/codec";

import { RejectCodes } from "../src/index.js";
import { MidgardRedeemerTag } from "../src/midgard-redeemers.js";
import {
  EMPTY_SIDECAR,
  type Expected,
  funded,
  KEY_ADDRESS,
  keySpent,
  mintedOutput,
  mintOf,
  nativeAll,
  plutus,
  PLUTUS_SIDECAR,
  type Row,
} from "./emission-order.support.js";
import { scriptAddressBytes } from "./reject-subject.support.js";
import {
  FUNDED_OUTPUT_LOVELACE,
  hashScriptWitness,
  makeProtectedScriptOutput,
  makeRedeemersCbor,
  nativeScriptWitness,
  outRefFromByte,
} from "./validation-fixtures.js";

/**
 * Emission order of native-script failures, case (e). The machine scans every
 * field-6 witness in Phase A (witness mode) and every resolved output's
 * reference script at input resolution, so its execution-mode scan meets only
 * native scripts both scans found well formed and within the limits. What is
 * left to it is a false result, from a script the transaction does not carry
 * inline: one on a reference input or on the spent output itself. The machine
 * runs every native execution in execution order (spend, mint, observe,
 * receive, each by pointer index), before the script integrity hash and
 * before any non-native execution.
 *
 * Each multi-fault row has a single-fault control per fault, and one row
 * accepts. Every row here is a `guard`: the producer already emitted the
 * machine's first fault before this case was written down. Every row balances
 * its value, so the value check, which runs after the scripts, adds no fault.
 *
 * The refusal rows pin the classes an execution-mode scan never reaches: a
 * malformed native witness stops at Phase A, and a native script over the
 * node or depth limit outgrows every carrier and fails canonical decode
 * (`limitRefusal`). The resolved-output twins (ResolvedReferenceScript*) wait
 * on X-N2C2.
 *
 * `emission-order.aiken-rows.test.ts` checks each row's stopping step on
 * chain, through the Aiken one-step dispatcher.
 */

type NativeScript = ReturnType<typeof nativeScriptWitness>;

/** A native script whose only key never signs the rows. */
const falseNative = (keyByte: number) =>
  nativeScriptWitness({ type: "sig", keyHash: Buffer.alloc(28, keyByte) });
const FALSE_POLICY_SCRIPT = falseNative(0x06);
const FALSE_POLICY = hashScriptWitness(FALSE_POLICY_SCRIPT);
const FALSE_LOCK_SCRIPT = falseNative(0x07);
const TRUE_POLICY_SCRIPT = nativeScriptWitness({
  type: "any",
  scripts: [nativeAll],
});

const withScriptRef = (script: NativeScript, address: Buffer = KEY_ADDRESS) =>
  encodeMidgardTxOutput({
    address,
    value: { lovelace: FUNDED_OUTPUT_LOVELACE, assets: new Map() },
    script_ref: script,
  });

/** A second key-locked input. */
const keySpentTwo = outRefFromByte(0x83);
/** A reference input carrying a policy script. */
const reference = outRefFromByte(0x90);
/**
 * A spent output locked by a false native script and carrying it. It sorts
 * after `keySpent`: spend pointer index 1.
 */
const nativeSpent = outRefFromByte(0x82);
const nativeLocked = withScriptRef(
  FALSE_LOCK_SCRIPT,
  scriptAddressBytes(hashScriptWitness(FALSE_LOCK_SCRIPT)),
);
/** A protected output paying a Plutus V3 script: a receive purpose. */
const plutusReceive = makeProtectedScriptOutput(
  hashScriptWitness(plutus),
  FUNDED_OUTPUT_LOVELACE,
);
const receiveRedeemer = makeRedeemersCbor([
  { tag: MidgardRedeemerTag.Receiving, index: 0n },
]);

const executionFalse = (
  purpose_kind: bigint,
  purpose_index: bigint,
): Expected => ({
  code: RejectCodes.NativeScriptInvalid,
  reason: { ExecutionNativeScriptFalse: { purpose_kind, purpose_index } },
});
const FALSE_SPEND = executionFalse(0n, 1n);
const FALSE_MINT = executionFalse(1n, 0n);
const INLINE_FALSE: Expected = {
  code: RejectCodes.NativeScriptInvalid,
  reason: { WitnessNativeScriptFalse: { script_index: 0n } },
};
const INTEGRITY_MISMATCH: Expected = {
  code: RejectCodes.InvalidFieldType,
  reason: "ScriptIntegrityHashMismatch",
};
const RECEIVE_PLUTUS_V3: Expected = {
  code: RejectCodes.PlutusScriptInvalid,
  reason: {
    ReceivePurposePlutusV3Forbidden: { purpose_kind: 3n, purpose_index: 0n },
  },
};

type Body = Pick<Row, "state" | "sidecar" | "tx">;

/** A key spend minting under a policy whose script is on a reference input. */
const referenceMint = (
  policyScript: NativeScript,
  extra: Partial<Row["tx"]> = {},
): Body => {
  const policy = hashScriptWitness(policyScript);
  return {
    state: [
      [keySpent, funded],
      [reference, withScriptRef(policyScript)],
    ],
    sidecar: EMPTY_SIDECAR,
    tx: {
      spendInputs: [keySpent],
      referenceInputs: [reference],
      outputs: [mintedOutput(policy)],
      mintPreimageCbor: mintOf(policy),
      ...extra,
    },
  };
};

/** `referenceMint`, also paying a Plutus V3 script under a receive redeemer. */
const referenceMintAndReceive = (policyScript: NativeScript): Body => {
  const policy = hashScriptWitness(policyScript);
  return {
    state: [...referenceMint(policyScript).state, [keySpentTwo, funded]],
    sidecar: PLUTUS_SIDECAR,
    tx: referenceMint(policyScript, {
      spendInputs: [keySpent, keySpentTwo],
      outputs: [mintedOutput(policy), plutusReceive],
      scriptWitnesses: [plutus],
      redeemerTxWitsPreimageCbor: receiveRedeemer,
      scriptLanguages: ["PlutusV3"],
    }).tx,
  };
};

/** A key spend and the false native spend. */
const nativeSpend = (extra: Partial<Row["tx"]> = {}): Body => ({
  state: [
    [keySpent, funded],
    [nativeSpent, nativeLocked],
  ],
  sidecar: EMPTY_SIDECAR,
  tx: {
    spendInputs: [keySpent, nativeSpent],
    outputs: [funded, funded],
    ...extra,
  },
});

export const NATIVE_ROWS: readonly Row[] = [
  {
    name: "e_accept_reference_native_mint",
    kind: "accept",
    guard: true,
    ...referenceMint(TRUE_POLICY_SCRIPT),
    expected: null,
  },
  // Native executions run in execution order: the spend before the mint.
  {
    name: "e_false_spend_and_false_mint",
    kind: "multi",
    guard: true,
    state: [
      ...nativeSpend().state,
      [reference, withScriptRef(FALSE_POLICY_SCRIPT)],
    ],
    sidecar: EMPTY_SIDECAR,
    tx: referenceMint(FALSE_POLICY_SCRIPT, {
      spendInputs: [keySpent, nativeSpent],
      outputs: [mintedOutput(FALSE_POLICY), funded],
    }).tx,
    expected: FALSE_SPEND,
  },
  {
    name: "e_false_spend_control_false_spend",
    kind: "control",
    guard: true,
    ...nativeSpend(),
    expected: FALSE_SPEND,
  },
  {
    name: "e_false_spend_control_false_mint",
    kind: "control",
    guard: true,
    ...referenceMint(FALSE_POLICY_SCRIPT),
    expected: FALSE_MINT,
  },
  // Every native execution runs before the script integrity hash. An
  // all-native transaction hashes no language, so naming one mismatches.
  {
    name: "e_false_mint_and_integrity_mismatch",
    kind: "multi",
    guard: true,
    ...referenceMint(FALSE_POLICY_SCRIPT, { scriptLanguages: ["PlutusV3"] }),
    expected: FALSE_MINT,
  },
  {
    name: "e_false_mint_and_integrity_control_integrity",
    kind: "control",
    guard: true,
    ...referenceMint(TRUE_POLICY_SCRIPT, { scriptLanguages: ["PlutusV3"] }),
    expected: INTEGRITY_MISMATCH,
  },
  {
    name: "e_false_mint_and_integrity_control_false_mint",
    kind: "control",
    guard: true,
    ...referenceMint(FALSE_POLICY_SCRIPT),
    expected: FALSE_MINT,
  },
  // Every native execution runs before any non-native one.
  {
    name: "e_false_mint_and_receive_plutus_v3",
    kind: "multi",
    guard: true,
    ...referenceMintAndReceive(FALSE_POLICY_SCRIPT),
    expected: FALSE_MINT,
  },
  {
    name: "e_false_mint_and_receive_control_receive",
    kind: "control",
    guard: true,
    ...referenceMintAndReceive(TRUE_POLICY_SCRIPT),
    expected: RECEIVE_PLUTUS_V3,
  },
  {
    name: "e_false_mint_and_receive_control_false_mint",
    kind: "control",
    guard: true,
    ...referenceMint(FALSE_POLICY_SCRIPT),
    expected: FALSE_MINT,
  },
  // Phase A evaluates every inline native witness before any execution.
  {
    name: "e_inline_false_and_false_spend",
    kind: "multi",
    guard: true,
    ...nativeSpend({
      outputs: [mintedOutput(FALSE_POLICY), funded],
      scriptWitnesses: [FALSE_POLICY_SCRIPT],
      mintPreimageCbor: mintOf(FALSE_POLICY),
    }),
    expected: INLINE_FALSE,
  },
  {
    name: "e_inline_false_control_inline_false",
    kind: "control",
    guard: true,
    state: [[keySpent, funded]],
    sidecar: EMPTY_SIDECAR,
    tx: {
      spendInputs: [keySpent],
      outputs: [mintedOutput(FALSE_POLICY)],
      scriptWitnesses: [FALSE_POLICY_SCRIPT],
      mintPreimageCbor: mintOf(FALSE_POLICY),
    },
    expected: INLINE_FALSE,
  },
  {
    name: "e_inline_false_control_false_spend",
    kind: "control",
    guard: true,
    ...nativeSpend(),
    expected: FALSE_SPEND,
  },
  // A malformed native witness stops at Phase A, before any execution scan.
  {
    name: "e_refusal_malformed_native_witness",
    kind: "refusal",
    guard: true,
    state: [[keySpent, funded]],
    sidecar: EMPTY_SIDECAR,
    tx: { spendInputs: [keySpent], outputs: [funded] },
    // `[0, h'820700']`: a native payload whose constructor tag does not exist.
    scriptItems: [Buffer.from("820043820700", "hex")],
    expected: {
      code: RejectCodes.InvalidFieldType,
      reason: { WitnessNativeScriptMalformed: { script_index: 0n } },
    },
  },
];

/** The scan limit, in nodes and in nested composites alike. */
export const NATIVE_SCRIPT_LIMIT = 16_384;
/** No native node is shorter: an array head, a tag and one more byte. */
export const MIN_NATIVE_NODE_BYTES = 3;

/** `[4, 0]`: invalid before slot 0, a shortest leaf. */
const LEAF = Buffer.from("820400", "hex");

/**
 * The shortest native script over the node limit (`all` of 16,384 leaves) or
 * over the depth limit (16,385 nested single-child `all`s around one leaf).
 */
export const overLimitNative = (limit: "node" | "depth"): Buffer =>
  limit === "node"
    ? Buffer.concat([
        Buffer.from("8201994000", "hex"),
        ...Array.from({ length: NATIVE_SCRIPT_LIMIT }, () => LEAF),
      ])
    : Buffer.concat([
        ...Array.from({ length: NATIVE_SCRIPT_LIMIT + 1 }, () =>
          Buffer.from("820181", "hex"),
        ),
        LEAF,
      ]);

/**
 * That script as the transaction's one native witness. It fails canonical
 * decode, which a direct proof convicts: there is no machine trace for it.
 */
export const limitRefusal = (limit: "node" | "depth"): Row => ({
  name: `e_refusal_${limit}_limit_native_witness`,
  kind: "refusal",
  guard: true,
  state: [[keySpent, funded]],
  sidecar: EMPTY_SIDECAR,
  tx: { spendInputs: [keySpent], outputs: [funded] },
  scriptItems: [
    Buffer.concat([
      Buffer.from([0x82, 0x00]),
      encodeMidgardDefiniteBytes(overLimitNative(limit)),
    ]),
  ],
  expected: { code: RejectCodes.InvalidFieldType, reason: null },
});
