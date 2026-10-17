import { protectMidgardAddress } from "@al-ft/midgard-core/codec";
import { describe, it } from "vitest";

import { RejectCodes } from "../src/index.js";
import { MidgardRedeemerTag } from "../src/midgard-redeemers.js";
import {
  EMPTY_SIDECAR,
  expectControlledRows,
  type Expected,
  expectRowVerdicts,
  funded,
  keyAddress,
  keySpent,
  mintedOutput,
  mintOf,
  native,
  NATIVE_POLICY,
  nativeAll,
  type NativeTxOptions,
  plutus,
  PLUTUS_SIDECAR,
  type Row,
} from "./emission-order.support.js";
import { scriptAddressBytes } from "./reject-subject.support.js";
import {
  FUNDED_OUTPUT_LOVELACE,
  hashScriptWitness,
  makeOutput,
  makeRedeemersCbor,
  nativeScriptWitness,
  outRefFromByte,
} from "./validation-fixtures.js";

/**
 * Emission order on multi-fault transactions. Each multi-fault row carries two
 * faults the machine meets at different positions of its traversal, and the
 * producer (Phase A then Phase B), the trace builder and the one-step argument
 * of the trace's stopping step must all name the machine's first one. Every
 * multi-fault row has a single-fault control per fault, and one row accepts.
 * A row marked `guard` is one the producer already got right before the
 * reconciliation; it pins the order rather than proving a change.
 *
 * Cases (machine script-sources stages):
 * - (b) a repeated redeemer pointer is an unused redeemer of the stage-12
 *   audit, after every purpose's source and redeemer search (stages 9-10);
 * - (c) output network and protected-key signer checks (stage 5) precede every
 *   purpose's source and redeemer search;
 * - (d) the unused inline witness audit (stage 11) precedes the unused
 *   redeemer audit (stage 12).
 */

/** A key address on network 1; the rows expect network 0. */
const FOREIGN_NETWORK_ADDRESS = keyAddress(1, 0x6b);
/** A protected key output whose key does not sign. */
const PROTECTED_UNSIGNED = makeOutput(
  FUNDED_OUTPUT_LOVELACE,
  protectMidgardAddress(keyAddress(0, 0x6c)),
);

/** A second passing native script no purpose names. */
const unusedNative = nativeScriptWitness({
  type: "any",
  scripts: [nativeAll],
});
/** A policy no source carries. */
const ABSENT_POLICY = "ee".repeat(28);

const plutusSpent = outRefFromByte(0x82);
const plutusLocked = makeOutput(
  FUNDED_OUTPUT_LOVELACE,
  scriptAddressBytes(hashScriptWitness(plutus)),
);
const absentLocked = makeOutput(
  FUNDED_OUTPUT_LOVELACE,
  scriptAddressBytes(ABSENT_POLICY),
);

const spendRedeemer = { tag: MidgardRedeemerTag.Spend, index: 0n };
const mintRedeemer = { tag: MidgardRedeemerTag.Mint, index: 0n };

const unusedRedeemer = (redeemer_index: bigint): Expected => ({
  code: RejectCodes.InvalidFieldType,
  reason: { UnusedRedeemer: { redeemer_index } },
});
const unusedWitness = (script_index: bigint): Expected => ({
  code: RejectCodes.InvalidFieldType,
  reason: { UnusedScriptWitness: { script_index } },
});
const sourceMissing = (purpose_kind: bigint, purpose_index = 0n): Expected => ({
  code: RejectCodes.MissingRequiredWitness,
  reason: { ScriptSourceMissing: { purpose_kind, purpose_index } },
});
const networkMismatch: Expected = {
  code: RejectCodes.NetworkIdMismatch,
  reason: "NetworkIdMismatch",
};
const signerMissing = (output_index: bigint): Expected => ({
  code: RejectCodes.MissingRequiredWitness,
  reason: { ProtectedOutputSignerMissing: { output_index } },
});

/** A Plutus spend at pointer Spend/0 with its source inline. */
const plutusSpend = (
  redeemers: readonly {
    readonly tag: number;
    readonly index: bigint;
  }[],
  extra: NativeTxOptions = {},
): Pick<Row, "state" | "tx" | "sidecar"> => ({
  state: [[plutusSpent, plutusLocked]],
  sidecar: PLUTUS_SIDECAR,
  tx: {
    spendInputs: [plutusSpent],
    outputs: [funded],
    scriptWitnesses: [plutus],
    redeemerTxWitsPreimageCbor: makeRedeemersCbor(redeemers),
    scriptLanguages: ["PlutusV3"],
    ...extra,
  },
});

/** A key spend, with no redeemers. */
const keySpend = (
  extra: NativeTxOptions = {},
): Pick<Row, "state" | "tx" | "sidecar"> => ({
  state: [[keySpent, funded]],
  sidecar: EMPTY_SIDECAR,
  tx: { spendInputs: [keySpent], outputs: [funded], ...extra },
});

const ROWS: readonly Row[] = [
  {
    name: "accept_native_mint",
    kind: "accept",
    guard: true,
    ...keySpend({
      outputs: [mintedOutput(NATIVE_POLICY)],
      scriptWitnesses: [native],
      mintPreimageCbor: mintOf(NATIVE_POLICY),
    }),
    expected: null,
  },
  // (b) A repeated pointer is unused only at the stage-12 audit.
  {
    name: "b_repeated_pointer_and_missing_mint_source",
    kind: "multi",
    guard: false,
    ...plutusSpend([spendRedeemer, spendRedeemer], {
      mintPreimageCbor: mintOf(ABSENT_POLICY),
    }),
    expected: sourceMissing(1n),
  },
  {
    name: "b_control_repeated_pointer",
    kind: "control",
    guard: true,
    ...plutusSpend([spendRedeemer, spendRedeemer]),
    expected: unusedRedeemer(1n),
  },
  {
    name: "b_control_missing_mint_source",
    kind: "control",
    guard: true,
    ...plutusSpend([spendRedeemer], {
      mintPreimageCbor: mintOf(ABSENT_POLICY),
    }),
    expected: sourceMissing(1n),
  },
  {
    name: "b_repeated_pointer_at_native_purpose",
    kind: "multi",
    guard: false,
    ...plutusSpend([spendRedeemer, mintRedeemer, mintRedeemer], {
      scriptWitnesses: [plutus, native],
      mintPreimageCbor: mintOf(NATIVE_POLICY),
      outputs: [mintedOutput(NATIVE_POLICY)],
    }),
    // Neither Mint/0 redeemer is used: the first one is the first unused.
    expected: unusedRedeemer(1n),
  },
  {
    name: "b_control_redeemer_at_native_purpose",
    kind: "control",
    guard: true,
    ...plutusSpend([spendRedeemer, mintRedeemer], {
      scriptWitnesses: [plutus, native],
      mintPreimageCbor: mintOf(NATIVE_POLICY),
      outputs: [mintedOutput(NATIVE_POLICY)],
    }),
    expected: unusedRedeemer(1n),
  },
  // (c) Output checks run at stage 5, before every source search.
  {
    name: "c_output_network_and_missing_mint_source",
    kind: "multi",
    guard: false,
    ...keySpend({
      outputs: [makeOutput(FUNDED_OUTPUT_LOVELACE, FOREIGN_NETWORK_ADDRESS)],
      mintPreimageCbor: mintOf(ABSENT_POLICY),
    }),
    expected: networkMismatch,
  },
  {
    name: "c_protected_signer_and_missing_spend_source",
    kind: "multi",
    guard: false,
    state: [
      [keySpent, funded],
      [plutusSpent, absentLocked],
    ],
    sidecar: EMPTY_SIDECAR,
    tx: {
      spendInputs: [keySpent, plutusSpent],
      outputs: [funded, PROTECTED_UNSIGNED],
    },
    expected: signerMissing(1n),
  },
  {
    name: "c_control_output_network",
    kind: "control",
    guard: true,
    ...keySpend({
      outputs: [makeOutput(FUNDED_OUTPUT_LOVELACE, FOREIGN_NETWORK_ADDRESS)],
    }),
    expected: networkMismatch,
  },
  {
    name: "c_control_protected_signer",
    kind: "control",
    guard: true,
    ...keySpend({ outputs: [funded, PROTECTED_UNSIGNED] }),
    expected: signerMissing(1n),
  },
  {
    name: "c_control_missing_mint_source",
    kind: "control",
    guard: true,
    ...keySpend({ mintPreimageCbor: mintOf(ABSENT_POLICY) }),
    expected: sourceMissing(1n),
  },
  {
    name: "c_control_missing_spend_source",
    kind: "control",
    guard: true,
    state: [
      [keySpent, funded],
      [plutusSpent, absentLocked],
    ],
    sidecar: EMPTY_SIDECAR,
    tx: { spendInputs: [keySpent, plutusSpent], outputs: [funded] },
    // The script spend sorts second: spend pointer index 1.
    expected: sourceMissing(0n, 1n),
  },
  // (d) Stage 11 (unused inline witness) before stage 12 (unused redeemer).
  {
    name: "d_unused_witness_and_unused_redeemer",
    kind: "multi",
    guard: false,
    ...plutusSpend([spendRedeemer, mintRedeemer], {
      scriptWitnesses: [plutus, unusedNative],
    }),
    expected: unusedWitness(1n),
  },
  {
    name: "d_control_unused_witness",
    kind: "control",
    guard: true,
    ...plutusSpend([spendRedeemer], {
      scriptWitnesses: [plutus, unusedNative],
    }),
    expected: unusedWitness(1n),
  },
  {
    name: "d_control_unused_redeemer",
    kind: "control",
    guard: true,
    ...plutusSpend([spendRedeemer, mintRedeemer]),
    expected: unusedRedeemer(1n),
  },
];

describe("emission order: producer, trace builder and one-step agree on the first fault", () => {
  it("every multi-fault row has a single-fault control per fault", () => {
    expectControlledRows(ROWS, ["b_", "c_", "d_"]);
  });

  it.each(ROWS)("$name", expectRowVerdicts, 180_000);
});
