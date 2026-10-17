import { encodeMidgardCekProgramMaterialSidecar } from "@al-ft/midgard-core/cek-proof";
import { encodeCbor, protectMidgardAddress } from "@al-ft/midgard-core/codec";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import type { MidgardRejectionReasonV1 } from "@al-ft/midgard-core/validation-trace";
import { Lambda, UPLCEncoder, UPLCProgram, UPLCVar } from "@harmoniclabs/uplc";
import { CML } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { buildMidgardCanonicalCekProgram } from "../src/cek-program.js";
import {
  buildDeterministicValidationMachineTrace,
  buildValidationMachineLedgerInsertOp,
  buildValidationMachineLedgerMutationSteps,
  RejectCodes,
  rejectionReasonOfSubject,
  runPhaseAValidation,
  runPhaseBValidationWithPatch,
} from "../src/index.js";
import { MidgardRedeemerTag } from "../src/midgard-redeemers.js";
import type { RejectCode } from "../src/types.js";
import { buildValidationOneStepArgument } from "../src/validation-machine-data.js";
import { scriptAddressBytes } from "./reject-subject.support.js";
import {
  FUNDED_OUTPUT_LOVELACE,
  hashScriptWitness,
  makeMintPreimageCbor,
  makeNativeTx,
  makeOutput,
  makeRedeemersCbor,
  nativeScriptWitness,
  outRefFromByte,
  outRefFromTxId,
  plutusV3ScriptWitness,
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

// A fixed key, so that a row's bytes do not change between runs.
const KEY = CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 0x6b));
const keyAddress = (networkId: number, keyByte: number) =>
  Buffer.from(
    CML.EnterpriseAddress.new(
      networkId,
      CML.Credential.new_pub_key(
        CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, keyByte))
          .to_public()
          .hash(),
      ),
    )
      .to_address()
      .to_raw_bytes(),
  );
const KEY_ADDRESS = keyAddress(0, 0x6b);
/** A key address on network 1; the rows expect network 0. */
const FOREIGN_NETWORK_ADDRESS = keyAddress(1, 0x6b);
/** A protected key output whose key does not sign. */
const PROTECTED_UNSIGNED = makeOutput(
  FUNDED_OUTPUT_LOVELACE,
  protectMidgardAddress(keyAddress(0, 0x6c)),
);

/** A Plutus V3 spend the rows never execute: each stops before the CEK. */
const program = buildMidgardCanonicalCekProgram(
  Buffer.from(
    UPLCEncoder.compile(new UPLCProgram([1, 1, 0], new Lambda(new UPLCVar(0)))),
  ),
);
const plutus = plutusV3ScriptWitness(program.envelopeCbor);
const PLUTUS_SIDECAR = encodeMidgardCekProgramMaterialSidecar([
  ...program.material.values(),
]);
const EMPTY_SIDECAR = encodeMidgardCekProgramMaterialSidecar([]);

const nativeAll: Parameters<typeof nativeScriptWitness>[0] = {
  type: "all",
  scripts: [],
};
const native = nativeScriptWitness(nativeAll);
const NATIVE_POLICY = hashScriptWitness(native);
/** A second passing native script no purpose names. */
const unusedNative = nativeScriptWitness({
  type: "any",
  scripts: [nativeAll],
});
/** A policy no source carries. */
const ABSENT_POLICY = "ee".repeat(28);

const keySpent = outRefFromByte(0x81);
const plutusSpent = outRefFromByte(0x82);
const funded = makeOutput(FUNDED_OUTPUT_LOVELACE, KEY_ADDRESS);
const plutusLocked = makeOutput(
  FUNDED_OUTPUT_LOVELACE,
  scriptAddressBytes(hashScriptWitness(plutus)),
);
const absentLocked = makeOutput(
  FUNDED_OUTPUT_LOVELACE,
  scriptAddressBytes(ABSENT_POLICY),
);

const assetName = Buffer.from("42", "hex");
const mintOf = (policy: string) =>
  makeMintPreimageCbor(
    new Map([[Buffer.from(policy, "hex"), new Map([[assetName, 5n]])]]),
  );
const mintedOutput = (policy: string) =>
  makeOutput(
    FUNDED_OUTPUT_LOVELACE,
    KEY_ADDRESS,
    new Map([[policy, new Map([[assetName.toString("hex"), 5n]])]]),
  );
const spendRedeemer = { tag: MidgardRedeemerTag.Spend, index: 0n };
const mintRedeemer = { tag: MidgardRedeemerTag.Mint, index: 0n };

type NativeTxOptions = NonNullable<Parameters<typeof makeNativeTx>[0]>;

type Expected = {
  readonly code: RejectCode;
  readonly reason: MidgardRejectionReasonV1;
};

type Row = {
  readonly name: string;
  readonly kind: "multi" | "control" | "accept";
  /** The producer already emitted this verdict before the reconciliation. */
  readonly guard: boolean;
  readonly state: readonly (readonly [Buffer, Buffer])[];
  readonly tx: NativeTxOptions & { readonly spendInputs: readonly Buffer[] };
  readonly sidecar: Buffer;
  readonly expected: Expected | null;
};

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

const PHASE_A_CONFIG = {
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  concurrency: 1,
  strictnessProfile: "phase1_midgard",
  consensusProfile: MIDGARD_CONSENSUS_PROFILE,
} as const;

const transactionOf = (row: Row) =>
  makeNativeTx({ ...row.tx, privateKey: KEY });

/** The producer: Phase A, then Phase B on what Phase A admits. */
const producer = async (row: Row) => {
  const transaction = transactionOf(row);
  const phaseA = await Effect.runPromise(
    runPhaseAValidation(
      [
        {
          txId: transaction.txId,
          txCbor: transaction.txCbor,
          arrivalSeq: 0n,
          createdAt: new Date(0),
          programMaterialSidecarCbor: row.sidecar,
        },
      ],
      PHASE_A_CONFIG,
    ),
  );
  if (phaseA.rejected.length > 0) return phaseA.rejected[0]!;
  const phaseB = await Effect.runPromise(
    runPhaseBValidationWithPatch(
      phaseA.accepted,
      new Map(row.state.map(([key, output]) => [key.toString("hex"), output])),
      {
        nowCardanoSlotNo: 100n,
        bucketConcurrency: 1,
        l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
      },
    ),
  );
  return phaseB.rejected[0] ?? null;
};

const machineTrace = async (row: Row) => {
  const transaction = transactionOf(row);
  const entries = row.state.map(([outRef, output]) => ({ outRef, output }));
  const accepted = row.expected === null;
  const expectedLedgerOps = accepted
    ? [
        ...row.tx.spendInputs.map((key) => ({ type: "delete" as const, key })),
        ...(row.tx.outputs ?? []).map((outputCbor, index) =>
          buildValidationMachineLedgerInsertOp({
            key: outRefFromTxId(transaction.txId, BigInt(index)),
            outputCbor,
          }),
        ),
      ]
    : [];
  const ledgerMutationSteps = accepted
    ? await buildValidationMachineLedgerMutationSteps({
        initialEntries: entries,
        operations: expectedLedgerOps,
      })
    : [];
  const unchangedRoot = Buffer.alloc(32, 0x2e).toString("hex");
  return await Effect.runPromise(
    buildDeterministicValidationMachineTrace({
      l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
      consensusProfile: MIDGARD_CONSENSUS_PROFILE,
      eventKeyCbor: encodeCbor([2n, Buffer.alloc(32, 0x41)]),
      sourceKind: "normal",
      blockTimeMs: 1_750_000_000_000,
      expectedNetworkId: 0n,
      minFeeA: 0n,
      minFeeB: 0n,
      blockSlot: 100n,
      transactionId: transaction.txId,
      canonicalTransactionCbor: transaction.txCbor,
      programMaterialSidecarCbor: row.sidecar,
      priorUtxosRoot: accepted
        ? ledgerMutationSteps[0]!.preRoot.toString("hex")
        : unchangedRoot,
      postUtxosRoot: accepted
        ? ledgerMutationSteps.at(-1)!.postRoot.toString("hex")
        : unchangedRoot,
      ledgerWitnessEntries: entries,
      expectedLedgerOps,
      ledgerMutationSteps,
      expectedVerdict: accepted ? "accepted" : "rejected",
      expectedRejectionCode: row.expected?.code ?? null,
    }),
  );
};

describe("emission order: producer, trace builder and one-step agree on the first fault", () => {
  it("every multi-fault row has a single-fault control per fault", () => {
    const names = ROWS.map(({ name }) => name);
    expect(new Set(names).size).toBe(names.length);
    expect(ROWS.filter(({ kind }) => kind === "accept")).toHaveLength(1);
    for (const prefix of ["b_", "c_", "d_"]) {
      const rows = ROWS.filter(({ name }) => name.startsWith(prefix));
      const count = (kind: Row["kind"]) =>
        rows.filter((row) => row.kind === kind).length;
      expect(count("multi")).toBeGreaterThan(0);
      expect(count("control")).toBeGreaterThanOrEqual(2);
    }
  });

  it.each(ROWS)(
    "$name",
    async (row) => {
      const rejection = await producer(row);
      if (row.expected === null) {
        expect(rejection).toBeNull();
      } else {
        expect(rejection?.code).toBe(row.expected.code);
        // An output network mismatch is tx-global and records no subject.
        const producerReason =
          rejection?.subject === undefined
            ? rejection?.code === RejectCodes.NetworkIdMismatch
              ? "NetworkIdMismatch"
              : undefined
            : rejectionReasonOfSubject(rejection.subject, "machine");
        expect(producerReason).toStrictEqual(row.expected.reason);
      }

      const trace = await machineTrace(row);
      expect(trace.verdict).toBe(
        row.expected === null ? "accepted" : "rejected",
      );
      expect(trace.rejectionCode).toBe(row.expected?.code ?? null);
      expect(trace.rejectionReason).toStrictEqual(row.expected?.reason ?? null);

      // The stopping step's one-step argument builds from this trace.
      const stop =
        trace.witnesses.length -
        1 -
        [...trace.witnesses]
          .reverse()
          .findIndex((witness) => witness.phase !== "terminal");
      const argument = buildValidationOneStepArgument({
        trace,
        stateIndex: stop,
      });
      expect(argument.semanticResolverIndex).toBeGreaterThanOrEqual(0);
    },
    180_000,
  );
});
