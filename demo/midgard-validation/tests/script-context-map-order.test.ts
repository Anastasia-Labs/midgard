import {
  encodeCbor,
  encodeMidgardCekProgramMaterialSidecar,
  MIDGARD_CONSENSUS_PROFILE,
} from "@al-ft/midgard-core";
import {
  decodeMidgardDatum,
  decodeMidgardTxOutput,
  encodeMidgardTxOutput,
  protectMidgardAddress,
} from "@al-ft/midgard-core/codec";
import {
  DataConstr,
  DataI,
  DataMap,
  dataToCbor,
} from "@harmoniclabs/plutus-data";
import {
  Application,
  Builtin,
  Delay,
  ErrorUPLC,
  Force,
  Lambda,
  UPLCBuiltinTag,
  UPLCConst,
  UPLCEncoder,
  UPLCProgram,
  type UPLCTerm,
  UPLCVar,
} from "@harmoniclabs/uplc";
import { CML } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  buildDeterministicValidationMachineTrace,
  buildMidgardCanonicalCekProgram,
  buildMidgardScriptContext,
  buildPlutusV3ScriptContext,
  buildValidationMachineLedgerInsertOp,
  buildValidationMachineLedgerMutationSteps,
  encodeScriptContextCbor,
  type MidgardLedgerRedeemer,
  MidgardRedeemerTag,
  type MidgardScriptPurpose,
  RejectCodes,
  runPhaseBValidationWithPatch,
  scriptContextTxOutData,
  type ScriptContextView,
} from "../src/index.js";
import {
  FUNDED_OUTPUT_LOVELACE,
  hashScriptWitness,
  makeMintPreimageCbor,
  makeNativeTx,
  makeOutput,
  makePhaseBCandidate,
  makeRedeemersCbor,
  outRefFromByte,
  outRefFromTxId,
  plutusV3ScriptWitness,
} from "./validation-fixtures.js";

// A node evaluates a script over the context it builds; a fault proof
// adjudicates it over the context the transaction commits, where datum and
// redeemer maps keep their CBOR entry order and duplicate keys and the redeemer
// map is in ledger order, (tag, index) ascending, whatever the witness list
// order. These tests pin that the two contexts agree.

/** `{h'ff': 1, h'0000': 2}`: not in byte order. */
const UNSORTED_MAP = "a241ff0142000002";
/** `{h'0000': 2, h'ff': 1}`: the same entries in byte order. */
const SORTED_MAP = "a24200000241ff01";
/** `{h'ff': 1, h'ff': 2}`: a duplicate key. */
const DUPLICATE_KEY_MAP = "a241ff0141ff02";

const EX_UNITS = [1_000_000_000n, 1_000_000_000n] as const;

const builtin = (tag: UPLCBuiltinTag): UPLCTerm => new Builtin(tag);
const apply = (fn: UPLCTerm, ...args: UPLCTerm[]): UPLCTerm =>
  args.reduce<UPLCTerm>((term, arg) => new Application(term, arg), fn);
const constrFields = (data: UPLCTerm): UPLCTerm =>
  apply(
    builtin(UPLCBuiltinTag.sndPair),
    apply(builtin(UPLCBuiltinTag.unConstrData), data),
  );
const nth = (list: UPLCTerm, index: number): UPLCTerm => {
  let rest = list;
  for (let step = 0; step < index; step += 1) {
    rest = apply(builtin(UPLCBuiltinTag.tailList), rest);
  }
  return apply(builtin(UPLCBuiltinTag.headList), rest);
};
const firstKey = (map: UPLCTerm): UPLCTerm =>
  apply(
    builtin(UPLCBuiltinTag.fstPair),
    apply(
      builtin(UPLCBuiltinTag.headList),
      apply(builtin(UPLCBuiltinTag.unMapData), map),
    ),
  );
/** Succeeds when `condition` holds and fails otherwise. */
const guard = (condition: UPLCTerm): UPLCTerm =>
  new Lambda(
    new Force(
      apply(
        builtin(UPLCBuiltinTag.ifThenElse),
        condition,
        new Delay(UPLCConst.unit),
        new Delay(new ErrorUPLC()),
      ),
    ),
  );
const flat = (body: UPLCTerm): Buffer =>
  Buffer.from(
    UPLCEncoder.compile(new UPLCProgram([1, 1, 0], body)).toBuffer().buffer,
  );

const context = new UPLCVar(0);

/** Succeeds when the first key of `map` is two bytes long. */
const firstKeyIsTwoBytes = (map: UPLCTerm): Buffer =>
  flat(
    guard(
      apply(
        builtin(UPLCBuiltinTag.equalsInteger),
        apply(
          builtin(UPLCBuiltinTag.lengthOfByteString),
          apply(builtin(UPLCBuiltinTag.unBData), firstKey(map)),
        ),
        UPLCConst.int(2),
      ),
    ),
  );

/** A PlutusV3 spend script: the first key of its datum is two bytes long. */
const firstDatumKeyIsTwoBytes = firstKeyIsTwoBytes(
  nth(constrFields(nth(constrFields(nth(constrFields(context), 2)), 1)), 0),
);

/** A PlutusV3 script: the first key of the first output's datum is two bytes long. */
const firstOutputDatumKeyIsTwoBytes = firstKeyIsTwoBytes(
  nth(
    constrFields(
      nth(
        constrFields(
          apply(
            builtin(UPLCBuiltinTag.headList),
            apply(
              builtin(UPLCBuiltinTag.unListData),
              nth(constrFields(nth(constrFields(context), 0)), 2),
            ),
          ),
        ),
        2,
      ),
    ),
    0,
  ),
);

/** A PlutusV3 script: the context's first redeemer has purpose `tag`. */
const firstRedeemerPurposeIs = (tag: number): Buffer =>
  flat(
    guard(
      apply(
        builtin(UPLCBuiltinTag.equalsInteger),
        apply(
          builtin(UPLCBuiltinTag.fstPair),
          apply(
            builtin(UPLCBuiltinTag.unConstrData),
            firstKey(nth(constrFields(nth(constrFields(context), 0)), 9)),
          ),
        ),
        UPLCConst.int(tag),
      ),
    ),
  );

/** PlutusV3 `ScriptPurpose` is `Minting` (0) or `Spending` (1) here. */
const firstRedeemerIsMint = firstRedeemerPurposeIs(0);
const firstRedeemerIsSpend = firstRedeemerPurposeIs(1);

const identity = flat(new Lambda(new UPLCVar(0)));

const scriptOutput = (scriptHash: string, datumHex?: string): Buffer =>
  encodeMidgardTxOutput({
    address: protectMidgardAddress(
      Buffer.from(
        CML.EnterpriseAddress.new(
          0,
          CML.Credential.new_script(CML.ScriptHash.from_hex(scriptHash)),
        )
          .to_address()
          .to_raw_bytes(),
      ),
    ),
    value: { lovelace: FUNDED_OUTPUT_LOVELACE, assets: new Map() },
    ...(datumHex === undefined
      ? {}
      : { datum: decodeMidgardDatum(Buffer.from(datumHex, "hex")) }),
  });

const outputWithDatum = (datumHex: string): Buffer =>
  encodeMidgardTxOutput({
    address: decodeMidgardTxOutput(makeOutput(1n)).address,
    value: { lovelace: FUNDED_OUTPUT_LOVELACE, assets: new Map() },
    datum: decodeMidgardDatum(Buffer.from(datumHex, "hex")),
  });

type Scenario = {
  readonly program: Buffer;
  readonly spentDatum?: string;
  readonly outputDatum?: string;
  readonly spendRedeemer?: string;
  /** Adds a mint of the script's own policy; the order of the two redeemers. */
  readonly mint?: "spend-first" | "mint-first";
  /** Runs the flat script with no CEK program material. */
  readonly bareScript?: true;
};

const buildScenario = (scenario: Scenario) => {
  const program = buildMidgardCanonicalCekProgram(scenario.program);
  const script = plutusV3ScriptWitness(
    scenario.bareScript === true ? scenario.program : program.envelopeCbor,
  );
  const scriptHash = hashScriptWitness(script);
  const spent = outRefFromByte(0x5b);
  const spentOutput = scriptOutput(scriptHash, scenario.spentDatum);
  const output =
    scenario.outputDatum !== undefined
      ? outputWithDatum(scenario.outputDatum)
      : scenario.mint !== undefined
        ? makeOutput(
            FUNDED_OUTPUT_LOVELACE,
            undefined,
            new Map([[scriptHash, new Map([["aced", 5n]])]]),
          )
        : makeOutput(FUNDED_OUTPUT_LOVELACE);
  const spend = {
    tag: MidgardRedeemerTag.Spend,
    index: 0n,
    exUnits: EX_UNITS,
    ...(scenario.spendRedeemer === undefined
      ? {}
      : { data: Buffer.from(scenario.spendRedeemer, "hex") }),
  };
  const mint = { tag: MidgardRedeemerTag.Mint, index: 0n, exUnits: EX_UNITS };
  const txOptions = {
    scriptWitnesses: [script],
    redeemerTxWitsPreimageCbor: makeRedeemersCbor(
      scenario.mint === undefined
        ? [spend]
        : scenario.mint === "spend-first"
          ? [spend, mint]
          : [mint, spend],
    ),
    ...(scenario.mint === undefined
      ? {}
      : {
          mintPreimageCbor: makeMintPreimageCbor(
            new Map([
              [
                Buffer.from(scriptHash, "hex"),
                new Map([[Buffer.from("aced", "hex"), 5n]]),
              ],
            ]),
          ),
        }),
    scriptLanguages: ["PlutusV3" as const],
  };
  const sidecar =
    scenario.bareScript === true
      ? null
      : encodeMidgardCekProgramMaterialSidecar([...program.material.values()]);
  return {
    spent,
    spentOutput,
    output,
    sidecar,
    transaction: makeNativeTx({
      version: 1n,
      spendInputs: [spent],
      outputs: [output],
      ...txOptions,
    }),
    candidate: makePhaseBCandidate({
      spent: [spent],
      outputs: [output],
      programMaterialSidecarCbor: sidecar,
      ...txOptions,
    }),
  };
};

const nodeVerdict = async (scenario: Scenario) => {
  const { candidate, spent, spentOutput } = buildScenario(scenario);
  const result = await Effect.runPromise(
    runPhaseBValidationWithPatch(
      [candidate],
      new Map([[spent.toString("hex"), spentOutput]]),
      { nowCardanoSlotNo: 100n, bucketConcurrency: 1 },
    ),
  );
  return result.accepted.length === 1
    ? "accepted"
    : `rejected ${result.rejected[0]?.code ?? "?"}`;
};

/** Builds the fault-proof trace, which refuses a context it does not commit. */
const proofVerdict = async (
  scenario: Scenario,
  verdict: "accepted" | "rejected",
) => {
  const { transaction, spent, spentOutput, output, sidecar } =
    buildScenario(scenario);
  const ledgerOps = [
    { type: "delete" as const, key: spent },
    buildValidationMachineLedgerInsertOp({
      key: outRefFromTxId(transaction.txId),
      outputCbor: output,
    }),
  ];
  const steps = await buildValidationMachineLedgerMutationSteps({
    initialEntries: [{ outRef: spent, output: spentOutput }],
    operations: ledgerOps,
  });
  const preRoot = steps[0]!.preRoot.toString("hex");
  const trace = await Effect.runPromise(
    buildDeterministicValidationMachineTrace({
      consensusProfile: MIDGARD_CONSENSUS_PROFILE,
      eventKeyCbor: encodeCbor([2n, Buffer.alloc(32, 0x41)]),
      sourceKind: "normal",
      blockEndTimeMs: 1_750_000_000_000,
      expectedNetworkId: 0n,
      minFeeA: 0n,
      minFeeB: 0n,
      blockSlot: 100n,
      transactionId: transaction.txId,
      canonicalTransactionCbor: transaction.txCbor,
      programMaterialSidecarCbor: sidecar!,
      priorUtxosRoot: preRoot,
      postUtxosRoot:
        verdict === "accepted"
          ? steps.at(-1)!.postRoot.toString("hex")
          : preRoot,
      ledgerWitnessEntries: [{ outRef: spent, output: spentOutput }],
      expectedLedgerOps: verdict === "accepted" ? ledgerOps : [],
      ledgerMutationSteps: verdict === "accepted" ? steps : [],
      expectedVerdict: verdict,
      expectedRejectionCode:
        verdict === "accepted" ? null : RejectCodes.PlutusScriptInvalid,
    }),
  );
  return trace.verdict;
};

/** The context's redeemer map, as (purpose constructor, redeemer int). */
const redeemerMapEntries = (
  context: DataConstr,
  redeemersField: number,
): readonly (readonly [number, bigint])[] => {
  const txInfo = context.fields[0] as DataConstr;
  const map = txInfo.fields[redeemersField] as DataMap<DataConstr, DataI>;
  return map.map.map(
    (pair) => [Number(pair.fst.constr), BigInt(pair.snd.int)] as const,
  );
};

const orderingView = (): {
  readonly view: ScriptContextView;
  readonly spend: {
    readonly purpose: MidgardScriptPurpose;
    readonly redeemer: MidgardLedgerRedeemer;
  };
} => {
  const scriptHash = "ab".repeat(28);
  const entry = (
    purpose: MidgardScriptPurpose,
    tag: number,
    index: bigint,
    value: bigint,
  ) => ({
    purpose,
    redeemer: {
      tag,
      index,
      dataCbor: Buffer.from(dataToCbor(new DataI(value)).toBuffer()),
      exUnits: { memory: 0n, steps: 0n },
    },
  });
  const spend0 = entry(
    { kind: "spend", scriptHash, outRefHex: outRefFromByte(1).toString("hex") },
    MidgardRedeemerTag.Spend,
    0n,
    0n,
  );
  const spend1 = entry(
    { kind: "spend", scriptHash, outRefHex: outRefFromByte(2).toString("hex") },
    MidgardRedeemerTag.Spend,
    1n,
    1n,
  );
  const mint = entry(
    { kind: "mint", scriptHash, policyId: scriptHash },
    MidgardRedeemerTag.Mint,
    0n,
    2n,
  );
  const observe = entry(
    { kind: "observe", scriptHash },
    MidgardRedeemerTag.Reward,
    0n,
    3n,
  );
  const receive = entry(
    { kind: "receive", scriptHash },
    MidgardRedeemerTag.Receiving,
    0n,
    4n,
  );
  return {
    view: {
      txId: Buffer.alloc(32, 0),
      inputs: [],
      referenceInputs: [],
      outputs: [],
      fee: 1n,
      observers: [scriptHash],
      signatories: [],
      mint: new Map(),
      // Deliberately not in ledger order.
      redeemers: [receive, observe, spend1, mint, spend0],
    },
    spend: spend0,
  };
};

describe("script context map order", () => {
  it("builds the redeemer map in ledger order from a view in any order", () => {
    const { view, spend } = orderingView();
    // MidgardV1: spend (1), mint (0), observe (2), receive (3) purposes.
    expect(
      redeemerMapEntries(
        buildMidgardScriptContext(view, spend.purpose, spend.redeemer),
        8,
      ),
    ).toEqual([
      [1, 0n],
      [1, 1n],
      [0, 2n],
      [2, 3n],
      [3, 4n],
    ]);
    // PlutusV3 has no receiving purpose, so it omits that redeemer.
    expect(
      redeemerMapEntries(
        buildPlutusV3ScriptContext(view, spend.purpose, spend.redeemer),
        9,
      ),
    ).toEqual([
      [1, 0n],
      [1, 1n],
      [0, 2n],
      [2, 3n],
    ]);
  });

  it("builds a TxOut whose datum keeps its key order and duplicate keys", () => {
    for (const datum of [UNSORTED_MAP, SORTED_MAP, DUPLICATE_KEY_MAP]) {
      const outputCbor = scriptOutput("33".repeat(28), datum);
      const txOut = scriptContextTxOutData(
        decodeMidgardTxOutput(outputCbor),
        "cardano",
      );
      // Script address, 10 ada, the inline datum as written, no script ref.
      expect(Buffer.from(encodeScriptContextCbor(txOut)).toString("hex")).toBe(
        `d8799fd8799fd87a9f581c${"33".repeat(28)}ffd87a80ff` +
          `a140a1401a00989680d87b9f${datum}ffd87a80ff`,
      );
    }
  });

  it("gives an order-sensitive script the datum in its own key order", async () => {
    for (const bareScript of [undefined, true] as const) {
      await expect(
        nodeVerdict({
          program: firstDatumKeyIsTwoBytes,
          spentDatum: UNSORTED_MAP,
          bareScript,
        }),
      ).resolves.toBe(`rejected ${RejectCodes.PlutusScriptInvalid}`);
      await expect(
        nodeVerdict({
          program: firstDatumKeyIsTwoBytes,
          spentDatum: SORTED_MAP,
          bareScript,
        }),
      ).resolves.toBe("accepted");
    }
  });

  it("proves the verdict over the datum and redeemer maps the node evaluated", async () => {
    const accepted: readonly Scenario[] = [
      { program: identity, spentDatum: UNSORTED_MAP },
      { program: identity, spentDatum: DUPLICATE_KEY_MAP },
      { program: identity, spendRedeemer: UNSORTED_MAP },
      { program: identity, spendRedeemer: DUPLICATE_KEY_MAP },
    ];
    for (const scenario of accepted) {
      await expect(nodeVerdict(scenario)).resolves.toBe("accepted");
      await expect(proofVerdict(scenario, "accepted")).resolves.toBe(
        "accepted",
      );
    }
    for (const [outputDatum, expected] of [
      [UNSORTED_MAP, "rejected"],
      [SORTED_MAP, "accepted"],
    ] as const) {
      const scenario = { program: firstOutputDatumKeyIsTwoBytes, outputDatum };
      await expect(nodeVerdict(scenario)).resolves.toBe(
        expected === "accepted"
          ? "accepted"
          : `rejected ${RejectCodes.PlutusScriptInvalid}`,
      );
      await expect(proofVerdict(scenario, expected)).resolves.toBe(expected);
    }
  });

  it("orders the redeemer map by purpose whatever the witness list order", async () => {
    for (const mint of ["spend-first", "mint-first"] as const) {
      for (const [program, expected] of [
        [firstRedeemerIsSpend, "accepted"],
        [firstRedeemerIsMint, "rejected"],
      ] as const) {
        const scenario = { program, mint };
        await expect(nodeVerdict(scenario)).resolves.toBe(
          expected === "accepted"
            ? "accepted"
            : `rejected ${RejectCodes.PlutusScriptInvalid}`,
        );
        await expect(proofVerdict(scenario, expected)).resolves.toBe(expected);
      }
      await expect(
        proofVerdict({ program: identity, mint }, "accepted"),
      ).resolves.toBe("accepted");
    }
  }, 60_000);
});
