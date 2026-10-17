import {
  encodeMidgardSpendInputItem,
  type MidgardVersionedScript,
} from "@al-ft/midgard-core";
import { encodeMidgardCekProgramMaterialSidecar } from "@al-ft/midgard-core/cek-proof";
import {
  encodeMidgardTxOutput,
  protectMidgardAddress,
} from "@al-ft/midgard-core/codec";
import {
  decodeSingleCbor,
  encodeCbor,
  encodeCborUnsigned,
} from "@al-ft/midgard-core/codec/cbor";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import type * as SDK from "@al-ft/midgard-sdk";
import {
  buildDeterministicValidationMachineTrace,
  buildMidgardCanonicalCekProgram,
  MidgardRedeemerTag,
  RejectCodes,
  runPhaseBValidationWithPatch,
  validatePhaseASingle,
} from "@al-ft/midgard-validation";
import {
  FUNDED_OUTPUT_LOVELACE,
  hashScriptWitness,
  makeMintPreimageCbor,
  makeNativeTx,
  makeOutput,
  makePhaseBCandidate,
  makeQueued,
  makeRedeemersCbor,
  nativeScriptWitness,
  outRefFromByte,
  plutusV3ScriptWitness,
} from "@al-ft/midgard-validation/tests/validation-fixtures";
import { CML } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  buildDirectSourceIndex,
  buildDirectSourceIndexEvidence,
  prepareDirectScriptMaterial,
} from "../src/direct-script-derivation/index.js";
import { checkLedgerAgainstRoot, ledgerEntriesRoot } from "../src/ledger-at.js";
import { reconstructDaPayload } from "../src/transition-trace/reconstruct.js";
import {
  buildCanonicalBlockFixture,
  fixtureTransactionFromCanonicalCbor,
} from "./helpers/canonical-block-evidence-fixture.js";

/**
 * The direct source map selects, for each script hash, the first source the
 * T224 lookup visits. These rows hold the map to the other two consumers of
 * the shared source-order function: Phase B's purpose lookup and the
 * validation trace builder's stage-nine scan. Every row carries one script in
 * an inline witness, a spent output and a reference input at once.
 */

const KEY = CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 0x5a));
const ADDRESS = Buffer.from(
  CML.EnterpriseAddress.new(
    0,
    CML.Credential.new_pub_key(KEY.to_public().hash()),
  )
    .to_address()
    .to_raw_bytes(),
);

const script = nativeScriptWitness({ type: "all", scripts: [] });
const policy = Buffer.from(hashScriptWitness(script), "hex");
const name = Buffer.from("42", "hex");
const carrying = encodeMidgardTxOutput({
  address: ADDRESS,
  value: { lovelace: FUNDED_OUTPUT_LOVELACE, assets: new Map() },
  script_ref: script,
});
const minted = makeOutput(
  FUNDED_OUTPUT_LOVELACE,
  ADDRESS,
  new Map([[policy.toString("hex"), new Map([[name.toString("hex"), 5n]])]]),
);
const mint = makeMintPreimageCbor(new Map([[policy, new Map([[name, 5n]])]]));

const context = {
  consensusProfile: MIDGARD_CONSENSUS_PROFILE,
  eventKeyCbor: encodeCbor([2n, Buffer.alloc(32, 0x41)]),
  sourceKind: "normal" as const,
  blockTimeMs: 1_750_000_000_000,
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  blockSlot: 100n,
};

/** Direct material for one transaction over an exact predecessor ledger. */
const directMaterial = async (
  txId: Buffer,
  canonicalCbor: Buffer,
  state: readonly (readonly [Buffer, Buffer])[],
) => {
  const entries = state.map(([outRef, output]) => ({ outRef, output }));
  const root = await ledgerEntriesRoot(entries);
  const predecessor = await checkLedgerAgainstRoot({
    entries,
    utxosRoot: root,
    label: "direct source order rows",
  });
  const tx = fixtureTransactionFromCanonicalCbor({
    txId: txId.toString("hex"),
    canonicalCbor,
  });
  const eventKey: SDK.EventKey = {
    L2TransactionEventKey: { tx_id: tx.txId },
  };
  const fixture = await buildCanonicalBlockFixture({
    transactions: [tx],
    prevUtxosRoot: root,
    transitionSteps: [
      {
        schema_version: 1n,
        step_index: 0n,
        event_key: eventKey,
        phase: "L2Transaction",
        pre_utxos_root: root,
        post_utxos_root: root,
      },
    ],
  });
  return await prepareDirectScriptMaterial({
    current: await reconstructDaPayload({
      payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
    }),
    eventKey,
    predecessor,
  });
};

type Row = {
  readonly name: string;
  readonly spent: Buffer;
  readonly reference: Buffer;
  /** The resolved output the lookup visits first. */
  readonly selected: "spent" | "reference";
};

const ROWS: readonly Row[] = [
  {
    name: "spent output below the reference input",
    spent: outRefFromByte(0x71),
    reference: outRefFromByte(0x73),
    selected: "spent",
  },
  {
    name: "reference input below the spent output",
    spent: outRefFromByte(0x74),
    reference: outRefFromByte(0x72),
    selected: "reference",
  },
];

const txOptions = (row: Row) => ({
  privateKey: KEY,
  outputs: [minted],
  mintPreimageCbor: mint,
  referenceInputs: [row.reference],
  scriptWitnesses: [script],
});

/** The script-sources control stage a witness carries (control item 9). */
const scriptSourcesStage = (cbor: Buffer): number =>
  Number((decodeSingleCbor(cbor) as readonly bigint[])[9]);

describe("direct source order is the shared T224 lookup order", () => {
  it.each(ROWS)(
    "$name: the direct map, Phase B and the trace builder select one source",
    async (row) => {
      const state = [
        [row.spent, carrying],
        [row.reference, carrying],
      ] as const;
      const transaction = makeNativeTx({
        spendInputs: [row.spent],
        ...txOptions(row),
      });
      const winner = row.selected === "spent" ? row.spent : row.reference;

      const indexed = await buildDirectSourceIndexEvidence(
        await directMaterial(transaction.txId, transaction.txCbor, state),
      );
      const direct = (await indexed.lookup(policy.toString("hex"))).selected;
      expect(direct).toMatchObject({
        field_index: row.selected === "spent" ? 0n : 1n,
        source_key: winner.toString("hex"),
      });
      expect(indexed.index.count).toBe(1n);

      // Phase B: a resolved output shadows the inline witness, which is then
      // unused.
      const phaseB = await Effect.runPromise(
        runPhaseBValidationWithPatch(
          [makePhaseBCandidate({ spent: [row.spent], ...txOptions(row) })],
          new Map(state.map(([key, output]) => [key.toString("hex"), output])),
          {
            nowCardanoSlotNo: 100n,
            bucketConcurrency: 1,
            l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
          },
        ),
      );
      expect(phaseB.accepted).toHaveLength(0);
      expect(phaseB.rejected[0]?.subject).toStrictEqual({
        arm: "UnusedScriptWitness",
        index: 0n,
      });

      const unchangedRoot = Buffer.alloc(32, 0x2e).toString("hex");
      const trace = await Effect.runPromise(
        buildDeterministicValidationMachineTrace({
          l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
          ...context,
          transactionId: transaction.txId,
          canonicalTransactionCbor: transaction.txCbor,
          priorUtxosRoot: unchangedRoot,
          postUtxosRoot: unchangedRoot,
          ledgerWitnessEntries: state.map(([outRef, output]) => ({
            outRef,
            output,
          })),
          expectedLedgerOps: [],
          ledgerMutationSteps: [],
          expectedVerdict: "rejected",
          expectedRejectionCode: RejectCodes.InvalidFieldType,
        }),
      );
      const visits = trace.witnesses.flatMap((witness) =>
        witness.phase === "scriptSources" &&
        scriptSourcesStage(witness.cbor) === 9 &&
        witness.auxiliary?.kind === "scriptSourceScan"
          ? [witness.auxiliary]
          : [],
      );
      // The inline witness sits at physical index 0; the lookup's first visit
      // is the lower resolved output, at physical index 1, and matches.
      expect(
        visits.map((visit) => ({
          sourceIndex: visit.sourceIndex,
          originKind: visit.originKind,
          sourceKey: Buffer.from(visit.sourceKey).toString("hex"),
        })),
      ).toStrictEqual([
        {
          sourceIndex: 1,
          originKind: "reference",
          sourceKey: winner.toString("hex"),
        },
      ]);
    },
  );

  it("admits a fixed duplicate vector to the root the on-chain map computes", async () => {
    // The same vector, in the same scan order, is admitted on-chain by
    // `direct_source_order_vector_matches_the_off_chain_root` in
    // source-index.test.ak, which pins this root.
    const source = (
      field_index: bigint,
      item_index: bigint,
      source_key: Buffer,
    ): { descriptor: SDK.DirectScriptSource; item: Buffer } => ({
      item: Buffer.alloc(0),
      descriptor: {
        field_index,
        item_index,
        source_key: source_key.toString("hex"),
        language_tag: 0n,
        script_hash: "22".repeat(28),
        total_length: 4n,
        item_commitment: "44".repeat(32),
      },
    });
    const outRef = (byte: number, outputIndex: number) =>
      encodeMidgardSpendInputItem({
        txId: Buffer.alloc(32, byte),
        outputIndex,
      });
    const vector = [
      source(0n, 0n, outRef(0x55, 3)),
      source(1n, 0n, outRef(0x33, 9)),
      source(6n, 0n, encodeCborUnsigned(0n)),
      source(6n, 1n, encodeCborUnsigned(1n)),
    ];
    const built = await buildDirectSourceIndex(vector);
    expect(built.index).toStrictEqual({
      root: "55f9bdabe4400653437d3a254a210a6823b120de719d2b96ca0f36c030af1661",
      count: 1n,
    });
    expect([...built.selected.values()].map((s) => s.descriptor)).toEqual([
      vector[1]!.descriptor,
    ]);
  });

  it("selects a spent-output Plutus source without throwing while admission refuses it with a typed verdict", async () => {
    const program = buildMidgardCanonicalCekProgram(
      Buffer.from("010100200101", "hex"),
    );
    const plutus: MidgardVersionedScript = plutusV3ScriptWitness(
      program.envelopeCbor,
    );
    const plutusHash = hashScriptWitness(plutus);
    const spent = outRefFromByte(0x81);
    const locked = encodeMidgardTxOutput({
      address: protectMidgardAddress(
        Buffer.from(
          CML.EnterpriseAddress.new(
            0,
            CML.Credential.new_script(CML.ScriptHash.from_hex(plutusHash)),
          )
            .to_address()
            .to_raw_bytes(),
        ),
      ),
      value: { lovelace: FUNDED_OUTPUT_LOVELACE, assets: new Map() },
      script_ref: plutus,
    });
    const slot = 200_000n;
    const transaction = makeNativeTx({
      spendInputs: [spent],
      outputs: [makeOutput(FUNDED_OUTPUT_LOVELACE)],
      validityIntervalStart: slot,
      redeemerTxWitsPreimageCbor: makeRedeemersCbor([
        {
          tag: MidgardRedeemerTag.Spend,
          index: 0n,
          exUnits: [10_000_000n, 10_000_000_000n],
        },
      ]),
      scriptLanguages: ["PlutusV3"],
    });

    const indexed = await buildDirectSourceIndexEvidence(
      await directMaterial(transaction.txId, transaction.txCbor, [
        [spent, locked],
      ]),
    );
    expect((await indexed.lookup(plutusHash)).selected).toMatchObject({
      field_index: 0n,
      source_key: spent.toString("hex"),
      language_tag: 3n,
    });

    const phaseA = validatePhaseASingle(
      {
        ...makeQueued(transaction.txId, transaction.txCbor),
        programMaterialSidecarCbor: encodeMidgardCekProgramMaterialSidecar([]),
      },
      {
        expectedNetworkId: 0n,
        minFeeA: 0n,
        minFeeB: 0n,
        concurrency: 1,
        strictnessProfile: "phase-a-unit",
      },
    );
    if (!("ledgerTx" in phaseA)) throw new Error("Phase A refused the row");
    const phaseB = await Effect.runPromise(
      runPhaseBValidationWithPatch(
        [phaseA],
        new Map([[spent.toString("hex"), locked]]),
        {
          nowCardanoSlotNo: slot + 100n,
          bucketConcurrency: 1,
          l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
        },
      ),
    );
    expect(phaseB.rejected[0]).toMatchObject({
      code: RejectCodes.PlutusScriptInvalid,
      subject: {
        arm: "PlutusExecutionFailed",
        purposeKind: 0n,
        purposeIndex: 0n,
      },
    });
  });
});
