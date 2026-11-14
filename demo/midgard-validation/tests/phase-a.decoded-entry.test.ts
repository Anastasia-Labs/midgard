import { ledgerEd25519SignatureReady } from "@al-ft/midgard-core";
import { decodeMidgardFieldPreimage } from "@al-ft/midgard-core/codec";
import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core/consensus-profile";
import { CML } from "@lucid-evolution/lucid";
import { beforeAll, describe, expect, it } from "vitest";

import {
  decodeMidgardSubmittedNativeTxFromCanonicalCbor,
  type PhaseAConfig,
  type QueuedTx,
  validatePhaseASingle,
  validatePhaseASingleDecoded,
} from "../src/index.js";
import { validatePhaseASingleFromBytes } from "../src/phase-a.validate-phase-asingle.js";
import { ASSET_ENTRY_ROWS } from "./emission-order.asset-entries.rows.js";
import {
  FORCED_OUTPUT_ROWS,
  FORCED_ROWS,
} from "./emission-order.forced.rows.js";
import { NATIVE_ROWS } from "./emission-order.native.rows.js";
import { FULL, PLAIN } from "./emission-order.resolve-inputs.rows.js";
import { ORDER_ROWS } from "./emission-order.rows.js";
import { transactionOf } from "./emission-order.support.js";
import {
  encodeByteList,
  encodeRecomputedNativeTx,
  FUNDED_OUTPUT_LOVELACE,
  makeMinAdaFundedExactSizeOutputItem,
  makeNativeTx,
  makeOutput,
  makeQueued,
  outRefFromByte,
} from "./validation-fixtures.js";

/**
 * `validatePhaseASingleDecoded` is Phase A on a normal tx the caller already
 * decoded, and `validatePhaseASingle` takes it for every normal tx that
 * decodes. Both must return exactly what `validatePhaseASingleFromBytes`, the
 * path that derives everything from the bytes, returns for the same bytes:
 * same verdict, code, detail, phase and subject, or the same validated
 * candidate.
 */

beforeAll(() => ledgerEd25519SignatureReady());

const [low, high] = [outRefFromByte(0x31), outRefFromByte(0x32)];
const outputs = [makeOutput(FUNDED_OUTPUT_LOVELACE)];
const secondSigner = CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 0x05));
const twoSigners = makeNativeTx({
  spendInputs: [low],
  outputs,
  extraVkeyWitnessKeys: [secondSigner],
});
const witnessItems = decodeMidgardFieldPreimage(
  twoSigners.tx.witnessSet.addrTxWitsPreimageCbor,
);
const withWitnesses = (items: readonly Buffer[]) =>
  encodeRecomputedNativeTx({
    ...twoSigners.tx,
    witnessSet: {
      ...twoSigners.tx.witnessSet,
      addrTxWitsPreimageCbor: encodeByteList(items),
    },
  });

type Case = {
  readonly name: string;
  readonly queued: QueuedTx;
};

const fixtureCase = (
  name: string,
  fixture: { readonly txId: Buffer; readonly txCbor: Buffer },
  txId = fixture.txId,
): Case => ({ name, queued: makeQueued(txId, fixture.txCbor) });

/** One tx per Phase A refusal class a submitter can send, plus valid ones. */
const handBuiltCases: readonly Case[] = [
  fixtureCase("valid, no fee", makeNativeTx({ outputs })),
  fixtureCase(
    "valid, fee over the ingress minimum",
    makeNativeTx({ outputs, fee: 1_000_000n }),
  ),
  fixtureCase("valid, two signers", twoSigners),
  fixtureCase(
    "valid, testnet network id",
    makeNativeTx({ outputs, networkId: 0n }),
  ),
  fixtureCase(
    "valid, long expired",
    makeNativeTx({ outputs, validityIntervalEnd: 1n }),
  ),
  fixtureCase(
    "bad signature",
    makeNativeTx({ outputs, invalidVkeyWitness: true }),
  ),
  fixtureCase("no witness", makeNativeTx({ outputs, omitVkeyWitness: true })),
  fixtureCase("wrong network", makeNativeTx({ outputs, networkId: 1n })),
  fixtureCase(
    "validity start after end",
    makeNativeTx({
      outputs,
      validityIntervalStart: 10n,
      validityIntervalEnd: 5n,
    }),
  ),
  fixtureCase("no spend inputs", makeNativeTx({ outputs, spendInputs: [] })),
  fixtureCase(
    "descending spend inputs",
    makeNativeTx({ outputs, spendInputs: [high, low] }),
  ),
  fixtureCase(
    "descending reference inputs",
    makeNativeTx({ outputs, referenceInputs: [high, low] }),
  ),
  fixtureCase(
    "repeated spend input",
    makeNativeTx({ outputs, spendInputs: [low, low] }),
  ),
  fixtureCase(
    "spend input also a reference input",
    makeNativeTx({ outputs, spendInputs: [low], referenceInputs: [low] }),
  ),
  fixtureCase(
    "descending address witnesses",
    withWitnesses([witnessItems[1]!, witnessItems[0]!]),
  ),
  fixtureCase(
    "repeated address witness",
    withWitnesses([witnessItems[0]!, witnessItems[0]!]),
  ),
  fixtureCase(
    "output past the canonical width bound",
    makeNativeTx({
      outputs: [
        makeMinAdaFundedExactSizeOutputItem(
          MIDGARD_CONSENSUS_LIMITS.maxLedgerOutputPreimageBytes + 1,
        ),
      ],
    }),
  ),
  fixtureCase(
    "required signer without a witness",
    makeNativeTx({ outputs, requiredSignerItems: [Buffer.alloc(28, 0x09)] }),
  ),
  fixtureCase(
    "auxiliary data hash",
    makeNativeTx({ outputs, auxiliaryDataHash: Buffer.alloc(32, 0x01) }),
  ),
  fixtureCase(
    "is_valid false",
    makeNativeTx({ outputs, validity: "TxIsInvalid" }),
  ),
  fixtureCase(
    "queued id is not the tx's",
    makeNativeTx({ outputs }),
    Buffer.alloc(32, 0xee),
  ),
];

const ROWS = [
  ...ORDER_ROWS,
  ...NATIVE_ROWS,
  ...ASSET_ENTRY_ROWS,
  ...FULL,
  ...PLAIN,
  ...FORCED_ROWS,
  ...FORCED_OUTPUT_ROWS,
];

/** Every normal tx of the emission-order corpora (Phase A and B faults). */
const rowCases: readonly Case[] = ROWS.filter(
  (row) => row.source !== "forced",
).map((row) => fixtureCase(`row ${row.name}`, transactionOf(row)));

/** Every forced tx of the same corpora, which never takes the decoded path. */
const forcedRowCases: readonly Case[] = ROWS.filter(
  (row) => row.source === "forced",
).map((row) => {
  const { txId, txCbor } = transactionOf(row);
  return {
    name: `forced row ${row.name}`,
    queued: { ...makeQueued(txId, txCbor), sourceKind: "forced" },
  };
});

/** Bytes that do not decode, each refused by the bytes path. */
const valid = makeNativeTx({ outputs });
const undecodableCases: readonly Case[] = [
  fixtureCase("empty bytes", { ...valid, txCbor: Buffer.alloc(0) }),
  fixtureCase("an empty CBOR array", { ...valid, txCbor: Buffer.from([0x80]) }),
  fixtureCase("truncated by one byte", {
    ...valid,
    txCbor: valid.txCbor.subarray(0, -1),
  }),
  fixtureCase("one trailing byte", {
    ...valid,
    txCbor: Buffer.concat([valid.txCbor, Buffer.from([0x00])]),
  }),
  ...[8, Math.floor(valid.txCbor.length / 2), valid.txCbor.length - 4].map(
    (offset) => {
      const txCbor = Buffer.from(valid.txCbor);
      txCbor[offset] = txCbor[offset]! ^ 0x01;
      return fixtureCase(`byte ${offset.toString()} flipped`, {
        ...valid,
        txCbor,
      });
    },
  ),
];

const CONFIGS: readonly (readonly [string, PhaseAConfig])[] = [
  [
    "no fee floor",
    {
      expectedNetworkId: 0n,
      minFeeA: 0n,
      minFeeB: 0n,
      concurrency: 1,
      strictnessProfile: "phase1_midgard",
    },
  ],
  [
    "ingress fee floor",
    {
      expectedNetworkId: 0n,
      minFeeA: 1n,
      minFeeB: 2_000n,
      concurrency: 1,
      strictnessProfile: "phase1_midgard",
    },
  ],
];

const verdictLabel = (verdict: ReturnType<typeof validatePhaseASingle>) => {
  if (!("code" in verdict)) return "accepted";
  const subject = verdict.subject as
    | { readonly arm?: string; readonly fieldIndex?: bigint }
    | undefined;
  return subject?.arm === "FieldItemWidthIllegal"
    ? `${verdict.code}/FieldItemWidthIllegal/field${String(subject.fieldIndex)}`
    : verdict.code;
};

describe("Phase A on an already-decoded tx", () => {
  it("returns the bytes path's verdict for every tx that decodes", () => {
    const labels = new Set<string>();
    const undecodable: string[] = [];
    let compared = 0;
    for (const { name, queued } of [...handBuiltCases, ...rowCases]) {
      let decoded;
      try {
        decoded = decodeMidgardSubmittedNativeTxFromCanonicalCbor(
          queued.txCbor,
        );
      } catch {
        undecodable.push(name);
        continue;
      }
      for (const [configName, config] of CONFIGS) {
        const fromBytes = validatePhaseASingleFromBytes(queued, config);
        const fromDecoded = validatePhaseASingleDecoded(
          queued,
          decoded,
          config,
        );
        expect(fromDecoded, `${name} (${configName})`).toStrictEqual(fromBytes);
        labels.add(verdictLabel(fromBytes));
        compared += 1;
      }
    }
    // The comparison reaches every verdict the decoded path computes on its
    // own: the field-order scan (fields 0, 1, 7), the output width scan
    // (field 2) and the consensus check, besides the shared rules.
    expect([...labels].sort()).toEqual(
      expect.arrayContaining([
        "accepted",
        "E_AUX_DATA_FORBIDDEN",
        "E_INVALID_FIELD_TYPE/FieldItemWidthIllegal/field0",
        "E_INVALID_FIELD_TYPE/FieldItemWidthIllegal/field1",
        "E_INVALID_FIELD_TYPE/FieldItemWidthIllegal/field2",
        "E_INVALID_FIELD_TYPE/FieldItemWidthIllegal/field7",
        "E_INVALID_SIGNATURE",
        "E_IS_VALID_FALSE_FORBIDDEN",
        "E_MIN_FEE",
        "E_NETWORK_ID_MISMATCH",
        "E_TX_HASH_MISMATCH",
      ]),
    );
    expect(compared).toBeGreaterThan(2 * handBuiltCases.length);
    // A native witness that does not parse fails the ledger-stage decode, so
    // a caller refuses it before Phase A and the decoded path never sees it.
    expect(undecodable).toEqual(["row e_refusal_malformed_native_witness"]);
  });

  it("returns the bytes path's verdict from the bytes entry point for every tx", () => {
    const labels = new Set<string>();
    let compared = 0;
    for (const { name, queued } of [
      ...handBuiltCases,
      ...rowCases,
      ...forcedRowCases,
      ...undecodableCases,
    ]) {
      for (const [configName, config] of CONFIGS) {
        const fromBytes = validatePhaseASingleFromBytes(queued, config);
        expect(
          validatePhaseASingle(queued, config),
          `${name} (${configName})`,
        ).toStrictEqual(fromBytes);
        labels.add(verdictLabel(fromBytes));
        compared += 1;
      }
    }
    // Besides every verdict of the decoded path, the comparison reaches the
    // refusals of bytes that do not decode, at the canonical decode and at
    // the ledger-stage decode.
    expect([...labels].sort()).toEqual(
      expect.arrayContaining([
        "accepted",
        "E_CBOR_DESERIALIZATION",
        "E_INVALID_FIELD_TYPE",
        "E_INVALID_FIELD_TYPE/FieldItemWidthIllegal/field0",
        "E_INVALID_FIELD_TYPE/FieldItemWidthIllegal/field2",
        "E_INVALID_FIELD_TYPE/FieldItemWidthIllegal/field7",
        "E_INVALID_SIGNATURE",
        "E_MIN_FEE",
        "E_TX_HASH_MISMATCH",
      ]),
    );
    expect(forcedRowCases.length).toBeGreaterThan(0);
    expect(compared).toBe(
      CONFIGS.length *
        (handBuiltCases.length +
          rowCases.length +
          forcedRowCases.length +
          undecodableCases.length),
    );
  });

  it("refuses a forced transaction", () => {
    const fixture = makeNativeTx({ outputs });
    const decoded = decodeMidgardSubmittedNativeTxFromCanonicalCbor(
      fixture.txCbor,
    );
    expect(() =>
      validatePhaseASingleDecoded(
        { ...makeQueued(fixture.txId, fixture.txCbor), sourceKind: "forced" },
        decoded,
        CONFIGS[0]![1],
      ),
    ).toThrow("validatePhaseASingleDecoded takes normal transactions");
  });
});
