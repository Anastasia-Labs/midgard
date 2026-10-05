import "@al-ft/midgard-core/cek-proof";
import "@al-ft/midgard-core/codec";
import "@al-ft/midgard-core/codec/cbor";
import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "vitest";
import "../src/da/payload.js";
import "./helpers.js";

import {
  encodeMidgardCekProgramEnvelope,
  encodeMidgardCekProgramMaterialDaValue,
  encodeMidgardCekTermNode,
  hashMidgardCekTermNode,
} from "@al-ft/midgard-core/cek-proof";
import {
  computeMidgardNativeTxId,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeMidgardNativeTxCanonical,
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
  materializeMidgardNativeTxFromCanonical,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
} from "@al-ft/midgard-core/codec";
import { encodeCbor } from "@al-ft/midgard-core/codec/cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { Data as LucidData } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  DaPayloadValidationError,
  type PreBlockUtxos,
  validateDaPayloadEventProgramCoverage,
} from "../src/da/payload.js";
import { makePayloadFixture } from "./helpers.js";

const ADDRESS = Buffer.concat([Buffer.from([0x60]), Buffer.alloc(28, 9)]);

const terminal = { kind: "error" } as const;
const termPreimage = encodeMidgardCekTermNode(terminal);
const termRoot = hashMidgardCekTermNode(terminal);
const envelope = {
  uplcVersion: [1n, 1n, 0n] as const,
  termRoot,
  nodeCount: 1n,
  materialByteLength: BigInt(termPreimage.length),
};
const material: SDK.DaPayloadEntry = [
  termRoot.toString("hex"),
  encodeMidgardCekProgramMaterialDaValue({
    kind: "term",
    preimage: termPreimage,
  }).toString("hex"),
];

const plainOutput = (lovelace: bigint): Buffer =>
  encodeMidgardTxOutput({
    address: ADDRESS,
    value: { lovelace, assets: new Map() },
  });

/** An output carrying the MidgardV1 program `envelope` as its script_ref. */
const scriptRefOutput = encodeMidgardTxOutput({
  address: ADDRESS,
  value: { lovelace: 2_000_000n, assets: new Map() },
  script_ref: {
    language: "MidgardV1",
    scriptBytes: encodeMidgardCekProgramEnvelope(envelope),
  },
});

const outRefItem = (seed: number, outputIndex = 0): Buffer =>
  encodeMidgardSpendInputItem({ txId: Buffer.alloc(32, seed), outputIndex });

const byteList = (items: readonly Uint8Array[]): Buffer =>
  items.length === 0
    ? Buffer.from(EMPTY_CBOR_LIST)
    : encodeCbor(items.map((item) => Buffer.from(item)));

const nativeTx = ({
  spend,
  reference = [],
  outputs = [plainOutput(1_000_000n)],
}: {
  readonly spend: readonly Buffer[];
  readonly reference?: readonly Buffer[];
  readonly outputs?: readonly Buffer[];
}) => {
  const tx = materializeMidgardNativeTxFromCanonical({
    version: MIDGARD_NATIVE_TX_VERSION,
    validity: "TxIsValid",
    body: {
      spendInputsPreimageCbor: byteList(spend),
      referenceInputsPreimageCbor: byteList(reference),
      outputsPreimageCbor: byteList(outputs),
      fee: 0n,
      validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
      validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
      requiredObserversPreimageCbor: EMPTY_CBOR_LIST,
      requiredSignersPreimageCbor: EMPTY_CBOR_LIST,
      mintPreimageCbor: EMPTY_CBOR_LIST,
      scriptIntegrityHash: EMPTY_NULL_ROOT,
      auxiliaryDataHash: EMPTY_NULL_ROOT,
      networkId: MIDGARD_NATIVE_NETWORK_ID_NONE,
    },
    witnessSet: {
      addrTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      scriptTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    },
  });
  const txId = computeMidgardNativeTxId(tx);
  return {
    txId: txId.toString("hex"),
    txCbor: encodeMidgardNativeTxCanonical(tx).toString("hex"),
    output: (outputIndex: number) =>
      encodeMidgardSpendInputItem({ txId, outputIndex }),
  };
};

type NativeTx = ReturnType<typeof nativeTx>;

const sorted = (entries: SDK.DaPayloadEntry[]): SDK.DaPayloadEntry[] =>
  entries.sort(([left], [right]) => (left < right ? -1 : 1));

/** A block body holding `ordered` as its normal transactions, applied in
 * that order (their `event_to_step` steps), with `programMaterial`. */
const bodyOf = async (
  ordered: readonly NativeTx[],
  programMaterial: readonly SDK.DaPayloadEntry[] = [material],
): Promise<SDK.DaPayloadBody> => {
  const base = (await makePayloadFixture(1)).payload.block_body;
  return {
    ...base,
    transactions: sorted(ordered.map(({ txId }) => [txId, "00"])),
    transaction_preimages: sorted(
      ordered.map(({ txId, txCbor }) => [txId, txCbor]),
    ),
    event_to_step: sorted(
      ordered.map(({ txId }, index) => [
        LucidData.to(
          { L2TransactionEventKey: { tx_id: txId } } as never,
          SDK.EventKeySchema as never,
        ),
        LucidData.to(
          {
            step_index: BigInt(index),
            phase: "L2Transaction",
          } satisfies SDK.EventToStepValue as never,
          SDK.EventToStepValueSchema as never,
        ),
      ]),
    ),
    cek_program_material: [...programMaterial],
  };
};

const replay = async (
  ordered: readonly NativeTx[],
  preBlockUtxos: PreBlockUtxos,
  programMaterial?: readonly SDK.DaPayloadEntry[],
): Promise<unknown> => {
  const body = await bodyOf(ordered, programMaterial);
  try {
    validateDaPayloadEventProgramCoverage(body, preBlockUtxos);
    return undefined;
  } catch (error) {
    return error;
  }
};

const absentAtPosition = expect.objectContaining({
  code: "malformed_transaction",
  message: expect.stringMatching(
    /is absent from the state immediately before the transaction/u,
  ),
});

/**
 * Every spent and reference input resolves against the ledger state
 * immediately before its own transaction, as on Cardano, and a transaction's
 * program set is taken at that position.
 */
describe("DA committee replay of normal transactions at their positions", () => {
  const x = outRefItem(1);
  const a = outRefItem(2);
  const b = outRefItem(3);
  const preState: PreBlockUtxos = [
    [x.toString("hex"), scriptRefOutput],
    [a.toString("hex"), plainOutput(2_000_000n)],
    [b.toString("hex"), plainOutput(2_000_000n)],
  ];

  it("attests a referencer of X before a later spender of X, from the state before the block", async () => {
    const referencer = nativeTx({ spend: [a], reference: [x] });
    const spender = nativeTx({ spend: [x] });

    await expect(replay([referencer, spender], preState)).resolves.toBe(
      undefined,
    );
    // The referencer's program is part of the block's program set.
    await expect(
      replay([referencer, spender], preState, []),
    ).resolves.toMatchObject({ code: "coverage_mismatch" });
  });

  it("refuses a referencer applied after the spender of its reference", async () => {
    const referencer = nativeTx({ spend: [a], reference: [x] });
    const spender = nativeTx({ spend: [x] });

    const refusal = await replay([spender, referencer], preState);
    expect(refusal).toBeInstanceOf(DaPayloadValidationError);
    expect(refusal).toEqual(absentAtPosition);
  });

  it("resolves a reference to an output produced earlier in the block", async () => {
    const producer = nativeTx({ spend: [b], outputs: [scriptRefOutput] });
    const referencer = nativeTx({
      spend: [a],
      reference: [producer.output(0)],
    });

    await expect(replay([producer, referencer], preState)).resolves.toBe(
      undefined,
    );
    const refusal = await replay([referencer, producer], preState);
    expect(refusal).toBeInstanceOf(DaPayloadValidationError);
    expect(refusal).toEqual(absentAtPosition);
  });
});
