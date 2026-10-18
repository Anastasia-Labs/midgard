import { createHash } from "node:crypto";
import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

import {
  decodeMidgardCekProgramMaterialSidecar,
  encodeMidgardCekProgramEnvelope,
  encodeMidgardCekProgramMaterialSidecar,
  encodeMidgardCekTermNode,
  hashMidgardCekTermNode,
} from "@al-ft/midgard-core/cek-proof";
import {
  decodeMidgardFieldPreimage,
  encodeMidgardFieldPreimage,
  encodeMidgardForcedTxCanonical,
  encodeMidgardTxOutput,
  materializeMidgardForcedTxFromCanonical,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardForcedTxCanonical,
} from "@al-ft/midgard-core/codec";
import { encodeCbor, readCborBytes } from "@al-ft/midgard-core/codec/cbor";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { screenMidgardForcedTxAdmission } from "@al-ft/midgard-core/consensus-validation";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Cause, Effect, Exit } from "effect";
import { describe, expect, it } from "vitest";

import { FUNDED_OUTPUT_LOVELACE } from "../../midgard-validation/tests/validation-fixtures.js";
import { ForcedTransactionsDB } from "../src/database/index.js";
import {
  buildPlaceholderValidationTraceMembers,
  classifyForcedTransactions,
} from "../src/mpf/index.js";
import {
  makeOutput,
  TEST_ADDRESS,
} from "./forced-transactions.make-signed-effectful-transaction.js";

/**
 * Forced verdict totality: every order the tx-order mint admits gets a forced
 * leaf. The corpus is the admitted rows of the `forced-tx-admission-v1`
 * channel, mutated item by item (outputs, inline datums, native script
 * payloads, and every other field) and kept only when the mint's off-chain
 * twin (`screenMidgardForcedTxAdmission`) still admits it. Ingest encoding,
 * classification and the placeholder descriptor must all succeed on each.
 */

type Row = {
  readonly name: string;
  readonly expected: string | null;
  readonly fields: readonly string[];
  readonly validityIntervalEnd: string;
  readonly scriptIntegrityHash: string;
  readonly auxiliaryDataHash: string;
  readonly networkId: string;
};

const golden = JSON.parse(
  readFileSync(
    fileURLToPath(
      new URL(
        "../../midgard-core/tests/fixtures/forced-tx-admission-v1.generated.json",
        import.meta.url,
      ),
    ),
    "utf8",
  ),
) as { readonly rows: readonly Row[] };

const SAMPLES = 1_200;

const random = (() => {
  let seed = 0x2b2b2b2b;
  return () => {
    seed ^= seed << 13;
    seed >>>= 0;
    seed ^= seed >>> 17;
    seed ^= seed << 5;
    seed >>>= 0;
    return seed / 2 ** 32;
  };
})();
const below = (n: number) => Math.floor(random() * n);
const bytes = (n: number) =>
  Buffer.from(Array.from({ length: n }, () => below(256)));

// Inline datum payloads: canonical Data, non-canonical encodings of the same
// values, and bytes that are not Data at all.
const DATUMS = [
  "d8799f00ff",
  "00",
  "4100",
  "d87980",
  "9f0102ff",
  "a10102",
  "820102",
  "1817",
  "5f4100ff",
  "d8799f00",
  "ff",
  "9f9f9f9f00ffffffff",
  "a20000",
  "c249010000000000000000",
];

const mutateBytes = (original: Buffer): Buffer => {
  if (original.length === 0) return bytes(1 + below(4));
  const out = Buffer.from(original);
  const at = below(out.length);
  switch (below(5)) {
    case 0:
      out[at] = out[at]! ^ (1 << below(8));
      return out;
    case 1:
      out[at] = below(256);
      return out;
    case 2:
      return Buffer.concat([
        out.subarray(0, at),
        Buffer.from([below(256)]),
        out.subarray(at),
      ]);
    case 3:
      return Buffer.concat([out.subarray(0, at), out.subarray(at + 1)]);
    default:
      return out.subarray(0, below(out.length + 1));
  }
};

/** A native script witness `[0, payload]` around arbitrary payload bytes. */
const nativeWitness = (payload: Buffer): Buffer =>
  Buffer.concat([Buffer.from([0x82, 0x00]), encodeCbor(payload)]);

/** A two-entry output (address, value) given `datum` as its inline datum. */
const withDatum = (output: Buffer, datum: Buffer): Buffer | null =>
  output[0] === 0xa2
    ? Buffer.concat([
        Buffer.from([0xa3]),
        output.subarray(1),
        Buffer.from([0x02]),
        encodeCbor(datum),
      ])
    : null;

const mutateField = (index: number, preimage: Buffer): Buffer => {
  let items: Buffer[];
  try {
    items = decodeMidgardFieldPreimage(preimage).map((item) =>
      Buffer.from(item),
    );
  } catch {
    return preimage;
  }
  const choice = below(4);
  if (index === 6 && (items.length === 0 || choice === 0)) {
    items.push(nativeWitness(bytes(below(40))));
  } else if (index === 2 && items.length > 0 && choice <= 1) {
    const at = below(items.length);
    const datum = Buffer.from(DATUMS[below(DATUMS.length)]!, "hex");
    const replaced = withDatum(items[at]!, datum);
    if (replaced !== null) items[at] = replaced;
    else items[at] = mutateBytes(items[at]!);
  } else if (index === 6 && items.length > 0 && choice === 1) {
    const at = below(items.length);
    const item = items[at]!;
    // Keep the native header; mutate the payload only.
    let payload: Buffer;
    try {
      payload = Buffer.from(readCborBytes(item, 2, "payload").value);
    } catch {
      payload = Buffer.from(item.subarray(2));
    }
    items[at] = nativeWitness(mutateBytes(payload));
  } else if (items.length > 0) {
    const at = below(items.length);
    items[at] = mutateBytes(items[at]!);
  } else {
    return preimage;
  }
  return encodeMidgardFieldPreimage(items);
};

const transactionOf = (row: Row): MidgardForcedTxCanonical => {
  const field = (index: number) => Buffer.from(row.fields[index]!, "hex");
  return {
    version: MIDGARD_NATIVE_TX_VERSION,
    body: {
      spendInputsPreimageCbor: field(0),
      referenceInputsPreimageCbor: field(1),
      outputsPreimageCbor: field(2),
      fee: 0n,
      validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
      validityIntervalEnd: BigInt(row.validityIntervalEnd),
      requiredObserversPreimageCbor: field(3),
      requiredSignersPreimageCbor: field(4),
      mintPreimageCbor: field(5),
      scriptIntegrityHash: Buffer.from(row.scriptIntegrityHash, "hex"),
      auxiliaryDataHash: Buffer.from(row.auxiliaryDataHash, "hex"),
      networkId: BigInt(row.networkId),
    },
    witnessSet: {
      scriptTxWitsPreimageCbor: field(6),
      addrTxWitsPreimageCbor: field(7),
      redeemerTxWitsPreimageCbor: field(8),
    },
  };
};

const FIELD_KEYS = [
  ["body", "spendInputsPreimageCbor"],
  ["body", "referenceInputsPreimageCbor"],
  ["body", "outputsPreimageCbor"],
  ["body", "requiredObserversPreimageCbor"],
  ["body", "requiredSignersPreimageCbor"],
  ["body", "mintPreimageCbor"],
  ["witnessSet", "scriptTxWitsPreimageCbor"],
  ["witnessSet", "addrTxWitsPreimageCbor"],
  ["witnessSet", "redeemerTxWitsPreimageCbor"],
] as const;

const mutate = (tx: MidgardForcedTxCanonical): MidgardForcedTxCanonical => {
  const body = { ...tx.body };
  const witnessSet = { ...tx.witnessSet };
  const parts = { body, witnessSet } as Record<
    "body" | "witnessSet",
    Record<string, unknown>
  >;
  for (let count = 1 + below(3); count > 0; count -= 1) {
    // Outputs and script witnesses carry the reachable faults; weight them.
    const index = below(3) === 0 ? below(9) : below(2) === 0 ? 2 : 6;
    const [part, key] = FIELD_KEYS[index]!;
    parts[part][key] = mutateField(index, parts[part][key] as Buffer);
  }
  return { ...tx, body, witnessSet };
};

const admitted = (tx: MidgardForcedTxCanonical): Buffer | null => {
  if (
    screenMidgardForcedTxAdmission({
      body: tx.body,
      witnessSet: tx.witnessSet,
    }) !== null
  )
    return null;
  try {
    return encodeMidgardForcedTxCanonical(
      materializeMidgardForcedTxFromCanonical(tx),
    );
  } catch {
    // Not a §5.1 envelope: the mint's whole-view door refuses it.
    return null;
  }
};

const sidecar = encodeMidgardCekProgramMaterialSidecar([]);

/** An L2 output carrying a script reference whose material no one retains. */
const scriptRefOutput = (() => {
  const terminal = { kind: "error" } as const;
  const termPreimage = encodeMidgardCekTermNode(terminal);
  return encodeMidgardTxOutput({
    address: TEST_ADDRESS,
    value: { lovelace: FUNDED_OUTPUT_LOVELACE, assets: new Map() },
    script_ref: {
      language: "MidgardV1",
      scriptBytes: encodeMidgardCekProgramEnvelope({
        uplcVersion: [1n, 1n, 0n],
        termRoot: hashMidgardCekTermNode(terminal),
        nodeCount: 1n,
        materialByteLength: BigInt(termPreimage.length),
      }),
    },
  });
})();

const itemsOf = (preimage: Buffer): readonly Buffer[] => {
  try {
    return decodeMidgardFieldPreimage(preimage).map((item) =>
      Buffer.from(item),
    );
  } catch {
    return [];
  }
};

/**
 * The ledger before `tx` with every input live: spent inputs hold plain
 * outputs, and every reference input holds a script reference, so the
 * classifier reaches reference resolution with no program material.
 */
const liveInputsOf = (tx: MidgardForcedTxCanonical): Map<string, Buffer> =>
  new Map([
    ...itemsOf(tx.body.spendInputsPreimageCbor).map(
      (outRef) =>
        [outRef.toString("hex"), makeOutput(FUNDED_OUTPUT_LOVELACE)] as const,
    ),
    ...itemsOf(tx.body.referenceInputsPreimageCbor).map(
      (outRef) => [outRef.toString("hex"), scriptRefOutput] as const,
    ),
  ]);

const entryOf = (nativeTxCbor: Buffer, label: number) =>
  ForcedTransactionsDB.encodeForcedInclusionValueV1({
    nativeTxCbor,
    verdict: "ForcedTxValid",
    consensusProfile: MIDGARD_CONSENSUS_PROFILE,
  }).pipe(
    Effect.map(
      (encoded): ForcedTransactionsDB.Entry => ({
        [ForcedTransactionsDB.Columns.TX_ORDER_ID]: Buffer.from(
          Data.to(
            {
              transactionId: Buffer.alloc(32, label % 256).toString("hex"),
              outputIndex: BigInt(label),
            },
            SDK.OutputReference,
          ),
          "hex",
        ),
        [ForcedTransactionsDB.Columns.TX_ORDER_L1_TX_HASH]: Buffer.alloc(
          32,
          label % 256,
        ),
        [ForcedTransactionsDB.Columns.TX_ORDER_L1_OUTPUT_INDEX]: label,
        [ForcedTransactionsDB.Columns.ASSET_NAME]: Buffer.from([label % 256]),
        [ForcedTransactionsDB.Columns.RAW_DATUM]: Buffer.from([label % 256]),
        [ForcedTransactionsDB.Columns.TX_ID]: encoded.txId,
        [ForcedTransactionsDB.Columns.TX_COMPACT]: encoded.txCompact,
        [ForcedTransactionsDB.Columns.FORCED_INCLUSION_VALUE]: encoded.value,
        [ForcedTransactionsDB.Columns.CONSENSUS_PROFILE_ID]:
          MIDGARD_CONSENSUS_PROFILE.profileId,
        [ForcedTransactionsDB.Columns.NATIVE_TX_CBOR]: nativeTxCbor,
        [ForcedTransactionsDB.Columns.TRANSACTION_COMMITMENT]:
          encoded.transactionCommitment,
        [ForcedTransactionsDB.Columns.CEK_PROGRAM_MATERIAL_SIDECAR_CBOR]:
          sidecar,
        [ForcedTransactionsDB.Columns.CEK_PROGRAM_MATERIAL_SIDECAR_SHA256]:
          createHash("sha256").update(sidecar).digest(),
        [ForcedTransactionsDB.Columns.INCLUSION_TIME]: new Date(
          "2026-07-23T12:00:00.000Z",
        ),
        [ForcedTransactionsDB.Columns.PROJECTED_HEADER_HASH]: null,
        [ForcedTransactionsDB.Columns.STATUS]:
          ForcedTransactionsDB.Status.Awaiting,
      }),
    ),
  );

/** Ingest, classify and describe one admitted order: its leaf's reason arm. */
const leafOf = (
  nativeTxCbor: Buffer,
  label: number,
  initialState: Map<string, Buffer> = new Map(),
) =>
  Effect.gen(function* () {
    const entry = yield* entryOf(nativeTxCbor, label);
    const [classified] = yield* classifyForcedTransactions({
      entries: [entry],
      initialState,
      blockTime: new Date("2026-07-23T12:01:00.000Z"),
      consensusProfile: MIDGARD_CONSENSUS_PROFILE,
      validation: {
        l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
        expectedNetworkId: 0n,
        minFeeA: 0n,
        minFeeB: 0n,
        bucketConcurrency: 1,
        slotForUnixTime: () => 100n,
      },
    });
    const leaf = Data.from(
      classified!.entry[
        ForcedTransactionsDB.Columns.FORCED_INCLUSION_VALUE
      ].toString("hex"),
      SDK.ForcedInclusionTxV1,
    );
    // A forced transaction never commits program material.
    if (
      decodeMidgardCekProgramMaterialSidecar(
        classified!.programMaterialSidecarCbor,
      ).length !== 0
    )
      return yield* Effect.die("forced transaction committed program material");
    yield* buildPlaceholderValidationTraceMembers({
      transactions: [
        {
          eventKey: {
            ForcedTransactionEventKey: {
              tx_order_id: Data.from(
                entry[ForcedTransactionsDB.Columns.TX_ORDER_ID].toString("hex"),
                SDK.OutputReference,
              ),
            },
          },
          canonicalTransactionCbor: nativeTxCbor,
          sourceKind: "forced",
          verdict: leaf.verdict === "ForcedTxValid" ? "accepted" : "rejected",
          rejectionReason: classified!.rejectionReason,
        },
      ],
    });
    return leaf.verdict === "ForcedTxValid"
      ? "ForcedTxValid"
      : (() => {
          const reason = leaf.verdict.ForcedTxInvalid.reason;
          return typeof reason === "string" ? reason : Object.keys(reason)[0]!;
        })();
  });

describe("forced verdict totality over mint-admissible orders", () => {
  it("gives every admitted order a leaf, with no hold and no defect", async () => {
    const bases = golden.rows
      .filter(({ expected }) => expected === null)
      .map(transactionOf);
    expect(bases.length).toBeGreaterThan(10);
    const residue: string[] = [];
    let checked = 0;
    for (let sample = 0; sample < SAMPLES; sample += 1) {
      const base = bases[sample % bases.length]!;
      const candidate = sample < bases.length ? base : mutate(base);
      const nativeTxCbor = admitted(candidate);
      if (nativeTxCbor === null) continue;
      checked += 1;
      const exit = await Effect.runPromiseExit(leafOf(nativeTxCbor, sample));
      if (!Exit.isSuccess(exit))
        residue.push(
          `${nativeTxCbor.toString("hex")}: ${Cause.pretty(exit.cause).split("\n")[0]}`,
        );
    }
    expect(residue).toEqual([]);
    expect(checked).toBeGreaterThan(SAMPLES / 4);
  }, 600_000);

  it("gives a leaf when every input is live and every reference input carries an unretained script reference", async () => {
    const bases = golden.rows
      .filter(({ expected }) => expected === null)
      .map(transactionOf);
    const residue: string[] = [];
    let referencing = 0;
    for (let sample = 0; sample < SAMPLES / 4; sample += 1) {
      const base = bases[sample % bases.length]!;
      const candidate = sample < bases.length ? base : mutate(base);
      const nativeTxCbor = admitted(candidate);
      if (nativeTxCbor === null) continue;
      const state = liveInputsOf(candidate);
      if ([...state.values()].includes(scriptRefOutput)) referencing += 1;
      const exit = await Effect.runPromiseExit(
        leafOf(nativeTxCbor, sample, state),
      );
      if (!Exit.isSuccess(exit))
        residue.push(
          `${nativeTxCbor.toString("hex")}: ${Cause.pretty(exit.cause).split("\n")[0]}`,
        );
    }
    expect(residue).toEqual([]);
    expect(referencing).toBeGreaterThan(0);
  }, 600_000);
});
