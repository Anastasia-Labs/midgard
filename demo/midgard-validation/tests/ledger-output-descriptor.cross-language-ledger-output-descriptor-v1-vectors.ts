import "./ledger-output-descriptor.canonical-ledger-output-descriptor-v1.js";

import {
  buildMidgardLedgerOutputProofTrace,
  decodeMidgardLedgerOutputCommitment,
  summarizeMidgardLedgerOutputCardanoSpendDatum,
  summarizeMidgardLedgerOutputCardanoTxOut,
  summarizeMidgardLedgerOutputMidgardTxOut,
  verifyMidgardLedgerOutputDescriptor,
} from "@al-ft/midgard-core";
import {
  decodeMidgardDatum,
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
} from "@al-ft/midgard-core/codec";
import { describe, expect, it } from "vitest";

import {
  buildCanonicalMidgardLedgerEntryOutputMaterial,
  buildCanonicalMidgardLedgerOutputMaterial,
  createCanonicalMidgardLedgerDescriptorResolver,
} from "../src/ledger-output-descriptor.js";
import { outputFixture } from "./ledger-output-descriptor.output-fixture.js";

// The cross-language descriptor vectors. Each triple below is pinned
// byte-for-byte in `onchain/aiken/lib/midgard/ledger-output-descriptor-v1.test.ak`
// as well, so the on-chain one-shot builder and this encoder cannot drift
// apart silently: whichever side moves, one of the two pinned sets goes red.
// This is the channel that matters for the transition-trace fraud proofs,
// which now derive the `utxos_root` value on chain and must derive exactly the
// value the node committed.
describe("cross-language ledger output descriptor V1 vectors", () => {
  it.each([
    {
      label: "minimal output, no datum and no reference script",
      outputIndex: 0,
      outputCbor:
        "a200581d7811111111111111111111111111111111111111111111111111111111018200a0",
      descriptorCbor:
        "90010018255820855089c279a2084237bfc980ad11c3cb72bd80055b3d89fdf9105faff6b9d3ec581d781111111111111111111111111111111111111111111111111111111100005820b6575c6c81264fc5d6802905bc4cb01d26fcca7c75412712fd4d4b7e5a23d6cd012040004083582068221e2117402083ee82606dd1c3296b6b06d69d71bd1417555a0f1848ab7f731834183c83582023656d4d955fd1968ba6d1d923f25fda39fe3bbc3c9a8e474b16c9340ab32a081834183c8358209525e1ea4350de9f831fc817b64355d7c3e26427effb7f4ca9bd29541d5eda390304",
    },
    {
      label: "inline datum only",
      outputIndex: 0,
      outputCbor:
        "a300581d7811111111111111111111111111111111111111111111111111111111018200a00243d87980",
      descriptorCbor:
        "900100182a5820831569d49cdf1d62af52a7ba84294373584f80bafa4b90aafb97dd9070ddbe41581d781111111111111111111111111111111111111111111111111111111100005820b6575c6c81264fc5d6802905bc4cb01d26fcca7c75412712fd4d4b7e5a23d6cd01204000408358200a2e786d1965b870e307b60f251e29f6653560d7dbf72fa5ece0c5c44bbfd03c18381840835820ad9e3605aefc6d2070c186504f4e16ec0afef0f6c1342f7f423fa47f569e93421838184083582029f8b517a22889be5795b8b91050155dbc935ce541d077fd7124de5af8dd664c0708",
    },
    {
      label: "PlutusV3 reference script",
      outputIndex: 0,
      outputCbor:
        "a300581d7811111111111111111111111111111111111111111111111111111111018200a0038203436b6b6b",
      descriptorCbor:
        "900100182c582022477a156a6db4a27cd5c7af0cda47026474f4a97dbd31f3baabeb17b5064b78581d781111111111111111111111111111111111111111111111111111111100005820b6575c6c81264fc5d6802905bc4cb01d26fcca7c75412712fd4d4b7e5a23d6cd0103581c556f006134510d3f7f607d251bd0e49aae300988b3fcfc0756c568e2065820e03a1cdbe9503904744a34d1163c1b22985c89586a997faa8c152a440043c34283582038d29cb45b5ac7901e51036d0fac974140394ebbd34ca5f70320e724380187521853185c835820bc293befa4e0e81021c3b002707d83cc1350338be43fc26303aa9acd600a55791853185c8358209525e1ea4350de9f831fc817b64355d7c3e26427effb7f4ca9bd29541d5eda390304",
    },
    {
      label: "native reference script at the top of the index domain",
      outputIndex: 65535,
      outputCbor:
        "a300581d7811111111111111111111111111111111111111111111111111111111018200a003820058208200581c33333333333333333333333333333333333333333333333333333333",
      descriptorCbor:
        "900119ffff184a58209cc398e4f08855f03f791f3e43d1d00fa55b196c2aa8432c8558e3c8df3dc9cf581d781111111111111111111111111111111111111111111111111111111100005820b6575c6c81264fc5d6802905bc4cb01d26fcca7c75412712fd4d4b7e5a23d6cd0100581cc78b7b4b696fffb06ba43034b2ddb692c43a88ea824ddfdf455b93721824582057e1c7765325cfa3e8676ca5c28b3477b878a1637cc3348d1027245a30a414978358204ccd01eb52febe88afa3b3be5af0a74c8936116264b85db97508b5cb88605d0c1853185c835820a06f11972ff408d3ca430a373f274af65377a84ed13bb21bd75008505ea71ec11853185c8358209525e1ea4350de9f831fc817b64355d7c3e26427effb7f4ca9bd29541d5eda390304",
    },
    {
      label:
        "multi-chunk output: two policies, base address, 2 KiB datum, reference script",
      outputIndex: 7,
      outputCbor:
        "a400583900111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111101821a007a1200a2581c55555555555555555555555555555555555555555555555555555555a24001420102182a581c66666666666666666666666666666666666666666666666666666666a15820abababababababababababababababababababababababababababababababab1b000000044b82fa09025908115f5840abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab5840abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab5840abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab5840abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab5840abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab5840abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab5840abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab5840abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab5840abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab5840abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab5840abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab5840abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab5840abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab5840abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab5840abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab5840abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab5840abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab5840abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab5840abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab5840abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab5840abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab5840abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab5840abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab5840abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab5840abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab5840abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab5840abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab5840abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab5840abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab5840abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab5840abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab50ababababababababababababababababff03820358646b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b",
      descriptorCbor:
        "90010719093358201d44e4026471138e8ee55364b7f5edfb548bbcb418ef6158fb2234dc604aebf758390011111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111a007a120003582008059de11e25e694627ded37fc07251446b431d0b0f01ad7988b50fae7b37cd2187703581c22c0e1c50c8393c705226fd7578a859b9049b24e0c9eb6c1a5f7b2bb18685820fa4940294dd31f0806fb59218d53fe245a6717649bf8785e8eba6e7fddf755fc8358205f721c66e18ad314fbda580939ed42add1cd7476d78918ff074a579a441407341909041908f78358205f721c66e18ad314fbda580939ed42add1cd7476d78918ff074a579a441407341909041908f7835820b0396a03f113d1587164a43e74652ff1f1054dd2e54d8c6ab7628031346a65a21908151907d8",
    },
  ])(
    "encodes the pinned $label vector exactly",
    ({ outputIndex, outputCbor, descriptorCbor }) => {
      const material = buildCanonicalMidgardLedgerOutputMaterial({
        outputIndex,
        outputCbor: Buffer.from(outputCbor, "hex"),
      });
      expect(Buffer.from(material.descriptorCbor).toString("hex")).toBe(
        descriptorCbor,
      );
      expect(
        decodeMidgardLedgerOutputCommitment(Buffer.from(descriptorCbor, "hex")),
      ).toEqual(material.descriptor);
    },
  );
});

describe("authenticated ledger output map order", () => {
  it.each([false, true])(
    "preserves mixed-width asset and datum keys through descriptor summaries datum=%s",
    (withDatum) => {
      const outputCbor = encodeMidgardTxOutput({
        address: Buffer.from("60" + "11".repeat(28), "hex"),
        value: {
          lovelace: 2_000_000n,
          assets: new Map([
            [
              "22".repeat(28),
              new Map([
                ["ff", 1n],
                ["0000", 2n],
              ]),
            ],
          ]),
        },
        ...(withDatum
          ? {
              datum: decodeMidgardDatum(Buffer.from("a241ff0142000002", "hex")),
            }
          : {}),
      });
      const descriptor = buildCanonicalMidgardLedgerOutputMaterial({
        outputIndex: 0,
        outputCbor,
      }).descriptor;
      const terminal = buildMidgardLedgerOutputProofTrace({
        outputIndex: 0,
        outputCbor,
      }).terminal;
      expect(descriptor.cardanoTxOut).toStrictEqual(
        summarizeMidgardLedgerOutputCardanoTxOut(terminal),
      );
      expect(descriptor.midgardTxOut).toStrictEqual(
        summarizeMidgardLedgerOutputMidgardTxOut(terminal),
      );
      expect(descriptor.cardanoSpendDatum).toStrictEqual(
        summarizeMidgardLedgerOutputCardanoSpendDatum(terminal),
      );
      expect(
        verifyMidgardLedgerOutputDescriptor({ control: terminal, descriptor }),
      ).toBe(true);
    },
  );
});

it("memoizes exact output bytes and index only within one reconstruction", () => {
  const resolve = createCanonicalMidgardLedgerDescriptorResolver();
  const { output, cbor } = outputFixture();
  const key = (index: number, txByte = 1) =>
    encodeMidgardSpendInputItem({
      txId: Buffer.alloc(32, txByte),
      outputIndex: index,
    });
  const first = resolve({ outRef: key(0), outputCbor: cbor });
  expect(resolve({ outRef: key(0, 2), outputCbor: cbor })).toEqual(first);
  const differentIndex = resolve({ outRef: key(1), outputCbor: cbor });
  expect(differentIndex).toEqual(
    buildCanonicalMidgardLedgerEntryOutputMaterial({
      outRef: key(1),
      outputCbor: cbor,
    }).descriptorCbor,
  );
  expect(differentIndex).not.toEqual(first);
  const changed = encodeMidgardTxOutput({
    ...output,
    value: { ...output.value, lovelace: output.value.lovelace + 1n },
  });
  expect(resolve({ outRef: key(0), outputCbor: changed })).toEqual(
    buildCanonicalMidgardLedgerEntryOutputMaterial({
      outRef: key(0),
      outputCbor: changed,
    }).descriptorCbor,
  );
  expect(resolve({ outRef: key(0), outputCbor: changed })).not.toEqual(first);
  first.fill(0);
  expect(resolve({ outRef: key(0), outputCbor: cbor })).toEqual(
    buildCanonicalMidgardLedgerEntryOutputMaterial({
      outRef: key(0),
      outputCbor: cbor,
    }).descriptorCbor,
  );
  expect(() =>
    resolve({ outRef: Buffer.from([0]), outputCbor: cbor }),
  ).toThrow();
});
