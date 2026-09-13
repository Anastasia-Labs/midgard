import { readFileSync } from "node:fs";

import {
  buildMidgardBoundedItem,
  buildMidgardBoundedItemChunkProof,
  encodeCbor,
  verifyMidgardBoundedItemChunkProof,
} from "@al-ft/midgard-core";
import { decodeSingleCbor } from "@al-ft/midgard-core/codec/cbor";
import { Constr, Data } from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";
import { describe, expect, it } from "vitest";

import {
  encodeMidgardCekContextPartsControl,
  encodeMidgardCekFinalContextControl,
  encodeMidgardCekRedeemerContextControl,
  encodeMidgardCekTxInfoAssemblyControl,
  hashMidgardCekContextPartsControl,
  hashMidgardCekFinalContextControl,
  hashMidgardCekRedeemerContextControl,
  hashMidgardCekTxInfoAssemblyControl,
} from "../src/cek-context.js";

const bytes = (hex: string): Buffer => Buffer.from(hex, "hex");
const hash = (byte: number): Buffer => Buffer.alloc(32, byte);

type GeneratedAuxiliaryFixture = {
  readonly constructors: readonly {
    readonly tag: number;
    readonly arity: number;
    readonly cbor: string;
  }[];
  readonly corpusCbor: string;
  readonly corpusBlake2b256: string;
};

const auxiliaryFixture = JSON.parse(
  readFileSync(
    new URL(
      "./fixtures/validation-auxiliary-witness-v1.generated.json",
      import.meta.url,
    ),
    "utf8",
  ),
) as GeneratedAuxiliaryFixture;
const auxiliaryVectors = auxiliaryFixture.constructors;

const shared32 = hash(0x41);
const resolveInputsValues = [
  bytes("01"),
  bytes("02"),
  bytes("03"),
  bytes("04"),
  0n,
  shared32,
  hash(0x42),
  0n,
  hash(0x43),
  bytes("00"),
  hash(0x44),
] as const;
const CANONICAL_RESOLVE_INPUTS_CONTROL_CBOR =
  "8b41014102410341040058204141414141414141414141414141414141414141414141414141414141414141582042424242424242424242424242424242424242424242424242424242424242420058204343434343434343434343434343434343434343434343434343434343434343410058204444444444444444444444444444444444444444444444444444444444444444";

const scriptDiscoveryValues = [
  0n,
  0n,
  0n,
  -1n,
  -1n,
  Buffer.alloc(0),
  Buffer.alloc(0),
  -1n,
  -1n,
  Buffer.alloc(0),
  Buffer.alloc(0),
  Buffer.alloc(0),
  Buffer.alloc(0),
  0n,
  [],
] as const;
const mintFoldValues = [
  -1n,
  0n,
  Buffer.alloc(0),
  Buffer.alloc(0),
  0n,
  Buffer.alloc(0),
  0n,
  0n,
  0n,
  Buffer.alloc(0),
  0n,
  [],
] as const;
const receiveScanValues = [
  0n,
  [],
  0n,
  Buffer.alloc(0),
  Buffer.alloc(0),
  [],
] as const;
const observerScanValues = [0n, Buffer.alloc(0), 0n] as const;
const scriptSourcesValues = [
  bytes("01"),
  bytes("02"),
  bytes("03"),
  bytes("04"),
  0n,
  hash(0x45),
  0n,
  hash(0x46),
  [],
  0n,
  0n,
  [],
  0n,
  [],
  0n,
  hash(0x47),
  hash(0x48),
  0n,
  0n,
  [],
  0n,
  0n,
  [],
  0n,
  receiveScanValues,
  0n,
  0n,
  observerScanValues,
  mintFoldValues,
  hash(0x49),
] as const;
const CANONICAL_SCRIPT_DISCOVERY_CONTROL_CBOR =
  "8f000000202040402020404040400080";
const CANONICAL_SCRIPT_SOURCES_CONTROL_CBOR =
  "981e41014102410341040058204545454545454545454545454545454545454545454545454545454545454545005820464646464646464646464646464646464646464646464646464646464646464680000080008000582047474747474747474747474747474747474747474747474747474747474747475820484848484848484848484848484848484848484848484848484848484848484800008000008000860080004040800000830040008c20004040004000000040008058204949494949494949494949494949494949494949494949494949494949494949";

const nativeScriptsValues = [
  bytes("01"),
  bytes("02"),
  bytes("03"),
  bytes("04"),
  0n,
  hash(0x4a),
  0n,
  [],
  0n,
  hash(0x4b),
  0n,
  [],
  0n,
  [],
  0n,
  [],
  0n,
  [],
  [],
  0n,
  [],
  0n,
  [],
  0n,
  0n,
  hash(0x4c),
] as const;
const CANONICAL_NATIVE_SCRIPTS_CONTROL_CBOR =
  "981a41014102410341040058204a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a00800058204b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b00800080008000808000800080000058204c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c";
const CANONICAL_SCRIPT_INTEGRITY_WITNESS_CBORS = [
  "825883981a41014102410341040058204a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a00800058204b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b00800080008000808000800080000058204c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c00",
  "825883981a41014102410341040058204a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a00800058204b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b00800080008000808000800080000058204c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c01",
  "845883981a41014102410341040058204a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a00800058204b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b00800080008000808000800080000058204c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c0258204d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d58204e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e",
  "845883981a41014102410341040058204a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a00800058204b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b00800080008000808000800080000058204c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c0358204f4f4f4f4f4f4f4f4f4f4f4f4f4f4f4f4f4f4f4f4f4f4f4f4f4f4f4f4f4f4f4f58205050505050505050505050505050505050505050505050505050505050505050",
] as const;

const emptySummary = {
  root: Buffer.alloc(0),
  cborLength: 0n,
  memory: 0n,
} as const;
const nonemptySummary = {
  root: hash(0x51),
  cborLength: 1n,
  memory: 1n,
} as const;
const nonemptySequence = {
  root: hash(0x52),
  length: 1n,
  payloadCborLength: 1n,
  memory: 1n,
} as const;
const v14RedeemerControl = {
  cursor: 1,
  mapItems: nonemptySequence,
  activeScanHash: hash(0x53),
  activeRedeemerLeaf: hash(0x54),
  activePurpose: emptySummary,
  currentRedeemer: nonemptySummary,
} as const;
const v14FinalControl = {
  txInfo: nonemptySummary,
  redeemer: nonemptySummary,
  scriptInfo: nonemptySummary,
} as const;
const v14PartsControl = {
  redeemerItems: nonemptySequence,
  redeemer: nonemptySummary,
  scriptInfo: nonemptySummary,
} as const;
const v14AssemblyControl = {
  tailFields: nonemptySequence,
  redeemer: nonemptySummary,
  scriptInfo: nonemptySummary,
} as const;
const CANONICAL_V14_CONTROL_CBORS = [
  "8601845820525252525252525252525252525252525252525252525252525252525252525201010158205353535353535353535353535353535353535353535353535353535353535353582054545454545454545454545454545454545454545454545454545454545454548340000083582051515151515151515151515151515151515151515151515151515151515151510101",
  "83835820515151515151515151515151515151515151515151515151515151515151515101018358205151515151515151515151515151515151515151515151515151515151515151010183582051515151515151515151515151515151515151515151515151515151515151510101",
  "8384582052525252525252525252525252525252525252525252525252525252525252520101018358205151515151515151515151515151515151515151515151515151515151515151010183582051515151515151515151515151515151515151515151515151515151515151510101",
  "8384582052525252525252525252525252525252525252525252525252525252525252520101018358205151515151515151515151515151515151515151515151515151515151515151010183582051515151515151515151515151515151515151515151515151515151515151510101",
] as const;
// The V14 control hashes are not transcribed from the hashers under test:
// each one is the ABI's own definition, blake2b-256 over the domain-separation
// label concatenated with the canonical control CBOR that this suite already
// pins independently above. The labels are the normative ABI strings, so a
// hasher that drops the domain, reuses a sibling's label, or hashes anything
// but the canonical encoding fails here.
const CANONICAL_V14_CONTROL_DOMAINS = [
  "MidgardCekRedeemerContextControlV1",
  "MidgardCekFinalContextControlV1",
  "MidgardCekContextPartsControlV1",
  "MidgardCekTxInfoAssemblyControlV1",
] as const;

const domainSeparatedDigest = (
  domain: string,
  controlCborHex: string,
): string =>
  Buffer.from(
    blake2b(
      Buffer.concat([Buffer.from(domain, "ascii"), bytes(controlCborHex)]),
      {
        dkLen: 32,
      },
    ),
  ).toString("hex");

const CANONICAL_V14_CONTROL_HASHES = CANONICAL_V14_CONTROL_DOMAINS.map(
  (domain, controlIndex) =>
    domainSeparatedDigest(domain, CANONICAL_V14_CONTROL_CBORS[controlIndex]!),
);

const expectExactArray = (
  cbor: Uint8Array,
  expectedArity: number,
): readonly unknown[] => {
  const decoded = decodeSingleCbor(Buffer.from(cbor));
  if (!Array.isArray(decoded) || decoded.length !== expectedArity) {
    throw new Error(`expected exact V1 array arity ${expectedArity}`);
  }
  return decoded;
};

describe("canonical validation controls V1 ABI", () => {
  it("freezes all 40 ValidationAuxiliaryWitnessV1 tags and arities", () => {
    expect(auxiliaryVectors).toHaveLength(40);
    for (const [vectorIndex, vector] of auxiliaryVectors.entries()) {
      expect(vector.tag).toBe(vectorIndex);
      const decoded = Data.from(vector.cbor);
      expect(decoded).toBeInstanceOf(Constr);
      expect((decoded as Constr<unknown>).index).toBe(vectorIndex);
      expect((decoded as Constr<unknown>).fields).toHaveLength(vector.arity);
    }
  });

  it("keeps the descriptor-only native execution witness bounded at the chunk limit", () => {
    const nativeExecution = Data.from(auxiliaryVectors[11]!.cbor);
    if (!(nativeExecution instanceof Constr)) {
      throw new Error("canonical tag 11 must be a constructor");
    }
    const maximumItem = buildMidgardBoundedItem({
      fieldIndex: 0,
      itemIndex: 0,
      bytes: Buffer.alloc(4_095, 0x5a),
    });
    const maximumChunkProof = buildMidgardBoundedItemChunkProof(maximumItem, 0);
    expect(
      verifyMidgardBoundedItemChunkProof({
        expectedCommitment: maximumItem.commitment,
        proof: maximumChunkProof,
      }),
    ).toBe(true);
    const maximumChunkProofData = new Constr(0, [
      1n,
      0n,
      0n,
      BigInt(maximumItem.bytes.length),
      0n,
      maximumItem.bytes.toString("hex"),
      [],
      [],
    ]);
    const fields = [...nativeExecution.fields];
    fields[10] = BigInt(maximumItem.bytes.length);
    fields[11] = maximumItem.commitment.toString("hex");
    fields[15] = maximumChunkProofData;
    const encoded = Buffer.from(Data.to(new Constr(11, fields)), "hex");
    const decoded = Data.from(encoded.toString("hex"));
    expect(decoded).toBeInstanceOf(Constr);
    expect((decoded as Constr<unknown>).index).toBe(11);
    expect((decoded as Constr<unknown>).fields).toHaveLength(16);
    expect(encoded.length).toBeLessThan(16 * 1024);
    expect((decoded as Constr<unknown>).fields[10]).toBe(
      BigInt(maximumItem.bytes.length),
    );
  });

  it("matches exact V11 resolve-inputs and V12 script-source controls", () => {
    const resolveInputs = encodeCbor(resolveInputsValues);
    const discovery = encodeCbor(scriptDiscoveryValues);
    const scriptSources = encodeCbor(scriptSourcesValues);

    expect(resolveInputs.toString("hex")).toBe(
      CANONICAL_RESOLVE_INPUTS_CONTROL_CBOR,
    );
    expect(discovery.toString("hex")).toBe(
      CANONICAL_SCRIPT_DISCOVERY_CONTROL_CBOR,
    );
    expect(scriptSources.toString("hex")).toBe(
      CANONICAL_SCRIPT_SOURCES_CONTROL_CBOR,
    );
    expectExactArray(resolveInputs, 11);
    expectExactArray(discovery, 15);
    expectExactArray(scriptSources, 30);
  });

  it("matches the V13 native control and every integrity witness shape", () => {
    const control = encodeCbor(nativeScriptsValues);
    const wrappers = [
      encodeCbor([control, 0n]),
      encodeCbor([control, 1n]),
      encodeCbor([control, 2n, hash(0x4d), hash(0x4e)]),
      encodeCbor([control, 3n, hash(0x4f), hash(0x50)]),
    ];

    expect(control.toString("hex")).toBe(CANONICAL_NATIVE_SCRIPTS_CONTROL_CBOR);
    expect(wrappers.map((value) => value.toString("hex"))).toEqual(
      CANONICAL_SCRIPT_INTEGRITY_WITNESS_CBORS,
    );
    expectExactArray(control, 26);
    expectExactArray(wrappers[0]!, 2);
    expectExactArray(wrappers[1]!, 2);
    expectExactArray(wrappers[2]!, 4);
    expectExactArray(wrappers[3]!, 4);
  });

  it("matches every exported V14 encoder and domain-separated hash", () => {
    const encodings = [
      encodeMidgardCekRedeemerContextControl(v14RedeemerControl),
      encodeMidgardCekFinalContextControl(v14FinalControl),
      encodeMidgardCekContextPartsControl(v14PartsControl),
      encodeMidgardCekTxInfoAssemblyControl(v14AssemblyControl),
    ];
    const hashes = [
      hashMidgardCekRedeemerContextControl(v14RedeemerControl),
      hashMidgardCekFinalContextControl(v14FinalControl),
      hashMidgardCekContextPartsControl(v14PartsControl),
      hashMidgardCekTxInfoAssemblyControl(v14AssemblyControl),
    ];

    expect(encodings.map((value) => value.toString("hex"))).toEqual(
      CANONICAL_V14_CONTROL_CBORS,
    );
    expect(hashes.map((value) => value.toString("hex"))).toEqual(
      CANONICAL_V14_CONTROL_HASHES,
    );
    // Domain separation is load-bearing: no control's digest may be reachable
    // by hashing its canonical encoding bare, or under a sibling control's
    // label, otherwise two distinct controls could be made to collide.
    for (const [controlIndex, digest] of hashes.entries()) {
      const controlCbor = CANONICAL_V14_CONTROL_CBORS[controlIndex]!;
      const reachableWithoutOwnDomain = [
        domainSeparatedDigest("", controlCbor),
        ...CANONICAL_V14_CONTROL_DOMAINS.filter(
          (_, domainIndex) => domainIndex !== controlIndex,
        ).map((domain) => domainSeparatedDigest(domain, controlCbor)),
      ];
      expect(reachableWithoutOwnDomain).not.toContain(digest.toString("hex"));
    }
    expectExactArray(encodings[0]!, 6);
    for (const encoding of encodings.slice(1)) {
      expectExactArray(encoding, 3);
    }
  });
});
