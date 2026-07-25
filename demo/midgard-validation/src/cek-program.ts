import {
  commitMidgardCekBlobV1,
  encodeMidgardCekProgramEnvelopeV1,
  encodeMidgardCekSequenceNodeV1,
  encodeMidgardCekTermNodeV1,
  encodeMidgardCekValueNodeV1,
  type Hash32,
  hashMidgardCekProgramEnvelopeV1,
  hashMidgardCekSequenceNodeV1,
  hashMidgardCekTermNodeV1,
  hashMidgardCekValueNodeV1,
  MIDGARD_CEK_EMPTY_SEQUENCE_ROOT_V1,
  MIDGARD_CONSENSUS_LIMITS_V1,
  type MidgardCekProgramEnvelopeV1,
  type MidgardCekProgramMaterialEntryV1,
  type MidgardCekTermNodeV1,
  verifyMidgardCekProgramMaterialV1,
} from "@al-ft/midgard-core";
import { dataFromCbor } from "@harmoniclabs/plutus-data";
import {
  Application,
  Builtin,
  Case,
  Constr,
  Delay,
  ErrorUPLC,
  Force,
  getNRequiredForces,
  Lambda,
  parseUPLC,
  UPLCConst,
  UPLCEncoder,
  type UPLCTerm,
  UPLCVar,
} from "@harmoniclabs/uplc";

import type { MidgardCekConstantValueWitnessV1 } from "./cek-builtin.js";
import {
  encodeMidgardCekCanonicalConstantV1,
  MIDGARD_CEK_MAX_DIRECT_CONSTANT_PAYLOAD_BYTES_V1,
  midgardCekConstantMemorySizeV1,
} from "./cek-constant.js";
import { commitMidgardCekDataTreeV1 } from "./cek-data-tree.js";

export const MIDGARD_CEK_MAX_PROGRAM_NODE_COUNT =
  MIDGARD_CONSENSUS_LIMITS_V1.maxCekProgramNodeCount;
export const MIDGARD_CEK_MAX_PROGRAM_MATERIAL_BYTES =
  MIDGARD_CONSENSUS_LIMITS_V1.maxCekProgramMaterialBytes;

export type MidgardCekProgramMaterialKindV1 =
  MidgardCekProgramMaterialEntryV1["kind"];
export type MidgardCekProgramMaterialNodeV1 =
  MidgardCekProgramMaterialEntryV1;

export type MidgardCanonicalCekProgramV1 = {
  readonly envelope: MidgardCekProgramEnvelopeV1;
  readonly envelopeCbor: Buffer;
  readonly envelopeHash: Hash32;
  readonly material: ReadonlyMap<string, MidgardCekProgramMaterialNodeV1>;
  readonly constantWitnesses: ReadonlyMap<
    string,
    MidgardCekConstantValueWitnessV1
  >;
};

const rootHex = (root: Uint8Array): string =>
  Buffer.from(root).toString("hex");

const sameBytes = (left: Uint8Array, right: Uint8Array): boolean =>
  Buffer.from(left).equals(Buffer.from(right));

const unwrapCanonicalCborByteString = (
  bytes: Buffer,
): Buffer | null => {
  if (bytes.length === 0 || (bytes[0]! >> 5) !== 2) return null;
  const additional = bytes[0]! & 0x1f;
  let headerLength = 1;
  let payloadLength: bigint;
  if (additional < 24) {
    payloadLength = BigInt(additional);
  } else if (additional === 24) {
    if (bytes.length < 2 || bytes[1]! < 24) return null;
    headerLength = 2;
    payloadLength = BigInt(bytes[1]!);
  } else if (additional === 25) {
    if (bytes.length < 3) return null;
    const length = bytes.readUInt16BE(1);
    if (length <= 0xff) return null;
    headerLength = 3;
    payloadLength = BigInt(length);
  } else if (additional === 26) {
    if (bytes.length < 5) return null;
    const length = bytes.readUInt32BE(1);
    if (length <= 0xffff) return null;
    headerLength = 5;
    payloadLength = BigInt(length);
  } else if (additional === 27) {
    if (bytes.length < 9) return null;
    const length = bytes.readBigUInt64BE(1);
    if (length <= 0xffff_ffffn) return null;
    headerLength = 9;
    payloadLength = length;
  } else {
    return null;
  }
  if (
    payloadLength > BigInt(Number.MAX_SAFE_INTEGER) ||
    BigInt(headerLength) + payloadLength !== BigInt(bytes.length)
  ) {
    return null;
  }
  return bytes.subarray(
    headerLength,
    headerLength + Number(payloadLength),
  );
};

const canonicalFlatProgramBytes = (scriptBytes: Buffer): Buffer => {
  let flat = scriptBytes;
  for (;;) {
    const unwrapped = unwrapCanonicalCborByteString(flat);
    if (unwrapped === null) break;
    flat = unwrapped;
  }
  return flat;
};

/**
 * Decodes a PlutusV3/MidgardV1 Flat/CBOR program into the canonical,
 * hash-addressed CEK graph used by the canonical V1 proof profile. Raw UPLC is an SDK input;
 * the returned envelope is the consensus script payload.
 */
export const buildMidgardCanonicalCekProgramV1 = (
  scriptBytes: Uint8Array,
): MidgardCanonicalCekProgramV1 => {
  const raw = Buffer.from(scriptBytes);
  if (raw.length === 0) {
    throw new Error("CEK program input must not be empty");
  }

  const flat = canonicalFlatProgramBytes(raw);
  const program = parseUPLC(flat, "flat");
  const reencoded = Buffer.from(
    UPLCEncoder.compile(program).toBuffer().buffer,
  );
  if (!flat.equals(reencoded)) {
    throw new Error(
      "V1 requires canonical Flat bytes with exactly the builtin forces implied by UPLC 1.1.0",
    );
  }
  if (
    program.version.major !== 1n ||
    program.version.minor !== 1n ||
    program.version.patch !== 0n
  ) {
    throw new Error(
      `V1 supports only UPLC 1.1.0, received ${program.version.toString()}`,
    );
  }

  const material = new Map<string, MidgardCekProgramMaterialNodeV1>();
  const constantWitnesses = new Map<
    string,
    MidgardCekConstantValueWitnessV1
  >();
  let materialByteLength = 0;
  const addMaterial = (
    kind: MidgardCekProgramMaterialKindV1,
    root: Hash32,
    preimage: Uint8Array,
  ): void => {
    const key = rootHex(root);
    const exactPreimage = Buffer.from(preimage);
    const prior = material.get(key);
    if (prior !== undefined) {
      if (prior.kind !== kind || !sameBytes(prior.preimage, exactPreimage)) {
        throw new Error("CEK material hash collision across distinct nodes");
      }
      return;
    }
    material.set(
      key,
      Object.freeze({
        kind,
        root,
        preimage: exactPreimage,
      }),
    );
    materialByteLength += exactPreimage.length;
  };

  const addBlob = (bytes: Uint8Array): Hash32 => {
    const committed = commitMidgardCekBlobV1(bytes);
    for (const [key, node] of committed.nodes) {
      const root = Buffer.from(key, "hex") as Hash32;
      addMaterial(
        node.kind === "chunk" ? "blobChunk" : "blobBranch",
        root,
        node.preimage,
      );
    }
    return committed.root;
  };

  const addTermNode = (node: MidgardCekTermNodeV1): Hash32 => {
    const root = hashMidgardCekTermNodeV1(node);
    addMaterial("term", root, encodeMidgardCekTermNodeV1(node));
    return root;
  };

  const addTermSequence = (terms: readonly UPLCTerm[]): Hash32 => {
    let root = MIDGARD_CEK_EMPTY_SEQUENCE_ROOT_V1;
    for (let index = terms.length - 1; index >= 0; index -= 1) {
      const head = addTerm(terms[index]!);
      const length = BigInt(terms.length - index);
      const node = { head, tail: root, length };
      root = hashMidgardCekSequenceNodeV1(node);
      addMaterial("sequence", root, encodeMidgardCekSequenceNodeV1(node));
    }
    return root;
  };

  const addConstantValue = (constant: UPLCConst): Hash32 => {
    const canonical = encodeMidgardCekCanonicalConstantV1(constant);
    if (
      canonical.payloadCbor.length >
      MIDGARD_CEK_MAX_DIRECT_CONSTANT_PAYLOAD_BYTES_V1
    ) {
      throw new Error(
        "V1 source constant payload exceeds the 9,215-byte L1 proof envelope",
      );
    }
    const typeRoot = addBlob(canonical.typeCbor);
    const payload = dataFromCbor(canonical.payloadCbor);
    const semantic = commitMidgardCekDataTreeV1(payload);
    for (const [key, entry] of semantic.dataNodes) {
      addMaterial(
        "dataNode",
        Buffer.from(key, "hex") as Hash32,
        entry.preimage,
      );
    }
    for (const [key, entry] of semantic.listNodes) {
      addMaterial(
        "dataList",
        Buffer.from(key, "hex") as Hash32,
        entry.preimage,
      );
    }
    for (const [key, entry] of semantic.pairNodes) {
      addMaterial(
        "dataPair",
        Buffer.from(key, "hex") as Hash32,
        entry.preimage,
      );
    }
    for (const [key, entry] of semantic.blobNodes) {
      addMaterial(
        entry.kind === "chunk" ? "blobChunk" : "blobBranch",
        Buffer.from(key, "hex") as Hash32,
        entry.preimage,
      );
    }
    const node = {
      kind: "constant",
      typeRoot,
      payloadRoot: semantic.root,
      payloadLength: semantic.cborLength,
      semanticRoot: semantic.root,
      memory: midgardCekConstantMemorySizeV1(
        canonical.type,
        payload,
      ),
    } as const;
    const root = hashMidgardCekValueNodeV1(node);
    addMaterial("value", root, encodeMidgardCekValueNodeV1(node));
    constantWitnesses.set(
      rootHex(root),
      Object.freeze({
        kind: "constant",
        witness: Object.freeze({
          typeCbor: canonical.typeCbor,
          payloadCbor: canonical.payloadCbor,
        }),
      }),
    );
    return root;
  };

  const addTerm = (term: UPLCTerm): Hash32 => {
    if (term instanceof UPLCVar) {
      return addTermNode({ kind: "variable", index: term.deBruijn });
    }
    if (term instanceof Delay) {
      return addTermNode({
        kind: "delay",
        body: addTerm(term.delayedTerm),
      });
    }
    if (term instanceof Lambda) {
      return addTermNode({ kind: "lambda", body: addTerm(term.body) });
    }
    if (term instanceof Application) {
      return addTermNode({
        kind: "application",
        function: addTerm(term.funcTerm),
        argument: addTerm(term.argTerm),
      });
    }
    if (term instanceof UPLCConst) {
      return addTermNode({
        kind: "constant",
        value: addConstantValue(term),
      });
    }
    if (term instanceof Force) {
      return addTermNode({
        kind: "force",
        term: addTerm(term.termToForce),
      });
    }
    if (term instanceof ErrorUPLC) {
      return addTermNode({ kind: "error" });
    }
    if (term instanceof Builtin) {
      let root = addTermNode({
        kind: "builtin",
        tag: BigInt(term.tag),
      });
      // Harmonic's AST erases the type-instantiation forces which its Flat
      // encoder inserts around polymorphic builtins. Restore those nodes so
      // the committed graph and its CEK costs match the canonical Flat term.
      const requiredForces = getNRequiredForces(term.tag);
      for (let index = 0; index < requiredForces; index += 1) {
        root = addTermNode({ kind: "force", term: root });
      }
      return root;
    }
    if (term instanceof Constr) {
      return addTermNode({
        kind: "constr",
        tag: term.index,
        termsCount: BigInt(term.terms.length),
        termsRoot: addTermSequence(term.terms),
      });
    }
    if (term instanceof Case) {
      return addTermNode({
        kind: "case",
        scrutinee: addTerm(term.constrTerm),
        branchesCount: BigInt(term.continuations.length),
        branchesRoot: addTermSequence(term.continuations),
      });
    }
    throw new Error("unsupported UPLC term in V1 canonicalizer");
  };

  const termRoot = addTerm(program.body);
  if (material.size > MIDGARD_CEK_MAX_PROGRAM_NODE_COUNT) {
    throw new Error(
      `canonical CEK graph exceeds the derived ${MIDGARD_CEK_MAX_PROGRAM_NODE_COUNT.toString(10)} node bound`,
    );
  }
  if (materialByteLength > MIDGARD_CEK_MAX_PROGRAM_MATERIAL_BYTES) {
    throw new Error(
      `canonical CEK graph exceeds the derived ${MIDGARD_CEK_MAX_PROGRAM_MATERIAL_BYTES.toString(10)} byte DA bound`,
    );
  }

  const envelope = Object.freeze({
    uplcVersion: [
      program.version.major,
      program.version.minor,
      program.version.patch,
    ] as const,
    termRoot,
    nodeCount: BigInt(material.size),
    materialByteLength: BigInt(materialByteLength),
  });
  verifyMidgardCekProgramMaterialV1(envelope, material.values());
  return Object.freeze({
    envelope,
    envelopeCbor: encodeMidgardCekProgramEnvelopeV1(envelope),
    envelopeHash: hashMidgardCekProgramEnvelopeV1(envelope),
    material,
    constantWitnesses,
  });
};
