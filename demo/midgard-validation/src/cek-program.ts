import {
  commitMidgardCekBlob,
  decodeMidgardCekProgramMaterialSidecar,
  encodeMidgardCekProgramEnvelope,
  encodeMidgardCekProgramMaterialSidecar,
  encodeMidgardCekSequenceNode,
  encodeMidgardCekTermNode,
  encodeMidgardCekValueNode,
  type Hash32,
  hashMidgardCekProgramEnvelope,
  hashMidgardCekSequenceNode,
  hashMidgardCekTermNode,
  hashMidgardCekValueNode,
  hashMidgardVersionedScript,
  MIDGARD_CEK_EMPTY_SEQUENCE_ROOT,
  MIDGARD_CONSENSUS_LIMITS,
  type MidgardCekProgramEnvelope,
  type MidgardCekProgramMaterialEntry,
  type MidgardCekTermNode,
  type MidgardVersionedScript,
  verifyMidgardCekProgramMaterial,
  verifyMidgardCekProgramMaterialBundle,
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

import type { MidgardCekConstantValueWitness } from "./cek-builtin.js";
import {
  encodeMidgardCekCanonicalConstant,
  MIDGARD_CEK_MAX_DIRECT_CONSTANT_PAYLOAD_BYTES,
  midgardCekConstantMemorySize,
} from "./cek-constant.js";
import { commitMidgardCekDataTree } from "./cek-data-tree.js";

export const MIDGARD_CEK_MAX_PROGRAM_NODE_COUNT =
  MIDGARD_CONSENSUS_LIMITS.maxCekProgramNodeCount;
export const MIDGARD_CEK_MAX_PROGRAM_MATERIAL_BYTES =
  MIDGARD_CONSENSUS_LIMITS.maxCekProgramMaterialBytes;

export type MidgardCekProgramMaterialKind =
  MidgardCekProgramMaterialEntry["kind"];
export type MidgardCekProgramMaterialNode = MidgardCekProgramMaterialEntry;

export type MidgardCanonicalCekProgram = {
  readonly envelope: MidgardCekProgramEnvelope;
  readonly envelopeCbor: Buffer;
  readonly envelopeHash: Hash32;
  readonly material: ReadonlyMap<string, MidgardCekProgramMaterialNode>;
  readonly constantWitnesses: ReadonlyMap<
    string,
    MidgardCekConstantValueWitness
  >;
};

export type MidgardCanonicalScriptArtifactLanguage = "PlutusV3" | "MidgardV1";

export type MidgardCanonicalScriptArtifactInput = {
  readonly language: MidgardCanonicalScriptArtifactLanguage;
  readonly sourceRawFlatProgramBytes: Uint8Array;
};

/**
 * A canonical script authoring result with deliberately distinct source and
 * consensus identities. The source hash is audit/remapping metadata only;
 * credentials must use canonicalMidgardCredentialScriptHash.
 *
 * Byte-bearing accessors return defensive values so callers cannot mutate the
 * artifact or create aliases between its script, program, material, or sidecar
 * representations.
 */
export type MidgardCanonicalScriptArtifact = {
  readonly canonicalMidgardCredentialScript: MidgardVersionedScript;
  readonly canonicalMidgardCredentialScriptHash: string;
  readonly sourceRawScriptAuditHash: string;
  readonly canonicalProgram: MidgardCanonicalCekProgram;
  readonly canonicalMaterialEntries: readonly MidgardCekProgramMaterialEntry[];
  readonly canonicalMaterialSidecarCbor: Buffer;
};

const rootHex = (root: Uint8Array): string => Buffer.from(root).toString("hex");

const sameBytes = (left: Uint8Array, right: Uint8Array): boolean =>
  Buffer.from(left).equals(Buffer.from(right));

const unwrapCanonicalCborByteString = (bytes: Buffer): Buffer | null => {
  if (bytes.length === 0 || bytes[0]! >> 5 !== 2) return null;
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
  return bytes.subarray(headerLength, headerLength + Number(payloadLength));
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
export const buildMidgardCanonicalCekProgram = (
  scriptBytes: Uint8Array,
): MidgardCanonicalCekProgram => {
  const raw = Buffer.from(scriptBytes);
  if (raw.length === 0) {
    throw new Error("CEK program input must not be empty");
  }

  const flat = canonicalFlatProgramBytes(raw);
  const program = parseUPLC(flat, "flat");
  const reencoded = Buffer.from(UPLCEncoder.compile(program).toBuffer().buffer);
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

  const material = new Map<string, MidgardCekProgramMaterialNode>();
  const constantWitnesses = new Map<string, MidgardCekConstantValueWitness>();
  let materialByteLength = 0;
  const addMaterial = (
    kind: MidgardCekProgramMaterialKind,
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
    const committed = commitMidgardCekBlob(bytes);
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

  const addTermNode = (node: MidgardCekTermNode): Hash32 => {
    const root = hashMidgardCekTermNode(node);
    addMaterial("term", root, encodeMidgardCekTermNode(node));
    return root;
  };

  const addTermSequence = (terms: readonly UPLCTerm[]): Hash32 => {
    let root = MIDGARD_CEK_EMPTY_SEQUENCE_ROOT;
    for (let index = terms.length - 1; index >= 0; index -= 1) {
      const head = addTerm(terms[index]!);
      const length = BigInt(terms.length - index);
      const node = { head, tail: root, length };
      root = hashMidgardCekSequenceNode(node);
      addMaterial("sequence", root, encodeMidgardCekSequenceNode(node));
    }
    return root;
  };

  const addConstantValue = (constant: UPLCConst): Hash32 => {
    const canonical = encodeMidgardCekCanonicalConstant(constant);
    if (
      canonical.payloadCbor.length >
      MIDGARD_CEK_MAX_DIRECT_CONSTANT_PAYLOAD_BYTES
    ) {
      throw new Error(
        "V1 source constant payload exceeds the 9,215-byte L1 proof envelope",
      );
    }
    const typeRoot = addBlob(canonical.typeCbor);
    const payload = dataFromCbor(canonical.payloadCbor);
    const semantic = commitMidgardCekDataTree(payload);
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
      memory: midgardCekConstantMemorySize(canonical.type, payload),
    } as const;
    const root = hashMidgardCekValueNode(node);
    addMaterial("value", root, encodeMidgardCekValueNode(node));
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
  verifyMidgardCekProgramMaterial(envelope, material.values());
  return Object.freeze({
    envelope,
    envelopeCbor: encodeMidgardCekProgramEnvelope(envelope),
    envelopeHash: hashMidgardCekProgramEnvelope(envelope),
    material,
    constantWitnesses,
  });
};

const copyProgramEnvelope = (
  envelope: MidgardCekProgramEnvelope,
): MidgardCekProgramEnvelope => {
  const termRoot = Buffer.from(envelope.termRoot);
  return Object.freeze({
    uplcVersion: Object.freeze([...envelope.uplcVersion]) as readonly [
      bigint,
      bigint,
      bigint,
    ],
    get termRoot(): Buffer {
      return Buffer.from(termRoot);
    },
    nodeCount: envelope.nodeCount,
    materialByteLength: envelope.materialByteLength,
  });
};

const copyProgramMaterialEntry = (
  entry: MidgardCekProgramMaterialEntry,
): MidgardCekProgramMaterialEntry => {
  const root = Buffer.from(entry.root);
  const preimage = Buffer.from(entry.preimage);
  return Object.freeze({
    kind: entry.kind,
    get root(): Hash32 {
      return Buffer.from(root) as Hash32;
    },
    get preimage(): Buffer {
      return Buffer.from(preimage);
    },
  });
};

const copyConstantValueWitness = (
  value: MidgardCekConstantValueWitness,
): MidgardCekConstantValueWitness => {
  if (value.kind === "constant") {
    const typeCbor = Buffer.from(value.witness.typeCbor);
    const payloadCbor = Buffer.from(value.witness.payloadCbor);
    return Object.freeze({
      kind: "constant",
      get witness() {
        return Object.freeze({
          get typeCbor(): Buffer {
            return Buffer.from(typeCbor);
          },
          get payloadCbor(): Buffer {
            return Buffer.from(payloadCbor);
          },
        });
      },
    });
  }
  const typeCbor = Buffer.from(value.witness.typeCbor);
  const payloadRoot = Buffer.from(value.witness.payload.root);
  return Object.freeze({
    kind: "semanticConstant",
    get witness() {
      return Object.freeze({
        get typeCbor(): Buffer {
          return Buffer.from(typeCbor);
        },
        get payload() {
          return Object.freeze({
            get root(): Buffer {
              return Buffer.from(payloadRoot);
            },
            cborLength: value.witness.payload.cborLength,
            memory: value.witness.payload.memory,
          });
        },
        memory: value.witness.memory,
      });
    },
  });
};

const copyCanonicalProgram = (
  program: MidgardCanonicalCekProgram,
): MidgardCanonicalCekProgram =>
  Object.freeze({
    envelope: copyProgramEnvelope(program.envelope),
    envelopeCbor: Buffer.from(program.envelopeCbor),
    envelopeHash: Buffer.from(program.envelopeHash) as Hash32,
    material: new Map(
      [...program.material].map(([key, entry]) => [
        key,
        copyProgramMaterialEntry(entry),
      ]),
    ),
    constantWitnesses: new Map(
      [...program.constantWitnesses].map(([key, value]) => [
        key,
        copyConstantValueWitness(value),
      ]),
    ),
  });

const copyCanonicalCredentialScript = (
  language: MidgardCanonicalScriptArtifactLanguage,
  envelopeCbor: Uint8Array,
): MidgardVersionedScript => {
  const scriptBytes = Buffer.from(envelopeCbor);
  return Object.freeze({
    language,
    get scriptBytes(): Buffer {
      return Buffer.from(scriptBytes);
    },
  });
};

/**
 * Builds the exact canonical V1 script artifact used for Midgard credentials
 * from raw PlutusV3 or MidgardV1 Flat authoring input.
 */
export const buildMidgardCanonicalScriptArtifact = ({
  language,
  sourceRawFlatProgramBytes,
}: MidgardCanonicalScriptArtifactInput): MidgardCanonicalScriptArtifact => {
  const sourceBytes = Buffer.from(sourceRawFlatProgramBytes);
  const canonicalProgram = buildMidgardCanonicalCekProgram(sourceBytes);
  const sourceRawScriptAuditHash = hashMidgardVersionedScript({
    language,
    scriptBytes: sourceBytes,
  });
  const canonicalCredentialScript = copyCanonicalCredentialScript(
    language,
    canonicalProgram.envelopeCbor,
  );
  const canonicalMidgardCredentialScriptHash = hashMidgardVersionedScript(
    canonicalCredentialScript,
  );
  const encodedSidecar = encodeMidgardCekProgramMaterialSidecar([
    ...canonicalProgram.material.values(),
  ]);
  const canonicalMaterialEntries =
    decodeMidgardCekProgramMaterialSidecar(encodedSidecar);
  verifyMidgardCekProgramMaterialBundle(
    [canonicalProgram.envelope],
    canonicalMaterialEntries,
  );

  return Object.freeze({
    get canonicalMidgardCredentialScript(): MidgardVersionedScript {
      return copyCanonicalCredentialScript(
        language,
        canonicalProgram.envelopeCbor,
      );
    },
    canonicalMidgardCredentialScriptHash,
    sourceRawScriptAuditHash,
    get canonicalProgram(): MidgardCanonicalCekProgram {
      return copyCanonicalProgram(canonicalProgram);
    },
    get canonicalMaterialEntries(): readonly MidgardCekProgramMaterialEntry[] {
      return Object.freeze(
        canonicalMaterialEntries.map(copyProgramMaterialEntry),
      );
    },
    get canonicalMaterialSidecarCbor(): Buffer {
      return Buffer.from(encodedSidecar);
    },
  });
};
