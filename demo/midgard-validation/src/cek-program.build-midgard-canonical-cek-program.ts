import {
  commitMidgardCekBlob,
  encodeMidgardCekSequenceNode,
  encodeMidgardCekTermNode,
  encodeMidgardCekValueNode,
  type Hash32,
  hashMidgardCekSequenceNode,
  hashMidgardCekTermNode,
  hashMidgardCekValueNode,
  isMidgardCekProgramUplcVersion,
  MIDGARD_CEK_EMPTY_SEQUENCE_ROOT,
  type MidgardCekTermNode,
  verifyMidgardCekProgramMaterial,
} from "@al-ft/midgard-core";
import {
  Application,
  Builtin,
  Case,
  Constr,
  Delay,
  ErrorUPLC,
  Force,
  Lambda,
  UPLCConst,
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
import { decodeMidgardCekCardanoFlatProgram } from "./cek-program.cardano-flat.js";
import {
  type MidgardCanonicalCekProgram,
  type MidgardCekProgramMaterialKind,
  type MidgardCekProgramMaterialNode,
  rootHex,
  sameBytes,
  storedFlatProgramBytes,
} from "./cek-program.unwrap-canonical-cbor-byte-string.js";
import { plutusDataFromCborIterative } from "./plutus-data-iterative.decode.js";

/** Derives local dispute material from the exact carried CBOR-wrapped Flat. */
export const buildMidgardCanonicalCekProgram = (
  scriptBytes: Uint8Array,
): MidgardCanonicalCekProgram => {
  const raw = Buffer.from(scriptBytes);
  if (raw.length === 0) {
    throw new Error("CEK program input must not be empty");
  }

  const flat = storedFlatProgramBytes(raw);
  // Data constants may be spelled in any form the Cardano ledger's Data
  // decoder accepts; the local graph is built from their values.
  const program = decodeMidgardCekCardanoFlatProgram(flat);
  if (
    !isMidgardCekProgramUplcVersion(
      BigInt(program.version.major),
      BigInt(program.version.minor),
      BigInt(program.version.patch),
    )
  ) {
    throw new Error(
      `V1 supports only UPLC 1.0.0 and 1.1.0, received ${program.version.toString()}`,
    );
  }
  const constrAndCaseAdmitted = program.version.minor !== 0;

  const material = new Map<string, MidgardCekProgramMaterialNode>();
  const constantWitnesses = new Map<string, MidgardCekConstantValueWitness>();
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
    const payload = plutusDataFromCborIterative(canonical.payloadCbor);
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

  const addTerm = (body: UPLCTerm): Hash32 => {
    type ParentTerm = Delay | Lambda | Application | Force | Constr | Case;
    type SequenceTask = {
      readonly terms: readonly UPLCTerm[];
      readonly index: number;
      readonly tail: Hash32;
    };
    type Task =
      | { readonly kind: "term"; readonly term: UPLCTerm }
      | { readonly kind: "parent"; readonly term: ParentTerm }
      | ({ readonly kind: "sequence" } & SequenceTask)
      | ({ readonly kind: "sequenceNode" } & SequenceTask);
    const pending: Task[] = [{ kind: "term", term: body }];
    const roots: Hash32[] = [];
    const sequence = (terms: readonly UPLCTerm[]): Task => ({
      kind: "sequence",
      terms,
      index: terms.length - 1,
      tail: MIDGARD_CEK_EMPTY_SEQUENCE_ROOT,
    });

    // Parent tasks consume their children's roots in postorder. Sequences
    // visit the last child first and commit each cons immediately, preserving
    // material insertion order without growing the JavaScript call stack.
    for (let task = pending.pop(); task !== undefined; task = pending.pop()) {
      if (task.kind === "sequence") {
        if (task.index < 0) roots.push(task.tail);
        else {
          pending.push(
            { ...task, kind: "sequenceNode" },
            { kind: "term", term: task.terms[task.index]! },
          );
        }
        continue;
      }
      if (task.kind === "sequenceNode") {
        const node = {
          head: roots.pop()!,
          tail: task.tail,
          length: BigInt(task.terms.length - task.index),
        };
        const tail = hashMidgardCekSequenceNode(node);
        addMaterial("sequence", tail, encodeMidgardCekSequenceNode(node));
        pending.push({
          ...task,
          kind: "sequence",
          index: task.index - 1,
          tail,
        });
        continue;
      }
      if (task.kind === "parent") {
        const { term } = task;
        if (
          !constrAndCaseAdmitted &&
          (term instanceof Constr || term instanceof Case)
        ) {
          throw new Error("UPLC 1.0.0 does not admit constr or case terms");
        }
        let node: MidgardCekTermNode;
        if (term instanceof Delay) {
          node = { kind: "delay", body: roots.pop()! };
        } else if (term instanceof Lambda) {
          node = { kind: "lambda", body: roots.pop()! };
        } else if (term instanceof Application) {
          const argument = roots.pop()!;
          node = { kind: "application", function: roots.pop()!, argument };
        } else if (term instanceof Force) {
          node = { kind: "force", term: roots.pop()! };
        } else if (term instanceof Constr) {
          node = {
            kind: "constr",
            tag: term.index,
            termsCount: BigInt(term.terms.length),
            termsRoot: roots.pop()!,
          };
        } else {
          const branchesRoot = roots.pop()!;
          node = {
            kind: "case",
            scrutinee: roots.pop()!,
            branchesCount: BigInt(term.continuations.length),
            branchesRoot,
          };
        }
        roots.push(addTermNode(node));
        continue;
      }

      const { term } = task;
      if (
        !constrAndCaseAdmitted &&
        (term instanceof Constr || term instanceof Case)
      ) {
        throw new Error("UPLC 1.0.0 does not admit constr or case terms");
      }
      if (term instanceof UPLCVar) {
        roots.push(
          addTermNode({ kind: "variable", index: BigInt(term.deBruijn) + 1n }),
        );
      } else if (term instanceof UPLCConst) {
        roots.push(
          addTermNode({ kind: "constant", value: addConstantValue(term) }),
        );
      } else if (term instanceof ErrorUPLC) {
        roots.push(addTermNode({ kind: "error" }));
      } else if (term instanceof Builtin) {
        // Type-instantiation forces are ordinary Force nodes around the builtin.
        roots.push(
          addTermNode({ kind: "builtin", tag: BigInt(term.builtinTag) }),
        );
      } else if (term instanceof Delay) {
        pending.push(
          { kind: "parent", term },
          { kind: "term", term: term.delayedTerm },
        );
      } else if (term instanceof Lambda) {
        pending.push(
          { kind: "parent", term },
          { kind: "term", term: term.body },
        );
      } else if (term instanceof Application) {
        pending.push(
          { kind: "parent", term },
          { kind: "term", term: term.arg },
          { kind: "term", term: term.func },
        );
      } else if (term instanceof Force) {
        pending.push(
          { kind: "parent", term },
          { kind: "term", term: term.forced },
        );
      } else if (term instanceof Constr) {
        pending.push({ kind: "parent", term }, sequence(term.terms));
      } else if (term instanceof Case) {
        pending.push({ kind: "parent", term }, sequence(term.continuations), {
          kind: "term",
          term: term.constrTerm,
        });
      } else {
        throw new Error("unsupported UPLC term in V1 canonicalizer");
      }
    }
    return roots.pop()!;
  };

  const termRoot = addTerm(program.body);
  verifyMidgardCekProgramMaterial(termRoot, material.values());
  return Object.freeze({
    uplcVersion: [
      BigInt(program.version.major),
      BigInt(program.version.minor),
      BigInt(program.version.patch),
    ] as const,
    termRoot,
    material,
    constantWitnesses,
  });
};
