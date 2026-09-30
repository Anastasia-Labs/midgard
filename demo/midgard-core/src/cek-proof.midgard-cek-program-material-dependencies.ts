import {
  decodeMidgardCekProgramBlobPreimage,
  isProgramMaterialRoot,
  uniqueProgramMaterialRoots,
} from "./cek-proof.decode-midgard-cek-program-blob-preimage.js";
import {
  encodeMidgardCekProgramMaterialEntry,
  type MidgardCekProgramMaterialEntry,
} from "./cek-proof.decode-midgard-cek-program-envelope.js";
import { decodeMidgardCekProgramMaterialEntry } from "./cek-proof.decode-midgard-cek-program-material-da-entry.js";
import {
  decodeMidgardCekProgramSequencePreimage,
  decodeMidgardCekProgramTermPreimage,
  decodeMidgardCekProgramValuePreimage,
} from "./cek-proof.decode-midgard-cek-program-term-preimage.js";
import { MIDGARD_CEK_EMPTY_SEQUENCE_ROOT } from "./cek-proof.encode-midgard-cek-value-node.js";
import {
  decodeMidgardCekDataListNode,
  decodeMidgardCekDataNode,
  decodeMidgardCekDataPairNode,
  MIDGARD_CEK_EMPTY_DATA_LIST_ROOT,
  MIDGARD_CEK_EMPTY_DATA_PAIR_ROOT,
} from "./cek-semantic.js";
import { type Hash32 } from "./codec/hash.js";

/**
 * Authenticates one V1 material entry and returns its ordered set of direct
 * content-addressed children. Canonical empty roots are implicit sentinels and
 * are therefore validated but never returned as dependencies.
 */
export const midgardCekProgramMaterialDependencies = (
  entry: MidgardCekProgramMaterialEntry,
): readonly Hash32[] => {
  const exact = decodeMidgardCekProgramMaterialEntry(
    encodeMidgardCekProgramMaterialEntry(entry),
  );
  const dependencies: Hash32[] = [];
  const add = (root: Uint8Array): void => {
    dependencies.push(root as Hash32);
  };

  if (exact.kind === "term") {
    const term = decodeMidgardCekProgramTermPreimage(exact.preimage);
    switch (term.kind) {
      case "variable":
      case "error":
      case "builtin":
        break;
      case "unaryTerm":
        add(term.child);
        break;
      case "application":
        add(term.function);
        add(term.argument);
        break;
      case "constant":
        add(term.value);
        break;
      case "contextConstant":
        throw new Error(
          "CEK source-program material contains a runtime-only context constant",
        );
      case "constr":
        if (term.count === 0n) {
          if (
            !isProgramMaterialRoot(
              term.sequence,
              MIDGARD_CEK_EMPTY_SEQUENCE_ROOT,
            )
          ) {
            throw new Error(
              "empty CEK constr sequence must use the canonical empty root",
            );
          }
        } else {
          if (
            isProgramMaterialRoot(
              term.sequence,
              MIDGARD_CEK_EMPTY_SEQUENCE_ROOT,
            )
          ) {
            throw new Error(
              "non-empty CEK constr sequence cannot use the canonical empty root",
            );
          }
          add(term.sequence);
        }
        break;
      case "case":
        add(term.scrutinee);
        if (term.count === 0n) {
          if (
            !isProgramMaterialRoot(
              term.sequence,
              MIDGARD_CEK_EMPTY_SEQUENCE_ROOT,
            )
          ) {
            throw new Error(
              "empty CEK case sequence must use the canonical empty root",
            );
          }
        } else {
          if (
            isProgramMaterialRoot(
              term.sequence,
              MIDGARD_CEK_EMPTY_SEQUENCE_ROOT,
            )
          ) {
            throw new Error(
              "non-empty CEK case sequence cannot use the canonical empty root",
            );
          }
          add(term.sequence);
        }
        break;
    }
    return uniqueProgramMaterialRoots(dependencies);
  }

  if (exact.kind === "value") {
    const value = decodeMidgardCekProgramValuePreimage(exact.preimage);
    if (!isProgramMaterialRoot(value.payloadRoot, value.semanticRoot)) {
      throw new Error(
        "CEK constant payload root must equal its canonical semantic root",
      );
    }
    add(value.typeRoot);
    add(value.semanticRoot);
    return uniqueProgramMaterialRoots(dependencies);
  }

  if (exact.kind === "sequence") {
    const sequence = decodeMidgardCekProgramSequencePreimage(exact.preimage);
    add(sequence.head);
    if (sequence.length === 1n) {
      if (
        !isProgramMaterialRoot(sequence.tail, MIDGARD_CEK_EMPTY_SEQUENCE_ROOT)
      ) {
        throw new Error(
          "one-item CEK sequence must end at the canonical empty root",
        );
      }
    } else {
      if (
        isProgramMaterialRoot(sequence.tail, MIDGARD_CEK_EMPTY_SEQUENCE_ROOT)
      ) {
        throw new Error(
          "multi-item CEK sequence cannot end at the canonical empty root",
        );
      }
      add(sequence.tail);
    }
    return uniqueProgramMaterialRoots(dependencies);
  }

  if (exact.kind === "blobChunk" || exact.kind === "blobBranch") {
    const blob = decodeMidgardCekProgramBlobPreimage(
      exact.kind,
      exact.preimage,
    );
    if (blob.kind === "branch") {
      add(blob.left);
      add(blob.right);
    }
    return uniqueProgramMaterialRoots(dependencies);
  }

  if (exact.kind === "dataNode") {
    const node = decodeMidgardCekDataNode(exact.preimage);
    if (node.kind === "constrSmall" || node.kind === "constrLarge") {
      if (node.fieldsCount === 0n) {
        if (
          !isProgramMaterialRoot(
            node.fieldsRoot,
            MIDGARD_CEK_EMPTY_DATA_LIST_ROOT,
          )
        ) {
          throw new Error(
            "empty CEK Data constructor must use the canonical fields root",
          );
        }
      } else {
        if (
          isProgramMaterialRoot(
            node.fieldsRoot,
            MIDGARD_CEK_EMPTY_DATA_LIST_ROOT,
          )
        ) {
          throw new Error(
            "non-empty CEK Data constructor cannot use the canonical fields root",
          );
        }
        add(node.fieldsRoot);
      }
      if (node.kind === "constrLarge") {
        add(node.constructorCborRoot);
      }
    } else if (node.kind === "map") {
      if (node.entriesCount === 0n) {
        if (
          !isProgramMaterialRoot(
            node.entriesRoot,
            MIDGARD_CEK_EMPTY_DATA_PAIR_ROOT,
          )
        ) {
          throw new Error(
            "empty CEK Data map must use the canonical entries root",
          );
        }
      } else {
        if (
          isProgramMaterialRoot(
            node.entriesRoot,
            MIDGARD_CEK_EMPTY_DATA_PAIR_ROOT,
          )
        ) {
          throw new Error(
            "non-empty CEK Data map cannot use the canonical entries root",
          );
        }
        add(node.entriesRoot);
      }
    } else if (node.kind === "list") {
      if (node.itemsCount === 0n) {
        if (
          !isProgramMaterialRoot(
            node.itemsRoot,
            MIDGARD_CEK_EMPTY_DATA_LIST_ROOT,
          )
        ) {
          throw new Error(
            "empty CEK Data list must use the canonical items root",
          );
        }
      } else {
        if (
          isProgramMaterialRoot(
            node.itemsRoot,
            MIDGARD_CEK_EMPTY_DATA_LIST_ROOT,
          )
        ) {
          throw new Error(
            "non-empty CEK Data list cannot use the canonical items root",
          );
        }
        add(node.itemsRoot);
      }
    } else if (node.kind === "integer") {
      add(node.cborRoot);
    } else {
      add(node.bytesRoot);
    }
    return uniqueProgramMaterialRoots(dependencies);
  }

  if (exact.kind === "dataList") {
    const node = decodeMidgardCekDataListNode(exact.preimage);
    if (node.length === 0n) {
      throw new Error("CEK Data list material length must be positive");
    }
    add(node.head);
    if (node.length === 1n) {
      if (!isProgramMaterialRoot(node.tail, MIDGARD_CEK_EMPTY_DATA_LIST_ROOT)) {
        throw new Error(
          "one-item CEK Data list must end at the canonical empty root",
        );
      }
    } else {
      if (isProgramMaterialRoot(node.tail, MIDGARD_CEK_EMPTY_DATA_LIST_ROOT)) {
        throw new Error(
          "multi-item CEK Data list cannot end at the canonical empty root",
        );
      }
      add(node.tail);
    }
    return uniqueProgramMaterialRoots(dependencies);
  }

  const node = decodeMidgardCekDataPairNode(exact.preimage);
  if (node.length === 0n) {
    throw new Error("CEK Data pair material length must be positive");
  }
  add(node.key);
  add(node.value);
  if (node.length === 1n) {
    if (!isProgramMaterialRoot(node.tail, MIDGARD_CEK_EMPTY_DATA_PAIR_ROOT)) {
      throw new Error(
        "one-item CEK Data pair list must end at the canonical empty root",
      );
    }
  } else {
    if (isProgramMaterialRoot(node.tail, MIDGARD_CEK_EMPTY_DATA_PAIR_ROOT)) {
      throw new Error(
        "multi-item CEK Data pair list cannot end at the canonical empty root",
      );
    }
    add(node.tail);
  }
  return uniqueProgramMaterialRoots(dependencies);
};
