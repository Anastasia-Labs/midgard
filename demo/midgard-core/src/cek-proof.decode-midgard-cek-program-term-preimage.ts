import {
  assertPreimageConsumed,
  type MidgardCekDecodedProgramSequence,
  type MidgardCekDecodedProgramTerm,
  type MidgardCekDecodedProgramValue,
  readExactHashAt,
} from "./cek-proof.decode-midgard-cek-program-material-da-entry.js";
import {
  boundedBuiltinTag,
  MidgardCekTermTags,
  MidgardCekValueTags,
  uint32,
  uint64,
} from "./cek-proof.encode-midgard-cek-term-node.js";
import { readCborArrayHeader, readCborUnsigned } from "./codec/cbor.js";

export const decodeMidgardCekProgramTermPreimage = (
  preimage: Buffer,
): MidgardCekDecodedProgramTerm => {
  const header = readCborArrayHeader(preimage, 0, "cek_program_term");
  const tag = readCborUnsigned(
    preimage,
    header.nextOffset,
    "cek_program_term.tag",
  );
  switch (tag.value) {
    case MidgardCekTermTags.Variable: {
      if (header.length !== 2) {
        throw new Error("CEK variable term must contain two fields");
      }
      const index = readCborUnsigned(
        preimage,
        tag.nextOffset,
        "cek_program_term.variable.index",
      );
      uint32(index.value, "cek_program_term.variable.index");
      assertPreimageConsumed(preimage, index.nextOffset, "CEK variable term");
      return { kind: "variable", index: index.value };
    }
    case MidgardCekTermTags.Delay:
    case MidgardCekTermTags.Lambda:
    case MidgardCekTermTags.Force: {
      if (header.length !== 2) {
        throw new Error("CEK unary term must contain two fields");
      }
      const child = readExactHashAt(
        preimage,
        tag.nextOffset,
        "cek_program_term.child",
      );
      assertPreimageConsumed(preimage, child.nextOffset, "CEK unary term");
      return {
        kind: "unaryTerm",
        termKind:
          tag.value === MidgardCekTermTags.Delay
            ? "delay"
            : tag.value === MidgardCekTermTags.Lambda
              ? "lambda"
              : "force",
        child: child.value,
      };
    }
    case MidgardCekTermTags.Application: {
      if (header.length !== 3) {
        throw new Error("CEK application term must contain three fields");
      }
      const functionRoot = readExactHashAt(
        preimage,
        tag.nextOffset,
        "cek_program_term.application.function",
      );
      const argument = readExactHashAt(
        preimage,
        functionRoot.nextOffset,
        "cek_program_term.application.argument",
      );
      assertPreimageConsumed(
        preimage,
        argument.nextOffset,
        "CEK application term",
      );
      return {
        kind: "application",
        function: functionRoot.value,
        argument: argument.value,
      };
    }
    case MidgardCekTermTags.Constant: {
      if (header.length !== 2) {
        throw new Error("CEK constant term must contain two fields");
      }
      const value = readExactHashAt(
        preimage,
        tag.nextOffset,
        "cek_program_term.constant.value",
      );
      assertPreimageConsumed(preimage, value.nextOffset, "CEK constant term");
      return { kind: "constant", value: value.value };
    }
    case MidgardCekTermTags.ContextConstant: {
      if (header.length !== 2) {
        throw new Error("CEK context-constant term must contain two fields");
      }
      const value = readExactHashAt(
        preimage,
        tag.nextOffset,
        "cek_program_term.context_constant.value",
      );
      assertPreimageConsumed(
        preimage,
        value.nextOffset,
        "CEK context-constant term",
      );
      return { kind: "contextConstant", value: value.value };
    }
    case MidgardCekTermTags.Error: {
      if (header.length !== 1) {
        throw new Error("CEK error term must contain one field");
      }
      assertPreimageConsumed(preimage, tag.nextOffset, "CEK error term");
      return { kind: "error" };
    }
    case MidgardCekTermTags.Builtin: {
      if (header.length !== 2) {
        throw new Error("CEK builtin term must contain two fields");
      }
      const builtin = readCborUnsigned(
        preimage,
        tag.nextOffset,
        "cek_program_term.builtin.tag",
      );
      boundedBuiltinTag(builtin.value);
      assertPreimageConsumed(preimage, builtin.nextOffset, "CEK builtin term");
      return { kind: "builtin", tag: builtin.value };
    }
    case MidgardCekTermTags.Constr: {
      if (header.length !== 4) {
        throw new Error("CEK constr term must contain four fields");
      }
      const constrTag = readCborUnsigned(
        preimage,
        tag.nextOffset,
        "cek_program_term.constr.tag",
      );
      uint64(constrTag.value, "cek_program_term.constr.tag");
      const count = readCborUnsigned(
        preimage,
        constrTag.nextOffset,
        "cek_program_term.constr.count",
      );
      uint32(count.value, "cek_program_term.constr.count");
      const sequence = readExactHashAt(
        preimage,
        count.nextOffset,
        "cek_program_term.constr.sequence",
      );
      assertPreimageConsumed(preimage, sequence.nextOffset, "CEK constr term");
      return {
        kind: "constr",
        tag: constrTag.value,
        count: count.value,
        sequence: sequence.value,
      };
    }
    case MidgardCekTermTags.Case: {
      if (header.length !== 4) {
        throw new Error("CEK case term must contain four fields");
      }
      const scrutinee = readExactHashAt(
        preimage,
        tag.nextOffset,
        "cek_program_term.case.scrutinee",
      );
      const count = readCborUnsigned(
        preimage,
        scrutinee.nextOffset,
        "cek_program_term.case.count",
      );
      uint32(count.value, "cek_program_term.case.count");
      const sequence = readExactHashAt(
        preimage,
        count.nextOffset,
        "cek_program_term.case.sequence",
      );
      assertPreimageConsumed(preimage, sequence.nextOffset, "CEK case term");
      return {
        kind: "case",
        scrutinee: scrutinee.value,
        count: count.value,
        sequence: sequence.value,
      };
    }
    default:
      throw new Error(`unknown CEK program term tag ${tag.value.toString()}`);
  }
};

export const decodeMidgardCekProgramValuePreimage = (
  preimage: Buffer,
): MidgardCekDecodedProgramValue => {
  const header = readCborArrayHeader(preimage, 0, "cek_program_value");
  if (header.length !== 6) {
    throw new Error("CEK source-program value must be a six-field constant");
  }
  const tag = readCborUnsigned(
    preimage,
    header.nextOffset,
    "cek_program_value.tag",
  );
  if (tag.value !== MidgardCekValueTags.Constant) {
    throw new Error("CEK source-program material may contain only constants");
  }
  const typeRoot = readExactHashAt(
    preimage,
    tag.nextOffset,
    "cek_program_value.constant.type_root",
  );
  const payloadRoot = readExactHashAt(
    preimage,
    typeRoot.nextOffset,
    "cek_program_value.constant.payload_root",
  );
  const payloadLength = readCborUnsigned(
    preimage,
    payloadRoot.nextOffset,
    "cek_program_value.constant.payload_length",
  );
  uint64(payloadLength.value, "cek_program_value.constant.payload_length");
  const semanticRoot = readExactHashAt(
    preimage,
    payloadLength.nextOffset,
    "cek_program_value.constant.semantic_root",
  );
  const memory = readCborUnsigned(
    preimage,
    semanticRoot.nextOffset,
    "cek_program_value.constant.memory",
  );
  uint64(memory.value, "cek_program_value.constant.memory");
  assertPreimageConsumed(preimage, memory.nextOffset, "CEK constant value");
  return {
    typeRoot: typeRoot.value,
    payloadRoot: payloadRoot.value,
    payloadLength: payloadLength.value,
    semanticRoot: semanticRoot.value,
    memory: memory.value,
  };
};

export const decodeMidgardCekProgramSequencePreimage = (
  preimage: Buffer,
): MidgardCekDecodedProgramSequence => {
  const header = readCborArrayHeader(preimage, 0, "cek_program_sequence");
  if (header.length !== 4) {
    throw new Error("CEK program sequence must contain four fields");
  }
  const tag = readCborUnsigned(
    preimage,
    header.nextOffset,
    "cek_program_sequence.tag",
  );
  if (tag.value !== 1n) {
    throw new Error("CEK material cannot encode an explicit empty sequence");
  }
  const head = readExactHashAt(
    preimage,
    tag.nextOffset,
    "cek_program_sequence.head",
  );
  const tail = readExactHashAt(
    preimage,
    head.nextOffset,
    "cek_program_sequence.tail",
  );
  const length = readCborUnsigned(
    preimage,
    tail.nextOffset,
    "cek_program_sequence.length",
  );
  uint32(length.value, "cek_program_sequence.length");
  if (length.value === 0n) {
    throw new Error("CEK non-empty sequence length must be positive");
  }
  assertPreimageConsumed(preimage, length.nextOffset, "CEK program sequence");
  return { head: head.value, tail: tail.value, length: length.value };
};
