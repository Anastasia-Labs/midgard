/**
 * Reads a script's Flat bytes exactly as the Cardano ledger does for Plutus V3
 * at protocol version 11 (plutus-core 1.71: `scriptCBORDecoder`,
 * SerialisedScript.hs:194-239, after the CBOR wrapper is removed). Every
 * program Cardano decodes is read to the program Cardano read, and every
 * program it refuses is refused, with the class and bit offset of the
 * refusal, with one exception: a Data constant with bytes after its CBOR item
 * (see `readValue` in cek-program.cardano-flat.constant.ts). The term is read
 * with explicit stacks, so input nesting never deepens the JavaScript stack.
 */
import {
  Application,
  Builtin,
  Case,
  Constr,
  Delay,
  ErrorUPLC,
  Force,
  Lambda,
  UPLCProgram,
  type UPLCTerm,
  UPLCVar,
} from "@harmoniclabs/uplc";

import { readConstant } from "./cek-program.cardano-flat.constant.js";
import {
  CardanoFlatReader,
  CardanoFlatRefusal,
  type CardanoFlatRefusalClass,
} from "./cek-program.cardano-flat.reader.js";

/** Builtins of batches 1 to 6 (tags 0 to 100), available to V3 at PV11. */
const LAST_AVAILABLE_BUILTIN = 100;
/** Highest builtin tag plutus-core 1.71 decodes (Builtins.hs:2694-2802). */
const LAST_KNOWN_BUILTIN = 105;
/** `mbConstr` at protocol version 11 (SerialisedScript.hs:227-234). */
const MAX_CONSTR_FIELDS = 1024;
/** A harmonic `UPLCVar` holds a safe integer, so wire index - 1 < 2^53. */
const MAX_VAR_INDEX = 1n << 53n;

/** A list whose elements are being read: how many have started. */
interface ListCount {
  count: number;
}

type Task =
  | { readonly op: "term" }
  | { readonly op: "delay" | "force" | "lambda" | "apply" }
  | { readonly op: "next"; readonly list: ListCount }
  | {
      readonly op: "constr";
      readonly list: ListCount;
      readonly index: bigint;
      readonly start: number;
    }
  | { readonly op: "case"; readonly list: ListCount };

const TERM: Task = { op: "term" };

/** The first item Cardano decodes but the profile cannot represent. */
interface ProfileGap {
  readonly bitOffset: number;
  readonly detail: string;
}

const versionAtLeast110 = (version: readonly bigint[]): boolean =>
  version[0]! > 1n || (version[0] === 1n && version[1]! >= 1n);

/**
 * The term (UntypedPlutusCore/Core/Instance/Flat.hs:99-186): a 4-bit tag, then
 * its fields. `gap` receives the first item outside the profile.
 */
const readTerm = (
  reader: CardanoFlatReader,
  version: readonly bigint[],
  gap: (found: ProfileGap) => void,
): UPLCTerm => {
  const tasks: Task[] = [TERM];
  const terms: UPLCTerm[] = [];
  const pop = (): UPLCTerm => terms.pop()!;
  const popMany = (count: number): UPLCTerm[] =>
    terms.splice(terms.length - count, count);
  for (let task = tasks.pop(); task !== undefined; task = tasks.pop()) {
    switch (task.op) {
      case "term":
        readTermTag(reader, version, gap, tasks, terms);
        break;
      case "delay":
        terms.push(new Delay(pop()));
        break;
      case "force":
        terms.push(new Force(pop()));
        break;
      case "lambda":
        terms.push(new Lambda(pop()));
        break;
      case "apply": {
        const arg = pop();
        terms.push(new Application(pop(), arg));
        break;
      }
      case "next":
        // `decodeListWith` (Strict.hs:60-87): 1 = another element, 0 = end.
        if (reader.bit()) {
          task.list.count += 1;
          tasks.push(task, TERM);
        }
        break;
      case "constr":
        if (task.list.count > MAX_CONSTR_FIELDS)
          reader.refuse(
            "constrTooManyFields",
            `constr with ${task.list.count} fields`,
            task.start,
          );
        terms.push(new Constr(task.index, popMany(task.list.count)));
        break;
      case "case": {
        const branches = popMany(task.list.count);
        terms.push(new Case(pop(), branches));
        break;
      }
    }
  }
  return terms.pop()!;
};

/** Reads one term tag and schedules the reading of its fields. */
const readTermTag = (
  reader: CardanoFlatReader,
  version: readonly bigint[],
  gap: (found: ProfileGap) => void,
  tasks: Task[],
  terms: UPLCTerm[],
): void => {
  const start = reader.bitOffset;
  const tag = reader.bits(4);
  switch (tag) {
    case 0: {
      // A DeBruijn index as a Word64 (FlatInstances.hs:362-365).
      const index = reader.word64();
      if (index === 0n || index > MAX_VAR_INDEX) {
        gap({ bitOffset: start + 4, detail: `variable index ${index}` });
        terms.push(new ErrorUPLC());
      } else terms.push(new UPLCVar(index - 1n));
      return;
    }
    case 1:
      tasks.push({ op: "delay" }, TERM);
      return;
    case 2:
      // The binder is 0 bits (FlatInstances.hs:375-378).
      tasks.push({ op: "lambda" }, TERM);
      return;
    case 3:
      tasks.push({ op: "apply" }, TERM, TERM);
      return;
    case 4: {
      const constant = readConstant(reader);
      if (constant.outsideProfile !== undefined)
        gap({ bitOffset: start + 4, detail: constant.outsideProfile });
      terms.push(constant.term);
      return;
    }
    case 5:
      tasks.push({ op: "force" }, TERM);
      return;
    case 6:
      terms.push(new ErrorUPLC());
      return;
    case 7: {
      const builtin = reader.bits(7);
      if (builtin > LAST_KNOWN_BUILTIN)
        reader.refuse("unknownBuiltin", `builtin tag ${builtin}`, start + 4);
      if (builtin > LAST_AVAILABLE_BUILTIN)
        reader.refuse(
          "builtinNotAvailable",
          `builtin tag ${builtin} is not available to V3 at PV11`,
          start + 4,
        );
      terms.push(new Builtin(builtin));
      return;
    }
    case 8:
    case 9: {
      if (!versionAtLeast110(version))
        reader.refuse(
          "constrOrCaseBeforeV110",
          `${tag === 8 ? "constr" : "case"} in a program below 1.1.0`,
          start,
        );
      const list: ListCount = { count: 0 };
      if (tag === 8)
        tasks.push(
          { op: "constr", list, index: reader.word64(), start },
          { op: "next", list },
        );
      else tasks.push({ op: "case", list }, { op: "next", list }, TERM);
      return;
    }
    default:
      reader.refuse("unknownTermTag", `term tag ${tag}`, start);
  }
};

/** The outcome of reading a Flat program. */
export type CardanoFlatDecodeResult =
  | { readonly ok: true; readonly program: UPLCProgram }
  | {
      readonly ok: false;
      readonly refusal: CardanoFlatRefusalClass;
      /** Bit offset from the start of the Flat bytes. */
      readonly bitOffset: number;
      readonly detail: string;
    };

const readProgram = (flat: Uint8Array): UPLCProgram => {
  const reader = new CardanoFlatReader(flat);
  let firstGap: ProfileGap | undefined;
  const gap = (found: ProfileGap): void => {
    firstGap ??= found;
  };
  // The version: three Naturals (FlatInstances.hs:177-179).
  const version: bigint[] = [];
  for (let component = 0; component < 3; component += 1) {
    const start = reader.bitOffset;
    const number = reader.natural();
    if (number > 255n)
      gap({ bitOffset: start, detail: `version number ${number}` });
    version.push(number);
  }
  const body = readTerm(reader, version, gap);
  reader.finish();
  if (firstGap !== undefined)
    reader.refuse("outsideProfile", firstGap.detail, firstGap.bitOffset);
  return new UPLCProgram(
    version.map((number) => Number(number)) as [number, number, number],
    body,
  );
};

/**
 * Reads a Flat program as Cardano does. Refused programs come back as the
 * refusal class, the bit offset of the refused item and a detail; any other
 * error is a defect and is thrown.
 */
export const decodeCardanoFlatProgram = (
  flat: Uint8Array,
): CardanoFlatDecodeResult => {
  try {
    return { ok: true, program: readProgram(flat) };
  } catch (error) {
    if (!(error instanceof CardanoFlatRefusal)) throw error;
    return {
      ok: false,
      refusal: error.refusal,
      bitOffset: error.bitOffset,
      detail: error.detail,
    };
  }
};

/**
 * Reads a Flat program as Cardano does, or throws the `CardanoFlatRefusal`.
 */
export const decodeCardanoFlatProgramOrThrow = (
  flat: Uint8Array,
): UPLCProgram => readProgram(flat);
