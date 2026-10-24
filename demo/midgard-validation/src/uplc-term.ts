/**
 * UPLC terms and programs.
 *
 * Derived from @harmoniclabs/uplc 2.0.7 (Copyright Harmonic Laboratories,
 * Apache License 2.0, https://github.com/HarmonicLabs/uplc). Changes: only the
 * constructors and fields are kept (no `clone`, no pretty printing), and a
 * builtin holds Cardano's builtin tag (`DefaultFun`, 0 to 100) rather than
 * upstream's renumbering.
 */
import type { UPLCConst } from "./uplc-constant.js";
import { UPLCTermTag } from "./uplc-term-tag.js";

export { UPLCTermTag } from "./uplc-term-tag.js";

/** Highest Cardano builtin tag of batches 1 to 6 (`DefaultFun`). */
export const MAX_CARDANO_BUILTIN_TAG = 100;

/** A UPLC term. */
export type UPLCTerm =
  | UPLCVar
  | Delay
  | Lambda
  | Application
  | UPLCConst
  | Force
  | ErrorUPLC
  | Builtin
  | Constr
  | Case;

/** A variable as a 0-based De Bruijn index (Flat spells index + 1). */
export class UPLCVar {
  readonly tag = UPLCTermTag.Var;
  readonly deBruijn: number;

  constructor(deBruijn: number | bigint) {
    const index = Number(deBruijn);
    if (!(Number.isSafeInteger(index) && index >= 0))
      throw new Error(`invalid De Bruijn index ${String(deBruijn)}`);
    this.deBruijn = index;
  }
}

export class Delay {
  readonly tag = UPLCTermTag.Delay;

  constructor(readonly delayedTerm: UPLCTerm) {}
}

export class Lambda {
  readonly tag = UPLCTermTag.Lambda;

  constructor(readonly body: UPLCTerm) {}
}

export class Application {
  readonly tag = UPLCTermTag.Application;

  constructor(
    readonly func: UPLCTerm,
    readonly arg: UPLCTerm,
  ) {}
}

export class Force {
  readonly tag = UPLCTermTag.Force;

  constructor(readonly forced: UPLCTerm) {}
}

export class ErrorUPLC {
  readonly tag = UPLCTermTag.Error;

  constructor(
    readonly msg?: string,
    readonly addInfos?: unknown,
  ) {}
}

/** A builtin, by its Cardano tag. */
export class Builtin {
  readonly tag = UPLCTermTag.Builtin;
  readonly builtinTag: number;

  constructor(builtinTag: number) {
    if (
      !(
        Number.isSafeInteger(builtinTag) &&
        builtinTag >= 0 &&
        builtinTag <= MAX_CARDANO_BUILTIN_TAG
      )
    )
      throw new Error(`unknown builtin tag ${String(builtinTag)}`);
    this.builtinTag = builtinTag;
  }
}

export class Constr {
  readonly tag = UPLCTermTag.Constr;
  readonly index: bigint;

  constructor(
    index: bigint | number,
    readonly terms: UPLCTerm[],
  ) {
    const alternative = BigInt(index);
    if (alternative < 0n)
      throw new Error(`invalid constr index ${String(index)}`);
    this.index = alternative;
  }
}

export class Case {
  readonly tag = UPLCTermTag.Case;

  constructor(
    readonly constrTerm: UPLCTerm,
    readonly continuations: UPLCTerm[],
  ) {}
}

/** A program's version: three numbers of 0 to 255. */
export class UPLCVersion {
  readonly major: number;
  readonly minor: number;
  readonly patch: number;

  constructor(
    major: number | bigint,
    minor: number | bigint,
    patch: number | bigint,
  ) {
    const parts = [major, minor, patch].map(Number);
    if (
      !parts.every((part) => Number.isInteger(part) && (part & 0xff) === part)
    )
      throw new Error("UPLC version numbers must be integers from 0 to 255");
    [this.major, this.minor, this.patch] = parts as [number, number, number];
  }

  toString(): string {
    return `${this.major}.${this.minor}.${this.patch}`;
  }
}

export class UPLCProgram {
  readonly version: UPLCVersion;

  constructor(
    version: UPLCVersion | readonly [number, number, number],
    readonly body: UPLCTerm,
  ) {
    this.version =
      version instanceof UPLCVersion ? version : new UPLCVersion(...version);
  }
}
