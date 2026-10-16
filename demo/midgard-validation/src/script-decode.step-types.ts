/**
 * The state of the script-decode step machine, as in
 * `onchain/aiken/lib/midgard/script-decode-v1.ak`.
 */
import type {
  MidgardCekBlobFrontier,
  MidgardCekDataSequenceSummary,
  MidgardCekDataSummary,
} from "@al-ft/midgard-core";

export type Summary = MidgardCekDataSummary;
export type SequenceSummary = MidgardCekDataSequenceSummary;

export type ConstantType =
  | {
      readonly k:
        | "integer"
        | "bytestring"
        | "string"
        | "unit"
        | "boolean"
        | "data"
        | "g1"
        | "g2"
        | "ml"
        | "value";
    }
  | { readonly k: "list" | "array"; readonly element: ConstantType }
  | {
      readonly k: "pair";
      readonly first: ConstantType;
      readonly second: ConstantType;
    };

export type BlobSink = {
  readonly frontier: MidgardCekBlobFrontier;
  readonly pending: Buffer;
};

export type MagSink =
  | {
      readonly k: "short";
      readonly negative: boolean;
      readonly magnitude: Buffer;
    }
  | {
      readonly k: "long";
      readonly negative: boolean;
      readonly sink: BlobSink;
      readonly part: Buffer;
      readonly total: number;
      readonly first: number;
      readonly ones: boolean;
    };

export type Frame =
  | {
      readonly k:
        | "delay"
        | "lambda"
        | "force"
        | "applyFunction"
        | "caseScrutinee"
        | "dataRoot";
    }
  | { readonly k: "applyArgument"; readonly function: Buffer }
  | { readonly k: "constrFields"; readonly tag: bigint; readonly count: number }
  | {
      readonly k: "caseBranches";
      readonly scrutinee: Buffer;
      readonly count: number;
    }
  | { readonly k: "termChild"; readonly hash: Buffer }
  | { readonly k: "constant"; readonly typeHash: Buffer }
  | {
      readonly k: "valueList";
      readonly element: ConstantType;
      readonly array: boolean;
      readonly count: number;
      readonly memory: bigint;
    }
  | { readonly k: "valueItem"; readonly summary: Summary }
  | { readonly k: "valuePairFirst"; readonly second: ConstantType }
  | {
      readonly k: "valuePairSecond";
      readonly first: Summary;
      readonly memory: bigint;
    }
  | {
      readonly k: "assetOuter";
      readonly count: number;
      readonly previous: Buffer | undefined;
      readonly invalid: boolean;
      readonly total: number;
    }
  | {
      readonly k: "assetInner";
      readonly currency: Buffer;
      readonly count: number;
      readonly previous: Buffer | undefined;
      readonly invalid: boolean;
    }
  | { readonly k: "pairEntry"; readonly key: Summary; readonly value: Summary }
  | {
      readonly k: "dataList";
      /** -1 for an indefinite list. */
      readonly remaining: bigint;
      readonly count: number;
      /** -1 for a plain list. */
      readonly constr: bigint;
      readonly breakAfter: boolean;
    }
  | {
      readonly k: "dataMap";
      /** -1 for an indefinite map. */
      readonly remaining: bigint;
      readonly count: number;
      readonly key: Summary | undefined;
    }
  /**
   * `count` list elements decoded in bulk, from the cons bit at `start`, whose
   * scan spent `units`.
   */
  | {
      readonly k: "valueRun";
      readonly start: number;
      readonly count: number;
      readonly units: number;
    };

export type NatPurpose =
  | { readonly k: "version"; readonly index: number }
  | { readonly k: "integer" }
  | { readonly k: "quantity"; readonly token: Buffer };

export type BytesPurpose = "plain" | "string" | "currency" | "token" | "data";

export type BytesAcc =
  | { readonly k: "blob"; readonly sink: BlobSink; readonly utf8: number }
  | { readonly k: "key"; readonly key: Buffer; readonly length: number };

export type Fold = {
  readonly pairs: boolean;
  readonly remaining: number;
  readonly sequence: SequenceSummary;
};

export type ConstFoldNext =
  | {
      readonly k: "list";
      readonly element: ConstantType;
      readonly array: boolean;
      readonly count: number;
      readonly memory: bigint;
    }
  | {
      readonly k: "assetInner";
      readonly currency: Buffer;
      readonly count: number;
      readonly invalid: boolean;
    }
  | {
      readonly k: "assetOuter";
      readonly invalid: boolean;
      readonly total: number;
    };

export type DataFoldNext =
  | {
      readonly k: "list";
      readonly constr: bigint;
      readonly breakAfter: boolean;
    }
  | { readonly k: "map" };

/** A byte position inside a Flat byte string and the bytes left in its chunk. */
export type Source = { readonly position: number; readonly left: number };

export type DataMode =
  | { readonly k: "item" | "skim" }
  | {
      readonly k: "bytes";
      readonly sink: BlobSink;
      readonly remaining: number;
      readonly indefinite: boolean;
    }
  | {
      readonly k: "bignum";
      readonly magnitude: MagSink;
      readonly remaining: number;
      readonly indefinite: boolean;
    }
  | { readonly k: "fold"; readonly fold: Fold; readonly next: DataFoldNext }
  | { readonly k: "trail"; readonly summary: Summary };

export type Control =
  | {
      readonly k: "nat";
      readonly purpose: NatPurpose;
      readonly start: number;
      readonly groups: number;
      readonly acc: bigint;
      readonly scale: bigint;
      readonly big: boolean;
    }
  | {
      readonly k: "magnitude";
      readonly start: number;
      readonly groups: number;
      readonly emitted: number;
      readonly magnitude: MagSink;
    }
  | {
      readonly k: "term" | "consBit" | "assetOuterBit" | "assetInnerBit";
    }
  | { readonly k: "return"; readonly hash: Buffer }
  | {
      readonly k: "constrFold";
      readonly tag: bigint;
      readonly remaining: number;
      readonly root: Buffer;
      readonly length: number;
    }
  | {
      readonly k: "caseFold";
      readonly scrutinee: Buffer;
      readonly remaining: number;
      readonly root: Buffer;
      readonly length: number;
    }
  | {
      readonly k: "types";
      readonly count: number;
      readonly pending: number;
      readonly top: number;
      readonly bad: boolean;
      readonly tags: readonly number[];
    }
  | { readonly k: "value"; readonly ty: ConstantType }
  | {
      readonly k: "valueReturn";
      readonly summary: Summary;
      readonly memory: bigint;
    }
  | { readonly k: "listBit"; readonly element: ConstantType }
  | { readonly k: "filler"; readonly purpose: BytesPurpose }
  | {
      readonly k: "bytes";
      readonly purpose: BytesPurpose;
      readonly acc: BytesAcc;
    }
  | {
      readonly k: "constFold";
      readonly fold: Fold;
      readonly next: ConstFoldNext;
    }
  | { readonly k: "data"; readonly source: Source; readonly mode: DataMode }
  | { readonly k: "final" | "done"; readonly termRoot: Buffer }
  | { readonly k: "refused"; readonly class: number; readonly offset: number };

export type Machine = {
  readonly cursor: number;
  /** `9 * major + 3 * minor + patch`, each component capped at 2. */
  readonly version: number;
  readonly binders: number;
  readonly constrCaseDepth: number;
  readonly openTerm: boolean;
  readonly stackRoot: Buffer;
  readonly control: Control;
};

export type ScriptDecodeRun = {
  readonly scriptHash: Buffer;
  readonly language: number;
  readonly flatStart: number;
  readonly flatLength: number;
  readonly byteLength: number;
  readonly machine: Machine;
};

export type ScriptDecodeRecord =
  | {
      readonly k: "decoded";
      readonly scriptHash: Buffer;
      readonly language: number;
      readonly programVersion: number;
      readonly termRoot: Buffer;
      readonly phase2Class: number;
    }
  | {
      readonly k: "refused";
      readonly scriptHash: Buffer;
      readonly language: number;
      readonly class: number;
      readonly bitOffset: number;
    };
