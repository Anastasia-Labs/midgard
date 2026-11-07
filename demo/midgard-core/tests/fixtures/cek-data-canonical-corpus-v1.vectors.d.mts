/**
 * Types for `cek-data-canonical-corpus-v1.vectors.mjs` (plain ESM so the
 * golden generator loads it on bare `node`; the package sets no `allowJs`).
 */

export declare const stages: Readonly<{
  readonly head: 0;
  readonly integer: 1;
  readonly bytes: 2;
  readonly largeConstructor: 3;
  readonly largeFields: 4;
  readonly close: 5;
  readonly fold: 6;
}>;

export type CekDataCanonicalCorpusVector =
  | {
      readonly name: string;
      readonly hex: string;
      readonly verdict: "accepted";
    }
  | {
      readonly name: string;
      readonly hex: string;
      readonly verdict: "unreadable";
      readonly stage: 0 | 1 | 2 | 3 | 4 | 5 | 6;
    };

export declare const vectors: readonly CekDataCanonicalCorpusVector[];
