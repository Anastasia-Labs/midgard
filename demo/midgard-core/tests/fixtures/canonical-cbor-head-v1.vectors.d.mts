/**
 * Types for `canonical-cbor-head-v1.vectors.mjs` (plain ESM so the golden
 * generator loads it on bare `node`; the package sets no `allowJs`).
 */

export type CanonicalCborHeadRefusalReason =
  | "out-of-range"
  | "truncated"
  | "non-minimal"
  | "reserved"
  | "indefinite";

export declare const reasons: Readonly<{
  readonly outOfRange: "out-of-range";
  readonly truncated: "truncated";
  readonly nonMinimal: "non-minimal";
  readonly reserved: "reserved";
  readonly indefinite: "indefinite";
}>;

export type CanonicalCborHeadResult =
  | {
      readonly verdict: "accept";
      readonly major: number;
      readonly value: bigint;
      readonly next: number;
    }
  | {
      readonly verdict: "refuse";
      readonly reason: CanonicalCborHeadRefusalReason;
    };

export type PermissiveCborHeadResult =
  | { readonly verdict: "accept"; readonly serialised: string }
  | { readonly verdict: "refuse" };

export type CanonicalCborHeadVector = {
  readonly name: string;
  readonly hex: string;
  readonly offset: number;
  readonly canonical: CanonicalCborHeadResult;
  readonly permissive?: PermissiveCborHeadResult;
};

export declare const vectors: readonly CanonicalCborHeadVector[];
