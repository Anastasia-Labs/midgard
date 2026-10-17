/** Narrow first-fault interface consumed by direct script derivation.
 * These are copies of the frozen shared first-fault declarations (same wire
 * schemas and the same coordinate projection), so the encodings agree with
 * the on-chain seam module. When the shared first-fault module carries them,
 * importers switch to it and this module is deleted. */
import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data } from "@lucid-evolution/lucid";

import type { Header } from "../ledger-state.js";
import {
  isRejectionReasonArm,
  type RejectionReason,
} from "../rejection-reason.js";
import type { NativeTxBodyCompact } from "./native.js";

export const NetworkOrderContextSchema = Data.Object({
  body_network_mismatch: Data.Boolean(),
});
export type NetworkOrderContext = Data.Static<typeof NetworkOrderContextSchema>;
export const NetworkOrderContext = asDataType<NetworkOrderContext>(
  NetworkOrderContextSchema,
);

export const PurposeCoordinateSchema = Data.Object({
  kind: Data.Integer(),
  index: Data.Integer(),
});
export type PurposeCoordinate = Data.Static<typeof PurposeCoordinateSchema>;
export const PurposeCoordinate = asDataType<PurposeCoordinate>(
  PurposeCoordinateSchema,
);

const hasExactKeys = (value: unknown, keys: readonly string[]): boolean =>
  typeof value === "object" &&
  value !== null &&
  !Array.isArray(value) &&
  Object.keys(value).length === keys.length &&
  keys.every((key) => Object.prototype.hasOwnProperty.call(value, key));

/** Both projections must already be authenticated under the same source/header. */
export const networkOrderContext = (
  compact: Readonly<{ body: NativeTxBodyCompact }>,
  header: Header,
): NetworkOrderContext => ({
  body_network_mismatch:
    compact.body.network_id !== 255n &&
    compact.body.network_id !== header.expectedNetworkId,
});

const requireKnownArm = (reason: RejectionReason): void => {
  const arm =
    typeof reason === "string"
      ? reason
      : typeof reason === "object" &&
          reason !== null &&
          !Array.isArray(reason) &&
          Object.keys(reason).length === 1
        ? Object.keys(reason)[0]
        : undefined;
  if (arm === undefined || !isRejectionReasonArm(arm))
    throw new Error("first-fault: unknown rejection arm");
};

export const purposeCoordinateOfReason = (
  reason: RejectionReason,
): PurposeCoordinate | null => {
  requireKnownArm(reason);
  if (typeof reason === "string") return null;
  const payload =
    "ScriptSourceMissing" in reason
      ? reason.ScriptSourceMissing
      : "RedeemerMissing" in reason
        ? reason.RedeemerMissing
        : null;
  return payload === null
    ? null
    : { kind: payload.purpose_kind, index: payload.purpose_index };
};

export const purposeCoordinateIsValid = (
  coordinate: PurposeCoordinate,
): boolean =>
  hasExactKeys(coordinate, ["kind", "index"]) &&
  typeof coordinate.kind === "bigint" &&
  typeof coordinate.index === "bigint" &&
  coordinate.kind >= 0n &&
  coordinate.kind <= 3n &&
  coordinate.index >= 0n;
