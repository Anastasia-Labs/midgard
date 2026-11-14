import { Data } from "@lucid-evolution/lucid";

import type { ValueNotPreservedContracts } from "./contracts.js";

export type ConservationPosition = Exclude<
  keyof ValueNotPreservedContracts,
  | "steps"
  | "computationThread"
  | "fraudProof"
  | "hubOraclePolicyId"
  | "stateQueuePolicyId"
  | "fieldPreimageCertificatePolicyId"
>;

export type ConservationAction = Readonly<{
  position: ConservationPosition;
  inputState: string;
  nextPosition: ConservationPosition | null;
  outputState: string | null;
  /** Constr 0 family args with neutral layout indexes, rewritten after balancing. */
  args: string;
  fieldIndex?: 0 | 2 | 5;
}>;

export const raw = <T>(value: T, schema: T): Data =>
  Data.from(Data.to(value as never, schema as never));

export const encoded = <T>(value: T, schema: T): string =>
  Data.to(value as never, schema as never);

export const EMPTY_DELTA_ROOT = "00".repeat(32);
