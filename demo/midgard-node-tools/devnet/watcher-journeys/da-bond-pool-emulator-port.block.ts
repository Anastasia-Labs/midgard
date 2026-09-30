import { type UTxO } from "@lucid-evolution/lucid";
import {
  type AvailabilityCommitFixture,
  type AvailabilityCommittedBlock,
  type OpenAvailability,
} from "midgard-node/tests/helpers/availability-challenge-emulator";

import type {
  DaBondPoolJourneyCommitIntent,
  DaBondPoolJourneyPort,
} from "./da-bond-pool-journey.js";

export type DaBondPoolEmulatorPortOptions = Readonly<{
  /**
   * The genesis pool's lovelace. Default `floor + da_bond + 20 ADA`: it backs
   * one bond and fewer than two, so the Timeout leaves it short (step 1).
   */
  poolLovelace?: bigint;
  /** Each committed block's DA payload size. Default 14,021 bytes. */
  payloadBytes?: number;
}>;

export type DaBondPoolEmulatorPort = DaBondPoolJourneyPort & {
  readonly fixture: AvailabilityCommitFixture;
  /** Removes the directory that holds the quorum steps' files. */
  dispose(): void;
};

export type Challenge = {
  readonly open: OpenAvailability;
  readonly record: UTxO;
  readonly threads: UTxO[];
  readonly carriers: (UTxO | undefined)[];
  terminal: UTxO;
};

export type Block = {
  readonly label: string;
  readonly responder: DaBondPoolJourneyCommitIntent["responder"];
  readonly fixture: AvailabilityCommittedBlock;
  /** Init and threshold signatures landed; a later attempt only applies. */
  signed: boolean;
  challenge?: Challenge;
  /** The Timeout burned the node. */
  removed: boolean;
};

export const APPLY_VALIDITY_MS = 60_000n;
