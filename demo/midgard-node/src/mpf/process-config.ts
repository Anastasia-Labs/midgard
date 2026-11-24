/**
 * Configuration of the commit-time MPF pipeline.
 */

import { type L1SlotConfig } from "@al-ft/midgard-core/l1-slot-config";
import type { Ledger } from "@al-ft/midgard-fault-proofs/ledger-store";
import { Effect } from "effect";

import { DatabaseError } from "../database/utils/common.js";
import type { ContractDeploymentIdentityValue } from "../services/midgard-contracts.js";
import { MpfError } from "./errors.js";
import { type NativeMpfBuildContext } from "./transition-trace.js";
import {
  type RetainedValidationTraceMember,
  type ValidationTraceBuildInput,
} from "./validation-trace.js";

export type ProcessMpfsConfig = {
  readonly consensusProfile?: ContractDeploymentIdentityValue["consensusProfile"];
  readonly forcedValidation?: {
    readonly expectedNetworkId: bigint;
    readonly minFeeA: bigint;
    readonly minFeeB: bigint;
    readonly bucketConcurrency: number;
    readonly slotForUnixTime: (unixTimeMs: number) => bigint;
    /** The slot mapping script contexts carry their validity range in. */
    readonly l1SlotConfig: L1SlotConfig;
  };
  readonly currentBlockStartTime?: Date;
  /** Fixed source-owned coverage end; event selection never moves beyond it. */
  readonly fixedBlockEndTime?: Date;
  /** Fixed commit clock used by validity verdicts and the authenticated block slot. */
  readonly fixedBlockTime?: Date;
  readonly processedOnlyEndTime?: Date;
  readonly depositOnlyEndTime?: Date;
  readonly depositVisibilityBarrierTime?: Date;
  readonly txOrderVisibilityBarrierTime?: Date;
  readonly withdrawalVisibilityBarrierTime?: Date;
  readonly recordCorpusPath?: string;
  /** Runs once a commit-stage rejection's mempool_ledger revert commits,
   * with the hex outrefs of the rows it deleted or inserted. */
  readonly onMempoolLedgerReverted?: (
    outRefs: readonly string[],
  ) => Effect.Effect<void>;
  /** Runs when a commit stage holds the build, and once per commit stage
   * whose rejections reach the build's limit. */
  readonly onCommitBuildAlarm?: (
    alarm: "held" | "breaker",
    stage: string,
  ) => Effect.Effect<void>;
  /**
   * Normal transactions a later commit stage could not take, keyed by
   * transaction id hex: they are rejected with the given code before
   * validation, so the block is built without them.
   */
  readonly commitEvictions?: ReadonlyMap<
    string,
    { readonly code: string; readonly detail: string }
  >;
  /** The block's base ledger: the ledger at the native generation's base
   * root, which every phase reads by key. */
  readonly baseLedger: Ledger;
  /** The native owner generation every commit builds its ledger root on. */
  readonly nativeMpf: NativeMpfBuildContext;
  /**
   * V1 blocks must retain a descriptor for every forced and included
   * normal transaction. The provider is deliberately mandatory for that
   * generation: a header must never substitute an empty or synthetic trace
   * when the deterministic validation machine is unavailable.
   */
  readonly validationTraceBuilder?: (
    input: ValidationTraceBuildInput,
  ) => Effect.Effect<
    readonly RetainedValidationTraceMember[],
    MpfError | DatabaseError
  >;
};
