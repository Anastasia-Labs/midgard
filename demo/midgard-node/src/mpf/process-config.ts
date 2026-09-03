/**
 * Configuration of the commit-time MPF pipeline.
 */

import { Effect } from "effect";

import { DatabaseError } from "../database/utils/common.js";
import * as Ledger from "../database/utils/ledger.js";
import type { ContractDeploymentIdentityValue } from "../services/midgard-contracts.js";
import { MpfError } from "./errors.js";
import { type UtxoPayloadSizeAggregate } from "./payload-size.js";
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
  };
  readonly currentBlockStartTime?: Date;
  readonly processedOnlyEndTime?: Date;
  readonly depositOnlyEndTime?: Date;
  readonly depositVisibilityBarrierTime?: Date;
  readonly txOrderVisibilityBarrierTime?: Date;
  readonly withdrawalVisibilityBarrierTime?: Date;
  readonly initialLedgerEntries?: readonly Ledger.MinimalEntry[];
  readonly selectedBaseUtxoRoot?: string;
  readonly payloadRootCheck?: "every_block" | "periodic" | "off";
  readonly baseUtxoPayloadAggregate?: UtxoPayloadSizeAggregate;
  readonly recordCorpusPath?: string;
  readonly excludedDepositEventIds?: ReadonlySet<string>;
  readonly excludedForcedTransactionEventIds?: ReadonlySet<string>;
  readonly excludedWithdrawalEventIds?: ReadonlySet<string>;
  /**
   * A speculative build must leave every durable queue/event row untouched
   * until the submitter owns the state-queue lease. The caller is responsible
   * for applying the returned projection/rejection side effects immediately
   * before submission.
   */
  readonly deferDatabaseWrites?: boolean;
  readonly nativeMpf?: NativeMpfBuildContext;
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
