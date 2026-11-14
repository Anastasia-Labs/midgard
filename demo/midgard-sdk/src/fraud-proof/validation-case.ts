import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { type MidgardValidationDispute } from "@al-ft/midgard-core/validation-dispute";
import { Data } from "@lucid-evolution/lucid";

import { H32Schema, ProofSchema, PubKeyHashSchema } from "../common.js";
import { EventKeySchema, HeaderHashSchema } from "../ledger-state.js";
import {
  ValidationDisputeTurnSchema,
  ValidationMachineStateSchema,
  ValidationTraceProofSchema,
} from "./validation-dispute.validation-machine-phase-schema.js";
import { bytesHex } from "./validation-dispute.validation-machine-state-data-from-core.js";

const ScriptHashSchema = Data.Bytes({ minLength: 28, maxLength: 28 });

/** `midgard/validation_case_v1.CaseId`: the case NFT's asset name. */
export const CaseIdSchema = H32Schema;

/** `midgard/validation_dispute_v1.DisputeWinner`. */
export const DisputeWinnerSchema = Data.Enum([
  Data.Literal("DefenderWins"),
  Data.Literal("ChallengerWins"),
  Data.Literal("NeitherClaimValid"),
]);

export type DisputeWinner = Data.Static<typeof DisputeWinnerSchema>;

export const DisputeWinner = asDataType<DisputeWinner>(DisputeWinnerSchema);

/** `midgard/validation_case_v1.CaseV1`: the case datum. */
export const CaseSchema = Data.Object({
  version: Data.Integer(),
  block_header_hash: HeaderHashSchema,
  event_key: EventKeySchema,
  challenger_root: H32Schema,
  step_count: Data.Integer(),
  challenger_terminal_hash: H32Schema,
  bounty_claimant: PubKeyHashSchema,
  agreed_initial_hash: H32Schema,
  started_at: Data.Integer(),
  open_threads: Data.Integer(),
  defenders_root: Data.Bytes(),
  defender_won: Data.Boolean(),
  neither_claim_valid: Data.Boolean(),
});

export type Case = Data.Static<typeof CaseSchema>;

export const Case = asDataType<Case>(CaseSchema);

/**
 * `midgard/validation_case_v1.ThreadHeaderV1`: the first field of the state of
 * every thread holder datum, read by the thread expiry script.
 */
export const ThreadHeaderSchema = Data.Object({
  started_at: Data.Integer(),
  defender_vkh: PubKeyHashSchema,
});

export type ThreadHeader = Data.Static<typeof ThreadHeaderSchema>;

export const ThreadHeader = asDataType<ThreadHeader>(ThreadHeaderSchema);

/** `midgard/validation_case_v1.CaseThreadV1`: one defender's thread. */
export const CaseThreadSchema = Data.Object({
  header: ThreadHeaderSchema,
  version: Data.Integer(),
  case_id: CaseIdSchema,
  challenger_root: H32Schema,
  defender_root: H32Schema,
  step_count: Data.Integer(),
  low_index: Data.Integer(),
  high_index: Data.Integer(),
  agreed_low_hash: H32Schema,
  defender_high_hash: H32Schema,
  challenger_high_hash: H32Schema,
  round: Data.Integer(),
  turn_started_at: Data.Integer(),
  defender_clock_ms: Data.Integer(),
  challenger_clock_ms: Data.Integer(),
  turn: ValidationDisputeTurnSchema,
});

export type CaseThread = Data.Static<typeof CaseThreadSchema>;

export const CaseThread = asDataType<CaseThread>(CaseThreadSchema);

/** The thread datum at the game, timeout and boundary validators. */
export const CaseThreadDatumSchema = Data.Object({
  fraud_prover: PubKeyHashSchema,
  data: Data.Nullable(CaseThreadSchema),
});

export type CaseThreadDatum = Data.Static<typeof CaseThreadDatumSchema>;

export const CaseThreadDatum = asDataType<CaseThreadDatum>(
  CaseThreadDatumSchema,
);

/** `midgard/validation_case_v1.LostDefenseReceiptV1`. */
export const LostDefenseReceiptSchema = Data.Object({
  case_id: CaseIdSchema,
  defender_vkh: PubKeyHashSchema,
  outcome: DisputeWinnerSchema,
  bounty_claimant: PubKeyHashSchema,
});

export type LostDefenseReceipt = Data.Static<typeof LostDefenseReceiptSchema>;

export const LostDefenseReceipt = asDataType<LostDefenseReceipt>(
  LostDefenseReceiptSchema,
);

/** `midgard/validation_case_v1.CaseRedeemerV1`, for mint and spend alike. */
export const CaseRedeemerSchema = Data.Enum([
  Data.Object({
    VerifySource: Data.Object({
      opener_input_index: Data.Integer(),
      case_output_index: Data.Integer(),
    }),
  }),
  Data.Object({
    Defend: Data.Object({
      case_output_index: Data.Integer(),
      thread_output_index: Data.Integer(),
      active_node_input_index: Data.Integer(),
      defender_vkh: PubKeyHashSchema,
      defenders_proof: ProofSchema,
      defender_root: H32Schema,
      defender_state: ValidationMachineStateSchema,
      defender_state_proof: ValidationTraceProofSchema,
    }),
  }),
  Data.Object({
    ThreadClose: Data.Object({
      case_output_index: Data.Integer(),
      thread_input_index: Data.Integer(),
      outcome: DisputeWinnerSchema,
      defender_vkh: PubKeyHashSchema,
    }),
  }),
  Data.Object({
    CaseClose: Data.Object({
      bond_input_index: Data.Integer(),
      bond_output_index: Data.Integer(),
      remainder_output_index: Data.Integer(),
    }),
  }),
  Data.Literal("BurnThreadToken"),
]);

export type CaseRedeemer = Data.Static<typeof CaseRedeemerSchema>;

export const CaseRedeemer = asDataType<CaseRedeemer>(CaseRedeemerSchema);

/**
 * `midgard/validation_case_config_v1.CaseConfigV1`: the inline datum beside
 * the case configuration token, from which the case reads its hand-off
 * scripts.
 */
export const CaseConfigSchema = Data.Object({
  source_v1: ScriptHashSchema,
  game_v1: ScriptHashSchema,
  boundary_v1: ScriptHashSchema,
  award_v1: ScriptHashSchema,
  timeout_v1: ScriptHashSchema,
  dispute_bond_v1: ScriptHashSchema,
  thread_expiry_v1: ScriptHashSchema,
});

export type CaseConfig = Data.Static<typeof CaseConfigSchema>;

export const CaseConfig = asDataType<CaseConfig>(CaseConfigSchema);

/** `midgard/validation_award_v1.SpendRedeemer`. */
export const ValidationAwardSpendRedeemerSchema = Data.Enum([
  Data.Object({
    CloseAwardedThread: Data.Object({
      input_index: Data.Integer(),
      case_input_index: Data.Integer(),
    }),
  }),
  Data.Literal("SpendReceipt"),
]);

export type ValidationAwardSpendRedeemer = Data.Static<
  typeof ValidationAwardSpendRedeemerSchema
>;

export const ValidationAwardSpendRedeemer =
  asDataType<ValidationAwardSpendRedeemer>(ValidationAwardSpendRedeemerSchema);

/** `midgard/operator_directory.SlashingDirectory`. */
export const SlashingDirectorySchema = Data.Enum([
  Data.Literal("ActiveOperatorsDirectory"),
  Data.Literal("RetiredOperatorsDirectory"),
]);

export type SlashingDirectory = Data.Static<typeof SlashingDirectorySchema>;

/** `midgard/validation_award_v1.ReceiptBurnPermissionV1`. */
export const ReceiptBurnPermissionSchema = Data.Enum([
  Data.Object({
    SlashedForThisLoss: Data.Object({ directory: SlashingDirectorySchema }),
  }),
  Data.Object({
    DefenderAlreadySlashed: Data.Object({
      active_operators_element_ref_input_index: Data.Integer(),
      retired_operators_element_ref_input_index: Data.Integer(),
    }),
  }),
]);

/**
 * `midgard/validation_award_v1.ReceiptRedeemerV1`: the award validator's mint
 * purpose, the lost-defense receipt policy.
 */
export const ReceiptRedeemerSchema = Data.Enum([
  Data.Object({
    MintReceipt: Data.Object({ case_input_index: Data.Integer() }),
  }),
  Data.Object({
    BurnReceipt: Data.Object({
      receipt_input_index: Data.Integer(),
      hub_oracle_ref_input_index: Data.Integer(),
      permission: ReceiptBurnPermissionSchema,
    }),
  }),
]);

export type ReceiptRedeemer = Data.Static<typeof ReceiptRedeemerSchema>;

export const ReceiptRedeemer = asDataType<ReceiptRedeemer>(
  ReceiptRedeemerSchema,
);

// The timeout redeemer has one Aiken constructor. Lucid unwraps a one-member
// Data.Enum, so its fields are modelled as a record; Data.Object still emits
// the constructor-0 wire shape.
/** `fraud_proofs/validation_trace/timeout_v1.SpendRedeemer`. */
export const ValidationTimeoutSpendRedeemerSchema = Data.Object({
  input_index: Data.Integer(),
  case_input_index: Data.Integer(),
});

export type ValidationTimeoutSpendRedeemer = Data.Static<
  typeof ValidationTimeoutSpendRedeemerSchema
>;

export const ValidationTimeoutSpendRedeemer =
  asDataType<ValidationTimeoutSpendRedeemer>(
    ValidationTimeoutSpendRedeemerSchema,
  );

/**
 * The thread datum state for an off-chain bisection: the dispute's operator
 * side is the thread's defender. Both roots and the step count come from the
 * two trace descriptors.
 */
export const validationCaseThreadDataFromCore = ({
  dispute,
  caseId,
  startedAt,
  defenderVkh,
}: {
  readonly dispute: MidgardValidationDispute;
  readonly caseId: string;
  readonly startedAt: bigint;
  readonly defenderVkh: string;
}): CaseThread => ({
  header: { started_at: startedAt, defender_vkh: defenderVkh },
  version: BigInt(dispute.version),
  case_id: caseId,
  challenger_root: bytesHex(dispute.challengerDescriptor.traceRoot),
  defender_root: bytesHex(dispute.operatorDescriptor.traceRoot),
  step_count: BigInt(dispute.challengerDescriptor.stepCount),
  low_index: BigInt(dispute.lowIndex),
  high_index: BigInt(dispute.highIndex),
  agreed_low_hash: bytesHex(dispute.agreedLowHash),
  defender_high_hash: bytesHex(dispute.operatorHighHash),
  challenger_high_hash: bytesHex(dispute.challengerHighHash),
  round: BigInt(dispute.round),
  turn_started_at: BigInt(dispute.turnStartedAt),
  defender_clock_ms: BigInt(dispute.operatorClockMs),
  challenger_clock_ms: BigInt(dispute.challengerClockMs),
  turn:
    dispute.turn.type === "awaitingOperator"
      ? { AwaitingOperator: { midpoint: BigInt(dispute.turn.midpoint) } }
      : dispute.turn.type === "awaitingChallenger"
        ? {
            AwaitingChallenger: {
              midpoint: BigInt(dispute.turn.midpoint),
              operator_midpoint_hash: bytesHex(
                dispute.turn.operatorMidpointHash,
              ),
            },
          }
        : "ReadyForOneStep",
});
