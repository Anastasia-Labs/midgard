import type { MidgardConsensusProfile } from "@al-ft/midgard-core/consensus-profile";
import type {
  DaPayloadEntry,
  EventKey,
  ValidationTraceDescriptor,
} from "@al-ft/midgard-sdk";
import type { buildDeterministicValidationMachineTrace } from "@al-ft/midgard-validation";
import type { Effect } from "effect";

type Replay = Parameters<typeof buildDeterministicValidationMachineTrace>[0];
export declare const buildDeterministicValidationTraceMembers: (input: {
  readonly consensusProfile: MidgardConsensusProfile;
  readonly blockEndTime: Date;
  readonly expectedNetworkId: bigint;
  readonly minFeeA: bigint;
  readonly minFeeB: bigint;
  readonly blockSlot: bigint;
  readonly transactions: readonly (Replay & {
    readonly eventKey: EventKey;
    readonly ledgerOps: Replay["expectedLedgerOps"];
    readonly verdict: Replay["expectedVerdict"];
    readonly rejectionCode: Replay["expectedRejectionCode"];
  })[];
}) => Effect.Effect<
  readonly {
    readonly keyCbor: Buffer;
    readonly valueCbor: Buffer;
    readonly value: ValidationTraceDescriptor;
    readonly witnesses: readonly DaPayloadEntry[];
  }[],
  unknown
>;
