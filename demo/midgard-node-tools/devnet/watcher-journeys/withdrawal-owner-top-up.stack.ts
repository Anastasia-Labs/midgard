import type { HistoryPredecessor } from "./history-cases.js";
import {
  awaitStackLedgerOwnerFunded,
  type StackLedgerOwnerFundingPorts,
} from "./stack-ledger-owner.js";
import {
  type OwnedOutput,
  type OwnerFundingRequirement,
  qualifyingOwnedOutput,
  smallestQualifyingOutput,
  type WithdrawalOwnerFundingPorts,
} from "./withdrawal-owner-funding.js";

/**
 * A stack's withdrawal-family funding (`ensureWithdrawalOwnerFunding`): its
 * top-up is the run's own ledger owner funding (`awaitStackLedgerOwnerFunded`)
 * with a deposit of the top-up's amount, recorded in the family's own record,
 * and it counts as absorbed only once our node's ledger and the attested
 * tail each hold an output of the owner's that funds the family. Once the
 * owner is found funded, a top-up still recorded is set aside.
 */
export const stackWithdrawalOwnerFundingPorts = (
  input: Readonly<{
    /** The owner's payment key hash. */
    owner: string;
    readTail(): Promise<HistoryPredecessor>;
    /** Our node's ledger outputs at the owner's wallet address. */
    readOurOutputs(): Promise<readonly OwnedOutput[]>;
    requirement(tail: HistoryPredecessor): Promise<OwnerFundingRequirement>;
    /** The funding's ports for a deposit of `lovelace`. */
    fundingPorts(lovelace: bigint): Promise<StackLedgerOwnerFundingPorts>;
    onStage(name: string): void;
  }>,
): WithdrawalOwnerFundingPorts => ({
  owner: input.owner,
  readTail: input.readTail,
  requirement: input.requirement,
  topUp: async (requirement) => {
    const ports = await input.fundingPorts(requirement.topUpLovelace);
    await awaitStackLedgerOwnerFunded({
      ...ports,
      ourLedgerHoldsOutput: async () =>
        (await smallestQualifyingOutput(
          await input.readOurOutputs(),
          requirement,
        )) !== undefined,
      ownerHoldsOutput: async () =>
        (await qualifyingOwnedOutput(
          await input.readTail(),
          input.owner,
          requirement,
        )) !== undefined,
    });
  },
  settle: async (requirement) => {
    const ports = await input.fundingPorts(requirement.topUpLovelace);
    if ((await ports.readPendingDeposit()) !== undefined)
      await ports.archivePendingDeposit();
  },
  onStage: input.onStage,
});
