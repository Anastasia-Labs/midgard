import {
  type BuildTxWithRedeemer,
  Data,
  Emulator,
  Lucid,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  ActiveOperatorSpendRedeemer,
  CORRECTION_LOCK_ASSET_NAME,
  fetchSortedStateQueueUTxOsProgram,
  hashBlockHeader,
  type Header as HeaderType,
  incompleteEmulatorCommitBlockHeaderTxProgram,
  requireInputIndex,
  requireMintRedeemerIndex,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
  STATE_QUEUE_NODE_ASSET_NAME_PREFIX,
  type StateQueueUTxO,
  utxoToStateQueueUTxO,
} from "../src/index.js";
import { isOnlyLovelace } from "./state-queue.build-test-contracts.js";
import { type StateQueueTestContracts } from "./state-queue.state-queue-operator-funding-inputs.js";

const makeCommitActiveOperatorRedeemer = ({
  contracts,
  operator,
  activeOperatorInput,
  hubOracle,
  continuedActiveOperatorDatum,
}: {
  readonly contracts: StateQueueTestContracts;
  readonly operator: string;
  readonly activeOperatorInput: UTxO;
  readonly hubOracle: UTxO;
  readonly continuedActiveOperatorDatum: string;
}): BuildTxWithRedeemer =>
  ((ctx) =>
    Data.to(
      {
        UpdateBondHoldNewState: {
          active_operator: operator,
          active_node_input_index: requireInputIndex(
            ctx,
            activeOperatorInput,
            "emulator commit active-operator input",
          ),
          active_node_output_index: requireUniqueOutputIndex(
            ctx.outputs,
            (output) =>
              output.address ===
                contracts.activeOperators.spendingScriptAddress &&
              output.datum === continuedActiveOperatorDatum,
            "emulator commit active-operator output",
          ),
          hub_oracle_ref_input_index: requireReferenceInputIndex(
            ctx,
            hubOracle,
            "emulator commit hub oracle",
          ),
          state_queue_redeemer_index: requireMintRedeemerIndex(
            ctx,
            contracts.stateQueue.policyId,
            "emulator commit state queue mint",
          ),
        },
      } satisfies ActiveOperatorSpendRedeemer,
      ActiveOperatorSpendRedeemer,
    )) satisfies BuildTxWithRedeemer;

export const submitCommitHeaderTx = async ({
  emulator,
  lucid,
  contracts,
  anchor,
  header,
  operator,
  scheduler,
  hubOracle,
  correctionLock,
  commitYield,
  activeOperatorInput,
  headerNodeLovelace,
}: {
  readonly emulator: Emulator;
  readonly lucid: Awaited<ReturnType<typeof Lucid>>;
  readonly contracts: StateQueueTestContracts;
  readonly anchor: StateQueueUTxO;
  readonly header: HeaderType;
  readonly operator: string;
  readonly scheduler: UTxO;
  readonly hubOracle: UTxO;
  readonly correctionLock: UTxO;
  readonly commitYield: UTxO;
  readonly activeOperatorInput: UTxO;
  readonly headerNodeLovelace?: bigint;
}): Promise<{
  readonly block: StateQueueUTxO;
  readonly activeOperatorInput: UTxO;
}> => {
  const orderedStateQueue = await Effect.runPromise(
    fetchSortedStateQueueUTxOsProgram(lucid, {
      stateQueueAddress: contracts.stateQueue.spendingScriptAddress,
      stateQueuePolicyId: contracts.stateQueue.policyId,
    }),
  );
  const canonicalTail = orderedStateQueue.at(-1);
  if (
    canonicalTail === undefined ||
    canonicalTail.utxo.txHash !== anchor.utxo.txHash ||
    canonicalTail.utxo.outputIndex !== anchor.utxo.outputIndex
  ) {
    throw new Error("Commit helper received a stale state-queue tail");
  }
  const confirmedStateRefInput =
    orderedStateQueue.length === 1 ? undefined : orderedStateQueue[0]!.utxo;
  const headStateQueueNodeRefInput =
    orderedStateQueue.length <= 2 ? undefined : orderedStateQueue[1]!.utxo;
  const continuedActiveOperatorDatum = Data.void();
  const validityStartSlot = lucid.currentSlot() + 1;
  const commitTx = await Effect.runPromise(
    incompleteEmulatorCommitBlockHeaderTxProgram(
      lucid,
      {
        stateQueueAddress: contracts.stateQueue.spendingScriptAddress,
        stateQueuePolicyId: contracts.stateQueue.policyId,
      },
      {
        anchorUTxO: anchor,
        newHeader: header,
        headerNodeLovelace,
        schedulerRefInput: scheduler,
        correctionLockRefInput: {
          utxo: correctionLock,
          datum: "Idle",
          assetName: CORRECTION_LOCK_ASSET_NAME,
        },
        confirmedStateRefInput,
        headStateQueueNodeRefInput,
        additionalRefInputs: [hubOracle],
        activeOperatorInput,
        validFrom: BigInt(lucid.slotToUnixTime(validityStartSlot)),
        validTo: header.endTime + 1n,
        activeOperatorSpendRedeemer: makeCommitActiveOperatorRedeemer({
          contracts,
          operator,
          activeOperatorInput,
          hubOracle,
          continuedActiveOperatorDatum,
        }),
        activeOperatorSpendingScript: contracts.activeOperators.spendingScript,
        continuedActiveOperatorOutput: {
          address: contracts.activeOperators.spendingScriptAddress,
          datum: continuedActiveOperatorDatum,
          assets: activeOperatorInput.assets,
        },
        stateQueueSpendingScript: contracts.stateQueue.spendingScript,
        stateQueueMintingScript: contracts.stateQueue.mintingScript,
        yieldWitness: {
          referenceInput: commitYield,
          script: contracts.commitYield.spendingScript,
        },
      },
    ),
  );
  const commitUnsigned = await commitTx.complete({ localUPLCEval: true });
  emulator.awaitSlot(1);
  const commitSigned = await commitUnsigned.sign.withWallet().complete();
  await lucid.awaitTx(await commitSigned.submit());

  const headerHash = await Effect.runPromise(hashBlockHeader(header));
  const blockUnit = toUnit(
    contracts.stateQueue.policyId,
    STATE_QUEUE_NODE_ASSET_NAME_PREFIX + headerHash,
  );
  const [blockUtxo] = await lucid.utxosAtWithUnit(
    contracts.stateQueue.spendingScriptAddress,
    blockUnit,
  );
  const nextActiveOperatorInput = (
    await lucid.utxosAt(contracts.activeOperators.spendingScriptAddress)
  ).find(isOnlyLovelace);
  if (blockUtxo === undefined || nextActiveOperatorInput === undefined) {
    throw new Error("Commit transaction did not produce expected UTxOs");
  }
  return {
    block: await Effect.runPromise(
      utxoToStateQueueUTxO(blockUtxo, contracts.stateQueue.policyId),
    ),
    activeOperatorInput: nextActiveOperatorInput,
  };
};
