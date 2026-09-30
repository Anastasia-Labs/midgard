import {
  castConfirmedStateToData,
  encodeLinkedListNodeView,
  makeGenesisConfirmedState,
  STATE_QUEUE_NODE_ASSET_NAME_PREFIX,
  STATE_QUEUE_ROOT_ASSET_NAME,
} from "@al-ft/midgard-sdk";
import { CML, toUnit } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  computeFraudProofRawL1PointId,
  deriveAuthenticatedStateQueueHeaderObservationFromRawL1,
  type FraudProofRawL1Snapshot,
  type FraudProofRawL1Utxo,
} from "../src/workflow/index.js";
import {
  fixture,
  hash32,
  input,
  output,
  raw,
} from "./support/raw-l1-terminal-fixture.js";

describe("raw L1 live header observation", () => {
  it("binds a live header to its NFT mint even after a later transaction re-created its output", async () => {
    const { snapshot, definition } = await fixture();
    const target = snapshot.transactions[0]!.resolvedInputs[0]!;
    const targetOutput = CML.TransactionOutput.from_cbor_hex(target.outputCbor);
    const stateUnit = toUnit(
      definition.stateQueue.policyId,
      STATE_QUEUE_NODE_ASSET_NAME_PREFIX + definition.headerHash,
    );
    const mintOutputs = CML.TransactionOutputList.new();
    mintOutputs.add(targetOutput);
    const mintBody = CML.TransactionBody.new(
      CML.TransactionInputList.new(),
      mintOutputs,
      0n,
    );
    const mint = CML.Mint.new();
    mint.set(
      CML.ScriptHash.from_hex(definition.stateQueue.policyId),
      CML.AssetName.from_hex(stateUnit.slice(56)),
      1n,
    );
    mintBody.set_mint(mint);
    const mintTxHash = CML.hash_transaction(mintBody).to_hex();
    const commitPointInput = {
      slot: "900",
      blockHash: hash32("53"),
      blockNo: "60",
    };
    const commitPoint = {
      ...commitPointInput,
      pointId: computeFraudProofRawL1PointId(commitPointInput),
    };
    const template = snapshot.transactions[0]!;
    const commit = {
      ...template,
      txHash: mintTxHash,
      bodyCbor: mintBody.to_canonical_cbor_hex(),
      inclusionPoint: commitPoint,
      confirmationDepth: 41,
      resolvedInputs: [],
      resolvedReferenceInputs: [],
    };
    const recreateInputs = CML.TransactionInputList.new();
    recreateInputs.add(input(`${mintTxHash}#0`));
    const recreateOutputs = CML.TransactionOutputList.new();
    recreateOutputs.add(targetOutput);
    const recreateBody = CML.TransactionBody.new(
      recreateInputs,
      recreateOutputs,
      0n,
    );
    const recreateTxHash = CML.hash_transaction(recreateBody).to_hex();
    const attestation = {
      ...template,
      txHash: recreateTxHash,
      bodyCbor: recreateBody.to_canonical_cbor_hex(),
      resolvedInputs: [raw(`${mintTxHash}#0`, targetOutput)],
      resolvedReferenceInputs: [],
    };
    const root = raw(
      `${hash32("44")}#0`,
      output({
        address: definition.stateQueue.address,
        assets: {
          lovelace: 3_000_000n,
          [toUnit(definition.stateQueue.policyId, STATE_QUEUE_ROOT_ASSET_NAME)]:
            1n,
        },
        datum: encodeLinkedListNodeView({
          key: "Empty",
          next: { Key: { key: definition.headerHash } },
          data: castConfirmedStateToData(
            makeGenesisConfirmedState(0n),
          ) as never,
        }),
      }),
    );
    const live = (
      headerUtxo: FraudProofRawL1Utxo,
      transactions: FraudProofRawL1Snapshot["transactions"],
    ): FraudProofRawL1Snapshot => ({
      ...snapshot,
      scopes: snapshot.scopes.map((scope) =>
        scope.role === "state_queue"
          ? { ...scope, utxos: [root, headerUtxo] }
          : scope,
      ) as FraudProofRawL1Snapshot["scopes"],
      transactions,
    });
    const observed =
      await deriveAuthenticatedStateQueueHeaderObservationFromRawL1({
        snapshot: live(raw(`${recreateTxHash}#0`, targetOutput), [
          commit,
          attestation,
        ]),
        definition,
      });
    expect(observed).toMatchObject({
      headerHash: definition.headerHash,
      chainPoint: { slot: 900n, blockHash: hash32("53") },
      confirmationDepth: 41,
    });
    await expect(
      deriveAuthenticatedStateQueueHeaderObservationFromRawL1({
        snapshot: live(raw(`${mintTxHash}#0`, targetOutput), [commit]),
        definition,
      }),
    ).resolves.toEqual(observed);
    await expect(
      deriveAuthenticatedStateQueueHeaderObservationFromRawL1({
        snapshot: live(raw(`${recreateTxHash}#0`, targetOutput), [attestation]),
        definition,
      }),
    ).rejects.toThrow("one authenticated NFT mint");
  });
});
