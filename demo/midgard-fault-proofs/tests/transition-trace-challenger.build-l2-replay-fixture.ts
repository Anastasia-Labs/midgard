import { Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  buildMidgardMpfDeletionOpening,
  parseMidgardMpfProofJson,
} from "@al-ft/midgard-core";
import { encodeCbor } from "@al-ft/midgard-core/codec/cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { h32 } from "@al-ft/midgard-test-support/hex";
import { Data } from "@lucid-evolution/lucid";

import {
  buildPayloadFixture,
  type L2ReplayFixture,
  reconstruct,
  sdkProof,
} from "./transition-trace-challenger.build-payload-fixture.js";
import {
  entry,
  eventToStepEntry,
  LEDGER_OUTPUT_CBOR,
  ledgerTrieValue,
  nativeMaterial,
  spendInputItem,
} from "./transition-trace-challenger.native-material.js";

export const buildL2ReplayFixture = async ({
  matchingCommittedRoot,
  withBranchProof = false,
  survivorCount = withBranchProof ? 16 : 0,
  spendInputCbor,
  outputCbor,
  replayedOutputCbor,
  includeProducedPayloadUtxo = true,
  producedWitnessValue = "descriptor",
}: {
  readonly matchingCommittedRoot: boolean;
  readonly withBranchProof?: boolean;
  /** Ledger entries besides the spent one; 16 with `withBranchProof`. */
  readonly survivorCount?: number;
  readonly spendInputCbor?: Buffer;
  readonly outputCbor?: Buffer;
  readonly replayedOutputCbor?: Buffer;
  readonly includeProducedPayloadUtxo?: boolean;
  readonly producedWitnessValue?: "descriptor" | "fullOutputBytes";
}): Promise<L2ReplayFixture> => {
  const spentKey = spendInputCbor ?? spendInputItem(h32(81), 0);
  const spentOutputCbor = Buffer.from(LEDGER_OUTPUT_CBOR, "hex");
  const spentValue = ledgerTrieValue(spentKey, spentOutputCbor);
  const sourceOutput = outputCbor ?? Buffer.from(LEDGER_OUTPUT_CBOR, "hex");
  const producedOutput = replayedOutputCbor ?? sourceOutput;
  const material = nativeMaterial(82, {
    spendInputsPreimageCbor: encodeCbor([spentKey]),
    outputsPreimageCbor: encodeCbor([sourceOutput]),
  });
  const producedKey = spendInputItem(material.txId, 0);
  const producedValue = ledgerTrieValue(producedKey, producedOutput);
  const producedWitnessBytes =
    producedWitnessValue === "descriptor" ? producedValue : producedOutput;
  const survivors = Array.from({ length: survivorCount }, (_, index) => {
    const key = spendInputItem(h32(100 + index), index);
    return {
      key,
      outputCbor: spentOutputCbor,
      value: ledgerTrieValue(key, spentOutputCbor),
    };
  });

  const ledger = await Trie.fromList([
    { key: spentKey, value: spentValue },
    ...survivors.map(({ key, value }) => ({ key, value })),
  ]);
  const preRoot = ledger.hash.toString("hex");
  const deleteProof = await ledger.prove(spentKey);
  const deletionOpening = await buildMidgardMpfDeletionOpening(
    ledger,
    spentKey,
    parseMidgardMpfProofJson(deleteProof.toJSON()),
  );
  await ledger.delete(spentKey);
  await ledger.insert(producedKey, producedValue);
  const insertProof = await ledger.prove(producedKey);
  const replayedPostRoot = ledger.hash.toString("hex");

  const source: SDK.L2TransactionSource = {
    tx_id: material.txId,
    source: material.source,
  };
  const eventKey: SDK.EventKey = {
    L2TransactionEventKey: { tx_id: material.txId },
  };
  const fixture = await buildPayloadFixture({
    prevUtxosRoot: preRoot,
    utxos: [
      ...survivors.map(
        ({ key, outputCbor: survivorOutputCbor }): SDK.DaPayloadEntry => [
          key.toString("hex"),
          survivorOutputCbor.toString("hex"),
        ],
      ),
      ...(includeProducedPayloadUtxo
        ? ([
            [producedKey.toString("hex"), producedOutput.toString("hex")],
          ] satisfies SDK.DaPayloadEntry[])
        : []),
    ],
    transactions: [
      entry(
        Buffer.from(material.txId, "hex"),
        Buffer.from(Data.to(source, SDK.L2TransactionSource), "hex"),
      ),
    ],
    transactionPreimages: [
      entry(Buffer.from(material.txId, "hex"), material.canonicalCbor),
    ],
    steps: [
      {
        schema_version: 1n,
        step_index: 0n,
        event_key: eventKey,
        phase: "L2Transaction",
        pre_utxos_root: preRoot,
        post_utxos_root: matchingCommittedRoot ? replayedPostRoot : h32(83),
      },
    ],
    eventToStep: [
      eventToStepEntry(eventKey, {
        step_index: 0n,
        phase: "L2Transaction",
      }),
    ],
  });
  const encodedDeleteProof = sdkProof(deleteProof);
  const encodedInsertProof = sdkProof(insertProof);
  return {
    reconstruction: await reconstruct(fixture),
    evidence: {
      stepIndex: 0n,
      spentUtxos: [
        {
          key: spentKey.toString("hex"),
          value: spentValue.toString("hex"),
          opening: deletionOpening.toString("hex"),
          delete_proof: encodedDeleteProof,
        },
      ],
      producedUtxos: [
        {
          key: producedKey.toString("hex"),
          value: producedWitnessBytes.toString("hex"),
          non_membership_proof: encodedInsertProof,
          insert_proof: encodedInsertProof,
        },
      ],
    },
    replayedPostRoot,
  };
};
