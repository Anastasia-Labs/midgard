/**
 * mintItemNonCanonical over a mint field that repeats a policy id across
 * adjacent items, or repeats an asset name within one item. Mint policy ids
 * and the names under each must be distinct and ascending; the scan holds
 * both with a strict byte-order comparison.
 *
 * One block accepts a transaction per shape: the small repeats, a repeated
 * last name in the largest item the field admits, and a repeated policy id at
 * the 910th item of a field near its byte bound. Each repeat is convicted from
 * Init through a real step-04 mint, each distinct ascending twin at the same
 * shape is refused at step 04, and the block is removed. A forced transaction
 * cannot carry either repeat: the L1 admission screen refuses its mint.
 */
import { screenMidgardForcedTxAdmission } from "@al-ft/midgard-core";
import { encodeMidgardSpendInputItem } from "@al-ft/midgard-core/codec";
import { acceptedVerdictSubject } from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  submitMintItemNonCanonicalStep01Accepted,
  submitMintItemNonCanonicalStep02,
  submitMintItemNonCanonicalStep03,
  submitMintItemNonCanonicalStep04,
} from "../src/mint-item-non-canonical/index.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { makeFaultProofEmulatorHarness } from "./support/emulator/harness.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import { makeNativeTx } from "./support/emulator/native-tx.js";
import { submitMintStep04Raw } from "./support/mint-item-non-canonical-emulator.js";
import {
  committedInclusions,
  evidenceOf,
  registeredContracts,
  witnessSetCborOf,
} from "./support/mint-item-non-canonical-lifecycle.js";
import {
  maximumCanonicalMintItem,
  mintField,
  mintItem,
} from "./support/mint-item-vectors.js";
import { setupFraudulentBlock } from "./support/submit-init-emulator-fixtures.js";

/** The largest item with its last 32-byte name repeating the one before. */
const maximumItemRepeatingLastName = () => {
  const item = maximumCanonicalMintItem();
  // The last name ends in the big-endian index 934; 933 is the name before.
  expect(item.readUInt16BE(item.length - 3)).toBe(934);
  item.writeUInt16BE(933, item.length - 3);
  return item;
};

/** 910 one-asset items near the field bound; `repeatLast` reuses item 908's id. */
const latePolicyItems = (repeatLast: boolean) =>
  Array.from({ length: 910 }, (_, index) => {
    const key = Buffer.alloc(28);
    key.writeUInt16BE(repeatLast && index === 909 ? 908 : index, 26);
    return Buffer.concat([
      Buffer.from("82581c", "hex"),
      key,
      Buffer.from("a14001", "hex"),
    ]);
  });

const shapes = [
  {
    label: "repeated policy id",
    items: [mintItem(), mintItem()],
    index: 1,
    repeated: true,
  },
  {
    label: "distinct ascending policy ids",
    items: [mintItem(), mintItem("a14001", "22")],
    index: 1,
    repeated: false,
  },
  {
    label: "repeated asset name",
    items: [mintItem("a3410101410201410201")],
    index: 0,
    repeated: true,
  },
  {
    label: "distinct ascending asset names",
    items: [mintItem("a3410101410201410301")],
    index: 0,
    repeated: false,
  },
  {
    label: "maximum item, repeated last name",
    items: [maximumItemRepeatingLastName()],
    index: 0,
    repeated: true,
  },
  {
    label: "maximum item, distinct last name",
    items: [maximumCanonicalMintItem()],
    index: 0,
    repeated: false,
  },
  {
    label: "910th item repeats a policy id",
    items: latePolicyItems(true),
    index: 909,
    repeated: true,
  },
  {
    label: "910th item, distinct policy id",
    items: latePolicyItems(false),
    index: 909,
    repeated: false,
  },
] as const;

describe("mintItemNonCanonical over a mint that repeats a key", () => {
  it("refuses to admit a forced transaction whose mint repeats a policy id or an asset name", () => {
    const forcedMint = (items: readonly Buffer[]) =>
      screenMidgardForcedTxAdmission(
        makeNativeTx({
          spendInputCbors: [
            encodeMidgardSpendInputItem({
              txId: Buffer.alloc(32, 0x31),
              outputIndex: 0,
            }),
          ],
          fee: 0n,
          mintPreimageCbor: mintField(...items),
        }),
      );
    for (const shape of shapes) {
      const refusal = forcedMint(shape.items);
      if (!shape.repeated) {
        expect(refusal, shape.label).toBeNull();
        continue;
      }
      expect(refusal, shape.label).toStrictEqual({
        check: "mint_items_are_valid(mint)",
        detail: expect.stringMatching(
          /^item \d+: (policy ids|asset names) out of order$/u,
        ),
      });
    }
  });

  it("convicts every repeat through a real step-04 mint, refuses every distinct twin at the same shape, then removes the block", async () => {
    const harness = await makeFaultProofEmulatorHarness({
      contractOptions: {
        realMintItemNonCanonical: true,
        alwaysFraudProofCatalogue: true,
      },
    });
    const registered = await registeredContracts(harness);
    const { contracts, references, common } = registered;
    const txs = shapes.map((shape, index) =>
      makeNativeTx({
        spendInputCbors: [],
        fee: BigInt(index + 1),
        mintPreimageCbor: mintField(...shape.items),
      }),
    );
    expect(txs[4]!.body.mintPreimageCbor).toHaveLength(32_768);
    const committed = await committedInclusions(txs);
    const setup = await setupFraudulentBlock({
      funderLucid: harness.funderLucid,
      emulator: harness.emulator,
      contracts: harness.contracts,
      catalogue: registered.catalogue,
      fixture: {
        transactionsRoot: committed.transactionsRoot,
        l2TransactionCount: BigInt(txs.length),
      },
    });
    const certificateReference = await registered.publishReferences();
    let transactions = 0;
    let peakMemory = 0n;
    let peakSteps = 0n;
    let peakBytes = 0;
    const measured = async <T>(action: () => Promise<T>) => {
      const result = await captureEmulatorSubmission(harness.emulator, action);
      for (const measurement of result.measurements) {
        expect(measurement.l1ByteMargin).toBeGreaterThan(0);
        transactions += 1;
        if (measurement.executionMemory > peakMemory)
          peakMemory = measurement.executionMemory;
        if (measurement.executionSteps > peakSteps)
          peakSteps = measurement.executionSteps;
        peakBytes = Math.max(peakBytes, measurement.completeSignedBytes);
      }
      return result.result;
    };
    let fraudulentHeaderHash = "";
    for (const [i, shape] of shapes.entries()) {
      const inclusion = committed.inclusions[i]!;
      const evidence = evidenceOf(
        acceptedVerdictSubject(inclusion.nativeTxId),
        txs[i]!,
        shape.index,
      );
      expect(evidence.canonical, shape.label).toBe(!shape.repeated);
      const source = {
        evidence,
        nativeTxCompactCbor: inclusion.nativeTxCompactCbor,
        witnessSetCompactCbor: witnessSetCborOf(txs[i]!),
      };
      const init = await measured(() =>
        registered.init(setup.fraudulentBlockOutRef),
      );
      fraudulentHeaderHash = init.result.fraudulentHeaderHash;
      const bound = await measured(async () =>
        submitMintItemNonCanonicalStep01Accepted({
          lucid: harness.proverLucid,
          blueprint: harness.realBlueprint,
          network: "Custom",
          contracts,
          signer: harness.proverSigner,
          finding: evidence,
          threadUtxo: await registered.threadUtxoOf(
            init.result.txHash,
            init.result.firstStepOutputIndex,
          ),
          threadToken: {
            unit: init.result.computationThreadUnit,
            fraudulentHeaderHash,
          },
          stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
          txInclusion: inclusion,
          referenceScriptUtxo: references[0]!,
          witnessReferenceScripts: harness.witnessReferenceScripts,
        }),
      );
      // Step 02 walks the field to the item, then step 03 scans it.
      let threadOutRef = bound.nextThreadOutRef;
      let carriage:
        | Awaited<ReturnType<typeof submitMintItemNonCanonicalStep02>>
        | undefined;
      for (;;) {
        const opened = await measured(() =>
          submitMintItemNonCanonicalStep02({
            ...common(threadOutRef, 1),
            ...source,
            publishCarriage: true,
            publishedCarriageUtxos: carriage?.publishedCarriageUtxos,
            certificateUtxo: carriage?.certificateUtxo,
            certificateReferenceScriptUtxo: certificateReference,
          }),
        );
        carriage ??= opened;
        threadOutRef = opened.nextThreadOutRef;
        if (opened.terminal) break;
      }
      for (;;) {
        const scanned = await measured(() =>
          submitMintItemNonCanonicalStep03({
            ...common(threadOutRef, 2),
            ...source,
            publishedCarriageUtxos: carriage.publishedCarriageUtxos,
            certificateUtxo: carriage.certificateUtxo,
            certificateReferenceScriptUtxo: certificateReference,
          }),
        );
        threadOutRef = scanned.nextThreadOutRef;
        if (scanned.terminal) break;
      }
      if (shape.repeated) {
        const minted = await measured(() =>
          submitMintItemNonCanonicalStep04({
            ...common(threadOutRef, 3),
            evidence,
            witnessReferenceScripts: harness.witnessReferenceScripts,
          }),
        );
        expect(minted.fraudProofUnit, shape.label).toBeTruthy();
      } else {
        await expectOnchainRefusal(
          () =>
            submitMintStep04Raw({
              ...common(threadOutRef, 3),
              witnessReferenceScripts: harness.witnessReferenceScripts,
            }),
          {
            refusedBy: "fraud_proofs/mint_item_non_canonical/step_04",
            check: /^Validator returned false$/u,
          },
        );
      }
    }
    const removed = await measured(() =>
      registered.removal(fraudulentHeaderHash),
    );
    expect(removed.result.fraudCategoryId).toBe("00000036");
    const [txHash, outputIndex] = setup.fraudulentBlockOutRef.split("#");
    expect(
      await harness.proverLucid.utxosByOutRef([
        { txHash: txHash!, outputIndex: Number(outputIndex) },
      ]),
    ).toHaveLength(0);
    console.info(
      `mintItemNonCanonical repeated keys: ${shapes.length.toString()} shapes, ${transactions.toString()} transactions; peak ${peakMemory.toString()} memory, ${peakSteps.toString()} CPU, ${peakBytes.toString()} signed bytes`,
    );
  }, 1_800_000);
});
